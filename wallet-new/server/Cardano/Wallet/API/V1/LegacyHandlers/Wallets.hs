module Cardano.Wallet.API.V1.LegacyHandlers.Wallets where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot, askWalletDB, removeHistoryCache, setWalletSyncTip)
import qualified Pos.Wallet.Web.State.Storage as V0 hiding (removeHistoryCache, setWalletSyncTip)

import           Pos.Core (makePubKeyAddressBoot)
import           Pos.Util.Servant (encodeCType)
import           Pos.Crypto (PublicKey (..), firstHardened)
import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import           Cardano.Crypto.Wallet (xpub)
import qualified Data.IxSet.Typed as IxSet
import           Pos.Update.Configuration ()
import           Pos.StateLock (Priority (..), withStateLockNoMetrics)

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, MonadWalletLogicRead)
import           Servant
import           Test.QuickCheck (arbitrary, generate)
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58)
import           Data.Maybe (isNothing, fromJust)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers = newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> newExternalWallet
    :<|> newAddressPath

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet
    :: (MonadThrow m, MonadWalletLogic ctx m)
    => NewWallet
    -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do
    let newWalletHandler CreateWallet  = V0.newWallet
        newWalletHandler RestoreWallet = V0.restoreWallet
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (V1 backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta backupPhrase
    single <$> do
        v0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
        ss <- V0.askWalletSnapshot
        addWalletInfo ss v0wallet

-- | Returns the full (paginated) list of wallets.
listWallets :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
            => RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    ws <- V0.askWalletSnapshot
    respondWith params fops sops (IxSet.fromList <$> do
        (V0.getWalletsWithInfo ws >>= migrate @_ @[V1.Wallet]))

updatePassword
    :: (MonadWalletLogic ctx m)
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    ss <- V0.askWalletSnapshot
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> do
        wallet <- V0.getWallet wid'
        addWalletInfo ss wallet

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: (MonadThrow m, MonadWalletLogicRead ctx m) => WalletId -> m (WalletResponse Wallet)
getWallet wid = do
    ss <- V0.askWalletSnapshot
    wid' <- migrate wid
    wallet <- V0.getWallet wid'
    single <$> addWalletInfo ss wallet

addWalletInfo
    :: (MonadThrow m, MonadWalletLogicRead ctx m)
    => V0.WalletSnapshot
    -> V0.CWallet
    -> m Wallet
addWalletInfo snapshot wallet = do
    case V0.getWalletInfo (V0.cwId wallet) snapshot of
        Nothing ->
            throwM WalletNotFound
        Just walletInfo ->
            migrate (wallet, walletInfo)

updateWallet
    :: (V0.MonadWalletLogic ctx m)
    => WalletId
    -> WalletUpdate
    -> m (WalletResponse Wallet)
updateWallet wid WalletUpdate{..} = do
    ws <- V0.askWalletSnapshot
    wid' <- migrate wid
    assurance <- migrate uwalAssuranceLevel
    walletMeta <- maybe (throwM WalletNotFound) pure $ V0.getWalletMeta wid' ws
    updated <- V0.updateWallet wid' walletMeta
        { V0.cwName = uwalName
        , V0.cwAssurance = assurance
        }
    single <$> do
        -- reacquire the snapshot because we did an update
        ws' <- V0.askWalletSnapshot
        addWalletInfo ws' updated

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newExternalWallet
    :: (MonadThrow m, MonadWalletLogic ctx m)
    => NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet NewExternalWallet{..} = do
    let newWalletHandler CreateWallet  = createNewExternalWallet
        newWalletHandler RestoreWallet = error "TODO"
    walletMeta <- V0.CWalletMeta <$> pure newewalName
                                 <*> migrate newewalAssuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newWalletHandler newewalOperation walletMeta newewalExtPubKey
        ss <- V0.askWalletSnapshot
        addWalletInfo ss v0wallet

-- | Creates new external wallet.
--
-- There's no spending password, because it's assumed that
-- extenal wallet has its own security mechanism.
-- For example, in Ledger Nano S device it's 4 digits PIN-code.
--
-- There's no backup phrase as well, because it's assumed that
-- external wallet already received backup phrase, created
-- a secret key based on it and stored this key.
createNewExternalWallet
    :: (MonadThrow m, MonadWalletLogic ctx m)
    => V0.CWalletMeta
    -> Text
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedExtPubKey = do
    let rawExtPubKey = decodeBase58 bitcoinAlphabet . encodeUtf8 $ encodedExtPubKey
    when (isNothing rawExtPubKey) $ throwM (UnkownError "Ext public key is not in proper Base58-form.")

    let extPubKey = xpub $ fromJust rawExtPubKey
    when (isLeft extPubKey) $ throwM (UnkownError "Invalid ext public key.")

    let (xPub:_) = rights [extPubKey] -- We already know that it's 'Right'.
        walletId = encodeCType . makePubKeyAddressBoot $ PublicKey xPub
        isReady  = True -- A brand new wallet doesn't need syncing with the blockchain.

    V0.CWallet{..} <- V0.createWalletSafe walletId walletMeta isReady
    -- Can't return this result, since balances can change.

    let accountMeta    = V0.CAccountMeta { caName = "Initial account" }
        accountInit    = V0.CAccountInit { caInitWId = cwId, caInitMeta = accountMeta }
        includeUnready = True
        passphrase     = mempty
    void $ V0.newAccountIncludeUnready includeUnready
                                       (V0.DeterminedSeed firstHardened)
                                       passphrase
                                       accountInit
    db <- V0.askWalletDB
    V0.removeHistoryCache db walletId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> V0.setWalletSyncTip db walletId tip
    V0.getWallet walletId

-- | Creates a new BIP44 derivation path for an external wallet.
newAddressPath
    :: (MonadThrow m, MonadWalletLogic ctx m)
    => WalletId
    -> m (WalletResponse AddressPath)
newAddressPath _ =
    single <$> (liftIO $ generate arbitrary)
