{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Functions where

import           Universum

import           Data.List.NonEmpty (fromList)

import           Control.Lens ((+~))
import           Test.QuickCheck (Gen, arbitrary, elements, frequency, generate)

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex, NewAccount (..),
                                              NewAddress (..), Payment (..),
                                              PaymentDistribution (..), PaymentSource (..),
                                              Transaction (..), Wallet (..), WalletAddress (..),
                                              WalletId)

import           Cardano.Wallet.Client (ClientError (..), WalletClient (..))

import           Error
import           Types


-- | The top function that we need to run in order
-- to test the backend.
runActionCheck
    :: (WalletTestMode m)
    => WalletClient m
    -> WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletClient walletState actionProb = do
    action <- chooseAction actionProb
    runAction walletClient walletState action


-- | Here we run the actions.
runAction
    :: (WalletTestMode m)
    => WalletClient m
    -> WalletState
    -> Action
    -> m WalletState
-- Wallets
runAction wc ws  CreateWallet = do
    newWall <-  liftIO $ generate arbitrary
    result  <-  respToRes $ postWallet wc newWall

    checkInvariant
        (walBalance result == minBound)
        (WalletBalanceNotZero result)

    -- Modify wallet state accordingly.
    pure $ ws
        & wallets    .~ ws ^. wallets <> [result]
        & actionsNum +~ 1

runAction wc ws GetWallets   = do
    -- We choose from the existing wallets.
    result  <-  respToRes $ getWallets wc

    checkInvariant
        (length result == length (ws ^. wallets))
        (LocalWalletsDiffers result)

    -- No modification required.
    pure $ ws
        & actionsNum +~ 1

runAction wc ws GetWallet    = do
    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement (ws ^. wallets)
    result  <-  respToRes $ getWallet wc (walId wallet)

    checkInvariant
        (walBalance result == minBound)
        (LocalWalletDiffers result)

    -- No modification required.
    pure $ ws
        & actionsNum +~ 1


-- Accounts
runAction wc ws  CreateAccount = do

    -- TODO(ks): Don't we need to know the wallet we want to add the account to?
    -- wallet     <- pickRandomElement localWallets
    let localWallets = ws ^. wallets

    -- Precondition, we need to have wallet in order
    -- to create an account.
    guard (length localWallets >= 1)

    newAcc  <-  liftIO $ generate generateNewAccount
    result  <-  respToRes $ postAccount wc newAcc

    checkInvariant
        (accAmount result == minBound)
        (AccountBalanceNotZero result)

    -- Modify wallet state accordingly.
    pure $ ws
        & accounts   .~ ws ^. accounts <> [result]
        & actionsNum +~ 1
  where
    -- | We don't want to memorize the passwords for now.
    generateNewAccount =
        NewAccount
            <$> pure Nothing
            <*> arbitrary

runAction wc ws GetAccounts   = do
    -- We choose from the existing wallets AND existing accounts.
    wallet  <-  pickRandomElement (ws ^. wallets)
    let walletId = walId wallet
    -- We get all the accounts.
    result  <-  respToRes $ getAccounts wc walletId

    checkInvariant
        (length result == length (ws ^. accounts))
        (LocalAccountsDiffers result)

    -- Modify wallet state accordingly.
    pure $ ws
        & actionsNum +~ 1

runAction wc ws GetAccount    = do
    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement (ws ^. accounts)
    let walletId = accWalletId account

    result  <-  respToRes $ getAccount wc walletId (accIndex account)

    checkInvariant
        (accAmount result == minBound)
        (LocalAccountDiffers result)

    -- Modify wallet state accordingly.
    pure $ ws
        & actionsNum +~ 1

-- Addresses
runAction wc ws CreateAddress = do

    -- The precondition is that we must have accounts.
    -- If we have accounts, that presupposes that we have wallets,
    -- which is the other thing we need here.
    let localAccounts = ws ^. accounts
    guard (length localAccounts >= 1)

    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement (ws ^. accounts)
    let walletId = accWalletId account

    let newAddress = createNewAddress walletId (accIndex account)

    result  <-  respToRes $ postAddress wc newAddress

    checkInvariant
        (addrBalance result == minBound)
        (AddressBalanceNotZero result)

    -- Modify wallet state accordingly.
    pure $ ws
        & addresses  .~ ws ^. addresses <> [result]
        & actionsNum +~ 1
  where
    createNewAddress :: WalletId -> AccountIndex -> NewAddress
    createNewAddress wId accIndex = NewAddress
        { newaddrSpendingPassword = Nothing
        , newaddrAccountIndex     = accIndex
        , newaddrWalletId         = wId
        }

-- Transactions
runAction wc ws CreateTransaction = do

    let localAccounts  = ws ^. accounts
    let localAddresses = ws ^. addresses

    let localAccsWithMoney = filter ((> minBound) . accAmount) localAccounts

    -- | The preconditions we need to generate a transaction.
    -- We need to have an account and two addresses.
    -- We also need money to execute a transaction.
    guard (length localAccounts       >= 1)
    guard (length localAddresses      >= 2)
    guard (length localAccsWithMoney  >= 1)

    -- From which source to pay.
    accountSource <- pickRandomElement localAccsWithMoney

    let _accountSourceMoney = accAmount accountSource

    -- We should probably have a sensible minimum value.
    -- moneyAmount <- liftIO $ mkCoin $ generate $ choose (0, getCoin accountSourceMoney)
    moneyAmount <- liftIO $ generate arbitrary

    let paymentSource =
            PaymentSource
                { psWalletId     = accWalletId accountSource
                , psAccountIndex = accIndex    accountSource
                }

    addressDestination <- pickRandomElement localAddresses

    let paymentDistribution =
            PaymentDistribution
                { pdAddress = addrId addressDestination
                , pdAmount  = moneyAmount
                }

    let newPayment =  createNewPayment
                          paymentSource
                          [paymentDistribution]

    result  <-  respToRes $ postTransaction wc newPayment

    checkInvariant
        (txAmount result == moneyAmount)
        (InvalidTransactionState result)

    -- Modify wallet state accordingly.
    pure $ ws
        & transactions  .~ ws ^. transactions <> [result]
        & actionsNum    +~ 1

  where
    createNewPayment :: PaymentSource -> [PaymentDistribution] -> Payment
    createNewPayment ps pd = Payment
        { pmtSource           = ps
        , pmtDestinations     = fromList pd
        , pmtGroupingPolicy   = Nothing
        -- ^ Simple for now.
        , pmtSpendingPassword = Nothing
        }


-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


-- | Generate action randomly, depending on the action distribution.
chooseActionGen
    :: ActionProbabilities
    -> Gen Action
chooseActionGen aProb =
    frequency $ map (\(a, p) -> (getProbability p, pure a)) aProb


-- | Generate action from the generator.
chooseAction
    :: (WalletTestMode m)
    => ActionProbabilities
    -> m Action
chooseAction = liftIO . generate . chooseActionGen


-- | We are not interested in the @WalletResponse@ for now.
respToRes
    :: forall m a. (MonadThrow m)
    => m (Either ClientError (WalletResponse a))
    -> m a
respToRes resp = do
    result <- resp
    either throwM (pure . wrData) result


-- | Pick a random element using @IO@.
pickRandomElement :: (MonadIO m) => [a] -> m a
pickRandomElement = liftIO . generate . elements


-- | A util function for checking the validity of invariants.
checkInvariant
    :: forall m. (MonadThrow m)
    => Bool
    -> WalletTestError
    -> m ()
checkInvariant True  _             = pure ()
checkInvariant False walletTestErr = throwM walletTestErr


-- | Output for @Text@.
printT :: Text -> IO ()
printT = putStrLn
