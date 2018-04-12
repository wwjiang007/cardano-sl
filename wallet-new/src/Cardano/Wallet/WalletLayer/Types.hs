module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Lenses
    , pwlCreateWallet
    , pwlGetWalletIds
    , pwlGetWallet
    , pwlUpdateWallet
    , pwlDeleteWallet

    , pwlCreateAccount
    , pwlGetAccounts
    , pwlGetAccount
    , pwlUpdateAccount
    , pwlDeleteAccount

    , pwlCreateAddress
    , pwlGetAddresses
    , pwlIsAddressValid
    ) where

import           Universum

import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, AddressValidity,
                                              NewAccount, NewAddress, NewWallet, Wallet,
                                              WalletAddress, WalletId, WalletUpdate)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))


-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet   :: NewWallet -> m Wallet
    , _pwlGetWalletIds   :: m [WalletId]
    , _pwlGetWallet      :: WalletId -> m (Maybe Wallet)
    , _pwlUpdateWallet   :: WalletId -> WalletUpdate -> m Wallet
    , _pwlDeleteWallet   :: WalletId -> m ()
    -- * accounts
    , _pwlCreateAccount  :: WalletId -> NewAccount -> m Account
    , _pwlGetAccounts    :: WalletId -> m [Account]
    , _pwlGetAccount     :: WalletId -> AccountIndex -> m (Maybe Account)
    , _pwlUpdateAccount  :: WalletId -> AccountIndex -> AccountUpdate -> m Account
    , _pwlDeleteAccount  :: WalletId -> AccountIndex -> m ()
    -- * addresses
    , _pwlCreateAddress  :: NewAddress -> m WalletAddress
    , _pwlGetAddresses   :: WalletId -> Maybe AccountIndex -> m [WalletAddress]
    , _pwlIsAddressValid :: WalletAddress -> m AddressValidity
    }

makeLenses ''PassiveWalletLayer

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

