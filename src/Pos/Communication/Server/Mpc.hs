{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( module Mpc
       , mpcListeners
       ) where

import           Control.TimeWarp.Rpc        (Listener (..))
-- import           Universum

import           Pos.Communication.Types.Mpc as Mpc
import qualified Pos.State                   as St
import           Pos.WorkMode                (WorkMode)

mpcListeners :: WorkMode m => [Listener m]
mpcListeners =
    [ Listener handleCommitment
    , Listener handleOpening
    , Listener handleShares
    , Listener handleVssCertificate ]

handleCommitment :: WorkMode m => SendCommitment -> m ()
handleCommitment (SendCommitment pk c) = do
    -- TODO: actually check the commitment
    St.processCommitment pk c

-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleOpening :: WorkMode m => SendOpening -> m ()
handleOpening (SendOpening pk o) = do
    St.processOpening pk o

handleShares :: WorkMode m => SendShares -> m ()
handleShares (SendShares pk s) = do
    St.processShares pk s

handleVssCertificate :: WorkMode m => SendVssCertificate -> m ()
handleVssCertificate (SendVssCertificate pk c) = do
    St.processVssCertificate pk c
