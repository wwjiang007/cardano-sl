module Pos.Util.JsonLog.Events
       ( JLEvent(..)
       , JLTxS (..)
       , JLTxR (..)
       , JLMemPool (..)
       , MemPoolModifyReason (..)
       , JLBlock (..)
       , JLTimedEvent(..)
       , JLSlotId
       ) where

import           Universum
import           Data.Aeson.TH (deriveJSON)

import           Serokell.Aeson.Options (defaultOptions)
import           Pos.Communication.Relay.Logic (InvReqDataFlowLog)

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

-- | Json log of one block with corresponding 'BlockId'.
data JLBlock = JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    } deriving Show

-- | Json log of one transaction sent from the (light) wallet.
data JLTxS = JLTxS
    { jlsNodeId :: Text
    , jlsTxId   :: Text
    , jlsInvReq :: InvReqDataFlowLog
    } deriving Show

-- | Json log of one transaction being received by a node.
data JLTxR = JLTxR
    { jlrTxId  :: Text
    , jlrError :: Maybe Text
    } deriving Show

-- | Enumeration of all reasons for modifying the mempool.
data MemPoolModifyReason =
      -- | Apply a block created by someone else.
      ApplyBlock
      -- | Apply a block, with rollback
    | ApplyBlockWithRollback
      -- | Apply a block created by us.
    | CreateBlock
      -- | Include a transaction. It came from this peer.
    | ProcessTransaction
      -- TODO COMMENT
    | Custom Text
      -- TODO COMMENT
    | Unknown
    deriving Show

-- | Json log of one mempool modification.
data JLMemPool = JLMemPool
    { -- | Reason for modifying the mempool
      jlmReason      :: MemPoolModifyReason
      -- | Queue length when trying to modify the mempool (not including this
      --   modifier, so it could be 0).
    , jlmQueueLength :: Int
      -- | Time spent waiting for the lock (microseconds)
    , jlmWait        :: Integer
      -- | Time spent doing the modification (microseconds, while holding the lock).
    , jlmModify      :: Integer
      -- | Size of the mempool before the modification.
    , jlmSizeBefore  :: Int
      -- | Size of the mempool after the modification.
    , jlmSizeAfter   :: Int
      -- | How much memory was allocated during the modification.
    , jlmAllocated   :: Int
    } deriving Show

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
             | JLTxSent JLTxS
             | JLTxReceived JLTxR
             | JLMemPoolEvent JLMemPool
  deriving (Show, Generic)

-- | 'JLEvent' with 'Timestamp' -- corresponding time of this event.
data JLTimedEvent = JLTimedEvent
    { jlTimestamp :: Integer
    , jlEvent     :: JLEvent
    } deriving Show

$(deriveJSON defaultOptions ''MemPoolModifyReason)
$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTimedEvent)
$(deriveJSON defaultOptions ''JLTxS)
$(deriveJSON defaultOptions ''JLTxR)
$(deriveJSON defaultOptions ''JLMemPool)