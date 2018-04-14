-- | Binary serialization of slotting types.

module Pos.Binary.Core.Slotting
       (
       ) where

import           Universum

import           Control.Lens (_Left)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import qualified Pos.Core.Slotting as T
import           Pos.Util.Util (toCborError)

instance Bi T.Timestamp where
    encode (T.Timestamp ms) = encode . toInteger $ ms
    decode = T.Timestamp . fromIntegral <$> decode @Integer
    encodedSize (T.Timestamp ms) = encodedSize . toInteger $ ms

instance Bi T.TimeDiff where
    encode = encode . toInteger
    decode = fromInteger <$> decode
    encodedSize = encodedSize . toInteger

instance Bi T.EpochIndex where
    encode (T.EpochIndex epoch) = encode epoch
    decode = T.EpochIndex <$> decode
    encodedSize (T.EpochIndex epoch) = encodedSize epoch

instance HasProtocolConstants => Bi T.LocalSlotIndex where
    encode = encode . T.getSlotIndex
    decode = do
        word16 <- decode @Word16
        toCborError $
            over _Left ("decode@LocalSlotIndex: " <>) $
            T.mkLocalSlotIndex word16
    encodedSize = encodedSize . T.getSlotIndex

deriveSimpleBiCxt [t| HasProtocolConstants |] ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance HasProtocolConstants => Bi T.EpochOrSlot where
    encode (T.EpochOrSlot e) = encode e
    decode = T.EpochOrSlot <$> decode @(Either T.EpochIndex T.SlotId)
    encodedSize (T.EpochOrSlot e) = encodedSize e

instance Bi T.SlotCount where
    encode = encode . T.getSlotCount
    decode = T.SlotCount <$> decode
    encodedSize = encodedSize . T.getSlotCount
