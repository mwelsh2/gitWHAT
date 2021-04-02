module Hash where
    import qualified Data.ByteString as BS
    import qualified Crypto.Hash.SHA1 as SHA1
    import Text.Hex as TH
    import Data.ByteString.Char8 as C

    hashString :: [Char] -> BS.ByteString
    hashString s = SHA1.hash (C.pack s)

    toHex :: BS.ByteString -> Text
    toHex = TH.encodeHex

    fromHex :: Text -> Maybe BS.ByteString
    fromHex = TH.decodeHex
