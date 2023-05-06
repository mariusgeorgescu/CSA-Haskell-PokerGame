{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module CardsCrypt where

import           Cards                  (Card)
import           Crypto.PubKey.RSA      (PrivateKey, PublicKey, generate)
import           Crypto.PubKey.RSA.Prim (dp, ep)
import           Data.Binary            (decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC (ByteString, fromStrict,
                                                toStrict)

cRsaEncryptBS :: PublicKey -> BSC.ByteString -> BSC.ByteString
cRsaEncryptBS = ep

cRsaDecryptBS :: PrivateKey -> BSC.ByteString -> BSC.ByteString
cRsaDecryptBS = dp Nothing


-- encryptCardRsa :: PublicKey -> Card -> BSC.ByteString
-- encryptCardRsa pubKey = cRsaEncryptBS pubKey . BSC.toStrict . encode

-- decryptCardRsa :: PrivateKey -> BSC.ByteString -> Card
-- decryptCardRsa privKey bs =
--   let keylen = BS.length bsÏƒ
--    in decode @Card . BSC.fromStrict . BS.drop (keylen - 2) $
--       cRsaDecryptBS privKey bs
demo :: IO ()
demo
    -- Generate keys for two players
 = do
  let keyLength = 128
  (pub1, priv1) <- generate keyLength 3
  (pub2, priv2) <- generate keyLength 3
    -- Simulate card encryption and decryption using ByteString
  let card = ("Ah" :: Card)
  putStrLn $ "Card: " ++ show card
  let encodedCard = encode card
  putStrLn $ "Encoded Card" ++ show encodedCard
  print $ BS.length . BSC.toStrict $ encodedCard
  let encryptedCard1 = cRsaEncryptBS pub1 . BSC.toStrict $ encodedCard
  putStrLn $ "e1 :" ++ show encryptedCard1
  let encryptedCard2 = cRsaEncryptBS pub2 encryptedCard1
  putStrLn $ "e2 :" ++ show encryptedCard2
  let decryptedCard1 = cRsaDecryptBS priv1 encryptedCard2
  putStrLn $ "d1 :" ++ show decryptedCard1
  let decryptedCard2 = cRsaDecryptBS priv2 decryptedCard1
  putStrLn $ "d2 :" ++ show decryptedCard2
  -- print $ BS.length decryptedCard2
  let decodedCard =
        decode @Card . BSC.fromStrict . BS.drop (keyLength - 2) $ decryptedCard2
  putStrLn $ "Decoded " ++ show decodedCard
