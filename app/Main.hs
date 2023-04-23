{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Cards                  (Card)
import           Crypto.PubKey.RSA      (PrivateKey, PublicKey, generate)
import           Crypto.PubKey.RSA.Prim
import           Data.Binary            (decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC (ByteString, fromStrict, pack,
                                                toStrict, unpack)
import qualified Data.ByteString as BS
import Foreign.Storable (Storable(sizeOf))

cRsaEncryptBS :: PublicKey -> BSC.ByteString -> BSC.ByteString
cRsaEncryptBS pubKey msg = ep pubKey msg

cRsaDecryptBS :: PrivateKey -> BSC.ByteString -> BSC.ByteString
cRsaDecryptBS privKey msg = dp Nothing privKey msg

main :: IO ()
main
    -- Generate keys for two players
 = do
  let keyLength = 512
  (pub1, priv1) <- generate keyLength 65537
  (pub2, priv2) <- generate keyLength 65537
    -- Simulate card encryption and decryption using ByteString
  let card = ("Ah" :: Card)
  print $ "Card: " ++ show card
  let encodedCard = encode card
  print $ "Encoded Card" ++ show encodedCard
  print $ BS.length . BSC.toStrict $ encodedCard
  let encryptedCard1 = cRsaEncryptBS pub1 . BSC.toStrict $ encodedCard
  print $ "e1 :" ++ show encryptedCard1
  let encryptedCard2 = cRsaEncryptBS pub2 encryptedCard1
  print $ "e2" ++ show encryptedCard2
  let decryptedCard1 = cRsaDecryptBS priv2 encryptedCard2
  print $ "d1" ++ show decryptedCard1
  let decryptedCard2 = cRsaDecryptBS priv1 decryptedCard1
  print $ "d2" ++ show decryptedCard2
  let decodedCard =
        decode @Card . BSC.fromStrict . BS.drop (keyLength - 2) $ decryptedCard2
  print $ "Decoded " ++ show decodedCard
