{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cards (Card)
import Crypto.PubKey.RSA (PrivateKey, PublicKey, generate)
import Crypto.PubKey.RSA.Prim (dp, ep)
import Data.Binary (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
  ( ByteString,
    fromStrict,
    toStrict,
  )

cRsaEncryptBS :: PublicKey -> BSC.ByteString -> BSC.ByteString
cRsaEncryptBS = ep

cRsaDecryptBS :: PrivateKey -> BSC.ByteString -> BSC.ByteString
cRsaDecryptBS = dp Nothing

demo :: IO ()
demo =
  -- Generate keys for two players
  do
    let keyLength = 128
    (pub1, priv1) <- generate keyLength 3
    (pub2, priv2) <- generate keyLength 3
    putStrLn $ "\nSimulate card commutative encryption and decryption\n"
    let card = ("Ah" :: Card)
    putStrLn $ "Card: " ++ show card
    let encodedCard = encode card
    putStrLn $ "Encoded Card: " ++ show encodedCard
    putStrLn $ "Lenght of encoded card is: " ++ (show . BS.length . BSC.toStrict) encodedCard
    let encryptedCard1 = cRsaEncryptBS pub1 . BSC.toStrict $ encodedCard
    putStrLn "\nEncrypt card:"
    putStrLn $ "Encrypted by 1st pubkey: " ++ show encryptedCard1
    let encryptedCard2 = cRsaEncryptBS pub2 encryptedCard1
    putStrLn $ "Encrypted by 2nd pubkey: " ++ show encryptedCard2
    putStrLn "\nDecrypt in reverse order"
    let decryptedCard1 = cRsaDecryptBS priv1 encryptedCard2
    putStrLn $ "Decrypted by 1st pubkey: " ++ show decryptedCard1
    let decryptedCard2 = cRsaDecryptBS priv2 decryptedCard1
    putStrLn $ "Decrypted by 2nd pubkey: " ++ show decryptedCard2
    -- print $ BS.length decryptedCard2
    let decodedCard =
          decode @Card . BSC.fromStrict . BS.drop (keyLength - 2) $ decryptedCard2
    putStrLn $ "Decoded Card: " ++ show decodedCard

main :: IO ()
main = demo