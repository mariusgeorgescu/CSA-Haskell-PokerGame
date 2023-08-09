{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cards (Card)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.ECC.Generate (generate, generateQ)
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA
import Crypto.Random
import Crypto.PubKey.ECC.DH
import Data.Binary (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
  ( ByteString,
    fromStrict,
    toStrict,
  )
import System.Random 
import Crypto.Number.ModArithmetic (inverse)
import Data.Maybe


-- Generate a public key from a secret key

generatePublicKey curve sk = calculatePublic curve sk

-- Encrypt a point P using a secret key s

encrypt curve p s = pointMul curve s p

-- Decrypt a point C using a secret key s
decrypt :: Curve -> Point -> Integer -> Point
decrypt curve c s = pointMul curve sInv c
  where
    sInv = fromJust $ inverse s (ecc_n (common_curve curve))

-- Example usage
main :: IO ()
main = do
  -- Curve parameters (e.g., secp256k1)
  let curve = getCurveByName SEC_p256k1

  -- Generate Alice's secret and public keys
  (alicePubk, alicePrivk) <- generate curve
  (bobPubk, bobPrivk) <- generate curve
  let value = 10
  let point = generateQ curve value

  let alicePubk2 = generateQ curve (private_d alicePrivk)
  print $ "ALICE PUB:"  ++ show (public_q alicePubk)
  print $ "ALICE PUB:"  ++ show alicePubk2
  
  print  $ "Point                   : " ++ show point
  let encryptedA = encrypt curve point (private_d alicePrivk)
  let encryptedA' = generateQ curve (value * (private_d alicePrivk))
  print  $ "Encrypted by Alice      : "  ++ show encryptedA
  print  $ "Encrypted by Alice      : "  ++ show encryptedA'
  let encryptedAB = encrypt curve encryptedA (private_d bobPrivk)
  print  $ "Encrypted by Alice & Bob: " ++ show encryptedAB


  let decryptedB = decrypt curve encryptedAB (private_d alicePrivk)
  print  $ "                        : " ++ show decryptedB
  let decryptedBA = decrypt curve decryptedB (private_d bobPrivk)
  print  $ "                        : " ++ show decryptedBA
  print  $ decryptedBA == point

  -- -- Print the original point and the decrypted point
  -- putStrLn ("Original point: " ++ show point)
  -- putStrLn ("Decrypted point: " ++ show decrypted)

-- cRsaEncryptBS :: PublicKey -> BSC.ByteString -> BSC.ByteString
-- cRsaEncryptBS = ep

-- cRsaDecryptBS :: PrivateKey -> BSC.ByteString -> BSC.ByteString
-- cRsaDecryptBS = dp Nothing

-- demo :: IO ()
-- demo =
--   -- Generate keys for two players
--   do
--     let keyLength = 128
--     (pub1, priv1) <- generate keyLength 3
--     (pub2, priv2) <- generate keyLength 3
--     putStrLn $ "\nSimulate card commutative encryption and decryption\n"
--     let card = ("Ah" :: Card)
--     putStrLn $ "Card: " ++ show card
--     let encodedCard = encode card
--     putStrLn $ "Encoded Card: " ++ show encodedCard
--     putStrLn $ "Lenght of encoded card is: " ++ (show . BS.length . BSC.toStrict) encodedCard
--     let encryptedCard1 = cRsaEncryptBS pub1 . BSC.toStrict $ encodedCard
--     putStrLn "\nEncrypt card:"
--     putStrLn $ "Encrypted by 1st pubkey: " ++ show encryptedCard1
--     let encryptedCard2 = cRsaEncryptBS pub2 encryptedCard1
--     putStrLn $ "Encrypted by 2nd pubkey: " ++ show encryptedCard2
--     putStrLn "\nDecrypt in reverse order"
--     let decryptedCard1 = cRsaDecryptBS priv1 encryptedCard2
--     putStrLn $ "Decrypted by 1st pubkey: " ++ show decryptedCard1
--     let decryptedCard2 = cRsaDecryptBS priv2 decryptedCard1
--     putStrLn $ "Decrypted by 2nd pubkey: " ++ show decryptedCard2
--     -- print $ BS.length decryptedCard2
--     let decodedCard =
--           decode @Card . BSC.fromStrict . BS.drop (keyLength - 2) $ decryptedCard2
--     putStrLn $ "Decoded Card: " ++ show decodedCard
