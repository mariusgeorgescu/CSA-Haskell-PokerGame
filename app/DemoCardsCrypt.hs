

module Main where

import Crypto.PubKey.ECC.Generate (generate, generateQ)
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA

import Crypto.PubKey.ECC.DH
import Crypto.Number.ModArithmetic (inverse)
import Data.Maybe
import PokerGameApp (gameMessage)


-- Generate a public key from a secret key

generatePublicKey :: Curve -> PrivateNumber -> PublicPoint
generatePublicKey  = calculatePublic 

-- Encrypt a point P using a secret key s

encrypt :: Curve -> Point -> Integer -> Point
encrypt curve= flip $ pointMul curve 

-- Decrypt a point C using a secret key s
decrypt :: Curve -> Point -> Integer -> Point
decrypt curve c s = pointMul curve sInv c
  where
    sInv = fromJust $ inverse s (ecc_n (common_curve curve))

-- Example usage
main :: IO ()
main = do
  gameMessage "Simulating card commutative encryption and decryption"
  -- Curve parameters (e.g., secp256k1)
  let curve = getCurveByName SEC_p256k1

  -- Generate Alice's secret and public keys
  (alicePubk, alicePrivk) <- generate curve
  (_bobPubk, bobPrivk) <- generate curve
  let value = 10
  let point = generateQ curve value

  let alicePubk2 = generateQ curve (private_d alicePrivk)
  print $ "ALICE PUB                :"  ++ show (public_q alicePubk)
  print $ "ALICE PUB v2             :"  ++ show alicePubk2
  
  print  $ "Point                   : " ++ show point
  let encryptedA = encrypt curve point (private_d alicePrivk)
  let encryptedA' = generateQ curve (value * private_d alicePrivk)
  print  $ "Encrypted by Alice      : "  ++ show encryptedA
  print  $ "Encrypted by Alice  v2  : "  ++ show encryptedA'

  let encryptedAB = encrypt curve encryptedA (private_d bobPrivk)
  print  $ "Encrypted by Alice & Bob: " ++ show encryptedAB


  let decryptedB = decrypt curve encryptedAB (private_d alicePrivk)
  print  $ "Decrypted by Alice      : " ++ show decryptedB
  let decryptedBA = decrypt curve decryptedB (private_d bobPrivk)
  print  $ "Decrypted by Bob        : " ++ show decryptedBA
  print  $ decryptedBA == point

