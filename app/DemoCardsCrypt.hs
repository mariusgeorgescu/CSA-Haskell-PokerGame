{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cards (Card)
import Criterion.Main
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.RSA (PrivateKey, PublicKey, generate)
import Crypto.PubKey.RSA.Prim (dp, ep)
import Crypto.PubKey.RSA.Types
import Data.Binary (decode, encode)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
  ( ByteString,
    fromStrict,
    toStrict,
  )

-- cRsaEncryptBS :: PublicKey -> BSC.ByteString -> BSC.ByteString
-- cRsaEncryptBS = ep

cRsaEncryptBS2 :: PublicKey -> BSC.ByteString -> BSC.ByteString
cRsaEncryptBS2 pk m = i2ospOf_ (public_size pk) $ modExp (os2ip m) (public_e pk) (public_n pk)

-- cRsaDecryptBS :: PrivateKey -> BSC.ByteString -> BSC.ByteString
-- cRsaDecryptBS = dp Nothing

cRsaDecryptBS2 :: PrivateKey -> BSC.ByteString -> BSC.ByteString
cRsaDecryptBS2 pk c = i2ospOf_ (private_size pk) $ modExp (os2ip c) (private_d pk) (private_n pk)

modExp :: Integer -> Integer -> Integer -> Integer
modExp 1 e m = 1
modExp b 0 m = 1
modExp b 1 m = b `mod` m
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where
    t = if testBit e 0 then b `mod` m else 1

modExp2 :: Integer -> Integer -> Integer -> Integer
modExp2 b e m
  | b == 1 = b
  | e == 0 = 1
  | e == 1 = b `mod` m
  | even e =
      let p = modExp2 b (e `div` 2) m `mod` m
       in (p ^ (2 :: Integer)) `mod` m
  | otherwise = (b * modExp2 b (e - 1) m) `mod` m

demo :: ((PublicKey, PrivateKey), (PublicKey, PrivateKey), Int ) -> IO()
demo ((pub1, priv1), (pub2, priv2), keyLength)  =
  -- Generate keys for two players
  do


    let card = ("Ah" :: Card)
    putStrLn $ "Card: " ++ show card
    let encodedCard = encode card
    putStrLn $ "Encoded Card: " ++ show encodedCard
    putStrLn $ "Lenght of encoded card is: " ++ (show . BS.length . BSC.toStrict) encodedCard
    let encryptedCard1 = cRsaEncryptBS2 pub1 . BSC.toStrict $ encodedCard
    putStrLn "\nEncrypt card:"
    putStrLn $ "Encrypted by 1st pubkey: " ++ show encryptedCard1
    let encryptedCard2 = cRsaEncryptBS2 pub2 encryptedCard1
    putStrLn $ "Encrypted by 2nd pubkey: " ++ show encryptedCard2
    putStrLn "\nDecrypt in reverse order"
    let decryptedCard1 = cRsaDecryptBS2 priv1 encryptedCard2
    putStrLn $ "Decrypted by 1st pubkey: " ++ show decryptedCard1
    let decryptedCard2 = cRsaDecryptBS2 priv2 decryptedCard1
    putStrLn $ "Decrypted by 2nd pubkey: " ++ show decryptedCard2
    print $ BS.length decryptedCard2
    print $ private_e priv1
    let decodedCard =
          decode @Card . BSC.fromStrict . BS.drop (keyLength - 2) $ decryptedCard2
    putStrLn $ "Decoded Card: " ++ show decodedCard
    print $ modExp 2 7 50

main :: IO ()
main = do
  let keyLength = 512
  putStrLn $ "\nGenerating keys\n"
  a <- generate keyLength 3
  b <- generate keyLength 3
  putStrLn $ "\nSimulate card commutative encryption and decryption\n"
  defaultMain [bench "demo" $ whnf demo (a,b, keyLength)]
  defaultMain [bench "demo" $ whnf demo (a,b, keyLength)]