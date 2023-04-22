{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.Prim
import Crypto.Random
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)


cRsaEncryptBS :: PublicKey -> ByteString -> ByteString
cRsaEncryptBS pubKey msg = ep pubKey msg

cRsaDecryptBS :: PrivateKey -> ByteString -> ByteString
cRsaDecryptBS privKey msg = dp Nothing privKey msg

main :: IO ()
main = do
    -- Generate keys for two players
    (pub1, priv1) <- generate 256 3
    (pub2, priv2) <- generate 256 3
 
    -- Simulate card encryption and decryption using ByteString
    let card = pack "42" -- Assuming a small card identifier
    let encryptedCard1 = cRsaEncryptBS pub1 card
    print $ "e1 :" ++ show encryptedCard1

    let encryptedCard2 = cRsaEncryptBS pub2 encryptedCard1
    print $ "e2" ++ show encryptedCard2
    
    let decryptedCard1 = cRsaDecryptBS priv2 encryptedCard2
    print $ "d1" ++ show decryptedCard1
    
    let decryptedCard2 = cRsaDecryptBS priv1 decryptedCard1
    print "result"
 
    putStrLn $ "Decrypted card: " ++ unpack decryptedCard2