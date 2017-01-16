{-# LANGUAGE OverloadedStrings #-}

module Stack1 where

import Data.Binary.Put (runPut)
import Data.Foldable (fold)
import Data.Text
import qualified Data.ByteString.Lazy as BS

import Codec.JVM.ASM (mkClassFile, mkMethodDef)
import Codec.JVM.ASM.Code
import Codec.JVM.Class (ClassFile, putClassFile)
import Codec.JVM.Types
import Codec.JVM.Class

mainClass :: ClassFile
mainClass = mkClassFile java8 [] "Stack1" Nothing [] []
  [ mkMethodDef "Stack1" [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
    [
     iconst jint 3,
     gstore jint 1,
     gload jint 1,
     iconst jint 2,
     if_icmpne (fold $ [getstatic systemOut, bipush jint 42, invokevirtual printlnI, startLabel exitLabel, vreturn]) (fold $ [getstatic systemOut, bipush jint 43, invokevirtual printlnI, goto exitLabel])
    ]
  ]
    where
      systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
      printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt] void

exitLabel = mkLabel 44

generateHelloWorld :: IO ()
generateHelloWorld = BS.writeFile "Stack1.class" $ runPut . putClassFile $ mainClass

