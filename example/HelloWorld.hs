{-# LANGUAGE OverloadedStrings #-}

module HelloWorld where
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
mainClass = mkClassFile java8 [] "HelloWorld" Nothing [] []
  [ mkMethodDef "HelloWorld" [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
    [ getstatic systemOut
    , bipush jint 42
    , invokevirtual printlnI
    , vreturn ]
  ]
    where
      systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
      printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt] void

generateHelloWorld :: IO ()
generateHelloWorld = BS.writeFile "HelloWorld.class" $ runPut . putClassFile $ mainClass
