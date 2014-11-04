{-# LINE 1 "src/Example.hsc" #-}
{-# LANGUAGE CPP                         #-}
{-# LINE 2 "src/Example.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}

module Example where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String


{-# LINE 16 "src/Example.hsc" #-}

{-# LINE 17 "src/Example.hsc" #-}

-- |
-- | FOO
-- |

data Foo = Foo
    { fooName   :: !String
    , fooBars   :: ![Bar]
    } deriving(Eq, Show)

type FooPtr = Ptr Foo

instance Storable Foo where
  alignment _ = 8
{-# LINE 31 "src/Example.hsc" #-}
  sizeOf _    = (80)
{-# LINE 32 "src/Example.hsc" #-}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 37 "src/Example.hsc" #-}
      `apArr` ((\hsc_ptr -> peekByteOff hsc_ptr 64)  p,
{-# LINE 38 "src/Example.hsc" #-}
               (\hsc_ptr -> peekByteOff hsc_ptr 72)      p)
{-# LINE 39 "src/Example.hsc" #-}

  poke p      = undefined

-- |
-- | BAR
-- |

data Bar = Bar
    { barName   :: !String
    , barType   :: !Int
    , barMin    :: !Double
    , barMax    :: !Double
    } deriving (Eq, Show)

type BarPtr = Ptr Bar

instance Storable Bar where
  alignment _ = 8
{-# LINE 57 "src/Example.hsc" #-}
  sizeOf _    = (88)
{-# LINE 58 "src/Example.hsc" #-}

  peek p      = do
    Bar
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 62 "src/Example.hsc" #-}
      `apInt` (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 63 "src/Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 72)  p
{-# LINE 64 "src/Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 80)  p
{-# LINE 65 "src/Example.hsc" #-}

  poke p      = undefined

-- |
-- | WEIRD UNION
-- |

data WeirdUnion = UString String |
                  UDouble Double |
                  UBool   Bool
                deriving (Eq, Show)


instance Storable WeirdUnion where
  alignment _ = 8
{-# LINE 80 "src/Example.hsc" #-}
  sizeOf _    = (16)
{-# LINE 81 "src/Example.hsc" #-}

  peek p      = do
    unionType  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 84 "src/Example.hsc" #-}
    unionValue <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 85 "src/Example.hsc" #-}

    let
        val = case (mkInt unionType) of
          0 -> UString <$> (peekCString $  (\hsc_ptr -> hsc_ptr `plusPtr` 0)  unionValue)
{-# LINE 89 "src/Example.hsc" #-}
          1 -> UDouble <$> (mkDbl      <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 90 "src/Example.hsc" #-}
          2 -> do
            a <- mkInt <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 92 "src/Example.hsc" #-}
            return $ UBool $ case a of
              0 -> False
              1 -> True
    val

  poke p      = undefined

type WeirdUnionPtr = Ptr WeirdUnion

foreign export ccall entrypoint :: FooPtr -> WeirdUnionPtr -> WeirdUnionPtr -> IO ()
entrypoint :: FooPtr -> WeirdUnionPtr -> WeirdUnionPtr -> IO ()
entrypoint foo wustr wudbl = do
  a <- peek foo
  b <- peek wustr
  c <- peek wudbl
  print $ a
  print $ b
  print $ c
  return ()


mkInt :: CInt -> Int
mkInt = fromIntegral

mkDbl :: CDouble -> Double
mkDbl d = realToFrac d


infixl 4 `apInt`, `apDbl`, `fpStr`, `apArr`

fpStr :: (String -> b) -> CString -> IO b
fpStr a b = a <$> (peekCString b)

peekCArray :: (Storable a) => CInt -> IO (Ptr a) -> IO [a]
peekCArray i ir = ir >>= peekArray (mkInt i)

apArr :: Storable a => IO ([a] -> b) -> (IO CInt, IO (Ptr a)) -> IO b
apArr f (i, b) = do
  i' <- i
  r  <- peekCArray i' b
  f' <- f
  return $ f' r

apInt :: (Applicative f) => f (Int -> b) -> f CInt -> f b
apInt a b = a <*> (mkInt <$> b)

apDbl :: (Applicative f) => f (Double -> b) -> f CDouble -> f b
apDbl a b = a <*> (mkDbl <$> b)
