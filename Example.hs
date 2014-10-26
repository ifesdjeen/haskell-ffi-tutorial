{-# LINE 1 "Example.hsc" #-}
{-# LANGUAGE CPP                         #-}
{-# LINE 2 "Example.hsc" #-}
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


{-# LINE 16 "Example.hsc" #-}

{-# LINE 17 "Example.hsc" #-}

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
{-# LINE 31 "Example.hsc" #-}
  sizeOf _    = (80)
{-# LINE 32 "Example.hsc" #-}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 37 "Example.hsc" #-}
      `apArr` ((\hsc_ptr -> peekByteOff hsc_ptr 64)  p,
{-# LINE 38 "Example.hsc" #-}
               (\hsc_ptr -> peekByteOff hsc_ptr 72)      p)
{-# LINE 39 "Example.hsc" #-}

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
{-# LINE 57 "Example.hsc" #-}
  sizeOf _    = (88)
{-# LINE 58 "Example.hsc" #-}

  peek p      = do
    Bar
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 62 "Example.hsc" #-}
      `apInt` (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 63 "Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 72)  p
{-# LINE 64 "Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 80)  p
{-# LINE 65 "Example.hsc" #-}

  poke p      = undefined



foreign export ccall entrypoint :: FooPtr -> IO ()
entrypoint :: FooPtr -> IO ()
entrypoint foo = do
  b <- peek foo
  -- b <- fromC a
  print $ b
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
