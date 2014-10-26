{-# LINE 1 "Example.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, TypeFamilies, RecordWildCards, FlexibleInstances, FlexibleContexts #-}
{-# LINE 2 "Example.hsc" #-}

module Example where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String


{-# LINE 11 "Example.hsc" #-}

{-# LINE 12 "Example.hsc" #-}

-- |
-- | FOO
-- |

data Foo = Foo
           { fooName   :: !String
           , fooBars   :: ![Bar]
           } deriving(Eq, Show)

instance Storable Foo where
  alignment _ = 8
{-# LINE 24 "Example.hsc" #-}
  sizeOf _    = (80)
{-# LINE 25 "Example.hsc" #-}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 30 "Example.hsc" #-}
      `apArr` ((\hsc_ptr -> peekByteOff hsc_ptr 64)  p, (\hsc_ptr -> peekByteOff hsc_ptr 72)  p)
{-# LINE 31 "Example.hsc" #-}
    -- a1 <- peekCString $ #{ptr foo_t, name} p
    -- a2 <- #{peek foo_t, bar_num} p
    -- a3 <- peekCArray a2 $ #{peek foo_t, bar} p
    -- return $ Foo a1 a3

  poke p      = undefined

type FooPtr = Ptr Foo

-- instance Copy Foo where
--   fromC CFoo{..} = do
--     a <- mapM fromC cfooBars
--     return $ Foo cfooName a


-- |
-- | BAR
-- |

data Bar = Bar
           { barName   :: String
           , barType   :: Int
           , barMin    :: Double
           , barMax    :: Double
           } deriving (Eq, Show)

type BarPtr = Ptr Bar

instance Storable Bar where
  alignment _ = 8
{-# LINE 61 "Example.hsc" #-}
  sizeOf _    = (88)
{-# LINE 62 "Example.hsc" #-}

  peek p      = do
    Bar
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 66 "Example.hsc" #-}
      `apInt` (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 67 "Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 72)  p
{-# LINE 68 "Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 80)  p
{-# LINE 69 "Example.hsc" #-}

  poke p      = undefined

-- instance Copy Bar where
--   fromC Bar{..} = do
--     return $ Bar cbarName (mkInt cbarType) (mkDbl cbarMin) (mkDbl cbarMax)



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

-- apArr :: IO ([a] -> b) -> IO (Ptr a) -> IO CInt -> IO b
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
