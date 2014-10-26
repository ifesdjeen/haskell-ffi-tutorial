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

data family Struct a

class Copy a where
  copy  :: (Storable (Struct a)) => Ptr (Struct a) -> IO a
  copy ptr = (peek ptr) >>= fromC
  fromC :: Struct a -> IO a

-- |
-- | FOO
-- |

data Foo = Foo
           { fooName :: !String
           , bars    :: [Bar]
           } deriving (Eq, Show)

data instance Struct Foo = CFoo
                           { cfooName   :: String
                           , cfooBarNum :: CInt
                           , cfooBars   :: [(Struct Bar)] --
                           } deriving(Show)

instance Storable (Struct Foo) where
  alignment _ = 8
{-# LINE 37 "Example.hsc" #-}
  sizeOf _    = (80)
{-# LINE 38 "Example.hsc" #-}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    a1 <- peekCString $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) p
{-# LINE 42 "Example.hsc" #-}
    a2 <- (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 43 "Example.hsc" #-}
    ir <- (\hsc_ptr -> peekByteOff hsc_ptr 72) p
{-# LINE 44 "Example.hsc" #-}
    a3 <- peekArray (mkInt a2) ir
    return $ CFoo a1 a2 a3

  poke p      = undefined

type FooPtr = Ptr (Struct Foo)

instance Copy Foo where
  fromC CFoo{..} = do
    a <- mapM fromC cfooBars
    return $ Foo cfooName a


-- |
-- | BAR
-- |

data Bar = Bar
           { barName   :: String
           , barType   :: Int
           , barMin    :: Double
           , barMax    :: Double
           } deriving (Eq, Show)

data instance Struct Bar = CBar
                           { cbarName   :: String
                           , cbarType   :: CInt
                           , cbarMin    :: CDouble
                           , cbarMax    :: CDouble
                           } deriving(Show)

type BarPtr   = Ptr (Struct Bar)

instance Storable (Struct Bar) where
  alignment _ = 8
{-# LINE 79 "Example.hsc" #-}
  sizeOf _    = (88)
{-# LINE 80 "Example.hsc" #-}

  peek p      = do
    a1 <- peekCString $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) p
{-# LINE 83 "Example.hsc" #-}
    a2 <- (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 84 "Example.hsc" #-}
    a3 <- (\hsc_ptr -> peekByteOff hsc_ptr 72)  p
{-# LINE 85 "Example.hsc" #-}
    a4 <- (\hsc_ptr -> peekByteOff hsc_ptr 80)  p
{-# LINE 86 "Example.hsc" #-}
    return $ CBar a1 a2 a3 a4
  poke p      = undefined

instance Copy Bar where
  fromC CBar{..} = do
    return $ Bar cbarName (mkInt cbarType) (mkDbl cbarMin) (mkDbl cbarMax)



foreign export ccall entrypoint :: FooPtr -> IO ()
entrypoint :: FooPtr -> IO ()
entrypoint foo = do
  b <- copy foo
  -- b <- fromC a
  print $ b
  return ()


mkInt :: CInt -> Int
mkInt = fromIntegral

mkDbl :: CDouble -> Double
mkDbl d = realToFrac d
