{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, TypeFamilies, RecordWildCards, FlexibleInstances, FlexibleContexts #-}

module Example where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include "myfile.h"

-- |
-- | FOO
-- |

data Foo = Foo
           { fooName   :: !String
           , fooBars   :: ![Bar]
           } deriving(Eq, Show)

instance Storable Foo where
  alignment _ = #{alignment foo_t}
  sizeOf _    = #{size      foo_t}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` #{ptr foo_t, name}  p
      `apArr` (#{peek foo_t, bar_num}  p, #{peek foo_t, bar}  p)

  poke p      = undefined

type FooPtr = Ptr Foo


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
  alignment _ = #{alignment bar_t}
  sizeOf _    = #{size      bar_t}

  peek p      = do
    Bar
      `fpStr` #{ptr bar_t, name}  p
      `apInt` #{peek bar_t, type} p
      `apDbl` #{peek bar_t, min}  p
      `apDbl` #{peek bar_t, max}  p

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
