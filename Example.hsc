{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, TypeFamilies, RecordWildCards, FlexibleInstances, FlexibleContexts #-}

module Example where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include "myfile.h"

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
  alignment _ = #{alignment foo_t}
  sizeOf _    = #{size      foo_t}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    a1 <- peekCString $ #{ptr foo_t, name} p
    a2 <- #{peek foo_t, bar_num} p
    ir <- #{peek foo_t, bar} p
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
  alignment _ = #{alignment bar_t}
  sizeOf _    = #{size      bar_t}

  peek p      = do
    a1 <- peekCString $ #{ptr bar_t, name} p
    a2 <- #{peek bar_t, type} p
    a3 <- #{peek bar_t, min}  p
    a4 <- #{peek bar_t, max}  p
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
