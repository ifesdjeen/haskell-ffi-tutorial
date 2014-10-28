{-# LANGUAGE CPP                         #-}
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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include "myfile.h"

-- |
-- | FOO
-- |

data Foo = Foo
    { fooName   :: !String
    , fooBars   :: ![Bar]
    } deriving(Eq, Show)

type FooPtr = Ptr Foo

instance Storable Foo where
  alignment _ = #{alignment foo_t}
  sizeOf _    = #{size      foo_t}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` #{ptr foo_t, name}  p
      `apArr` (#{peek foo_t, bar_num}  p,
               #{peek foo_t, bar}      p)

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
  alignment _ = #{alignment bar_t}
  sizeOf _    = #{size      bar_t}

  peek p      = do
    Bar
      `fpStr` #{ptr bar_t, name}  p
      `apInt` #{peek bar_t, type} p
      `apDbl` #{peek bar_t, min}  p
      `apDbl` #{peek bar_t, max}  p

  poke p      = undefined

-- |
-- | WEIRD UNION
-- |

data WeirdUnion = UString String |
                  UDouble Double |
                  UBool   Bool
                deriving (Eq, Show)


instance Storable WeirdUnion where
  alignment _ = #{alignment weird_union_t}
  sizeOf _    = #{size      weird_union_t}

  peek p      = do
    unionType  <- #{peek weird_union_t, type} p
    unionValue <- #{peek weird_union_t, value} p

    let
        val = case (mkInt unionType) of
          0 -> UString <$> (peekCString $  #{ptr weird_union_t, value}  unionValue)
          1 -> UDouble <$> (mkDbl      <$> #{peek weird_union_t, value} p)
          2 -> do
            a <- mkInt <$> #{peek weird_union_t, value} p
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
