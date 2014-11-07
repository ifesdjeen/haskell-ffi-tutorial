{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE RecordWildCards            #-}

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

  poke p Foo{..} = do
    cFooName     <- newCString fooName
    fooNameValue <- peekArray (length fooName) cFooName
    pokeArray (#{ptr foo_t, name} p) fooNameValue

    let fooBarsCount = length fooBars
    #{poke foo_t, bar_num} p fooBarsCount
    barArrPtr  <- mallocArray fooBarsCount
    pokeArray barArrPtr fooBars

    #{poke foo_t, bar} p barArrPtr
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

  poke p Bar{..} = do
    cBarName     <- newCString barName
    barNameValue <- peekArray (length barName) cBarName
    pokeArray (#{ptr bar_t, name} p) barNameValue

    #{poke bar_t, type} p barType
    #{poke bar_t, min}  p barMin
    #{poke bar_t, max}  p barMax

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

    case (mkInt unionType) of
      0 -> do
        unionValue <- #{peek weird_union_t, value} p
        UString <$> (peekCString $  #{ptr weird_union_t, value}  unionValue)

      1 -> UDouble <$> (mkDbl      <$> #{peek weird_union_t, value} p)

      2 -> do
        a <- mkInt <$> #{peek weird_union_t, value} p
        return $ UBool $ case a of
          0 -> False
          1 -> True

  poke p      = undefined

type WeirdUnionPtr = Ptr WeirdUnion

foreign export ccall entrypoint :: FooPtr -> WeirdUnionPtr -> WeirdUnionPtr -> IO ()
entrypoint :: FooPtr -> WeirdUnionPtr -> WeirdUnionPtr -> IO ()
entrypoint foo wustr wudbl = do
  print "===== Foo Read from Haskell Code ======"
  a <- peek foo
  print $ a

  print "===== Union Read from Haskell Code ======"
  b <- peek wustr
  c <- peek wudbl
  print $ b
  print $ c

  print "===== Changed from Haskell Code ======"
  let someFoo = Foo "created foo" [Bar "1 bar name"   100 0.001 1000.0
                                  , Bar "2 bar name"  200 0.002 2000.0
                                  , Bar "3 bar name"  300 0.003 3000.0]

  poke foo someFoo

  a <- peek foo
  print $ a

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