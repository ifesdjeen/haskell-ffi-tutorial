{-# LINE 1 "src/Example.hsc" #-}
{-# LANGUAGE CPP                         #-}
{-# LINE 2 "src/Example.hsc" #-}
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


{-# LINE 17 "src/Example.hsc" #-}

{-# LINE 18 "src/Example.hsc" #-}

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
{-# LINE 32 "src/Example.hsc" #-}
  sizeOf _    = (80)
{-# LINE 33 "src/Example.hsc" #-}

  -- peek :: FooPtr -> IO (Struct Foo)
  peek p      = do
    Foo
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 38 "src/Example.hsc" #-}
      `apArr` ((\hsc_ptr -> peekByteOff hsc_ptr 64)  p,
{-# LINE 39 "src/Example.hsc" #-}
               (\hsc_ptr -> peekByteOff hsc_ptr 72)      p)
{-# LINE 40 "src/Example.hsc" #-}

  poke p Foo{..} = do
    cFooName     <- newCString fooName
    fooNameValue <- peekArray (length fooName) cFooName
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p) fooNameValue
{-# LINE 45 "src/Example.hsc" #-}

    let fooBarsCount = length fooBars
    (\hsc_ptr -> pokeByteOff hsc_ptr 64) p fooBarsCount
{-# LINE 48 "src/Example.hsc" #-}
    barArrPtr  <- mallocArray fooBarsCount
    pokeArray barArrPtr fooBars

    (\hsc_ptr -> pokeByteOff hsc_ptr 72) p barArrPtr
{-# LINE 52 "src/Example.hsc" #-}
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
{-# LINE 67 "src/Example.hsc" #-}
  sizeOf _    = (88)
{-# LINE 68 "src/Example.hsc" #-}

  peek p      = do
    Bar
      `fpStr` (\hsc_ptr -> hsc_ptr `plusPtr` 0)  p
{-# LINE 72 "src/Example.hsc" #-}
      `apInt` (\hsc_ptr -> peekByteOff hsc_ptr 64) p
{-# LINE 73 "src/Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 72)  p
{-# LINE 74 "src/Example.hsc" #-}
      `apDbl` (\hsc_ptr -> peekByteOff hsc_ptr 80)  p
{-# LINE 75 "src/Example.hsc" #-}

  poke p Bar{..} = do
    cBarName     <- newCString barName
    barNameValue <- peekArray (length barName) cBarName
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p) barNameValue
{-# LINE 80 "src/Example.hsc" #-}

    -- fooNamePtr <- newCString fooName
    -- #{poke }
    (\hsc_ptr -> pokeByteOff hsc_ptr 64) p barType
{-# LINE 84 "src/Example.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 72)  p barMin
{-# LINE 85 "src/Example.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 80)  p barMax
{-# LINE 86 "src/Example.hsc" #-}

-- |
-- | WEIRD UNION
-- |

data WeirdUnion = UString String |
                  UDouble Double |
                  UBool   Bool
                deriving (Eq, Show)


instance Storable WeirdUnion where
  alignment _ = 8
{-# LINE 99 "src/Example.hsc" #-}
  sizeOf _    = (16)
{-# LINE 100 "src/Example.hsc" #-}

  peek p      = do
    unionType  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 103 "src/Example.hsc" #-}

    case (mkInt unionType) of
      0 -> do
        unionValue <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 107 "src/Example.hsc" #-}
        UString <$> (peekCString $  (\hsc_ptr -> hsc_ptr `plusPtr` 0)  unionValue)
{-# LINE 108 "src/Example.hsc" #-}

      1 -> UDouble <$> (mkDbl      <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 110 "src/Example.hsc" #-}

      2 -> do
        a <- mkInt <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 113 "src/Example.hsc" #-}
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
