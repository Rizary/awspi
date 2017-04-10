{-# LANGUAGE ForeignFunctionInterface, InterruptibleFFI, CApiFFI #-}
module FFIExtensions where

foreign import ccall interruptible
   "sleep" sleep :: CUint -> IO CUint

foreign import capi "header.h f" f :: CInt -> IO CInt
