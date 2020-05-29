{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- Copyright (c) 2014, Herbert Valerio Riedel <hvr@gnu.org>
--
-- This code is BSD3 licensed, see ../LICENSE file for details
--

-- | Internal low-level bindings to liblzma
--
-- See @<lzma.h>@ header file for documentation about primitives not
-- documented here
module LibLzma
    ( -- * Stream
      LzmaStream
    -- ** Stream accessors
    , getStreamAvailIn
    , getStreamNextIn
    , getStreamNextOut
    , getStreamTotalIn
    , getStreamTotalOut

    , setStreamAvailIn
    , setStreamNextIn
    , setStreamNextOut
    , setStreamTotalIn
    , setStreamTotalOut

    -- ** Constructing decompression stream
    , newDecodeLzmaStream
    , DecompressParams(..)
    , defaultDecompressParams

    -- ** Constructing compression stream
    , newEncodeLzmaStream
    , CompressParams(..)
    , defaultCompressParams

    -- ** Running a stream
    , runLzmaStream

    -- ** Destructors
    , endLzmaStream

    -- * Block
    , LzmaBlock
    , newBlock

    -- * Stream flags
    , LzmaStreamFlags
    , newStreamFlags
    , getStreamFlagsCheck
    , getStreamFlagsVersion
    , getStreamFlagsBackwardSize

    -- * Index and index iterator
    , LzmaIndex
    , withIndexForeignPtr
    , LzmaIndexIter

    -- * Misc
    , LzmaRet(..)
    , IntegrityCheck(IntegrityCheckNone, IntegrityCheckCrc32, IntegrityCheckCrc64, IntegrityCheckSha256)
    , CompressionLevel(..)

    , LzmaAction(..)

    -- * Variable-length integer
    , LzmaVli(VliUnknown)
    , pattern LzmaStreamHeaderSize
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Typeable
import           Foreign
import           Foreign.C.Types (CInt)
import           Prelude

#include <lzma.h>

newtype LzmaStream = LS (ForeignPtr LzmaStream)

-- | Get the number of available input bytes in the input buffer.
getStreamAvailIn :: LzmaStream -> IO Int
getStreamAvailIn (LS fptr) = withForeignPtr fptr (#peek lzma_stream, avail_in)

-- | Set the number of available input bytes in the input buffer.
setStreamAvailIn :: LzmaStream -> Int -> IO ()
setStreamAvailIn (LS fptr) availIn =
    withForeignPtr fptr $ \ptr -> (#poke lzma_stream, avail_in) ptr availIn

-- | Get a pointer to the next input byte.
getStreamNextIn :: LzmaStream -> IO (Ptr Word8)
getStreamNextIn (LS fptr) = withForeignPtr fptr (#peek lzma_stream, next_in)

-- | Set a pointer to the next input byte.
setStreamNextIn :: LzmaStream -> Ptr Word8 -> IO ()
setStreamNextIn (LS fptr) nextIn =
    withForeignPtr fptr $ \ptr -> (#poke lzma_stream, next_in) ptr nextIn

-- | Get a pointer to the next output position.
getStreamNextOut :: LzmaStream -> IO (Ptr Word8)
getStreamNextOut (LS fptr) = withForeignPtr fptr (#peek lzma_stream, next_out)

-- | Set a pointer to the next input byte.
setStreamNextOut :: LzmaStream -> Ptr Word8 -> IO ()
setStreamNextOut (LS fptr) nextOut =
    withForeignPtr fptr $ \ptr -> (#poke lzma_stream, next_out) ptr nextOut

-- | Get the total number of bytes read by liblzma
getStreamTotalIn :: LzmaStream -> IO Int
getStreamTotalIn (LS fptr) = withForeignPtr fptr (#peek lzma_stream, total_in)

-- | Set the total number of bytes read by liblzma
setStreamTotalIn :: LzmaStream -> Int -> IO ()
setStreamTotalIn (LS fptr) totalIn =
    withForeignPtr fptr $ \ptr -> (#poke lzma_stream, total_in) ptr totalIn

-- | Get the total number of bytes written by liblzma
getStreamTotalOut :: LzmaStream -> IO Int
getStreamTotalOut (LS fptr) = withForeignPtr fptr (#peek lzma_stream, total_out)

-- | Set the total number of bytes written by liblzma
setStreamTotalOut :: LzmaStream -> Int -> IO ()
setStreamTotalOut (LS fptr) totalOut =
    withForeignPtr fptr $ \ptr -> (#poke lzma_stream, total_out) ptr totalOut

data LzmaRet = LzmaRetOK
             | LzmaRetStreamEnd
             | LzmaRetUnsupportedCheck
             | LzmaRetGetCheck
             | LzmaRetMemError
             | LzmaRetMemlimitError
             | LzmaRetFormatError
             | LzmaRetOptionsError
             | LzmaRetDataError
             | LzmaRetBufError
             | LzmaRetProgError
             deriving (Eq,Ord,Read,Show,Typeable)

instance Exception LzmaRet

toLzmaRet :: Int -> Maybe LzmaRet
toLzmaRet i = case i of
    (#const LZMA_OK               ) -> Just LzmaRetOK
    (#const LZMA_STREAM_END       ) -> Just LzmaRetStreamEnd
    (#const LZMA_UNSUPPORTED_CHECK) -> Just LzmaRetUnsupportedCheck
    (#const LZMA_GET_CHECK        ) -> Just LzmaRetGetCheck
    (#const LZMA_MEM_ERROR        ) -> Just LzmaRetMemError
    (#const LZMA_MEMLIMIT_ERROR   ) -> Just LzmaRetMemlimitError
    (#const LZMA_FORMAT_ERROR     ) -> Just LzmaRetFormatError
    (#const LZMA_OPTIONS_ERROR    ) -> Just LzmaRetOptionsError
    (#const LZMA_DATA_ERROR       ) -> Just LzmaRetDataError
    (#const LZMA_BUF_ERROR        ) -> Just LzmaRetBufError
    (#const LZMA_PROG_ERROR       ) -> Just LzmaRetProgError
    _                               -> Nothing

-- | Integrity check type (only supported when compressing @.xz@ files)
newtype IntegrityCheck = IC CInt deriving (Eq, Ord, Show, Typeable, Enum, Storable)
{-# COMPLETE IntegrityCheckNone, IntegrityCheckCrc32, IntegrityCheckCrc64, IntegrityCheckSha256 #-}

-- | Disable integrity check (not recommended)
pattern IntegrityCheckNone :: IntegrityCheck
pattern IntegrityCheckNone = IC (#const LZMA_CHECK_NONE)

-- | CRC32 using the polynomial from IEEE-802.3
pattern IntegrityCheckCrc32 :: IntegrityCheck
pattern IntegrityCheckCrc32 = IC (#const LZMA_CHECK_CRC32)

-- | CRC64 using the polynomial from ECMA-182
pattern IntegrityCheckCrc64 :: IntegrityCheck
pattern IntegrityCheckCrc64 = IC (#const LZMA_CHECK_CRC64)

-- | SHA-256
pattern IntegrityCheckSha256 :: IntegrityCheck
pattern IntegrityCheckSha256 = IC (#const LZMA_CHECK_SHA256)

fromIntegrityCheck :: IntegrityCheck -> Int
fromIntegrityCheck = fromEnum

-- | Compression level presets that define the tradeoff between
-- computational complexity and compression ratio
--
-- 'CompressionLevel0' has the lowest compression ratio as well as the
-- lowest memory requirements, whereas 'CompressionLevel9' has the
-- highest compression ratio and can require over 600MiB during
-- compression (and over 60MiB during decompression). The
-- <https://www.freebsd.org/cgi/man.cgi?query=xz&sektion=1&manpath=FreeBSD+10.2-stable&arch=default&format=html man-page for xz(1)>
-- contains more detailed information with tables describing the
-- properties of all compression level presets.
--
-- 'CompressionLevel6' is the default setting in
-- 'defaultCompressParams' as it provides a good trade-off and
-- matches the default of the @xz(1)@ tool.

data CompressionLevel = CompressionLevel0
                      | CompressionLevel1
                      | CompressionLevel2
                      | CompressionLevel3
                      | CompressionLevel4
                      | CompressionLevel5
                      | CompressionLevel6
                      | CompressionLevel7
                      | CompressionLevel8
                      | CompressionLevel9
                      deriving (Eq,Ord,Read,Show,Enum,Typeable)

-- | Set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
data DecompressParams = DecompressParams
    { decompressTellNoCheck          :: !Bool -- ^ 'DecompressParams' field: If set, abort if decoded stream has no integrity check.
    , decompressTellUnsupportedCheck :: !Bool -- ^ 'DecompressParams' field: If set, abort (via 'LzmaRetGetCheck') if decoded stream integrity check is unsupported.
    , decompressTellAnyCheck         :: !Bool -- ^ 'DecompressParams' field: If set, abort (via 'LzmaRetGetCheck') as soon as the type of the integrity check has been detected.
    , decompressConcatenated         :: !Bool -- ^ 'DecompressParams' field: If set, concatenated files as decoded seamless.
    , decompressAutoDecoder          :: !Bool -- ^ 'DecompressParams' field: If set, legacy @.lzma@-encoded streams are allowed too.
    , decompressMemLimit             :: !Word64 -- ^ 'DecompressParams' field: decompressor memory limit. Set to 'maxBound' to disable memory limit.
    } deriving (Eq,Show)

-- | The default set of parameters for decompression. This is
-- typically used with the 'decompressWith' function with specific
-- parameters overridden.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {..}
  where
    decompressTellNoCheck          = False
    decompressTellUnsupportedCheck = False
    decompressTellAnyCheck         = False
    decompressConcatenated         = True
    decompressAutoDecoder          = False
    decompressMemLimit             = maxBound -- disables limit-check

-- | Set of parameters for compression. The defaults are 'defaultCompressParams'.
data CompressParams = CompressParams
    { compressIntegrityCheck :: !IntegrityCheck -- ^ 'CompressParams' field: Specify type of integrity check
    , compressLevel          :: !CompressionLevel -- ^ 'CompressParams' field: See documentation of 'CompressionLevel'
    , compressLevelExtreme   :: !Bool  -- ^ 'CompressParams' field: Enable slower variant of the
                                       -- 'lzmaCompLevel' preset, see @xz(1)@
                                       -- man-page for details.
    } deriving (Eq,Show)

-- | The default set of parameters for compression. This is typically
-- used with the 'compressWith' function with specific parameters
-- overridden.
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {..}
  where
    compressIntegrityCheck = IntegrityCheckCrc64
    compressLevel          = CompressionLevel6
    compressLevelExtreme   = False

newDecodeLzmaStream :: DecompressParams -> ST s (Either LzmaRet LzmaStream)
newDecodeLzmaStream (DecompressParams {..}) = unsafeIOToST $ do
    fp <- mallocForeignPtrBytes (#size lzma_stream)
    addForeignPtrFinalizer c_hs_lzma_done_funptr fp
    rc <- withForeignPtr fp (\ptr -> c_hs_lzma_init_decoder ptr decompressAutoDecoder decompressMemLimit flags')
    rc' <- maybe (fail "newDecodeLzmaStream: invalid return code") pure $ toLzmaRet rc

    return $ case rc' of
        LzmaRetOK -> Right (LS fp)
        _         -> Left rc'
  where
    flags' =
        (if decompressTellNoCheck          then (#const LZMA_TELL_NO_CHECK)          else 0) .|.
        (if decompressTellUnsupportedCheck then (#const LZMA_TELL_UNSUPPORTED_CHECK) else 0) .|.
        (if decompressTellAnyCheck         then (#const LZMA_TELL_ANY_CHECK)         else 0) .|.
        (if decompressConcatenated         then (#const LZMA_CONCATENATED)           else 0)

newEncodeLzmaStream :: CompressParams -> ST s (Either LzmaRet LzmaStream)
newEncodeLzmaStream (CompressParams {..}) = unsafeIOToST $ do
    fp <- mallocForeignPtrBytes (#size lzma_stream)
    addForeignPtrFinalizer c_hs_lzma_done_funptr fp
    rc <- withForeignPtr fp (\ptr -> c_hs_lzma_init_encoder ptr preset check)
    rc' <- maybe (fail "newDecodeLzmaStream: invalid return code") pure $ toLzmaRet rc

    return $ case rc' of
        LzmaRetOK -> Right (LS fp)
        _         -> Left rc'

  where
    preset = fromIntegral (fromEnum compressLevel) .|.
             (if compressLevelExtreme then (#const LZMA_PRESET_EXTREME) else 0)
    check = fromIntegrityCheck compressIntegrityCheck

data LzmaAction = LzmaRun
                | LzmaSyncFlush
                | LzmaFullFlush
                | LzmaFinish
                deriving (Eq,Show)

runLzmaStream :: LzmaStream -> ByteString -> LzmaAction -> Int -> ST s (LzmaRet,Int,ByteString)
runLzmaStream (LS ls) ibs action0 buflen
  | buflen <= 0 = return (LzmaRetOptionsError,0,BS.empty)
  | otherwise = unsafeIOToST $ withForeignPtr ls $ \lsptr ->
      BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) -> do
          (obuf,rc) <- BS.createAndTrim' buflen $ \bufptr -> do
              rc' <- c_hs_lzma_run lsptr action (castPtr ibsptr) ibslen bufptr buflen
              rc'' <- maybe (fail "runLzmaStream: invalid return code") pure $ toLzmaRet rc'

              availOut <- (#peek lzma_stream, avail_out) lsptr
              unless (buflen >= availOut && availOut >= 0) $
                  fail "runLzmaStream: invalid avail_out"
              let produced = buflen - availOut

              return (0, produced, rc'')

          availIn <- (#peek lzma_stream, avail_in) lsptr
          unless (ibslen >= availIn && availIn >= 0) $
              fail "runLzmaStream: invalid avail_in"
          let consumed = ibslen - availIn
          -- print ("run", action0, BS.length ibs, buflen, rc, consumed, BS.length obuf)

          return (rc, fromIntegral consumed, obuf)
  where
    action = case action0 of
        LzmaRun       -> #const LZMA_RUN
        LzmaSyncFlush -> #const LZMA_SYNC_FLUSH
        LzmaFullFlush -> #const LZMA_FULL_FLUSH
        LzmaFinish    -> #const LZMA_FINISH


-- | Force immediate finalization of 'ForeignPtr' associated with
-- 'LzmaStream'.  This triggers a call to @lzma_end()@, therefore it's
-- a programming error to call 'runLzmaStream' afterwards.
endLzmaStream :: LzmaStream -> ST s ()
endLzmaStream (LS ls) = unsafeIOToST $ finalizeForeignPtr ls

newtype LzmaBlock = LB (ForeignPtr LzmaBlock)

newBlock :: IO LzmaBlock
newBlock = LB <$> mallocForeignPtrBytes (#size lzma_block)

-- | Options for encoding/decoding stream headers and footers
newtype LzmaStreamFlags = LSF (ForeignPtr LzmaStreamFlags)

newStreamFlags :: IO LzmaStreamFlags
newStreamFlags = undefined

getStreamFlagsCheck :: LzmaStreamFlags -> IO IntegrityCheck
getStreamFlagsCheck (LSF fptr) =
    withForeignPtr fptr (#peek lzma_stream_flags, check)

getStreamFlagsVersion :: LzmaStreamFlags -> IO Word32
getStreamFlagsVersion (LSF fptr) =
    withForeignPtr fptr (#peek lzma_stream_flags, version)

getStreamFlagsBackwardSize :: LzmaStreamFlags -> IO LzmaVli
getStreamFlagsBackwardSize (LSF fptr) =
    withForeignPtr fptr (#peek lzma_stream_flags, backward_size)

pattern LzmaStreamHeaderSize :: LzmaVli
pattern LzmaStreamHeaderSize = #const LZMA_STREAM_HEADER_SIZE

newtype LzmaIndex = LI (Ptr LzmaIndex) deriving Storable

withIndexForeignPtr :: ForeignPtr LzmaIndex -> (LzmaIndex -> IO a) -> IO a
withIndexForeignPtr fptr f = withForeignPtr fptr (peek >=> f)

newtype LzmaIndexIter = LII (ForeignPtr LzmaIndexIter)

-- | Variable-length integer
--
-- Valid 'LzmaVli' values are in the range [0, 'maxBound']. Unknown value is
-- indicated with 'VliUnknown', which is the maximum value of the underlaying
-- integer type.
newtype LzmaVli = LzmaVli Word64
    deriving (Eq, Ord, Num, Real, Integral, Bits, Storable)

instance Show LzmaVli where
    show (LzmaVli n) = show n

instance Bounded LzmaVli where
    minBound = LzmaVli 0
    maxBound = LzmaVli (#const LZMA_VLI_MAX)

instance Enum LzmaVli where
    toEnum i
        | i < 0 = error "LzmaVli should be greater than 0"
        | i > (#const LZMA_VLI_MAX) =
            error $ "LzmaVli should be smaller than " ++ show (maxBound :: LzmaVli)
        | otherwise = LzmaVli $ toEnum i
    fromEnum (LzmaVli n) = fromEnum n

pattern VliUnknown :: LzmaVli
pattern VliUnknown = LzmaVli (#const LZMA_VLI_UNKNOWN)

----------------------------------------------------------------------------
-- trivial helper wrappers defined in ../cbits/lzma_wrapper.c

foreign import ccall "hs_lzma_init_decoder"
    c_hs_lzma_init_decoder :: Ptr LzmaStream -> Bool -> Word64 -> Word32 -> IO Int

foreign import ccall "hs_lzma_init_encoder"
    c_hs_lzma_init_encoder :: Ptr LzmaStream -> Word32 -> Int -> IO Int

foreign import ccall "hs_lzma_run"
    c_hs_lzma_run :: Ptr LzmaStream -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall "&hs_lzma_done"
    c_hs_lzma_done_funptr :: FunPtr (Ptr LzmaStream -> IO ())
