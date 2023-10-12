{-# LANGUAGE CPP #-}
module Options.Applicative.OsString where

import System.OsString.Internal.Types                  -- filepath
import System.OsString (encodeWith)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.Win32.WindowsString.Console as OS
import qualified System.OsPath.Data.ByteString.Short.Word16 as BSS
#else
import qualified System.Posix.Env.PosixString as OS
import qualified System.OsPath.Data.ByteString.Short as BSS
#endif

import System.OsPath.Encoding
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.UTF8
import GHC.Stack (HasCallStack)

getArgs :: IO [OsString]
getArgs = fmap OsString <$> OS.getArgs


isPrefixOf :: OsString -> OsString -> Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isPrefixOf (OsString (WindowsString a)) (OsString (WindowsString b)) = BSS.isPrefixOf a b
#else
isPrefixOf (OsString (PosixString a)) (OsString (PosixString b)) = BSS.isPrefixOf a b
#endif

encodeUtfSafe :: String -> OsString
encodeUtfSafe = either (error . show) id . encodeWith (mkUTF8 TransliterateCodingFailure) (mkUTF16le_b TransliterateCodingFailure)

uncons :: OsString -> Maybe (OsChar, OsString)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
uncons (OsString (WindowsString a)) = do
                                       (x, xs) <- BSS.uncons a
                                       pure (OsChar (WindowsChar x), OsString (WindowsString xs))
#else
uncons (OsString (PosixString a)) = do
                                      (x, xs) <- BSS.uncons a
                                      pure (OsChar (PosixChar x), OsString (PosixString xs))
#endif

uncons2 :: OsString -> Maybe (OsChar, OsChar, OsString)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
uncons2 (OsString (WindowsString a)) = do
                                       (x, y, xs) <- BSS.uncons2 a
                                       pure (OsChar (WindowsChar x), OsChar (WindowsChar y), OsString (WindowsString xs))
#else
uncons2 (OsString (PosixString a)) = do
                                       (x, y, xs) <- BSS.uncons2 a
                                       pure (OsChar (PosixChar x), OsChar (PosixChar y), OsString (PosixString xs))
#endif

span :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
span f (OsString (WindowsString a)) = let (x, y) = BSS.span (f . OsChar . WindowsChar) a
                                      in (OsString (WindowsString x), OsString (WindowsString y))
#else
span f (OsString (PosixString a)) = let (x, y) = BSS.span (f . OsChar . PosixChar) a
                                    in (OsString (PosixString x), OsString (PosixString y))
#endif

tail :: HasCallStack => OsString -> OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
tail (OsString (WindowsString a)) = OsString . WindowsString $ BSS.tail a
#else
tail (OsString (PosixString a)) = OsString . PosixString $ BSS.tail a
#endif

init :: HasCallStack => OsString -> OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
init (OsString (WindowsString a)) = OsString . WindowsString $ BSS.init a
#else
init (OsString (PosixString a)) = OsString . PosixString $ BSS.init a
#endif

