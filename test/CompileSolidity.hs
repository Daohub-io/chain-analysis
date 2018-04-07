module CompileSolidity where

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding

data CompileType = Bin | BinRunTime deriving (Eq, Show)


compileSolidityFileBin :: FilePath -> IO B.ByteString
compileSolidityFileBin path =  compileSolidityFile Bin path

compileSolidityFileBinRunTime :: FilePath -> IO T.Text
compileSolidityFileBinRunTime path = do
    (_, Just hout, Just herr, hndl) <-
        createProcess (proc (joinPath ["solidity-windows", "solc.exe"]) ["--bin-runtime", path])
            { std_out = CreatePipe
            , std_err = CreatePipe }
    exitCode <- waitForProcess hndl
    -- print hout
    contents <- B.hGetContents hout
    contentsErr <- B.hGetContents herr
    let textContent = decodeUtf8 contents
        hexCode =if length (T.lines textContent) < 4 then error (show contents ++ show contentsErr) else T.filter (/= '\r') ((T.lines textContent) !! 3)
    pure $ T.filter (/= '\r') hexCode

compileSolidityFileBinFull :: FilePath -> IO T.Text
compileSolidityFileBinFull path = do
    (_, Just hout, _, hndl) <-
        createProcess (proc (joinPath ["solidity-windows", "solc.exe"]) ["--bin", path])
            { std_out = CreatePipe
            , std_err = NoStream }
    exitCode <- waitForProcess hndl
    -- print hout
    contents <- B.hGetContents hout
    let textContent = decodeUtf8 contents
        hexCode =if length (T.lines textContent) < 4 then error (show contents) else T.filter (/= '\r') ((T.lines textContent) !! 3)
    pure $ T.filter (/= '\r') hexCode

compileSolidityFile :: CompileType -> FilePath -> IO B.ByteString
compileSolidityFile compileType path = withSystemTempDirectory "solc-comp" $ \tempdir -> do
    let compileArg = case compileType of
            Bin -> "--bin"
            BinRunTime -> "--bin-runtime"
    (_, Just hout, _, hndl) <-
        createProcess (proc (joinPath ["solidity-windows", "solc.exe"]) ["--bin-runtime", path])
            { std_out = CreatePipe
            , std_err = NoStream }
    exitCode <- waitForProcess hndl
    contents <- B.hGetContents hout
    let textContent = decodeUtf8 contents
        hexCode =if length (T.lines textContent) < 4 then error (show contents) else T.filter (/= '\r') ((T.lines textContent) !! 3)
        (bsEncoded,rem) = B16.decode  $ encodeUtf8 hexCode
    if rem == B.empty then pure bsEncoded else error "decoding failed"


mangleFilename :: FilePath -> String
mangleFilename path =
    let
        withoutExtension = dropExtension path
        filepathPieces = splitDirectories withoutExtension
    in intercalate "_" filepathPieces