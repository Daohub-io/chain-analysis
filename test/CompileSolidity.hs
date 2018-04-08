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
compileSolidityFileBin path = do
    text  <- compileSolidityFile Bin path
    let (bsEncoded,rem) = B16.decode  $ encodeUtf8 text
    if rem == B.empty then pure bsEncoded else error "decoding failed"

compileSolidityFileBinRunTime :: FilePath -> IO T.Text
compileSolidityFileBinRunTime path = compileSolidityFile BinRunTime path

compileSolidityFileBinFull :: FilePath -> IO T.Text
compileSolidityFileBinFull path = compileSolidityFile Bin path

compileSolidityFile :: CompileType -> FilePath -> IO T.Text
compileSolidityFile compileType path = do
    let (compileArg, ext) = case compileType of
            Bin -> ("--bin", ".binbuild")
            BinRunTime -> ("--bin-runtime", ".binrun")
    -- check if the compiled version exists first
    let compiledPath = replaceExtension path ext
    exists <- doesFileExist compiledPath
    contents <- if exists
        then do
            B.readFile compiledPath
        else do
            (_, Just hout, _, hndl) <-
                createProcess (proc (joinPath ["solidity-windows", "solc.exe"]) [compileArg, path])
                    { std_out = CreatePipe
                    , std_err = NoStream }
            exitCode <- waitForProcess hndl
            -- print hout
            contents <- B.hGetContents hout
            B.writeFile compiledPath contents
            -- error $ "ehhh"
            -- contentsErr <- B.hGetContents herr
            pure contents
    let textContent = decodeUtf8 contents
        hexCode = if length (T.lines textContent) < 4 then error (show contents) else T.filter (/= '\r') ((T.lines textContent) !! 3)
    pure $ T.filter (/= '\r') hexCode


mangleFilename :: FilePath -> String
mangleFilename path =
    let
        withoutExtension = dropExtension path
        filepathPieces = splitDirectories withoutExtension
    in intercalate "_" filepathPieces