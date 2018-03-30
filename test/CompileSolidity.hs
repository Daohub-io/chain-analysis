module CompileSolidity where

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.List

compileSolidityFile :: FilePath -> IO B.ByteString
compileSolidityFile path = withSystemTempDirectory "solc-comp" $ \tempdir -> do
    (_, Just hout, _, hndl) <-
        createProcess (proc "node" ["node_modules/solc/solcjs", path, "--bin", "--output-dir", tempdir])
            { std_out = CreatePipe }
    exitCode <- waitForProcess hndl
    -- We are assuming this is the contract name
    let contractName = takeBaseName path
    bytecodeHex <- B.readFile (joinPath [tempdir, mangleFilename path ++ "_sol_" ++ contractName ++ ".bin"])
    let (bytecode,_) = decode bytecodeHex
    pure bytecode

mangleFilename :: FilePath -> String
mangleFilename path =
    let
        withoutExtension = dropExtension path
        filepathPieces = splitDirectories withoutExtension
    in intercalate "_" filepathPieces