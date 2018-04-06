module CompileSolidity where

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.List

data CompileType = Bin | ABI deriving (Eq, Show)


compileSolidityFileBin :: FilePath -> IO B.ByteString
compileSolidityFileBin path = withSystemTempDirectory "solc-comp" $ \tempdir -> do
    bytecodeHex <- compileSolidityFile Bin path
    let (bytecode,x) = decode bytecodeHex
    if x == B.empty then pure bytecode else error "decoding failed"

compileSolidityFileABI :: FilePath -> IO B.ByteString
compileSolidityFileABI path = withSystemTempDirectory "solc-comp" $ \tempdir -> do
    abiBS <- compileSolidityFile ABI path
    pure abiBS

compileSolidityFile :: CompileType -> FilePath -> IO B.ByteString
compileSolidityFile compileType path = withSystemTempDirectory "solc-comp" $ \tempdir -> do
    let (compileArg, outExt) = case compileType of
            Bin -> ("--bin", ".bin")
            ABI -> ("--abi", ".abi")
    (_, Just hout, _, hndl) <-
        createProcess (proc "node" ["node_modules/solc/solcjs", path, compileArg, "--output-dir", tempdir])
            { std_out = CreatePipe }
    exitCode <- waitForProcess hndl
    -- We are assuming this is the contract name
    let contractName = takeBaseName path
    B.readFile (joinPath [tempdir, mangleFilename path ++ "_sol_" ++ contractName ++ outExt])

mangleFilename :: FilePath -> String
mangleFilename path =
    let
        withoutExtension = dropExtension path
        filepathPieces = splitDirectories withoutExtension
    in intercalate "_" filepathPieces