{-# LANGUAGE OverloadedStrings #-}
import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Control.Monad

import Data.Attoparsec.ByteString
import Data.ByteString (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Base16 as B16
import Data.Either
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.List
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import Check.Stores
import OpCode.Exporter
import OpCode.Parser
import OpCode.Type
import Process
import OpCode.Utils

import Text.Printf
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Lib = Lib
    { lib_address :: Address
    , lib_metadata :: LibMetadata
    } deriving (Eq, Show, Ord)

data LibMetadata
    = KnownLib String
    | UnknownLib
    deriving (Eq, Show, Ord)

getLibMetadataMap :: IO (M.Map Address LibMetadata)
getLibMetadataMap = do
    file <- readFile "verified-addresses.txt"
    let entries = lines file
    pure $ M.fromList $ map getLibFromEntry entries

getLibFromEntry :: String -> (Address, LibMetadata)
getLibFromEntry str =
    let address = (\(Right x)->x) $ Address.fromText $ T.pack $ Data.List.take 40 $ Data.List.drop 2 $ str
        name = trim $ Data.List.drop 42 $ str
        metadata = if name == "" then UnknownLib else KnownLib name
    in (address, metadata)

-- |Invert the map so that we can see how often the libs are referenced.
invertReferences :: M.Map Address (S.Set Address) -> M.Map Address (S.Set Address)
invertReferences m = M.foldrWithKey' (\cAddress lAddresses acc -> S.foldr' (\s a -> M.insertWith S.union s (S.singleton cAddress) a) acc lAddresses) M.empty m

runWeb3 :: Web3 a -> IO (Either Web3Error a)
runWeb3 = runWeb3' (HttpProvider "http://localhost:8545")

main = do
    (cmd:args) <- getArgs
    case cmd of
        "count" -> do
            contractStore <- read <$> readFile dataFilePath
            putStrLn $ "Accounts: " ++ show (cds_nAccounts contractStore)
            putStrLn $ "Contracts: " ++ show (cds_nContracts contractStore)
        "get-transactions" -> mainBlocks
        "print-transactions" -> printTransactions
        "get-addresses" -> mainGetAddresses
        "get-data" -> mainGetData
        -- "get-transactions" -> mainGetTransactions
        "print-libs" -> do
            -- Get all the known contracts and accounts.
            contractStore <- read <$> readFile dataFilePath
            libNameMap <- getLibMetadataMap
            let
                -- |A map of contracts to references they hold to other contracts
                refMap = buildLibMap contractStore
                -- |A map of contracts to contracts that reference them
                libMap = invertReferences refMap
            printLibs libNameMap libMap

-- mainGetTransactions = do
--     -- Get all the known contracts and accounts.
--     contractStore <- read <$> readFile dataFilePath
--     let
--         -- |A map of contracts to references they hold to other contracts
--         refMap = buildLibMap contractStore
--         -- |A map of contracts to contracts that reference them
--         libMap = invertReferences refMap
--         -- targetLib is a wallet library of intereset
--         Right targetLib = Address.fromText "0x273930d21e01ee25e4c219b63259d214872220a2"
--         -- Get the set of all contracts which reference it
--         Just contractsOfInterest = M.lookup targetLib libMap
--     -- Print these contracts
--     mapM_ printTransactions $ S.toList contractsOfInterest
--     where
--         printTransactions address = do
--             nTransactionsFrom <- getNTransactionsFrom address
--             T.putStrLn $ ("0x" <> Address.toText address) <> " - " <> (T.pack $ show nTransactionsFrom)

getNTransactionsFrom :: Address -> IO (Either Web3Error Integer)
getNTransactionsFrom address = runWeb3 $ do
    unQuantity <$> Eth.getTransactionCount address block

buildLibMap :: ContractDataStore -> M.Map Address (S.Set Address)
buildLibMap cds@(ContractDataStore {cds_addresses = cds_addresses}) = Data.List.foldr f M.empty cds_addresses
    where
        f :: (Address, AddressInfo) -> M.Map Address (S.Set Address) -> M.Map Address (S.Set Address)
        f (address, addressInfo) acc = case addressInfo of
            AccountAddress -> acc
            ContractAddress refs -> M.insert address refs acc


-- printLibs :: M.Map Address (S.Set Address) -> IO ()
printLibs libNameMap = putStrLn . (showLibs libNameMap)

-- showLibs :: M.Map Address (S.Set Address) -> String
showLibs libNameMap = unlines . (map showIt) . (sortBy (\(_,a) (_,b)->compare a b)) . M.elems . (M.mapWithKey f)
    where
        showIt (address, count) = "0x" <> (T.unpack $ Address.toText address) <> " - " <> show count <> " references" ++ info address
        f k v = (k, S.size v)
        info address = case M.lookup address libNameMap of
            Just (KnownLib name) -> " (" ++ name ++ ")"
            _ -> ""

mainBlocks = do
    transactions <- concat <$> mapM getSimpleTransactions [1310324..1500000]
    writeFile "transactions.txt" (show transactions)

printTransactions = do
    transactions <- read <$> readFile "transactions.txt" :: IO [(Address, Address)]
    contractStore <- read <$> readFile dataFilePath
    libNameMap <- getLibMetadataMap
    let
        -- |A map of contracts to references they hold to other contracts
        refMap = buildLibMap contractStore
        -- |A map of contracts to contracts that reference them
        libMap = invertReferences refMap

    let m = foldr f M.empty transactions
    -- Go through each contract lib and add up the number of transactions it is
    -- associated with. The will result in double counting a transaction if it
    -- is on both ends.
    mapM_ (\(k,v) -> T.putStrLn ("0x" <> Address.toText k <> " - " <>  (T.pack $ show v) <> " references: " <> (T.pack $ show $ M.lookup k refMap)))
        $ sortBy (\(_,a) (_,b)-> compare a b) $ M.toList m
    let transMap = M.mapWithKey (g m) libMap
    mapM_ id $ M.mapWithKey (\k v -> putStrLn $ show k ++ " - " ++ show v) transMap
    printLibsWithTrans libNameMap transMap libMap
    where
        printFromTo (from, to) = print ("0x" <> Address.toText from, "0x" <> Address.toText to)
        f :: (Address, Address) -> M.Map Address Int -> M.Map Address Int
        f (from,to) m = M.insertWith (+) from 1 $ M.insertWith (+) from 1 m
        g m libAddress addresses =
            M.foldr (+) 0 $ m `M.restrictKeys` addresses

-- printLibs :: M.Map Address (S.Set Address) -> IO ()
printLibsWithTrans libNameMap transMap = putStrLn . (showLibsWithTrans libNameMap transMap)

-- showLibs :: M.Map Address (S.Set Address) -> String
showLibsWithTrans libNameMap transMap = unlines . (map showIt) . (sortBy (\(_,_,a) (_,_,b)->compare a b)) . M.elems . (M.mapWithKey f)
    where
        showIt (address, count, transCount) = "0x" <> (T.unpack $ Address.toText address) <> " - " <> show count <> " references" <> " - " <> show transCount <> " transactions" <> info address
        f k v = (k, S.size v, t)
            where t = case M.lookup k transMap of
                        Just x -> x
                        Nothing -> 0
        info address = case M.lookup address libNameMap of
            Just (KnownLib name) -> " (" ++ name ++ ")"
            _ -> ""


-- |Get a transaction between two addresses. Ignore those with no to address
getFromTo :: Transaction -> Maybe (Address, Address)
getFromTo trans = do
    to <- txTo trans
    pure (txFrom trans, to)

getAllTransactionFromBlock :: Integer -> IO [Transaction]
getAllTransactionFromBlock blocknumber = do
    let bnText = (T.pack $ printf "0x%x" blocknumber)
    putStrLn $ "Block Number: " ++ show bnText
    Right nTrans <- runWeb3 $ (unQuantity <$> getBlockTransactionCountByNumber bnText :: Web3 Integer)
    mapM (getTransactionFromBlock blocknumber) [0..(nTrans-1)]
    where
        getTransactionFromBlock blocknumber n =  do
            transR <- runWeb3 $ getTransactionByBlockNumberAndIndex (BlockWithNumber (BlockNumber blocknumber)) (T.pack $ show n)
            case transR of
                Left e -> error $ show e
                Right x -> case x of
                    Nothing -> error "Transaction could not be retrieved"
                    Just y -> pure y

getSimpleTransactions :: Integer -> IO [(Address, Address)]
getSimpleTransactions blocknumber =
    -- Discards all transactions without a to adddress
    mapMaybe (getFromTo) <$> getAllTransactionFromBlock blocknumber



dataFilePath :: FilePath
dataFilePath = "data.txt"

mainGetData :: IO ()
mainGetData = do
    m <- iterateAddresses emptyCDS Nothing 30000
    writeFile dataFilePath (show m)

mainGetAddresses = iterateGetAddresses Nothing 5000

iterateGetAddresses :: Maybe Address -> Int -> IO ()
iterateGetAddresses offset n = do
    let del = 50
    if n > 0
        -- There are more addresses to be retrieved.
        then do
            let numReq = (min del n)
            -- Ask for some addresses
            adds <- getAddresses offset numReq
            case adds of
                -- There was an error in retrieving thelist of addresses
                Left e -> error (show e)
                -- The list of addresses was retrieved
                Right [] -> pure ()
                Right x -> do
                    mapM_ (T.putStrLn . ((<>) "0x") . Address.toText) x
                    iterateGetAddresses (Just $ last x) (n-(length x))
        -- We have retrieved the desired number of addresses, end.
        else pure ()

emptyCDS = ContractDataStore 0 0 []

data ContractDataStore = ContractDataStore
    { cds_nAccounts :: Int
    , cds_nContracts :: Int
    , cds_addresses ::[(Address, AddressInfo)]
    } deriving (Show, Read, Eq)

data AddressInfo = AccountAddress | ContractAddress (S.Set Address) deriving (Show, Read, Eq)

addAddress :: ContractDataStore -> Address -> AddressInfo -> ContractDataStore
addAddress cds addr addrInfo = cdsIncd {cds_addresses = ((addr,addrInfo):(cds_addresses cds))}
    where
        cdsIncd =  case addrInfo of
            AccountAddress -> incAccounts cds
            ContractAddress _ -> incContracts cds

incAccounts :: ContractDataStore -> ContractDataStore
incAccounts cds = cds {cds_nAccounts = cds_nAccounts cds  + 1}

incContracts :: ContractDataStore -> ContractDataStore
incContracts cds = cds {cds_nContracts = cds_nContracts cds + 1}

addAddressToStore :: ContractDataStore -> Address -> IO ContractDataStore
addAddressToStore cds address = do
    (T.putStr . ((<>) "0x") . Address.toText) address
    contractCode <- getContract address
    addrInfo <- case contractCode of
        (Right (Just code)) -> do
            let (bytecode, remainder) = B16.decode $ encodeUtf8 $ T.drop 2 code
            if remainder /= B.empty then error (show remainder) else pure ()
            case parseOnly (parseOpCodes <* endOfInput) bytecode of
                Left err -> do
                    putStrLn $ " - Opcodes could not be parsed in full: " ++ err
                    pure $ ContractAddress S.empty
                Right parsed -> do
                    let addresses = findReferences parsed
                    putStrLn $ " - Contract (" <>  show  ((T.length code) `div` 2) <> " bytes)"
                    pure $ ContractAddress addresses
        (Right Nothing) -> putStrLn " - Account" >> pure AccountAddress
        e -> error (show e)
    pure $ addAddress cds address addrInfo

findReferences :: [OpCode] -> S.Set Address
findReferences parsed =
    let addresses = map (\(PUSH20 bs) -> Address.fromText $ decodeUtf8 $ B16.encode bs) $ filter isPUSH20 parsed
    in case rights addresses of
        [] -> S.empty
        xs -> S.fromList xs


iterateAddresses :: ContractDataStore -> Maybe Address -> Int -> IO ContractDataStore
iterateAddresses cds offset n = do
    let del = 20
    if n > 0
        -- There are more addresses to be retrieved.
        then do
            let numReq = (min del n)
            -- Ask for some addresses
            adds <- getAddresses offset numReq
            case adds of
                -- There was an error in retrieving thelist of addresses
                Left e -> error (show e)
                -- The list of addresses was retrieved
                Right x -> do
                    -- For each of the addresses, retrieve its contract code (if
                    -- it is a contract) add add its information to the store
                    (_,cdsNew) <- foldM (\(i,cds) add-> do
                        putStr $ "(" ++ show i ++ " remaining) "
                        c <- addAddressToStore cds add
                        pure (i-1,c)
                        ) (n, cds) x
                    case x of
                        -- There were no more addresses, simply return
                        [] -> pure cds
                        -- There were addresses, add them
                        _ -> iterateAddresses cdsNew (Just $ last x) (n-(length x))
        -- We have retrieved the desired number of addresses, end.
        else pure cds

printMap :: (Show a, Show b, Traversable t) => M.Map a (t b) -> IO ()
printMap m = let l = M.toList m
    in mapM_ (\(k,v)->putStr "Contract: " >> print k >> putStrLn "  References: " >> mapM_ (\x->putStr "    " >> print x) v) l

block = (BlockWithNumber (BlockNumber 1500000))

getContract :: Address -> IO (Either Web3Error (Maybe Text))
getContract address = runWeb3 $ do
    code <- Eth.getCode address block
    pure $ if code == "0x"
        then Nothing
        else Just code

-- TODO: write something streaming for this
getAddresses :: Maybe Address -> Int -> IO (Either Web3Error [Address])
getAddresses start n = runWeb3 $ do
        theCall <- Eth.listAccounts n start block
        pure (theCall)

toGraph m = do
    putStrLn "digraph g {"
    let l = M.toList m
    mapM_ (\(k,v)->mapM_ (\x->putStr "  \"" >> putStr (T.unpack $ Address.toText k) >> putStr "\" -> \"" >> putStrLn (T.unpack (Address.toText x) ++ "\"")) v) l
    putStrLn "}"

filterAddress :: M.Map Address (S.Set Address) -> Address -> M.Map Address (S.Set Address)
filterAddress m address = M.filter (S.member address) m
