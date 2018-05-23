{-# LANGUAGE OverloadedStrings #-}
import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Control.Monad
import Control.Exception
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString
import Data.ByteString (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Base16 as B16
import qualified Data.Csv as CSV
import Data.Either
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.List
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Time

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp
import System.IO

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
-- runWeb3 :: Web3 a -> IO (Either Web3Error a)
-- runWeb3 = runWeb3' (HttpProvider "https://mainnet.infura.io/8cIXez93NfaTTAqSBhTt:8545")

blockDir :: FilePath
blockDir = joinPath ["data-query", "blocks"]

main :: IO ()
main = do
    (cmd:args) <- getArgs
    case cmd of
        "get-transactions" -> getTransactions
        "print-transactions" -> printTransactions
        "get-addresses" -> mainGetAddresses
        "get-data" -> do
            m <- iterateAddresses emptyCDS Nothing 30000
            writeFile fromStartDataFilePath (show m)
        -- "get-transactions" -> mainGetTransactions
        "print-contracts" -> printContractsMain
        "print-libs" -> do
            cacheExits <- doesFileExist dataFilePath
            oldCacheExits <- doesFileExist oldDataFilePath
            -- Get all the known contracts and accounts.
            oldCachedMap <- if oldCacheExits
                then read <$> readFile oldDataFilePath
                else pure M.empty
            newCachedMap <- if cacheExits
                then do
                    raw <- lines <$> readFile dataFilePath
                    let entryList = map read raw
                    pure $ M.fromList entryList
                else pure M.empty
            let cachedMap = M.union oldCachedMap newCachedMap
            libNameMap <- getLibMetadataMap
            let
                -- |A map of contracts to references they hold to other contracts
                refMap = M.map (\(ContractAddress ref)->ref) $ M.filter isContract cachedMap
                -- |A map of contracts to contracts that reference them
                libMap = invertReferences refMap
            printLibs libNameMap libMap
        "print-libs-end" -> do
            let n = case args of
                    (a:_) -> read a
                    _ -> 20000
            -- Find all the available block files in the block directory
            createDirectoryIfMissing True blockDir
            -- The the latest block number we will process. We will start here
            -- and walk backwards through the blockchain.
            let endBlockNumber = 4900000
                -- Number of blocks to process
            -- A map of addresses to known contract names
            libNameMap <- getLibMetadataMap
            -- Create a reference map, a map from a contract address to any
            -- addresses it references.
            -- First check if there is already a data
            -- file contiaining this information.
            cacheExits <- doesFileExist dataFilePath
            oldCacheExits <- doesFileExist oldDataFilePath
            -- If the cache exists, read it in and use that as a starting point,
            -- else start with an empy map.
            oldCachedMap <- if oldCacheExits
                then read <$> readFile oldDataFilePath
                else pure M.empty
            newCachedMap <- if cacheExits
                then do
                    raw <- lines <$> readFile dataFilePath
                    let entryList = map read raw
                    pure $ M.fromList entryList
                else pure M.empty
            let cachedMap = M.union oldCachedMap newCachedMap
            knownWallets <- S.fromList
                <$> map (\(Right x)->x) <$> map Address.fromText
                <$> T.lines <$> T.readFile "KnownWalletLibs.txt"
            -- Process each of the blocks, starting at the specified end block
            -- and working backwatds, updating the data structures as we go.
            (Just startTime, Just endTime, contractStore, transactionMap, newlyRecognisedWallets)
                <- foldM
                    (processBlock knownWallets)
                    (Nothing, Nothing, cachedMap, M.empty, [])
                    [endBlockNumber,(endBlockNumber-1)..(endBlockNumber-n-1)]
            let
                -- Filter this map to only list references to other contracts.
                -- This is now a map of contracts to contracts which they
                -- reference.
                refMap = M.map (\(ContractAddress ref)->ref) $ M.filter isContract contractStore
                -- Invert that map to create a map of contracts to contracts
                -- that reference them.
                libMap = invertReferences refMap
                -- Find the number of transactions for each lib. Go through each
                -- contract lib and add up the number of transactions it is
                -- associated with. The will result in double counting a
                -- transaction if it is on both ends.
                libTransMap = M.map (g transactionMap) libMap
            printLibsWithTrans libNameMap libTransMap libMap
            putStrLn $ "Data from " ++ show startTime ++ " to " ++ show endTime
            -- Filter transaction map to show only contracts that reference known wallet libs
            let knownTrans = M.filterWithKey (\k v->referencesKnownLib knownWallets contractStore k) (transactionMap :: M.Map Address Int)
                totalKnownTrans = M.foldr' (+) 0 knownTrans
            mapM_ id $ M.mapWithKey (\address n->printf "%s - %d - %.2f%%\n" ("0x" <> Address.toText address) n (100*(fromIntegral n)/(fromIntegral totalKnownTrans) :: Double)) knownTrans
            putStrLn "Recognised Wallet Growth"
            mapM_ (\(t,n)->printf "%s,%d\n" (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t) n) $ reduceNewlyRecongisedAddress newlyRecognisedWallets
            where
                printFromTo (from, to) = print ("0x" <> Address.toText from, "0x" <> Address.toText to)
                g transMap addresses =
                    M.foldr (+) 0 $ transMap `M.restrictKeys` addresses

reduceNewlyRecongisedAddress :: [(UTCTime, Int)] -> [(UTCTime,Int)]
reduceNewlyRecongisedAddress = reduceNewlyRecongisedAddressWorker []

reduceNewlyRecongisedAddressWorker :: [(UTCTime, Int)] -> [(UTCTime,Int)] -> [(UTCTime,Int)]
reduceNewlyRecongisedAddressWorker acc [] = reverse acc
reduceNewlyRecongisedAddressWorker acc ls =
    let (top,end) = if length ls < 50
            then (ls,[])
            else splitAt 50 ls
        t = fst $ last top
        s = sum $ map snd top
    in reduceNewlyRecongisedAddressWorker ((t,s):acc) end

referencesKnownLib :: S.Set Address -> M.Map Address AddressInfo -> Address -> Bool
referencesKnownLib knownLibs contractStore address =
    let refs = M.lookup address contractStore
    in case refs of
        Nothing -> False
        Just addrInfo -> case addrInfo of
            AccountAddress -> False
            ContractAddress rs -> not $ S.null $ S.intersection knownLibs rs


processBlock knownWalletLibs args bn = do
    res <- Control.Exception.try $ processBlock' knownWalletLibs args bn
    case res of
        Left e -> let y = e :: SomeException in processBlock' knownWalletLibs args bn
        Right x -> pure x

processBlock' knownWalletLibs (startTime, endTime, refMap, transactionMap, newlyRecognisedWallets) blocknumberInt = do
    putStr $ "Processing block: #" ++ show blocknumberInt
    let blocknumber = BlockNumber blocknumberInt
        filePath = joinPath [blockDir, show blocknumberInt ++ ".json"]
    -- First, check if the data is already on disk
    alreadyExists <- doesFileExist filePath
    block <- if alreadyExists
        -- File exists already, so use that
        then do
            readIn <- Aeson.eitherDecode <$> BL.readFile filePath
            let block = case readIn of
                    Left e -> error $ show e
                    Right x -> x
            putStr $ ("  " ++ (show $ Network.Ethereum.Web3.Types.blockNumber block)) ++ " from cache"
            pure block
        -- File does not exist and the block has to be retrieved
        else do
            let bnText = (T.pack $ printf "0x%x" blocknumberInt)
            blockR <- runWeb3 $ Eth.getBlockByNumber bnText
            case blockR of
                Left e -> error $ show e
                Right block -> do
                    BL.writeFile filePath $ Aeson.encode block
                    putStr $ ("  " ++ (show $ Network.Ethereum.Web3.Types.blockNumber block)) ++ " from Infura"
                    pure block
    let thisBlockTime = posixSecondsToUTCTime $ fromInteger $ read $ T.unpack $ blockTimestamp block
    putStrLn $ " - " ++ (show $ thisBlockTime)
    let newTransactionMap = getTransactionNumbers transactionMap $ blockTransactions block
    putStrLn $ "  Transaction map size: " ++ (show $ M.size newTransactionMap)
    newRefMap <- buildRefMap (BlockWithNumber blocknumber) refMap $ addressesFromBlock block
    let newEndTime = case endTime of
            Just s -> Just s
            Nothing -> Just thisBlockTime
        -- Get all of the addresses that are new in this block, that is
        -- addresses from transactions of this block that have not previously
        -- been involved in a transaction.
        newlyUsedAddresses = S.fromList $ filter (\addr -> not $ addr `M.member` transactionMap) $ addressesFromBlock block
        -- Filter this to only include addresses which reference wallet
        -- libraries. This needs to use the new refMap, as these are inherently
        -- new addresses.
        newWallets = S.filter (referencesKnownLib knownWalletLibs newRefMap) newlyUsedAddresses
    pure $ seq (S.size newWallets) $ (Just thisBlockTime, newEndTime, newRefMap, newTransactionMap, (thisBlockTime,S.size newWallets):newlyRecognisedWallets)

addressesFromBlock :: Block -> [Address]
addressesFromBlock block =
    let transactions = blockTransactions block
    in concat $ map addressesFromTransaction transactions
    where

addressesFromTransaction :: Transaction -> [Address]
addressesFromTransaction trans = case txTo trans of
    Nothing -> [txFrom trans]
    Just to -> [txFrom trans, to]

printContractsMain = do
    transactions <- read <$> readFile "transactions.txt" :: IO [(Address, Address)]
    -- Get all the known contracts and accounts.
    contractStore <- read <$> readFile dataFilePath
    libNameMap <- getLibMetadataMap
    let
        -- |A map of contracts to references they hold to other contracts
        refMap = buildLibMap contractStore
        -- |A map of contracts to contracts that reference them
        libMap = invertReferences refMap
        m = foldr f M.empty transactions :: M.Map Address Int
        transMap = M.mapWithKey (g m) libMap
    printContracts libNameMap m libMap
    where
        printFromTo (from, to) = print ("0x" <> Address.toText from, "0x" <> Address.toText to)
        f :: (Address, Address) -> M.Map Address Int -> M.Map Address Int
        f (from,to) m = M.insertWith (+) from 1 $ M.insertWith (+) from 1 m
        g m libAddress addresses =
            M.foldr (+) 0 $ m `M.restrictKeys` addresses

-- printLibs :: M.Map Address (S.Set Address) -> IO ()
printContracts libNameMap transMap = putStrLn . (showContracts libNameMap transMap)

-- showLibs :: M.Map Address (S.Set Address) -> String
showContracts libNameMap transMap = unlines . (map showIt) . (sortBy (\(_,a,_) (_,b,_)->compare a b)) . M.elems . (M.mapWithKey f)
    where
        showIt (address, count, refs) = "0x" <> (T.unpack $ Address.toText address) <> " - " <> show count <> " references" <> info address
            <> "\n" <> unlines (map (\x->"  0x" <> (T.unpack $ Address.toText x) <> " - " <> (show $ M.lookup x transMap)) $ S.toList refs)
        f k v = (k, S.size v, v)
        info address = case M.lookup address libNameMap of
            Just (KnownLib name) -> " (" ++ name ++ ")"
            _ -> ""

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

getTransactions = do
    transactions <- concat <$> mapM getAllTransactionFromBlock [1500000..1500003]
    BL.writeFile "transactions.csv" (CSV.encodeDefaultOrderedByName transactions)

instance CSV.FromNamedRecord Transaction
instance CSV.ToNamedRecord Transaction
instance CSV.DefaultOrdered Transaction where
    headerOrder _ = CSV.header
        [ "txBlockNumber"
        , "txTransactionIndex"
        , "txFrom"
        , "txTo"
        , "txHash"
        , "txNonce"
        , "txBlockHash"
        , "txValue"
        , "txGasPrice"
        , "txGas"
        , "txInput"
        ]

instance CSV.FromRecord Transaction where
    parseRecord v
        | length v == 11 = Transaction
            <$> v CSV..! 0
            <*> v CSV..! 1
            <*> v CSV..! 2
            <*> v CSV..! 3
            <*> v CSV..! 4
            <*> v CSV..! 5
            <*> v CSV..! 6
            <*> v CSV..! 7
            <*> v CSV..! 8
            <*> v CSV..! 9
            <*> v CSV..! 10
        | otherwise     = mzero

instance CSV.ToRecord Transaction where
    toRecord trans = CSV.record
        [ CSV.toField $ txHash trans
        , CSV.toField $ txNonce trans
        , CSV.toField $ txBlockHash trans
        , CSV.toField $ txBlockNumber trans
        , CSV.toField $ txTransactionIndex trans
        , CSV.toField $ txFrom trans
        , CSV.toField $ txTo trans
        , CSV.toField $ txValue trans
        , CSV.toField $ txGasPrice trans
        , CSV.toField $ txGas trans
        , CSV.toField $ txInput trans
        ]

instance CSV.FromField Address where
    parseField s = case Address.fromText $ decodeUtf8 s of
        Left _ -> mzero
        Right x -> pure x

instance CSV.ToField Address where
    toField address = encodeUtf8 $ "0x" <> Address.toText address

instance CSV.FromField BlockNumber where
    parseField s = BlockNumber <$> CSV.parseField s

instance CSV.ToField BlockNumber where
    toField (BlockNumber n) = CSV.toField n

printTransactions = do
    readIn <- CSV.decodeByName <$> BL.readFile "transactions.csv"
    let transactions = case readIn of
            Left e -> error $ show e
            Right (h, x) -> V.toList x :: [Transaction]

    contractStore <- read <$> readFile dataFilePath
    libNameMap <- getLibMetadataMap
    let
        -- |A map of contracts to references they hold to other contracts
        refMap = buildLibMap contractStore
        -- |A map of contracts to contracts that reference them
        libMap = invertReferences refMap

    let m = getTransactionNumbers M.empty transactions
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
        g m libAddress addresses =
            M.foldr (+) 0 $ m `M.restrictKeys` addresses

getTransactionNumbers m transactions = foldr f m transactions
    where
        f :: Transaction -> M.Map Address Int -> M.Map Address Int
        f trans m = M.insertWith (+) from 1 $ case toMaybe of
            Just to -> M.insertWith (+) to 1 m
            Nothing -> m
            where
                from = txFrom trans
                toMaybe = txTo trans
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
dataFilePath = "data-lines.txt"

oldDataFilePath :: FilePath
oldDataFilePath = "data.txt"

fromStartDataFilePath :: FilePath
fromStartDataFilePath = "from-start-data.txt"

mainGetAddresses = iterateGetAddresses Nothing 5000

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

buildRefMap :: DefaultBlock -> M.Map Address AddressInfo -> [Address] -> IO (M.Map Address AddressInfo)
buildRefMap block cachedMap addresses = do
    addAddressesToRefMap block cachedMap addresses

addAddressesToRefMap :: DefaultBlock -> M.Map Address AddressInfo -> [Address] -> IO (M.Map Address AddressInfo)
addAddressesToRefMap block cachedMap addresses = do
    let unknownAddresses = filter (\x-> not $ x `M.member` cachedMap) addresses
    if length unknownAddresses == 0
        then do
            putStrLn "  All addresses known"
            pure cachedMap
        else do
            print $ "Retrieving " ++ show (length unknownAddresses) ++ " addresses from network"
            t1 <- getCurrentTime
            (Right codes) <- getContracts block unknownAddresses
            t2 <- getCurrentTime
            print $ diffUTCTime t2 t1
            withFile dataFilePath AppendMode $ \handle -> do
                newRefMap <- foldM' (\cMap (address, contractCode) -> do
                    let addrInfoR = getAddressInfo address contractCode
                    T.putStrLn $ "    addAddressToRefMap: " <> "0x" <> (Address.toText address) <> " - " <> T.pack (show addrInfoR)
                    let addrInfo = fromRight (ContractAddress S.empty) addrInfoR
                    hPutStrLn handle $ show (address, addrInfo)
                    pure $ addAddressToRefMap cMap (address, addrInfo)
                    ) cachedMap (zip unknownAddresses codes) :: IO (M.Map Address AddressInfo)
                pure newRefMap

addAddressToRefMap :: M.Map Address AddressInfo -> (Address, AddressInfo) -> M.Map Address AddressInfo
addAddressToRefMap refMap (address, addressInfo) =
    M.insert address addressInfo refMap

getAddressInfo :: Address -> Maybe Text -> Either String AddressInfo
getAddressInfo address contractCode = do
    case contractCode of
        Just code -> do
            let (bytecode, remainder) = B16.decode $ encodeUtf8 $ T.drop 2 code
            if remainder /= B.empty then error (show remainder) else pure ()
            case parseOnly (parseOpCodes <* endOfInput) bytecode of
                Left err -> Left $ "  - Opcodes could not be parsed in full: " ++ err
                Right parsed ->
                    let addresses = findReferences parsed
                    in Right $ ContractAddress addresses
        Nothing -> Right AccountAddress

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

isContract (ContractAddress _) = True
isContract _ = False

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

getContracts :: DefaultBlock -> [Address] -> IO (Either Web3Error ([Maybe Text]))
getContracts block addresses = do
    codes <- Control.Exception.try $ runWeb3 $ do
        codes <- batchCall $ map (\add->getCodeReq add block) addresses :: Web3 [Text]
        pure $ map f codes
    case codes of
        Left e -> let y = e :: SomeException in getContracts block addresses
        Right x -> pure x
    where
        f :: Text -> Maybe Text
        f code = if code == "0x"
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
