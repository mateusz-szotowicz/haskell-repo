import Data.List (findIndices, isInfixOf, tails)
import System.Directory
import System.Environment (getArgs)

main = do
    args <- getArgs
    if (length args > 0) then do
        currentDir <- getCurrentDirectory
        goThroughContent currentDir (args !! 0)
    else
        putStrLn "No argument given!"
    
goThroughContent :: FilePath -> String -> IO ()
goThroughContent filePath stringToCrop = do
    isFile <- doesFileExist filePath
    if (isFile) then do
        putStrLn ("Renaming " ++ filePath)
        renameFile filePath (removePart filePath stringToCrop)
    else do
        isDir <- doesDirectoryExist filePath
        if (isDir) then do
            putStrLn $ "Currently checking directory " ++ filePath
            contentList <- listDirectory filePath
            mapM_ getPathAndTraverse contentList
        else
            putStrLn ("There is something weird with " ++ filePath)
    where
        getPathAndTraverse fileName = goThroughContent (filePath ++ ('\\' : fileName)) stringToCrop
        
extractFileName :: String -> (String, String)
extractFileName filePath =
    let cutIndex = succ . last $ findIndices (=='\\') filePath
    in (take cutIndex filePath, drop cutIndex filePath)
                
removePart :: FilePath -> String -> FilePath
removePart sourcePath stringToRemove =
    if (stringToRemove `isInfixOf` sourcePath) then
        let
            splitIndex = sourcePath `indexOfPart` stringToRemove
            firstPart = take splitIndex sourcePath
            secondPart = drop (splitIndex + (length stringToRemove)) sourcePath
        in
            firstPart ++ secondPart
    else
        sourcePath
        
extractFileName :: String -> (String, String)
extractFileName filePath =
    let (fileNameReversed, fileDirReversed) = break (\x -> x!= '\\' && x != '/') $ reverse filePath
    in (reverse fileDirReversed, reverse fileNameReversed)

indexOfPart :: String -> String -> Int
indexOfPart inputString part =
    let 
        partLength = length part
        partIndex = findIndex ((part == ) . take partLength) $ tails inputString
    in
        case partIndex of
            Just x -> x
            Nothing -> -1