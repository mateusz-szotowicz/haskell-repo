import Control.Monad (forM_)
import Data.List (findIndex, findIndices, isInfixOf, tails)
import System.Directory
import System.FilePath
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
    isDir <- doesDirectoryExist filePath
    if (isDir) then do
        putStrLn $ "Currently checking directory " ++ filePath
        contentList <- listDirectory filePath
        forM_ contentList (\content -> goThroughContent (filePath </> content) stringToCrop)
    else do
        xFileName <- return $ takeBaseName filePath
        if (stringToCrop `isInfixOf` xFileName) then do
            putStrLn ("Renaming " ++ filePath)
            renameFile filePath (replaceBaseName filePath (removePart xFileName stringToCrop))
        else
            return ()
                
removePart :: String -> String -> String
removePart sourceString stringToRemove =
    if (stringToRemove `isInfixOf` sourceString) then
        let
            splitIndex = sourceString `indexOfPart` stringToRemove
            firstPart = take splitIndex sourceString
            secondPart = drop (splitIndex + (length stringToRemove)) sourceString
        in
            firstPart ++ secondPart
    else
        sourceString

indexOfPart :: String -> String -> Int
indexOfPart inputString part =
    let 
        partLength = length part
        partIndex = findIndex ((part == ) . take partLength) $ tails inputString
    in
        case partIndex of
            Just x -> x
            Nothing -> -1