import System.Directory -- we need version over 1.2.1.0
import Text.Regex.Posix
import Control.Monad (filterM)
import Data.Function (fix)

-- String matching
-- http://www.comp.leeds.ac.uk/Perl/matching.html
-- http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html

basePath = "/Users/hiogawa/Dropbox/mydoc/english/instant_composition/jroyal_clips"


-- return only files in the directory given by an argument
getfiles :: FilePath -> IO [FilePath]
getfiles dir = do entries <- getDirectoryContents dir
                  filterM (\x -> doesFileExist x) $ map ((dir)++) entries


-- rename the names of files matching a certain regular expression
main0 = do setCurrentDirectory basePath
           entries <- getDirectoryContents "./"
           let files5678_m4a = filter (\x -> (x =~ "^[5678]") && (x =~ "m4a$")) entries
           mapM (\x -> renameFile x ("0" ++ x)) files5678_m4a

-- move files to other directory
main1 = do setCurrentDirectory basePath
           createDirectory "051_100"
           mapM (\x -> renameFile x ("051_100/" ++ x)) =<< getfiles "./"
           
-- rename files "2015-04-21_??.wav" to "???.m4a"
indexes = take 40 $ fix (\l n -> n : l (n + 1)) 111  -- 111 ~ 150
main2 = do setCurrentDirectory basePath
           files2015 <- fmap (filter (=~("^2015"))) $ getDirectoryContents "./"
           mapM (\(x,i) -> renameFile x (show i ++ ".wav")) $ zip files2015 indexes
           

indexes' = filter (\n -> 151 <= n && n <= 183) $ take 200 $ iterate (+1) 0
main3 = do setCurrentDirectory (basePath ++ "/151_200")
           files2015 <- fmap (filter (=~("^2015"))) $ getDirectoryContents "./"
           mapM (\(x,i) -> renameFile x (show i ++ ".wav")) $ zip files2015 indexes'
           
