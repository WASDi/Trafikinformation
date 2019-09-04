module TrafikLib.Play
  ( playUrl
  ) where

import qualified System.Process as P

playUrl :: String -> IO ()
playUrl url = do
  tempFile <- init <$> P.readProcess "mktemp" ["--suffix", ".mp3"] []
  download url tempFile
  play tempFile
  remove tempFile
  return ()

download :: String -> String -> IO String
download url outputFile = P.readProcess "wget" ["--quiet", "-O", outputFile, url] []

play :: String -> IO String
play file = P.readProcess "play" [file, "-q", "fade", "1", "-0", "1"] []

remove :: String -> IO String
remove file = P.readProcess "rm" [file] []
