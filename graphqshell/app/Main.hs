module Main where
import Relude
import Shell.Main (runShell)

main :: IO ()
main = runShell "http://foo.bar"
