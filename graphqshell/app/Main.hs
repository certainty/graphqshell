module Main where
import Shell.Main (runShell)

main :: IO ()
main = runShell "http://foo.bar"
