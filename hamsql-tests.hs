import System.Exit (ExitCode(ExitSuccess))
import System.Process (system)

main :: IO ()
main = do
    ExitSuccess <- system "make -C tests"
    return ()
