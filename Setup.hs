import Distribution.Simple
import Distribution.Types.LocalBuildInfo
import Distribution.Types.GenericPackageDescription
import System.Process

main = defaultMainWithHooks simpleUserHooks
    { preBuild = \args buildFlags -> do
        putStrLn "Running pre-build script"
        callProcess "scripts/build-TracyClient" []
        preBuild simpleUserHooks args buildFlags
    }
