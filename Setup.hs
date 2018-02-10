import Distribution.Simple
import Distribution.Simple.Test
import Distribution.Simple.LocalBuildInfo

import System.Environment

main = defaultMainWithHooks simpleUserHooks { testHook = myTest }

myTest args packageDescription localBuildInfo _ flags = do
    setEnv "CABAL_BUILD_DIR" (buildDir localBuildInfo)
    test args packageDescription localBuildInfo flags
