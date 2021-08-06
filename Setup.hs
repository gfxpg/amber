import Data.Maybe (fromJust)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple
import qualified Distribution.Simple.LocalBuildInfo as LBI
import System.Directory (getCurrentDirectory)

main = defaultMainWithHooks simpleUserHooks {confHook = amberConfHook}

amberConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> Setup.ConfigFlags -> IO LBI.LocalBuildInfo
amberConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = LBI.localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return
    localBuildInfo
      { LBI.localPkgDescr =
          packageDescription
            { PD.library =
                Just $
                  library
                    { PD.libBuildInfo =
                        libraryBuildInfo
                          { PD.includeDirs = (dir ++ "/cpp/src") : PD.includeDirs libraryBuildInfo,
                            PD.extraLibDirs = (dir ++ "/cpp/build/src") : PD.extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
