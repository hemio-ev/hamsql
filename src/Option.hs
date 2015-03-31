module Option where

import Options.Applicative

-- helper functions
boolFlag = flag False True
val xs = value xs <> metavar ("\""++xs++"\"")

-- Global

parserInfoHamsql = info (helper <*> parserCommand)
      ( fullDesc
     <> progDesc "A YamSql interpreter and smart executor."
     <> header "hamsql - YamSql interperter written in Haskell" )


-- Command
data Command =
    Install OptCommon OptCommonDb OptInstall |
    Upgrade OptCommon OptCommonDb OptUpgrade |
    Doc     OptCommon OptDoc
    deriving Show

parserCommand :: Parser Command
parserCommand = subparser
  ( command "install" (info parserCmdInstall
      ( progDesc "Installs the setup on a database from scratch." ))
 <> command "upgrade" (info parserCmdUpgrade
      ( progDesc "Upgrades an existing setup on a database." ))
 <> command "doc" (info parserCmdDoc
      ( progDesc "Produces a documentation of the setup." ))
  )

parserCmdInstall :: Parser Command
parserCmdInstall = Install
    <$> parserOptCommon 
    <*> parserOptCommonDb 
    <*> parserOptInstall

parserCmdUpgrade :: Parser Command
parserCmdUpgrade = Upgrade 
    <$> parserOptCommon 
    <*> parserOptCommonDb 
    <*> parserOptUpgrade

parserCmdDoc :: Parser Command
parserCmdDoc = Doc
    <$> parserOptCommon 
    <*> parserOptDoc

-- Commons

data OptCommon = OptCommon {
    optSetup :: FilePath,
    optVerbose :: Bool
}
    deriving Show

parserOptCommon :: Parser OptCommon
parserOptCommon = OptCommon
    <$> strOption
        (long "setup" <> 
        short 's' <> 
        help "Setup file (yaml)" <> 
        val "setup.yaml" <>
        action "file -X '!*.yml'" <>
        action "file -X '!*.yaml'" <>
        action "directory"
        )
    <*> boolFlag
        (long "verbose" <> short 'v' <> help "Verbose")

-- Commons Execute

data OptCommonDb = OptCommonDb {
    optEmulate :: Bool,
    optPrint :: Bool,
    optConnection :: String
}
    deriving Show

parserOptCommonDb :: Parser OptCommonDb
parserOptCommonDb = OptCommonDb
    <$> boolFlag
        (long "emulate" <> short 'e' <> help "Perform changes but rollback")
    <*> boolFlag
        (long "print" <> short 'p' <> help "Print SQL code instead of executing")
    <*> strOption
        (long "connection" <> short 'c' <> val "postgresql://")

-- Command Install
data OptInstall = OptInstall {
    optDeleteExistingDatabase :: Bool
}
    deriving Show

parserOptInstall :: Parser OptInstall
parserOptInstall = OptInstall
    <$> boolFlag
        (long "delete-existing-database"
        <> short 'd'
        <> help "Delete database if it allready exists"
        )

-- Command Upgrade
data OptUpgrade = OptUpgrade {
    optDeleteData :: Bool
}
    deriving Show

parserOptUpgrade :: Parser OptUpgrade
parserOptUpgrade = OptUpgrade
    <$> boolFlag
        (long "do-not-delete-data"
        <> short 's'
        <> help "Do not perform table/column deletion"
        )

-- Command Doc
data OptDoc = OptDoc {
    optFormat :: String
}
    deriving Show

parserOptDoc :: Parser OptDoc
parserOptDoc = OptDoc
    <$> strOption
        (long "format" <> short 'f' <> val "html" <> completeWith ["dot","html"])
