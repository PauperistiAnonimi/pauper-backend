{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.AppM
import App.AppM.Database (getConnection)
import App.Config
import Server

main :: IO ()
main = do
    logger <- getLoggerSet
    let jwtS = makeJWTSettings "il segreto di puente viejo è che casta wratte ad ogni turno pagando con life e non con mana"
        dbCfg = DatabaseConfig "localhost" 5432 "postgres" "password" "pauper"
    conn <- getConnection dbCfg
    let cfg = Config "dev" "" "" "DEBUG" 9090 dbCfg jwtS
    runServer (AppCtx cfg logger conn)
