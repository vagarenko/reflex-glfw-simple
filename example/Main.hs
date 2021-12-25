{-# LANGUAGE
      FlexibleContexts
    , RecursiveDo
#-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Witherable
import Reflex
import Reflex.GLFW.Simple.Events
import Reflex.Host.Basic
import Prelude hiding (filter)
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    bracket_ initialize GLFW.terminate $
        bracket createWindow (\w -> GLFW.setWindowShouldClose w True >> GLFW.destroyWindow w) $ \win -> do
            basicHostWithQuit $ mdo
                printE "errors"          =<< errors
                printE "monitor"         =<< monitor
                printE "joystick"        =<< joystick
                printE "windowPos"       =<< windowPos win
                printE "size"            =<< windowSize win
                printE "refresh"         =<< windowRefresh win
                printE "focus"           =<< windowFocus win
                printE "iconify"         =<< windowIconify win
                printE "framebufferSize" =<< framebufferSize win
                printE "char"            =<< char win
                printE "charMods"        =<< charMods win
                printE "mouseButton"     =<< mouseButton win
                printE "cursorEnter"     =<< cursorEnter win
                printE "scroll"          =<< scroll win
                printE "fileDrop"        =<< fileDrop win
                printE "cursorPos"       =<< cursorPos win
                
                eWindowClose <- windowClose win
                eKey <- key win

                printE "close" eWindowClose
                printE "key" eKey
                
                ePostBuild <- getPostBuild
                eInput <- performEventAsync ((\f -> liftIO $ GLFW.waitEvents >> f ()) <$ leftmost [ePostBuild, eInput])

                let escapePressed = void $ filter (\(k, _, s, _) -> k == GLFW.Key'Escape && s == GLFW.KeyState'Pressed ) eKey
                    eQuit = leftmost [escapePressed, eWindowClose]
                pure eQuit

printE :: (PerformEvent t m, MonadIO (Performable m), Show a) => String -> Event t a -> m ()
printE desc ev =
    performEvent_ (fmap (\e -> liftIO $ putStrLn (desc ++ " " ++ show e)) ev)

createWindow :: IO GLFW.Window
createWindow = do
    win <- GLFW.createWindow 600 400 "reflex-glfw-simple example" Nothing Nothing
    case win of
        Nothing -> error "GLFW.createWindow error."
        Just w -> do
            GLFW.makeContextCurrent $ Just w
            pure w

initialize :: IO ()
initialize = do
    r <- GLFW.init
    unless r (error "GLFW.init error.")
