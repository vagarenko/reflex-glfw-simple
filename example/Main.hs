{-# LANGUAGE
      FlexibleContexts
    , RecursiveDo
    , TypeApplications
    , RecordWildCards
#-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Witherable
import Reflex
import Reflex.GLFW.Simple
import Reflex.GLFW.Simple.Events (errors, monitor, joystick)
import Reflex.Host.Headless

import Prelude hiding (filter)

import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    bracket_ initialize GLFW.terminate $
        bracket createWindow (\w -> GLFW.setWindowShouldClose w True >> GLFW.destroyWindow w) $ \win -> do
            runHeadlessApp $ mdo
                WindowReflexes {..} <- windowReflexes win
                printE "errors"   =<< errors
                printE "monitor"  =<< monitor
                printE "joystick" =<< joystick

                printE "refresh"     windowRefresh
                printE "char"        char
                printE "charMods"    charMods
                printE "mouseButton" mouseButton
                printE "scroll"      scroll
                printE "fileDrop"    fileDrop
                printE "close"       windowClose
                printE "key"         key

                printDyn "windowPos"       windowPos
                printDyn "size"            windowSize
                printDyn "focus"           windowFocus
                printDyn "iconify"         windowIconify
                printDyn "framebufferSize" framebufferSize
                printDyn "cursorEnter"     cursorEnter
                printDyn "cursorPos"       cursorPos

                printDyn "key W down dyn" =<< mkKeyDownDyn GLFW.Key'W
                printDyn "key W up dyn"   =<< mkKeyUpDyn   GLFW.Key'W
                
                ePostBuild <- getPostBuild
                eInput <- performEventAsync ((\f -> liftIO $ GLFW.waitEvents >> f ()) <$ leftmost [ePostBuild, eInput])

                let escapePressed = void $ filter (\(k, _, s, _) -> k == GLFW.Key'Escape && s == GLFW.KeyState'Pressed ) key
                    eQuit = leftmost [escapePressed, windowClose]
                pure eQuit

printE :: (PerformEvent t m, MonadIO (Performable m), Show a) => String -> Event t a -> m ()
printE desc ev =
    performEvent_ (fmap (\e -> liftIO $ putStrLn (desc ++ " " ++ show e)) ev)

printDyn :: (PerformEvent t m, MonadIO (Performable m), Show a) => String -> Dynamic t a -> m ()
printDyn desc = printE desc . updated

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
