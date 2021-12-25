{-# LANGUAGE RecordWildCards  #-}

module Reflex.GLFW.Simple (
      E.errors
    , E.monitor
    , E.joystick
    , WindowEventsAndDyns(..)
    , windowEventsAndDyns
    , filterMouseButtonE
    , mouseButtonDyn
) where

import Data.Bifunctor
import Control.Monad.IO.Class ( MonadIO(..) )
import Reflex ( Reflex(Event, Dynamic), MonadHold(), TriggerEvent(), holdDyn, ffilter )
import qualified Graphics.UI.GLFW as GLFW
import qualified Reflex.GLFW.Simple.Events as E

-- | Events and Dynamics of a window.
data WindowEventsAndDyns t a = WindowEventsAndDyns
    { window          :: GLFW.Window
    , windowPos       :: Dynamic t (Int, Int)
    , windowSize      :: Dynamic t (Int, Int)
    , windowClose     :: Event t ()
    , windowRefresh   :: Event t ()
    , windowFocus     :: Dynamic t Bool
    , windowIconify   :: Dynamic t Bool
    , framebufferSize :: Dynamic t (Int, Int)
    , key             :: Event t (GLFW.Key, Int, GLFW.KeyState, GLFW.ModifierKeys)
    , char            :: Event t Char
    , charMods        :: Event t (Char, GLFW.ModifierKeys)
    , mouseButton     :: Event t (GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys)
    , cursorPos       :: Dynamic t (a, a)
    , cursorEnter     :: Dynamic t GLFW.CursorState
    , scroll          :: Event t (a, a)
    , fileDrop        :: Event t [FilePath]
    }

-- | Create Events and Dynamics for given window.
windowEventsAndDyns ::
       (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m, Floating a)
    => GLFW.Window
    -> m (WindowEventsAndDyns t a)
windowEventsAndDyns window = do
    windowPos0       <- liftIO $ GLFW.getWindowPos window
    windowSize0      <- liftIO $ GLFW.getWindowSize window
    windowFocus0     <- liftIO $ GLFW.getWindowFocused window
    windowIconify0   <- liftIO $ GLFW.getWindowIconified window
    framebufferSize0 <- liftIO $ GLFW.getFramebufferSize window
    cursorPos0       <- liftIO $ GLFW.getCursorPos window
    cursorEnter0     <- liftIO $ GLFW.getWindowAttrib window GLFW.WindowAttrib'Hovered

    windowPos       <- holdDyn windowPos0 =<< E.windowPos window
    windowSize      <- holdDyn windowSize0 =<< E.windowSize window
    windowClose     <- E.windowClose window
    windowRefresh   <- E.windowRefresh window
    windowFocus     <- holdDyn windowFocus0 =<< E.windowFocus window
    windowIconify   <- holdDyn windowIconify0 =<< E.windowIconify window
    framebufferSize <- holdDyn framebufferSize0 =<< E.framebufferSize window
    key             <- E.key window
    char            <- E.char window
    charMods        <- E.charMods window
    mouseButton     <- E.mouseButton window
    cursorPos       <- holdDyn (bimap realToFrac realToFrac cursorPos0) =<< E.cursorPos window
    cursorEnter     <- holdDyn (if cursorEnter0 then GLFW.CursorState'InWindow else GLFW.CursorState'NotInWindow)
                       =<< E.cursorEnter window
    scroll          <- E.scroll window
    fileDrop        <- E.fileDrop window
    pure WindowEventsAndDyns {..}

filterMouseButtonE ::
      (Reflex t)
    => Event t (GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys)
    -> GLFW.MouseButton
    -> Event t (GLFW.MouseButtonState, GLFW.ModifierKeys)
filterMouseButtonE e btn =
    (\(_, s, k) -> (s, k)) <$> ffilter (\(b, _, _) -> b == btn) e

mouseButtonDyn :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m, Floating a) => WindowEventsAndDyns t a -> GLFW.MouseButton -> m (Dynamic t GLFW.MouseButtonState)
mouseButtonDyn win btn = do
    s <- liftIO $ GLFW.getMouseButton (window win) btn
    holdDyn s (fst <$> filterMouseButtonE (mouseButton win) btn)
