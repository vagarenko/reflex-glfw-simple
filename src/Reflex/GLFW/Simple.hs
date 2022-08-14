{-# LANGUAGE RecordWildCards  #-}

module Reflex.GLFW.Simple (
      E.errors
    , E.monitor
    , E.joystick
    , WindowReflexes(..)
    , windowReflexes
    , filterMouseButtonE
    , mouseButtonDyn
) where

import Data.Bifunctor
import Control.Monad.IO.Class ( MonadIO(..) )
import Reflex ( Reflex(Event, Dynamic), MonadHold(), TriggerEvent(), holdDyn, ffilter )
import qualified Graphics.UI.GLFW as GLFW
import qualified Reflex.GLFW.Simple.Events as E

-- | Events and Dynamics of a window.
data WindowReflexes t = WindowReflexes
    { -- | Window handle.
      window          :: GLFW.Window
      -- | Window position.
    , windowPos       :: Dynamic t (Int, Int)
      -- | Window size.
    , windowSize      :: Dynamic t (Int, Int)
      -- | Event fired when the user attempts to close the window.
    , windowClose     :: Event t ()
      -- | Event fired when the client area of the window needs to be redrawn.
    , windowRefresh   :: Event t ()
      -- | Does window has focus?
    , windowFocus     :: Dynamic t Bool
      -- | Is window iconified?
    , windowIconify   :: Dynamic t Bool
      -- | Framebuffer size.
    , framebufferSize :: Dynamic t (Int, Int)
      -- | Event fired on key press, release or repeat.
      -- Carries 'Key' value, 'Int' scancode, 'KeyState' and wether 
      -- modifeir keys were pressed.
    , key             :: Event t (GLFW.Key, Int, GLFW.KeyState, GLFW.ModifierKeys)
      -- | Event fired when a character is typed.
    , char            :: Event t Char
      -- | Event fired when a character is typed. Additionally carries modifier keys state.
    , charMods        :: Event t (Char, GLFW.ModifierKeys)
      -- | Event fired when a mouse button pressed or released.
    , mouseButton     :: Event t (GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys)
      -- | Cursor position.
    , cursorPos       :: Dynamic t (Float, Float)
      -- | Is the cursor inside the window?
    , cursorEnter     :: Dynamic t GLFW.CursorState
      -- | Event fired when the user scrolls with the mouse wheel or a touch gesture.
    , scroll          :: Event t (Float, Float)
      -- | Event fired when one or more dragged files are dropped on the window.
    , fileDrop        :: Event t [FilePath]
    }

-- | Create Events and Dynamics for given window.
windowReflexes ::
       (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m)
    => GLFW.Window
    -> m (WindowReflexes t)
windowReflexes window = do
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
    pure WindowReflexes {..}

filterMouseButtonE ::
       (Reflex t)
    => Event t (GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys)
    -> GLFW.MouseButton
    -> Event t (GLFW.MouseButtonState, GLFW.ModifierKeys)
filterMouseButtonE e btn =
    (\(_, s, k) -> (s, k)) <$> ffilter (\(b, _, _) -> b == btn) e

mouseButtonDyn ::
       (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m)
    => WindowReflexes t
    -> GLFW.MouseButton
    -> m (Dynamic t GLFW.MouseButtonState)
mouseButtonDyn win btn = do
    s <- liftIO $ GLFW.getMouseButton (window win) btn
    holdDyn s (fst <$> filterMouseButtonE (mouseButton win) btn)
