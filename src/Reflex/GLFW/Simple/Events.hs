module Reflex.GLFW.Simple.Events (
      errors
    , monitor
    , joystick
    , windowPos
    , windowSize
    , windowClose
    , windowRefresh
    , windowFocus
    , windowIconify
    , framebufferSize
    , key
    , char
    , charMods
    , mouseButton
    , cursorPos
    , cursorEnter
    , scroll
    , fileDrop
) where

import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Reflex ( Reflex(Event), TriggerEvent(newTriggerEvent) )
import Graphics.UI.GLFW as GLFW

-- | Event fired when GLFW error occurs. Carries error code and human-readable description.
errors :: (Reflex t, TriggerEvent t m, MonadIO m) => m (Event t (GLFW.Error, String))
errors = mkEvent2 GLFW.setErrorCallback
{-# INLINE errors #-}

-- | Event fired when a monitor is connected or disconnected.
monitor :: (Reflex t, TriggerEvent t m, MonadIO m) => m (Event t (GLFW.Monitor, GLFW.MonitorState))
monitor = mkEvent2 GLFW.setMonitorCallback
{-# INLINE monitor #-}

-- | Event fired when a joystick is connected or disconnected.
joystick :: (Reflex t, TriggerEvent t m, MonadIO m) => m (Event t (GLFW.Joystick, GLFW.JoystickState))
joystick = mkEvent2 GLFW.setJoystickCallback
{-# INLINE joystick #-}

-- | Event fired when the window position changes.
windowPos :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (Int, Int))
windowPos = mkWinEvent2 . GLFW.setWindowPosCallback
{-# INLINE windowPos #-}

-- | Event fired when the window size changes.
windowSize :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (Int, Int))
windowSize = mkWinEvent2 . GLFW.setWindowSizeCallback 
{-# INLINE windowSize #-}

-- | Event fired when the user attempts to close the window.
windowClose :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t ())
windowClose = mkWinEvent0 . GLFW.setWindowCloseCallback
{-# INLINE windowClose #-}

-- | Event fired when the client area of the window needs to be redrawn.
windowRefresh :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t ())
windowRefresh = mkWinEvent0 . GLFW.setWindowRefreshCallback
{-# INLINE windowRefresh #-}

-- | Event fired when the window gains or loses focus.
windowFocus :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t Bool)
windowFocus = mkWinEvent1 . GLFW.setWindowFocusCallback
{-# INLINE windowFocus #-}

-- | Event fired when the window is iconified (minimized) or not.
windowIconify :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t Bool)
windowIconify = mkWinEvent1 . GLFW.setWindowIconifyCallback
{-# INLINE windowIconify #-}

-- | Event fired when the framebuffer's size changes.
framebufferSize :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (Int, Int))
framebufferSize = mkWinEvent2 . GLFW.setFramebufferSizeCallback
{-# INLINE framebufferSize #-}

-- | Event fired on key press, release or repeat.
-- Carries 'Key' value, 'Int' scancode, 'KeyState' and wether modifeir keys were pressed.
key :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (GLFW.Key, Int, GLFW.KeyState, GLFW.ModifierKeys))
key = mkWinEvent4 . GLFW.setKeyCallback
{-# INLINE key #-}

-- | Event fired when a character is typed.
char :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t Char)
char = mkWinEvent1 . GLFW.setCharCallback
{-# INLINE char #-}

-- | Event fired when a character is typed. Additionally carries modifier keys state.
charMods :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (Char, GLFW.ModifierKeys))
charMods = mkWinEvent2 . GLFW.setCharModsCallback
{-# INLINE charMods #-}

-- | Event fired when a mouse button pressed or released.
mouseButton :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t (GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys))
mouseButton = mkWinEvent3 . GLFW.setMouseButtonCallback
{-# INLINE mouseButton #-}

-- | Event fired when the cursor position changes. Sub-pixel accuracy is used, when available.
cursorPos :: (Reflex t, TriggerEvent t m, MonadIO m, Floating a) => GLFW.Window -> m (Event t (a, a))
cursorPos = fmap (fmap (bimap realToFrac realToFrac)) . mkWinEvent2 . GLFW.setCursorPosCallback
{-# INLINE cursorPos #-}

-- | Event fired when the cursor enters or leaves the window.
cursorEnter :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t GLFW.CursorState)
cursorEnter = mkWinEvent1 . GLFW.setCursorEnterCallback
{-# INLINE cursorEnter #-}

-- | Event fired when the user scrolls with the mouse wheel or a touch gesture.
scroll :: (Reflex t, TriggerEvent t m, MonadIO m, Floating a) => GLFW.Window -> m (Event t (a, a))
scroll = fmap (fmap (bimap realToFrac realToFrac)) . mkWinEvent2 . GLFW.setScrollCallback
{-# INLINE scroll #-} 

-- | Event fired when one or more dragged files are dropped on the window.
fileDrop :: (Reflex t, TriggerEvent t m, MonadIO m) => GLFW.Window -> m (Event t [FilePath])
fileDrop = mkWinEvent1 . GLFW.setDropCallback
{-# INLINE fileDrop #-}

-----------------------------------------------------------
-- PRIVATE
-----------------------------------------------------------
-- | Make event from glfw set*Callback function for 2 arguments.
mkEvent2 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (a -> b -> IO ()) -> IO ()) -> m (Event t (a, b))
mkEvent2 = mkEvent $ \fire a b -> fire (a, b)
{-# INLINE mkEvent2 #-}

-- | Make event from glfw setWin*Callback function for 0 arguments.
mkWinEvent0 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (GLFW.Window -> IO ()) -> IO ()) -> m (Event t ())
mkWinEvent0 = mkEvent $ \fire _ -> fire ()
{-# INLINE mkWinEvent0 #-}

-- | Make event from glfw setWin*Callback function for 1 argument.
mkWinEvent1 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (GLFW.Window -> a -> IO ()) -> IO ()) -> m (Event t a)
mkWinEvent1 = mkEvent $ \fire _ a -> fire a
{-# INLINE mkWinEvent1 #-}

-- | Make event from glfw setWin*Callback function for 2 arguments.
mkWinEvent2 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (GLFW.Window -> a -> b -> IO ()) -> IO ()) -> m (Event t (a, b))
mkWinEvent2 = mkEvent $ \fire _ a b -> fire (a, b)
{-# INLINE mkWinEvent2 #-}

-- | Make event from glfw setWin*Callback function for 3 arguments.
mkWinEvent3 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (GLFW.Window -> a -> b -> c -> IO ()) -> IO ()) -> m (Event t (a, b, c))
mkWinEvent3 = mkEvent $ \fire _ a b c -> fire (a, b, c)
{-# INLINE mkWinEvent3 #-}

-- | Make event from glfw setWin*Callback function for 4 arguments.
mkWinEvent4 :: (Reflex t, TriggerEvent t m, MonadIO m) => (Maybe (GLFW.Window -> a -> b -> c -> d -> IO ()) -> IO ()) -> m (Event t (a, b, c, d))
mkWinEvent4 = mkEvent $ \fire _ a b c d -> fire (a, b, c, d)
{-# INLINE mkWinEvent4 #-}

-- |
mkEvent :: (Reflex t, TriggerEvent t m, MonadIO m) => ((a -> IO ()) -> cb) -> (Maybe cb -> IO ()) -> m (Event t a)
mkEvent fireFn setCallback = do
    (event, fire) <- newTriggerEvent
    liftIO $ setCallback $ Just $ fireFn fire
    pure event
{-# INLINE mkEvent #-}
