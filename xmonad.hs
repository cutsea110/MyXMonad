--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Data.Monoid (mempty)
import qualified Data.Map as M
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hPutStrLn)

import XMonad
import XMonad.Hooks.DynamicLog (PP(..), wrap, shorten, dynamicLogWithPP, xmobarColor)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByTag)
import qualified XMonad.StackSet as W

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((smodm,  xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,   xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((smodm,  xK_p     ), spawn "gmrun")

    -- close focused window
    , ((smodm,  xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,   xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((smodm,  xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,   xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,   xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,   xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,   xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,   xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,   xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((smodm,  xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((smodm,  xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,   xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,   xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,   xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm,   xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm,   xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((smodm,  xK_q     ), io $ exitWith ExitSuccess)

    -- Restart xmonad
    , ((modm,   xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  where
    smodm = modm .|. shiftMask


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
mouseBindings' (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
layoutHook' = avoidStruts $ layoutHook defaultConfig

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
manageHook' = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
eventHook' = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP PP {
  ppCurrent         = xmobarColor "#f33" "",
  ppVisible         = xmobarColor "#cc0" "",
  ppHidden          = id,
  ppHiddenNoWindows = xmobarColor "#666" "",
  ppUrgent          = xmobarColor "#f33" "#ff0",
  ppSep             = wrap " " " " $ xmobarColor "" "#fff" " ",
  ppWsSep           = " ",
  ppTitle           = xmobarColor "#0f0" "" . shorten 60,
  ppLayout          = id,
  ppOrder           = id,
  ppSort            = getSortByTag,
  ppExtras          = [],
  ppOutput          = hPutStrLn h }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
startupHook' = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmobarHandle <- spawnPipe "xmobar"
  xmonad XConfig {
      -- simple stuff
        terminal           = "urxvt -fg grey -bg black",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["1","2","3","4","5","6","7","8","9"],
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",

      -- key bindings
        keys               = keys',
        mouseBindings      = mouseBindings',

      -- hooks, layouts
        layoutHook         = layoutHook',
        manageHook         = manageHook',
        handleEventHook    = eventHook',
        logHook            = logHook' xmobarHandle,
        startupHook        = startupHook'
    }
