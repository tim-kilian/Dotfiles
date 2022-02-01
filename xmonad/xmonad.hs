import XMonad
import XMonad.Core
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.BoringWindows (boringWindows, focusDown)
import XMonad.Layout.Gaps
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.CenteredMaster
import XMonad.Layout.IndependentScreens
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import XMonad.Actions.SpawnOn
import XMonad.Actions.Minimize
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.MouseGestures
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.Maybe as May
import qualified Data.Typeable as Type
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Util.Hacks as Hacks

import Control.Monad (replicateM_, liftM2)

import System.IO
import System.Exit

myKeys conf@(XConfig {modMask = mod4Mask}) = M.fromList $ [
    ((mod4Mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
    ((mod4Mask .|. shiftMask, xK_c), kill),
    ((mod4Mask, xK_space), sendMessage NextLayout),
    ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
    ((mod4Mask, xK_n), refresh),
    ((mod4Mask, xK_Tab), nextLayout),
    ((mod4Mask .|. shiftMask, xK_Tab), prevLayout),
    ((mod1Mask, xK_Tab), windows W.focusDown),
    ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp),
    ((mod4Mask, xK_j), windows W.focusDown),
    ((mod4Mask, xK_k), windows W.focusUp),
    ((mod4Mask, xK_m), withFocused minimizeWindow),
    ((mod4Mask .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus),
    ((mod4Mask, xK_Return), windows W.swapMaster),
    ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown),
    ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp),
    ((mod4Mask, xK_u), sendMessage Shrink),
    ((mod4Mask .|. shiftMask, xK_u), sendMessage ShrinkSlave),
    ((mod4Mask, xK_i), sendMessage Expand),
    ((mod4Mask .|. shiftMask, xK_i), sendMessage ExpandSlave),
    ((mod4Mask, xK_t), withFocused toggleFloat),
    ((mod4Mask, xK_f), toggleFull),
    ((mod4Mask, xK_comma), sendMessage (IncMasterN 1)),
    ((mod4Mask, xK_period), sendMessage (IncMasterN (-1))),
    ((mod4Mask, xK_Right), nextWS),
    ((mod4Mask .|. shiftMask, xK_Right), shiftToNext),
    ((mod4Mask, xK_Left), prevWS),
    ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev),
    ((mod4Mask .|. controlMask, xK_x), sendMessage $ MT.Toggle REFLECTX),
    ((mod4Mask .|. controlMask, xK_y), sendMessage $ MT.Toggle REFLECTY),
    ((0, xF86XK_AudioMute), spawn "amixer set Master 'toggle'"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5"),
    ((mod4Mask .|. shiftMask, xK_q), io exitSuccess),
    ((mod4Mask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ] ++ [
    ((m .|. mod4Mask, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  where
    nextLayout = sendMessage NextLayout
    prevLayout = sendMessage NextLayout

xmobarEscape = concatMap doubleLts
  where
    doubleLts x = [x]

myWorkspaces = clickable . map xmobarEscape $ ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
  where
    clickable l =
      [
        "<action=xdotool key super+" ++ show n ++ "><fn=3>" ++ ws ++ "</fn></action>" | (i,ws) <- zip [1..10] l,
        let n = i
      ]

myStartupHook = do
  --spawn "killall conky"
  spawn "killall lxsession"
  spawn "killall deadd-notification-center"
  spawn "killall trayer"
  spawn "killall cbatticon"
  spawn "killall clipit"
  spawn "killall volctl"
  spawn "lxsession"
  spawn "clipit -d"
  spawn "xlayoutdisplay -d 108"
  spawn "picom"
  spawn "deadd-notification-center"
  setWMName "LG3D"
  spawn "nitrogen --restore"
  --spawn "xmodmap -layout de -variant dvp -option caps:swapescape -option lv3:ralt_switch "
  spawn "xmodmap ~/.xmodmap-`uname -n`"
  -- spawn "tint2"
  spawn "volctl"
  spawn "cbatticon"
  spawn "trayer --tint 0x20222a --alpha 0 --transparent true --expand true --SetDockType true --iconspacing 3 --margin 16 --widthtype pixel --distance 16 --width 240 --height 30 --padding 8 --edge top --align right"
  spawn "onboard"
  spawn "xinput set-prop \"SynPS/2 Synaptics TouchPad\" \"libinput Tapping Enabled\" 1"
  spawn "xinput set-prop \"SynPS/2 Synaptics TouchPad\" \"libinput Natural Scrolling Enabled\" 1"
  spawn "gsettings set org.cinnamon.desktop.default-applications.terminal exec tilix"
  spawn "plank"
  spawn "dunst"
  --spawn "sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --margin 410 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint #424242 --height 24"
  --spawn "sleep 2 && conky -c ~/.conkyrc"
  --spawn "alttab -w 1 --theme -fg '#d58681' -bg '#4a4a4a' -frame '#eb564d' -t 128x150 -i 127x64"
  addScreenCorner SCLowerLeft (spawn "killall plank && plank")

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


tall = renamed [Replace "tall"]
    $ minimize
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ mouseResizableTile
magnify = renamed [Replace "magnify"]
    $ minimize
    $ magnifier
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3/100) (1/2) []
monocle = renamed [Replace "monocle"]
    $ minimize
    $ noBorders
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ limitWindows 20 Full
floats = renamed [Replace "floats"]
    $ minimize
    $ limitWindows 20 simplestFloat
grid = renamed [Replace "grid"]
    $ minimize
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16/10)
spirals = renamed [Replace "spirals"]
    $ minimize
    $ mySpacing 8
    $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
    $ minimize
    $ limitWindows 7
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ ThreeCol 1 (3/100) (1/2)
threeColMid = renamed [Replace "threeColMid"]
    $ minimize
    $ limitWindows 7
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ ThreeColMid 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
    $ minimize
    $ limitWindows 7
    $ mySpacing 8
    $ Mirror
    $ ThreeCol 1 (3/100) (1/2)
tabs = renamed [Replace "tabs"]
    $ noBorders
    $ minimize
    $ tabbed shrinkText def {
        fontName = "xft:Mononoki Nerd Font:regular:pixelsize=11",
        activeColor = "#292d3e",
        inactiveColor = "#3e445e",
        activeBorderColor = "#292d3e",
        inactiveBorderColor = "#292d3e",
        activeTextColor = "#ffffff",
        inactiveTextColor = "#d0d0d0"
      }

myBaseLayout = screenCornerLayoutHook
    $ mouseResize
    $ boringWindows
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ onWorkspace (myWorkspaces !! 1) codeLayouts
    $ onWorkspace (myWorkspaces !! 2) chatLayouts
    $ allLayouts
  where
    allLayouts = tall ||| threeColMid ||| magnify ||| monocle
      -- ||| floats
      -- ||| grid
      -- ||| spirals
    codeLayouts = tabs
    chatLayouts = tall

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens _ wset _ _ wrs =
    case W.peek wset of
      Nothing -> fmap fst wrs
      Just focused -> filter (/= focused) $ fmap fst wrs

toggleFloat w = windows
  ( \s ->
      if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s
  )

toggleFull = withFocused (\windowId -> do
{
   floats <- gets (W.floating . windowset);
   if windowId `M.member` floats
   then do
       withFocused $ windows . W.sink
   else do
       withFocused $  windows . (flip W.float $ W.RationalRect 0 0 1 1)
})

doLowerStack = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (lowerWindow dpy w) >> mempty

myHooks = manageSpawn <+> composeAll
  [
    isDialog --> doFloat <+> placeHook (fixed (0.5, 0.5)),
    isFullscreen --> doFullFloat,
    resource =? "desktop_window" --> doIgnore,
    resource =? "plank" --> hasBorder False,
    className =? "trayer" --> doLowerStack,
    resource =? "dunst" --> hasBorder False,
    resource =? "oblogout" --> doFullFloat,
    resource =? "gxmessage" --> doCenterFloat,
    resource =? "onboard" --> doFloat,
    resource =? "xmessage" --> doCenterFloat,
    className =? "Tor Browser" --> doFloat,
    className =? "code-oss" --> viewShift (code),
    className =? "jetbrains-idea" --> viewShift (code),
    className =? "Microsoft Teams - Preview" --> viewShift (chat),
    resource =? "pavucontrol" --> doFloat <+> placeHook (fixed (0.5, 0.5)),
    title =? "win0" --> doFloat,
    className =? "Xfce4-appfinder" --> doRectFloat (W.RationalRect 0 (1/50) (1/2) (1/2)),
    title =? "Microsoft Teams Notification" --> doSideFloat NE
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    web = myWorkspaces!!0
    code = myWorkspaces!!1
    chat = myWorkspaces!!2

when p s  = if p then s else pure ()
data Focus = NoFocus | Focus Window deriving (Eq, Read, Show, Typeable)

instance ExtensionClass Focus where
  initialValue = NoFocus

closeOnFocusLostLogHook clsName = do
  xState <- get
  let ws = windowset xState
      ext = extensibleState xState
      focus = maybe NoFocus Focus $ W.peek ws
      (prevVal, ext') = M.insertLookupWithKey update "prevFocus" (Right $ StateExtension focus) ext
      prevFocus = maybe NoFocus (either (const NoFocus) tryCast) prevVal
  put xState{ extensibleState = ext' }
  case prevFocus of
    Focus prevWin -> do
      c <- runQuery className prevWin
      when (c == clsName) $ do
        case focus of
          Focus currentWin -> when (currentWin /= prevWin) (killWindow prevWin)
          NoFocus -> killWindow prevWin
    _ -> return ()
  where update :: k -> v -> v -> v
        update _ newValue _ = newValue
        tryCast :: StateExtension -> Focus
        tryCast (StateExtension val) = May.fromMaybe initialValue $ Type.cast val
        tryCast _ = NoFocus

-- myLogHook xmproc0 xmproc1 xmproc2 = do
myLogHook xmproc0 = do
  fadeInactiveLogHook 1
  dynamicLogWithPP $ xmobarPP
    {
      ppOutput = \x -> hPutStrLn xmproc0 x, -- >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x,
      ppCurrent = xmobarColor "yellow" "" . wrap "" "",
      ppTitle   = xmobarColor "gray"  "" . shorten 70,
      ppUrgent  = xmobarColor "red" "yellow",
      --ppLayout = const (""),
      ppExtras  = [windowCount],
      ppSep = "   ",
      ppWsSep = "  ",
      ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
  closeOnFocusLostLogHook "Xfce4-appfinder"
  --gets (W.peek . windowset) >>= liftIO . updateEnv
  --  where
  --    updateEnv = spawn "gxmessage test"

windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myEventHook e = do
  screenCornerEventHook e

mySort = getSortByXineramaRule

main = do
  n <- countScreens
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar.hs"
  -- xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmonad/xmobar.hs"
  -- xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.xmonad/xmobar.hs"
  -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/apps.hs"
  -- xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/workspaces.hs"
  -- xmproc2 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/status.hs"
  xmonad $ docks . setEwmhWorkspaceSort mySort . ewmhFullscreen . ewmh $ Hacks.javaHack (def {
    modMask = mod4Mask,
    terminal = "tilix",
    borderWidth = 0,
    focusedBorderColor = "#e94e1b",

    workspaces = myWorkspaces,
    keys = myKeys,

    startupHook = myStartupHook,
    layoutHook =  smartBorders (lessBorders FocusedOnly (avoidStruts myBaseLayout)),
    manageHook = manageDocks <+> myHooks,
    handleEventHook = myEventHook,
    logHook = myLogHook xmproc0 -- xmproc1 xmproc2
  } `additionalKeysP` [
      ("M-l", spawn "slock"),
      ("M-r", spawn "rofi -combi-modi window,drun -theme android_notification -font \"hack 10\" -show combi"),
      ("M-p", spawn "dmenu_run -fn \"xft:Roboto:size=15\" -y 1"),
      --("<Escape>", spawn "killall plank"),
      --("<Backspace>", spawn "killall plank"),
      ("<Print>", spawn "flameshot gui -p ~/Pictures/Screenshots/"),
      ("M-<Print>", spawn "flameshot screen -p ~/Pictures/Screenshots/"),
      ("M-e", spawn "nemo"),
      ("M-@", spawn "onboard"),
      ("M-S-e", spawn "gedit"),
      ("M-S-p", spawn "xlayoutdisplay -d 108"),
      ("M-S-n", spawn "nitrogen --restore"),
      ("M-S-ß", xmessage help)
    ])

help = unlines ["The modifier key is 'Mod'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "mod-Shift-/      Show this help message with the default keybindings",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workSpace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
