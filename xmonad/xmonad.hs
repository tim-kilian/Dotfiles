import XMonad
import XMonad.Core
import XMonad.Prelude ((<&>), (>=>))
import XMonad.Layout.Accordion
import XMonad.Layout.Cross
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ResizeScreen
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Dishes
import XMonad.Layout.OneBig
import XMonad.Layout.Circle
import XMonad.Layout.Roledex
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.BoringWindows (boringWindows, focusDown)
import XMonad.Layout.Gaps
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TrackFloating
import XMonad.Layout.CenteredMaster
import XMonad.Layout.IndependentScreens
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.MagicFocus
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)

import XMonad.Actions.SpawnOn
import XMonad.Actions.Minimize
import XMonad.Actions.FloatSnap
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule, getSortByIndex)
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.MouseGestures
import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Image

import qualified XMonad.Layout.Dwindle as Dwindle
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Hooks.InsertPosition as InsertPosition
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.Maybe as May
import qualified Data.Typeable as Type
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Util.Hacks as Hacks

import Control.Monad (replicateM_, liftM2)

import Data.List

import System.IO
import System.Exit

myKeys conf@(XConfig {modMask = mod4Mask}) = M.fromList $ [
    ((mod4Mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
    ((mod4Mask .|. controlMask, xK_Return), spawn "alacritty"),
    ((mod4Mask .|. shiftMask, xK_c), kill),
    ((mod4Mask, xK_space), sendMessage NextLayout),
    ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
    ((mod4Mask, xK_n), refresh),
    ((mod4Mask, xK_Tab), windows W.focusDown),
    ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp),

    ((mod1Mask, xK_l), spawn "slock"),

    ((mod4Mask, xK_j), windows W.swapDown),
    ((mod4Mask, xK_k), windows W.swapUp),
    ((mod4Mask, xK_s), sendMessage  Arrange),
    ((mod4Mask .|. shiftMask, xK_s), sendMessage  DeArrange),
    ((mod4Mask .|. shiftMask, xK_h), sendMessage (MoveLeft 10)),
    ((mod4Mask .|. shiftMask, xK_l), sendMessage (MoveRight 10)),
    ((mod4Mask .|. shiftMask, xK_j), sendMessage (MoveDown 10)),
    ((mod4Mask .|. shiftMask, xK_k), sendMessage (MoveUp 10)),
    ((mod4Mask .|. controlMask, xK_h), sendMessage (IncreaseLeft 10)),
    ((mod4Mask .|. controlMask, xK_l), sendMessage (IncreaseRight 10)),
    ((mod4Mask .|. controlMask, xK_j), sendMessage (IncreaseDown 10)),
    ((mod4Mask .|. controlMask, xK_k), sendMessage (IncreaseUp 10)),
    ((mod4Mask .|. controlMask .|. shiftMask, xK_h), sendMessage (DecreaseLeft 10)),
    ((mod4Mask .|. controlMask .|. shiftMask, xK_l), sendMessage (DecreaseRight 10)),
    ((mod4Mask .|. controlMask .|. shiftMask, xK_j), sendMessage (DecreaseDown 10)),
    ((mod4Mask .|. controlMask .|. shiftMask, xK_k), sendMessage (DecreaseUp 10)),

    ((mod4Mask, xK_m), withFocused minimizeWindow),
    ((mod4Mask .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus),
    ((mod4Mask, xK_Return), windows W.swapMaster),
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
    ((mod4Mask, xK_x), sendMessage $ MT.Toggle REFLECTX),
    ((mod4Mask, xK_y), sendMessage $ MT.Toggle REFLECTY),
    ((mod4Mask, xK_o), sendMessage Mag.Toggle),
    ((mod4Mask, xK_b), sendMessage ToggleStruts),
    ((mod4Mask, xK_plus), sendMessage Mag.MagnifyMore),
    ((mod4Mask, xK_minus), sendMessage Mag.MagnifyLess),
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

myWorkspaces = map xmobarEscape ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


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
  spawn "trayer --tint 0x20222a --alpha 0 --transparent true --expand true --SetDockType true --iconspacing 3 --margin 16 --widthtype pixel --distance 16 --width 250 --height 30 --padding 8 --edge top --align right"
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

myTheme = def {
    windowTitleIcons = [
      (menuButton, CenterLeft 10),
      (closeButton, CenterRight 10)
      -- (maxiButton, CenterRight 30),
      -- (miniButton, CenterRight 50)
    ],
    fontName = "xft:Roboto Nerd Font:regular:pixelsize=11",
    activeColor = "#3e445e",
    inactiveColor = "#292d3e",
    activeBorderColor = "#292d3e",
    inactiveBorderColor = "#292d3e",
    activeTextColor = "#ffffff",
    inactiveTextColor = "#d0d0d0"
  }

tall = renamed [Replace "tall"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    -- $ windowSwitcherDecorationWithImageButtons shrinkText myTheme (draggingVisualizer $ mouseResizableTile)
    -- $ tabBar shrinkText myTheme Top (resizeVertical 20 $ mouseResizableTile )
    $ mouseResizableTile
accordion = renamed [Replace "accordion"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 12
    -- $ gaps [(D,72)]
    $ accordionDefaultResizable shrinkText myTheme
circle = renamed [Replace "circle"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ Circle
dishes = renamed [Replace "dishes"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ Dishes 2 (1/5)
oneBig = renamed [Replace "oneBig"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ OneBig (3/4) (3/5)
twoPane = renamed [Replace "twoPane"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ TwoPanePersistent Nothing (3/100) (1/2)
roledex = renamed [Replace "roledex"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ Roledex
monocle = renamed [Replace "monocle"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ noBorders
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ limitWindows 20 Full
floats = renamed [Replace "floats"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 20
    $ simplestFloat
grid = renamed [Replace "grid"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16/10)
spirals = renamed [Replace "spirals"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ mySpacing 8
    $ Dwindle.Dwindle Dwindle.R Dwindle.CW 1.5 1.1
threeCol = renamed [Replace "threeCol"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 7
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ ThreeCol 1 (3/100) (1/2)
threeColMid = renamed [Replace "threeColMid"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 7
    $ mySpacing 8
    -- $ gaps [(D,72)]
    $ ThreeColMid 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ limitWindows 7
    $ mySpacing 8
    $ Mirror
    $ ThreeCol 1 (3/100) (1/2)
tabs = renamed [Replace "tabs"]
    $ noBorders
    $ minimize
    $ Mag.magnifierOff
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ gaps [(D,16), (U,16), (L,16), (R,16)]
    $ trackFloating (useTransientFor (tabbed shrinkText (myTheme { windowTitleIcons = [] })))

myBaseLayout = screenCornerLayoutHook
    $ mouseResize
    $ boringWindows
    $ refocusLastLayoutHook
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
    $ onWorkspace (myWorkspaces !! 1) codeLayouts
    $ onWorkspace (myWorkspaces !! 2) chatLayouts
    $ onWorkspace (myWorkspaces !! 5) youtubeLayouts
    $ onWorkspace (myWorkspaces !! 8) settingsLayouts
    $ allLayouts
  where
    allLayouts = tall ||| twoPane ||| threeColMid ||| oneBig ||| dishes
      -- ||| floats
      -- ||| grid
      -- ||| spirals
    codeLayouts = tabs
    chatLayouts = tall
    youtubeLayouts = oneBig ||| monocle
    settingsLayouts = circle ||| grid ||| spirals

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
       withFocused $  windows . flip W.float (W.RationalRect 0 0 1 1)
})

doLowerStack = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (lowerWindow dpy w) >> mempty

myHooks = manageSpawn <+> InsertPosition.insertPosition InsertPosition.Below InsertPosition.Newer <+> composeAll
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
    youtube = myWorkspaces!!5

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

myWorkspaceNames = [
    ("1", "¹ <fn=3>\xf269 </fn>"),
    ("2", "² <fn=3>\xf7a1 </fn>"),
    ("3", "³ <fn=3>\xf687 </fn>"),
    ("4", "⁴ <fn=3>\xe795 </fn>"),
    ("5", "⁵ <fn=3>\xf1b2 </fn>"),
    ("6", "⁶ <fn=3>\xf16a </fn>"),
    ("7", "⁷ <fn=3>\xf001 </fn>"),
    ("8", "⁸ <fn=3>\xf7b3 </fn>"),
    ("9", "⁹ <fn=3>\xf013 </fn>")
  ]

myLayoutImages = [
    ("tall", "tall"),
    ("twoPane", "two-pane"),
    ("threeColMid", "threeColMid"),
    ("oneBig", "one-big"),
    ("dishes", "dishes"),
    ("tabs", "tab"),
    ("monocle", "full"),
    ("circle", "circle"),
    ("grid", "grid"),
    ("spirals", "dwindle")
  ]

translateMap val mapList defaultVal
  | not (null ([ key | (key,name) <- mapList, key==val])) = May.fromMaybe val (lookup val mapList)
  | otherwise = defaultVal

getIcon s = "<icon="++s++".xpm/>"

translateWorkspaces val = translateMap val myWorkspaceNames val
translateLayouts val = getIcon (translateMap val myLayoutImages "unknown")

clickableWrap i = xmobarAction ("xdotool key super+" ++ show (i+1)) "1"

getWsIndex = do
    wSort <- getSortByIndex
    spaces <- gets (map W.tag . wSort . W.workspaces . windowset)
    return $ flip elemIndex spaces

getClickable = getWsIndex <&> \idx s w -> maybe id clickableWrap (idx (W.tag w)) s

clickablePP pp = getClickable <&> \ren -> pp{ ppRename = ppRename pp >=> ren }

-- myLogHook xmproc0 xmproc1 xmproc2 = do
myLogHook xmproc0 = do
  fadeInactiveLogHook 1
  clickablePP xmobarPP
    {
      ppOutput = \x -> hPutStrLn xmproc0 x, -- >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x,
      ppRename = pure . translateWorkspaces,
      ppCurrent = xmobarColor "#389dff" "" . wrap "" "",
      ppVisibleNoWindows = Just (xmobarColor "yellow" "" . wrap "" ""),
      ppHidden = xmobarColor "white" "" . wrap "" "",
      ppHiddenNoWindows = xmobarColor "#767676" "" . wrap "" "",
      ppTitle   = xmobarColor "gray"  "" . shorten 70,
      ppUrgent  = xmobarColor "red" "yellow",
      --ppLayout = const (""),
      ppLayout = xmobarColor "white" "" . translateLayouts,
      ppExtras  = [windowCount],
      ppSep = "   ",
      ppWsSep = "  ",
      ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    } >>= dynamicLogWithPP
  closeOnFocusLostLogHook "Xfce4-appfinder"

windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myEventHook e = do
  screenCornerEventHook e
  refocusLastWhen isFloat e

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
      -- ("M-l", spawn "slock"),
      ("M-r", spawn "rofi -combi-modi window,drun -theme android_notification -font \"hack 10\" -show combi"),
      ("M-p", spawn "dmenu_run -fn \"xft:Roboto:size=15\" -y 1"),
      --("<Escape>", spawn "killall plank"),
      --("<Backspace>", spawn "killall plank"),
      ("<Print>", spawn "flameshot gui -p ~/Pictures/Screenshots/"),
      ("M-<Print>", spawn "flameshot screen -p ~/Pictures/Screenshots/"),
      ("M-e", spawn "nemo"),
      ("M-S-b", spawn "onboard"),
      ("M-@", spawn "onboard"),
      ("M-S-e", spawn "gedit"),
      ("M-S-p", spawn "xlayoutdisplay -d 108"),
      ("M-S-n", spawn "nitrogen --restore"),
      ("M-S-ß", xmessage help)
    ])

convertToBool = map (map (== 1))

menuButton = convertToBool [
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1]
  ]

miniButton = convertToBool [
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1]
  ]

maxiButton = convertToBool [
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1]
  ]

closeButton = convertToBool [
    [1,1,0,0,0,0,0,0,1,1],
    [1,1,1,0,0,0,0,1,1,1],
    [0,1,1,1,0,0,1,1,1,0],
    [0,0,1,1,1,1,1,1,0,0],
    [0,0,0,1,1,1,1,0,0,0],
    [0,0,0,1,1,1,1,0,0,0],
    [0,0,1,1,1,1,1,1,0,0],
    [0,1,1,1,0,0,1,1,1,0],
    [1,1,1,0,0,0,0,1,1,1],
    [1,1,0,0,0,0,0,0,1,1]
  ]

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
