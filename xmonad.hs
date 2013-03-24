import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Actions.GridSelect
import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import System.IO (hPutStrLn)
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS	
import Data.List		

myTerminal  	= "urxvt"
myFont	    	= "-*-tamsyn-medium-*-normal-*-11-*-*-*-*-*-*-*"

myBarIconDir 	= "/home/eugeneyche/.xmonad/dzen2/workspaces/"
myWMIconDir	= "/home/eugeneyche/.xmonad/dzen2/wm/"

myModMask 	= mod4Mask

myDmenu 	= "dmenu_run -fn 'Tamsyn-8' -h 20 -nb '#000000' -nf '#b2b2b2' -sb '#ffee55' -sf '#000000'"
myStartMenu 	= "/home/eugeneyche/.xmonad/start /home/eugeneyche/.xmonad/apps"
myXmonadBar 	= "dzen2 -x '150' -y '0' -h '20' -w '750' -ta 'r' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar 	= "conky -qc  /home/eugeneyche/.xmonad/.conky_dzen | dzen2 -x '900' -w '700' -h '20' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont

background  	= "#000000"
foreground  	= "#ffffff"
active	    	= "#ffee55"
inactive    	= "#b2b2b2"
nowindow    	= "#404040"
nborder	    	= "#343638"
aborder	    	= "#111111"


defaultLayouts = onWorkspace (myWorkspaces !! 0) (avoidStruts (Circle ||| tiled20) ||| fullTile)
			$ onWorkspace (myWorkspaces !! 1) (avoidStruts (tiled5 ||| tiled20) ||| fullScreen)
			$ onWorkspace (myWorkspaces !! 2) (avoidStruts (tiled5 ||| tiled20) ||| simplestFloat ||| fullScreen)
			$ onWorkspace (myWorkspaces !! 3) (avoidStruts (Circle ||| fullTile) ||| fullScreen)
			$ onWorkspace (myWorkspaces !! 4) (avoidStruts borderlessTile ||| fullScreen)
			$ avoidStruts (tiled5  ||| tiled20 ||| fullTile ||| Circle) ||| fullScreen
	where
		tiled5  	= spacing 5 $ ResizableTall nmaster delta ratio [] 
		tiled20  	= spacing 20 $ ResizableTall nmaster delta ratio [] 
		fullScreen 	= noBorders(fullscreenFull Full)
		fullTile 	= ResizableTall nmaster delta ratio [] 
		borderlessTile	= noBorders(fullTile)

		nmaster = 1
		delta 	= 5/100
		ratio 	= 1/2 

nobordersLayout = noBorders $ Full
myLayout = defaultLayouts

myWorkspaces  = clickable  $
		["^i(" ++ myBarIconDir ++ "me.xbm) me"
		,"^i(" ++ myBarIconDir ++ "shell.xbm) code"
		,"^i(" ++ myBarIconDir ++ "web.xbm) web"
		,"^i(" ++ myBarIconDir ++ "docs.xbm) docs"
		,"^i(" ++ myBarIconDir ++ "tunes.xbm) tunes" 
		,"^i(" ++ myBarIconDir ++ "mail.xbm) mail"]
      where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                              (i,ws) <- zip [1..] l,
                              let n = i ]			

myManageHook = composeAll 	[ resource =? "dmenu" --> doFloat
	       			, resource =? "gimp" --> doFloat
				, resource =? "google-chrome"--> doShift (myWorkspaces !! 2)
				]

newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks 

myLogHook h = dynamicLogWithPP (defaultPP
	{
		  ppCurrent		= dzenColor active background . pad
		, ppVisible		= dzenColor inactive background . pad
		, ppHidden		= dzenColor inactive background . pad
		, ppHiddenNoWindows	= dzenColor nowindow background . pad
		, ppWsSep		= ""
		, ppSep			= "  "
		, ppLayout		=  dzenColor inactive  background .
				(\x -> case x of
					"Full"				->	"^i(" ++ myWMIconDir ++ "wm_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(" ++ myWMIconDir ++ "wm_tall5.xbm)"
					"ResizableTall"			->	"^i(" ++ myWMIconDir ++ "wm_tall.xbm)"
					"SimplestFloat"			->	"^i(" ++ myWMIconDir ++ "wm_float.xbm)"
					"Circle"			->	"^i(" ++ myWMIconDir ++ "wm_circle.xbm)"
					_				->	"^i(" ++ myWMIconDir ++ "wm_default.xbm)"
				)
		, ppOrder	=  \(ws:l:t:_) -> [ws,l]
		, ppOutput	=   hPutStrLn h
	} )

main = do
	dzenStartMenu	<- spawnPipe myStartMenu
	dzenLeftBar 	<- spawnPipe myXmonadBar
	dzenRightBar	<- spawnPipe myStatusBar
	xmonad $ ewmh defaultConfig
		{ terminal		= myTerminal
		, borderWidth		= 1
		, normalBorderColor 	= nborder
		, focusedBorderColor  	= aborder
		, modMask 		= myModMask
		, layoutHook 		= myLayout
		, workspaces 		= myWorkspaces
		, manageHook		= newManageHook
		, handleEventHook 	= fullscreenEventHook <+> docksEventHook
		, startupHook 		= setWMName "LG3D"
		, logHook		= myLogHook dzenLeftBar
		}
		`additionalKeys`
		[((myModMask	, xK_p), spawn myDmenu)
		]