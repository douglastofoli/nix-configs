Config { font = "xft:RobotoMono Nerd Font:weight=bold:pixelsize=15:antialias=true:hinting=true"
  , additionalFonts = [ "xft:Mononoki:pixelsize=14"
                      , "xft:Font Awesome 6 Free Solid:pixelsize=13"
                      , "xft:Font Awesome 6 Brands:pixelsize=15"
                      ]
  , bgColor = "#282a36"
  , fgColor = "#f8f8f2"
  , position = TopSize L 100 24
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , overrideRedirect = True
  , iconRoot = ".config/xmonad/xpm/" -- default: "."
  , commands = [
      -- Echos a separator 
      Run Com "echo" ["<fc=#666666><fn=2>\xf053</fn></fc>"] "sep" 10000
      -- Echos a "penguin" icon in font
    , Run Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
      -- Get the kernel version
    , Run Com ".local/bin/kernel" [] "kernel" 36000
    , Run Volume "default" "Master" [
              "-t", "<status><fn=2>\xf027</fn> <volume>%</fc>",
              "--",
              "--onc", "#50fa7b",
              "-O", "<fc=#50fa7b>",
              "--offc", "#ff5555",
              "-o", "<fc=#ff5555>"
            ] 10
    , Run Date "<fc=#ff92d0><fn=2>\xf073</fn> %a %m/%_d/%y</fc>" "date" 100000
    , Run Date "<fc=#bd93f9><fn=2>\xf017</fn> %H:%M</fc>" "time" 10
   -- , Run Date " <fn=1>\xf073</fn> %a %b %d <fn=1>\xf017</fn> %H:%M " "date" 50
    , Run Com ".local/bin/trayer-padding-icon" [] "trayerpad" 20
    , Run UnsafeStdinReader
    ]
  , sepChar = "%",
  , alignSep = "}{"
  , template = " <icon=/home/douglas/.config/xmonad/xpm/haskell_20.xpm/> <fc=#666666>|</fc> %UnsafeStdinReader% }{ <box type=Bottom width=2 mb=2 color=#bd93f9> <fc=#bd93f9>%penguin% %kernel%</fc> </box> %sep% <box type=Bottom width=2 mb=2 color=#50fa7b> %default:Master% </box> %sep% <box type=Bottom width=2 mb=2 color=#ff92d0> <action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(cfw:open-org-calendar))'`>%date%</action> </box> %sep% <box type=Bottom width=2 mb=2 color=#bd93f9> %time% </box> %sep% %trayerpad%" 
}
