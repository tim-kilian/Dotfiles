Config
  { font = "xft:Roboto:size=11",
    additionalFonts =
      [ "xft:Font Awesome 5 Free Solid:size=10",
        "xft:Font Awesome 5 Brands:size=10",
        "xft:Hack Nerdfont:size=11"
      ],
    alpha = 255,
    border = BottomB,
    borderWidth = 0,
    borderColor = "#34363d",
    bgColor = "#20222A",
    fgColor = "#E0E0E0",
    position = Static {xpos = 512, ypos = 18, width = 1280, height = 30},
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    overrideRedirect = True,
    pickBroadest = False,
    persistent = True,
    iconRoot = ".xmonad/xpm/",
    commands =
      [ Run
          Cpu
          [ "--template",
            "<fc=#a9a1e1><fn=1>\xf2db</fn></fc>  <total>%",
            "-L",
            "3",
            "-H",
            "70",
            "--low",
            "gray",
            "--normal",
            "gray",
            "--high",
            "red"
          ]
          10,
        Run
          Memory
          [ "--template",
            "<fc=#51afef><fn=1>\xf538</fn></fc>  <usedratio>%",
            "-L",
            "10",
            "-H",
            "90",
            "--low",
            "gray",
            "--normal",
            "gray",
            "--high",
            "red"
          ]
          10,
        Run
          Battery
          [ "--template",
            "<acstatus>",
            "--Low",
            "15",
            "--High",
            "50",
            "--low",
            "darkred",
            "--normal",
            "darkorange",
            "--",
            "-o",
            "<leftipat> <left>%",
            "-O",
            "<leftipat> <left>%",
            "-i",
            "<leftipat> <left>%",
            "-a",
            "notify-send -u critical 'Battery running out!!'",
            "-A",
            "15",
            "--off-icon-pattern",
            "<fc=#B1DE76><fn=1>\xf240</fn></fc>",
            "--on-icon-pattern",
            "<fc=#B1DE76><fn=1>\xf0e7</fn></fc>",
            "--idle-icon-pattern",
            "<fc=#B1DE76><fn=1>\xf0e7</fn></fc>"
          ]
          50,
        Run Date "<action=`gnome-calendar` button=1><fn=3>%H:%M  </fn><fn=1>\xf073</fn><fn=3>  %d.%m.%Y</fn></action>" "date" 10,
        Run UnsafeStdinReader,
        Run
          Volume
          "default"
          "Master"
          [ "-t",
            "<status> <volume>%",
            "--",
            "-o",
            "<fn=1>\xf026</fn>",
            "-O",
            "<fn=1>\xf028</fn>",
            "-c",
            "#E0E0E0",
            "-C",
            "#E0E0E0"
          ]
          10
      ],
    sepChar = "%",
    alignSep = "}{",
    template = "%date%"
  }
