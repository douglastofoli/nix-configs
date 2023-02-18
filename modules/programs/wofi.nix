{ config, lib, ... }:

let
  wofiConf = ''
    width=800
    height=400
    location=center
    show=drun
    prompt=Search...
    filter_rate=100
    allow_markup=true
    no_actions=true
    halign=fill
    orientation=vertical
    content_halign=fill
    insensitive=true
    allow_images=true
    image_size=28
    gtk_dark=true
  '';

  wofiWifiConf = ''
    # Config for wofi-wifi-menu

    # position values:
    # 1 2 3
    # 8 0 4
    # 7 6 5
    POSITION=3

    #y-offset
    YOFF=10

    #x-offset
    XOFF=-65

    #fields to be displayed
    FIELDS=SSID,IN-USE,BARS,SECURITY
  '';

  wofiStyle = ''
    @define-color clear rgba(0, 0, 0, 0.0);
    @define-color primary rgba(0, 0, 0, 0.75);

    window {
      margin: 2px;
      border: 2px solid;
      border-color: #313244;
      background-color: #1e1e2e;
      border-radius: 10px;
      font-family: monospace;
      font-size: 14px;
    }
    #input {
      padding: 2px;
      margin: 4px;
      margin-bottom: 8px;
      border: none;
      color: @foreground;
      background-color: #313244;
      outline: none;
    }
    #inner-box {
      margin: 2px;
      border: 0px solid;
      background-color: @clear;
      border-radius: 8px;
    }
    #outer-box {
      margin: 5px;
      border: none;
      border-radius: 8px;
      background-color: @clear;
    }
    #scroll {
      margin: 0px;
      border: none;
    }
    #text:selected {
      color: @foreground;
      margin: 0px 0px;
      border: none;
      border-radius: 8px;
    }
    #entry {
      margin: 0px 0px;
      border: none;
      border-radius: 0px;
      background-color: transparent;
    }
    #entry:selected {
      margin: 0px 0px;
      border: none;
      border-radius: 8px;
      background-color: #585b70;
    }
  '';
in {
  xdg.configFile."wofi/config".text = wofiConf;
  xdg.configFile."wofi/wifi".text = wofiWifiConf;
  xdg.configFile."wofi/style.css".text = wofiStyle;
}
