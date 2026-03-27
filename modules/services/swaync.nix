{
  flake.modules.homeManager.swaync = {
    services = {
      swaync = {
        enable = true;

        settings = {
          positionX = "right";
          positionY = "top";
          layer = "overlay";
        };

        style = ''
          * {
            font-family: "JetBrainsMono Nerd Font", sans-serif;
            font-size: 13px;
            border-radius: 10px;
          }

          .control-center {
            background: #282a36;
            color: #f8f8f2;
            border: 1px solid #44475a;
          }

          .notification {
            background: #282a36;
            color: #f8f8f2;
            border: 1px solid #44475a;
            padding: 12px;
            margin: 8px;
          }

          .notification.critical {
            border: 1px solid #ff5555;
          }

          .notification.low,
          .notification.normal {
            border: 1px solid #8be9fd;
          }

          .notification-content,
          .notification-default-action,
          .notification-action,
          .summary,
          .body,
          .image {
            background: transparent;
            border: none;
            box-shadow: none;
          }

          .notification-title {
            color: #ff79c6;
            font-weight: bold;
          }

          .notification-body {
            color: #f8f8f2;
          }

          .notification-time {
            color: #6272a4;
          }

          button {
            background: #44475a;
            color: #f8f8f2;
            border: none;
          }

          button:hover {
            background: #bd93f9;
            color: #282a36;
          }
        '';
      };
    };
  };
}
