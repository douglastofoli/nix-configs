{
  services.udev = {
    enable = true;
    extraRules = ''
      SUBSYSTEM=="input", GROUP="input", MODE="0666"

      SUBSYSTEM=="usb", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="8008", MODE="0666", GROUP="plugdev"
      KERNEL=="hidraw*", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="8008", MODE="0666", GROUP="plugdev"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="8009", MODE="0666", GROUP="plugdev"
      KERNEL=="hidraw*", ATTRS{idVendor}=="04d9", ATTRS{idProduct}=="8009", MODE="0666", GROUP="plugdev"
    '';
  };
}
