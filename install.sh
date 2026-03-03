DISK=/dev/nvme0n1

parted $DISK -- mklabel gpt
parted $DISK -- mkpart root 512MB 100%
parted $DISK -- mkpart ESP fat32 1MB 512MB
parted $DISK -- set 2 esp on

mkfs.ext4 -L nixos ${DISK}p1
mkfs.fat -F 32 -n boot ${DISK}p2

mount /dev/disk/by-label/nixos /mnt

mkdir -p /mnt/boot
mount -o umask=077 /dev/disk/by-label/boot /mnt/boot

nixos-generate-config --root /mnt

nix-env -iA nixos.git

cd /mnt/etc/nixos

git clone https://github.com/douglastofoli/nix-configs
cd nix-configs

cp /mnt/etc/nixos/hardware-configuration.nix /mnt/etc/nixos/nix-configs/hosts/desktop/hardware-configuration.nix

#nixos-install --flake .#desktop
