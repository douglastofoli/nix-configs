#!/usr/bin/env bash

DISK=/dev/nvme0n1

cryptsetup --verify-passphrase -v luksFormat "$DISK"p2
cryptsetup open "$DISK"p2 enc

mkfs.fat -F 32 "$DISK"p1
mkfs.btrfs /dev/mapper/enc

mount -t btrfs /dev/mapper/enc /mnt

btrfs subvolume create /mnt/root
btrfs subvolume create /mnt/home
btrfs subvolume create /mnt/nix
btrfs subvolume create /mnt/swap

umount /mnt

mount -o compress=zstd,subvol=root /dev/mapper/enc /mnt
mkdir /mnt/{home,nix,swap}
mount -o compress=zstd,subvol=home /dev/mapper/enc /mnt/home
mount -o compress=zstd,noatime,subvol=nix /dev/mapper/enc /mnt/nix
mount -o subvol=swap /dev/mapper/enc /swap

mkdir /mnt/boot
mount "$DISK"p1 /mnt/boot

# Create swapfile
truncate -s 0 /mnt/swap/swapfile
chattr +C /mnt/swap/swapfile
btrfs property set /mnt/swap/swapfile compression none
dd if=/dev/zero of=/mnt/swap/swapfile bs=1M count=8192
chmod 0600 /mnt/swap/swapfile
mkswap /mnt/swap/swapfile

swapon /mnt/swap/swapfile

nixos-generate-config --root /mnt
