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

umount /mnt

mount -o compress=zstd,subvol=root /dev/mapper/enc /mnt
mkdir /mnt/{home,nix,swap}
mount -o compress=zstd,subvol=home /dev/mapper/enc /mnt/home
mount -o compress=zstd,noatime,subvol=nix /dev/mapper/enc /mnt/nix

mkdir /mnt/boot
mount "$DISK"p1 /mnt/boot

nixos-generate-config --root /mnt
