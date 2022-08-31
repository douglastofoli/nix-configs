#!/usr/bin/env bash

DISK=/dev/nvme0n1

mkfs.vfat -n BOOT "$DISK"p1

cryptsetup --verify-passphrase -v luksFormat "$DISK"p2
cryptsetup open "$DISK"p2 enc

# Create swap inside encrypted partition
pvcreate /dev/mapper/enc
vgcreate lvm /dev/mapper/enc

lvcreate --size 8G --name swap lvm
lvcreate --extents 100%FREE --name root lvm

mkswap /dev/lvm/swap
mkfs.btrfs /dev/lvm/root

swapon /dev/lvm/swap

# Create subvolumes
btrfs subvolume create /mnt/root
btrfs subvolume create /mnt/home
btrfs subvolume create /mnt/nix
btrfs subvolume create /mnt/persist
btrfs subvolume create /mnt/log