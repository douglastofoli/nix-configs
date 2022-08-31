#!/usr/bin/env bash

DISK=/dev/nvme0n1

mkfs.fat -F 32 -n BOOT "$DISK"p1

echo
echo "Crypt with cryptsetup"
echo
cryptsetup --verify-passphrase -v luksFormat "$DISK"p2
cryptsetup open "$DISK"p2 enc

echo
echo "Creating swap inside encrypted partition"
echo
pvcreate /dev/mapper/enc
vgcreate lvm /dev/mapper/enc

read -p "What is the swap size? Ex: 8G " swap_size
lvcreate --size $swap_size --name swap lvm
lvcreate --extents 100%FREE --name root lvm

echo "Making file sistem..."
echo
mkswap /dev/lvm/swap
mkfs.btrfs /dev/lvm/root

echo
echo "Mounting swap..."
echo
swapon /dev/lvm/swap

echo
echo "Creating subvolumes..."
echo

read -p "Want to create subvolume ROOT? [Y/n]" subvol_root
if [[ $subvol_root == "Y" || $subvol_root == "y" ]]; then
  btrfs subvolume create /mnt/root
fi
read -p "Want to create subvolume HOME? [Y/n]" subvol_home
if [[ $subvol_home == "Y" || $subvol_home == "y" ]]; then
  btrfs subvolume create /mnt/home
fi
read -p "Want to create subvolume NIX? [Y/n]" subvol_nix
if [[ $subvol_nix == "Y" || $subvol_nix == "y" ]]; then
  btrfs subvolume create /mnt/nix
fi

umount /mnt

echo
echo "Mounting directories..."
echo

if [[ $subvol_root == "Y" || $subvol_root == "y" ]]; then
  mount -o subvol=root,compress=zstd /dev/lvm/root /mnt
fi
if [[ $subvol_home == "Y" || $subvol_home == "y" ]]; then
  mkdir /mnt/home
  mount -o subvol=home,compress=zstd /dev/lvm/root /mnt/home
fi
if [[ $subvol_nix == "Y" || $subvol_nix == "y" ]]; then
  mkdir /mnt/nix
  mount -o subvol=nix,compress=zstd,noatime /dev/lvm/root /mnt/nix
fi

mkdir /mnt/boot
mount "$DISK"p1 /mnt/boot

echo
echo "Generation NixOs config"

nixos-generate-config --root /mnt

nix-env -iA nixos.git

cd /mnt/etc/nixos
git clone https://github.com/douglastofoli/nix-configs
