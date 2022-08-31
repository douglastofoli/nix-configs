#!/usr/bin/env bash

DISK=/dev/nvme0n1

echo
echo "Crypt with cryptsetup"
echo
cryptsetup --verify-passphrase -v luksFormat "$DISK"p2
cryptsetup open "$DISK"p2 enc

echo "Making file sistem..."
echo
mkfs.fat -F 32 -n BOOT "$DISK"p1
mkfs.btrfs /dev/mapper/enc

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
read -p "Want to create subvolume SWAP? [Y/n]" subvol_swap
if [[ $subvol_swap == "Y" || $subvol_swap == "y" ]]; then
  btrfs subvolume create /mnt/swap
fi

umount /mnt

echo
echo "Mounting directories..."
echo
if [[ $subvol_root == "Y" || $subvol_root == "y" ]]; then
  mount -o subvol=root,compress=zstd /dev/mapper/enc /mnt
fi
if [[ $subvol_home == "Y" || $subvol_home == "y" ]]; then
  mkdir /mnt/home
  mount -o subvol=home,compress=zstd /dev/mapper/enc /mnt/home
fi
if [[ $subvol_nix == "Y" || $subvol_nix == "y" ]]; then
  mkdir /mnt/nix
  mount -o subvol=nix,compress=zstd,noatime /dev/mapper/enc /mnt/nix
fi
if [[ $subvol_swap == "Y" || $subvol_swap == "y" ]]; then
  mkdir /mnt/swap
  mount -o subvol=swap /dev/mapper/enc /mnt/swap
fi

echo
echo "Mounting boot partition..."
echo
mkdir /mnt/boot
mount "$DISK"p1 /mnt/boot

echo
echo "Creating swap file..."
echo
truncate -s 0 /mnt/swap/swapfile
chattr +C /mnt/swap/swapfile
btrfs property set /mnt/swap/swapfile compression none
dd if=/dev/zero of=/mnt/swap/swapfile bs=1M count=8192
chmod 0600 /mnt/swap/swapfile
mkswap /mnt/swap/swapfile

echo
echo "Mounting swap..."
echo
swapon /mnt/swap/swapfile

echo
echo "Generation NixOs config"
echo
nixos-generate-config --root /mnt
