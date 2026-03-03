#!/usr/bin/env bash
set -euo pipefail
set -x

DISK=/dev/nvme0n1
MNT=/mnt
NIXOS_DIR=$MNT/etc/nixos
REPO_DIR=$NIXOS_DIR/nix-configs
HOST_DIR=$REPO_DIR/hosts/desktop

parted "$DISK" -- mklabel gpt
parted "$DISK" -- mkpart root 512MB 100%
parted "$DISK" -- mkpart ESP fat32 1MB 512MB
parted "$DISK" -- set 2 esp on

mkfs.ext4 -L nixos "${DISK}p1"
mkfs.fat -F 32 -n boot "${DISK}p2"

mount /dev/disk/by-label/nixos "$MNT"

mkdir -p "$MNT/boot"
mount -o umask=077 /dev/disk/by-label/boot "$MNT/boot"

nixos-generate-config --root "$MNT"

# garantir que o diretório existe
mkdir -p "$NIXOS_DIR"

# clonar repo
if [ ! -d "$REPO_DIR" ]; then
  git clone https://github.com/douglastofoli/nix-configs "$REPO_DIR"
fi

# garantir diretório do host
mkdir -p "$HOST_DIR"

# copiar hardware config
cp "$NIXOS_DIR/hardware-configuration.nix" \
   "$HOST_DIR/hardware-configuration.nix"

echo "✅ hardware-configuration.nix copiado com sucesso"

# nixos-install --flake "$REPO_DIR#desktop"
