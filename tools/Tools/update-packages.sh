#!/usr/bin/env bash

echo "Updating dnf packages."
sudo dnf update -y
echo "Updating snap packages."
sudo snap refresh
echo "Updating flatpak packages."
flatpak update -y
