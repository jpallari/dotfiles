#!/usr/bin/env bash
set -euo pipefail

dnf upgrade -y
dnf install -y zsh neovim git curl
