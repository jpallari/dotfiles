#!/usr/bin/env bash
set -euo pipefail

dnf upgrade -y
dnf install -y \
    zsh \
    neovim \
    git \
    curl \
    mc \
    fzf \
    @development-tools \
    clang \
    clang-devel \
    libasan \
    libubsan \
    cppcheck \
    rustup \
    golang
