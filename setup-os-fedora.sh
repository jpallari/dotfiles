#!/usr/bin/env bash
set -euo pipefail

dnf upgrade -y
dnf copr enable -y jdxcode/mise
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
    golang \
    mise
