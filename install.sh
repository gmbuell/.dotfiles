#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Install rcm if missing ---
if ! command -v rcup &>/dev/null; then
  echo "rcm not found, installing..."
  if command -v apt-get &>/dev/null; then
    sudo apt-get install -y rcm
  elif command -v brew &>/dev/null; then
    brew install rcm
  else
    echo "ERROR: Cannot install rcm. Install it manually: https://github.com/thoughtbot/rcm"
    exit 1
  fi
fi

# --- Symlink dotfiles ---
RCRC="$SCRIPT_DIR/rcrc" rcup -v

# --- Optionally build Emacs with tree-sitter ---
echo ""
read -rp "Build Emacs with tree-sitter support? [y/N] " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
  bash "$SCRIPT_DIR/emacs.d/upgrade-treesitter-emacs.sh"
fi
