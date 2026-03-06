#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Dependency check ---
REQUIRED_PKGS=(
  build-essential autoconf automake texinfo pkg-config git
  libxml2-dev libjansson-dev libgnutls28-dev libncurses-dev libmailutils-dev
)

# Detect available libgccjit version
GCCJIT_PKG=""
for v in 14 13 12 11; do
  if apt-cache show "libgccjit-${v}-dev" &>/dev/null; then
    GCCJIT_PKG="libgccjit-${v}-dev"
    break
  fi
done
if [ -z "$GCCJIT_PKG" ]; then
  echo "ERROR: No libgccjit-*-dev package found in apt cache."
  exit 1
fi
REQUIRED_PKGS+=("$GCCJIT_PKG")

MISSING=()
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! dpkg -s "$pkg" &>/dev/null; then
    MISSING+=("$pkg")
  fi
done

if [ ${#MISSING[@]} -gt 0 ]; then
  echo "Missing build dependencies:"
  echo "  sudo apt install ${MISSING[*]}"
  exit 1
fi

# --- Clone repos if missing ---
mkdir -p ~/github

if [ ! -d ~/github/tree-sitter ]; then
  echo "Cloning tree-sitter..."
  git clone --depth 1 https://github.com/tree-sitter/tree-sitter.git ~/github/tree-sitter
fi

if [ ! -d ~/github/emacs ]; then
  echo "Cloning emacs..."
  git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git ~/github/emacs
fi

# --- Clean stale compiled artifacts ---
if [ -d ~/.emacs.d/tree-sitter ]; then
  echo "Removing stale tree-sitter grammars..."
  rm -f ~/.emacs.d/tree-sitter/*.so
fi
if [ -d ~/.emacs.d/eln-cache ]; then
  echo "Removing stale eln-cache..."
  rm -rf ~/.emacs.d/eln-cache
fi

echo "=== Step 1: Install tree-sitter v0.25.10 (ABI 15, compatible with Emacs 30.x) ==="
cd ~/github/tree-sitter
git fetch --depth 1 origin tag v0.25.10
git checkout v0.25.10
make clean 2>/dev/null || true
make -j$(nproc)

echo ""
echo "Installing tree-sitter to /usr/local (requires sudo)..."
sudo mkdir -p /usr/local/lib/pkgconfig /usr/local/include/tree_sitter
sudo cp libtree-sitter.a /usr/local/lib/
sudo cp libtree-sitter.so /usr/local/lib/libtree-sitter.so.0.25
sudo ln -sf libtree-sitter.so.0.25 /usr/local/lib/libtree-sitter.so.0
sudo ln -sf libtree-sitter.so.0.25 /usr/local/lib/libtree-sitter.so
sudo cp lib/include/tree_sitter/api.h /usr/local/include/tree_sitter/api.h
sudo cp tree-sitter.pc /usr/local/lib/pkgconfig/
sudo ldconfig

echo "tree-sitter $(grep TREE_SITTER_LANGUAGE_VERSION /usr/local/include/tree_sitter/api.h | head -1)"

echo ""
echo "=== Step 2: Build Emacs 30.2 ==="
cd ~/github/emacs
git fetch --depth 1 origin tag emacs-30.2
git checkout emacs-30.2

make clean 2>/dev/null || true

if [ ! -f configure ]; then
  ./autogen.sh
fi

./configure \
  --with-x-toolkit=no \
  --with-xpm=no \
  --with-jpeg=no \
  --with-png=no \
  --with-gif=no \
  --with-tiff=no \
  --with-native-compilation=aot \
  --with-json \
  --without-x \
  --without-compress-install \
  --with-xml2 \
  --with-tree-sitter \
  --with-mailutils \
  --with-modules \
  'CFLAGS=-O3 -march=native -mtune=native -fomit-frame-pointer'

make -j$(nproc)

echo ""
echo "Installing Emacs (requires sudo)..."
sudo make install

echo ""
echo "=== Step 3: Install tree-sitter grammars ==="
emacs --batch -l "$SCRIPT_DIR/init.el" --eval '(mp-setup-install-grammars)' 2>&1

echo ""
echo "=== Step 4: Generate package-quickstart.el ==="
emacs --batch -l "$SCRIPT_DIR/init.el" --eval '(package-quickstart-refresh)' 2>&1

echo ""
echo "=== Step 5: Native-compile packages ==="
emacs --batch -f batch-native-compile \
  $(find ~/.emacs.d/elpa -name "*.el" -not -path "*/.*") 2>&1

echo ""
echo "=== Done ==="
emacs --version | head -1
emacs --batch --eval '(message "TS ABI max: %s" (treesit-library-abi-version t))' 2>&1
emacs --batch -L ~/.dotfiles/emacs.d/lisp/ --eval '
(progn
  (require (quote pkl-ts-mode))
  (with-temp-buffer
    (insert "class Foo { x: Int }")
    (pkl-ts-mode)
    (message "pkl-ts-mode: OK - parser: %s" (treesit-parser-list))))' 2>&1
