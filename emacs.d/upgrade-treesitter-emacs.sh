#!/usr/bin/env bash
set -euo pipefail

echo "=== Step 1: Install tree-sitter v0.25.10 (ABI 15, compatible with Emacs 30.x) ==="
cd ~/github/tree-sitter
git checkout v0.25.10
make clean
make -j$(nproc)

echo ""
echo "Installing tree-sitter to /usr/local (requires sudo)..."
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
git fetch --tags
git checkout emacs-30.2

make clean 2>/dev/null || true

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
echo "=== Step 3: Rebuild pkl grammar with ABI 15 ==="
cd /tank/config/user/github/pkl/tree-sitter-pkl
npx tree-sitter-cli@0.25.10 generate
cc -shared -o /home/garret/.emacs.d/tree-sitter/libtree-sitter-pkl.so \
  -fPIC src/parser.c src/scanner.c -I src
git checkout -- src/

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
