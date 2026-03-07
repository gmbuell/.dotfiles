#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Detect OS ---
OS="$(uname -s)"
case "$OS" in
  Linux)  PLATFORM=linux ;;
  Darwin) PLATFORM=macos ;;
  *)      echo "ERROR: Unsupported OS: $OS"; exit 1 ;;
esac

# --- Helper: portable nproc ---
num_cpus() {
  if [ "$PLATFORM" = "macos" ]; then
    sysctl -n hw.ncpu
  else
    nproc
  fi
}

# --- Helper: prompt to install missing packages (default: yes) ---
prompt_install() {
  local cmd="$1"
  echo ""
  echo "Missing build dependencies. Run the following?"
  echo "  $cmd"
  read -r -p "Install now? [Y/n] " answer
  answer="${answer:-y}"
  if [[ "$answer" =~ ^[Yy]$ ]]; then
    eval "$cmd"
  else
    echo "Aborting."
    exit 1
  fi
}

# --- Dependency check ---
if [ "$PLATFORM" = "linux" ] && command -v dpkg &>/dev/null; then
  # Debian/Ubuntu
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
    prompt_install "sudo apt install ${MISSING[*]}"
  fi
elif [ "$PLATFORM" = "linux" ] && command -v dnf &>/dev/null; then
  # Fedora/RHEL/CentOS
  REQUIRED_PKGS=(
    gcc gcc-c++ make autoconf automake texinfo pkgconf git
    libxml2-devel jansson-devel gnutls-devel ncurses-devel mailutils
    libgccjit-devel
  )

  MISSING=()
  for pkg in "${REQUIRED_PKGS[@]}"; do
    if ! rpm -q "$pkg" &>/dev/null; then
      MISSING+=("$pkg")
    fi
  done

  if [ ${#MISSING[@]} -gt 0 ]; then
    prompt_install "sudo dnf install ${MISSING[*]}"
  fi
elif [ "$PLATFORM" = "linux" ]; then
  echo "WARNING: Unrecognized Linux package manager. Skipping dependency check."
  echo "Ensure you have: gcc, make, autoconf, automake, texinfo, pkg-config, git,"
  echo "  libxml2-dev, jansson-dev, gnutls-dev, ncurses-dev, libgccjit-dev, mailutils"
else
  # macOS: check for Homebrew dependencies
  REQUIRED_BREWS=(autoconf automake texinfo pkg-config gnutls libgccjit jansson libxml2 mailutils)
  MISSING=()
  for pkg in "${REQUIRED_BREWS[@]}"; do
    if ! brew list "$pkg" &>/dev/null; then
      MISSING+=("$pkg")
    fi
  done

  if [ ${#MISSING[@]} -gt 0 ]; then
    prompt_install "brew install ${MISSING[*]}"
  fi
fi

# --- Clone repos if missing ---
mkdir -p ~/github

if [ ! -d ~/github/tree-sitter ]; then
  echo "Cloning tree-sitter..."
  git clone --depth 1 https://github.com/tree-sitter/tree-sitter.git ~/github/tree-sitter
fi

if [ ! -d ~/github/emacs ]; then
  echo "Cloning emacs..."
  git -c transfer.bundleURI=false clone --depth 1 https://git.savannah.gnu.org/git/emacs.git ~/github/emacs
fi

# --- Clean stale compiled artifacts ---
if [ -d ~/.emacs.d/tree-sitter ]; then
  echo "Removing stale tree-sitter grammars..."
  if [ "$PLATFORM" = "macos" ]; then
    rm -f ~/.emacs.d/tree-sitter/*.dylib
  else
    rm -f ~/.emacs.d/tree-sitter/*.so
  fi
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
make -j"$(num_cpus)"

echo ""
echo "Installing tree-sitter to /usr/local (requires sudo)..."
sudo mkdir -p /usr/local/lib/pkgconfig /usr/local/include/tree_sitter
sudo cp libtree-sitter.a /usr/local/lib/
if [ "$PLATFORM" = "macos" ]; then
  sudo cp libtree-sitter.dylib /usr/local/lib/libtree-sitter.0.25.dylib
  sudo ln -sf libtree-sitter.0.25.dylib /usr/local/lib/libtree-sitter.0.dylib
  sudo ln -sf libtree-sitter.0.25.dylib /usr/local/lib/libtree-sitter.dylib
else
  sudo cp libtree-sitter.so /usr/local/lib/libtree-sitter.so.0.25
  sudo ln -sf libtree-sitter.so.0.25 /usr/local/lib/libtree-sitter.so.0
  sudo ln -sf libtree-sitter.so.0.25 /usr/local/lib/libtree-sitter.so
fi
sudo cp lib/include/tree_sitter/api.h /usr/local/include/tree_sitter/api.h
sudo cp tree-sitter.pc /usr/local/lib/pkgconfig/
if [ "$PLATFORM" = "linux" ]; then
  sudo ldconfig
fi

echo "tree-sitter $(grep TREE_SITTER_LANGUAGE_VERSION /usr/local/include/tree_sitter/api.h | head -1)"

echo ""
echo "=== Step 2: Build Emacs 30.2 ==="
cd ~/github/emacs
git -c transfer.bundleURI=false fetch --depth 1 origin tag emacs-30.2
git checkout emacs-30.2

make clean 2>/dev/null || true

if [ ! -f configure ]; then
  ./autogen.sh
fi

CONFIGURE_ARGS=(
  --with-native-compilation=aot
  --with-json
  --without-compress-install
  --with-xml2
  --with-tree-sitter
  --with-mailutils
  --with-modules
)

if [ "$PLATFORM" = "macos" ]; then
  CONFIGURE_ARGS+=(
    --without-ns
    --without-x
  )
  # Ensure Homebrew paths are visible to configure
  export PKG_CONFIG_PATH="$(brew --prefix libgccjit)/lib/pkgconfig:$(brew --prefix gnutls)/lib/pkgconfig:$(brew --prefix jansson)/lib/pkgconfig:$(brew --prefix libxml2)/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
  export LDFLAGS="-L$(brew --prefix libgccjit)/lib -L$(brew --prefix gnutls)/lib ${LDFLAGS:-}"
  export CPPFLAGS="-I$(brew --prefix libgccjit)/include -I$(brew --prefix gnutls)/include ${CPPFLAGS:-}"
  export LIBRARY_PATH="$(brew --prefix libgccjit)/lib:${LIBRARY_PATH:-}"
  CONFIGURE_ARGS+=('CFLAGS=-O3 -fomit-frame-pointer')
else
  CONFIGURE_ARGS+=(
    --with-x-toolkit=no
    --with-xpm=no
    --with-jpeg=no
    --with-png=no
    --with-gif=no
    --with-tiff=no
    --without-x
  )
  CONFIGURE_ARGS+=('CFLAGS=-O3 -march=native -mtune=native -fomit-frame-pointer')
fi

./configure "${CONFIGURE_ARGS[@]}"

make -j"$(num_cpus)"

echo ""
echo "Installing Emacs (requires sudo)..."
sudo make install

echo ""
echo "=== Step 3: Install tree-sitter grammars ==="
emacs --batch -l "$SCRIPT_DIR/init.el" --eval '(mp-setup-install-grammars)' 2>&1

echo ""
echo "=== Step 4: Native-compile packages ==="
emacs --batch -f batch-native-compile \
  $(find ~/.emacs.d/elpa -name "*.el" -not -name ".[^.]*") 2>&1

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
