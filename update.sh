#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$HOME/game-master"
ENGINE_DIR="$REPO_DIR/engine"
BINARY="$ENGINE_DIR/_build/default/server/server.exe"
SERVICE_NAME="gamemaster"
INSTALL_BIN="/usr/local/bin/${SERVICE_NAME}"
GAMES_DIR="/usr/local/share/${SERVICE_NAME}/games"

cd "$ENGINE_DIR"
eval "$(opam env)"

echo "Pulling latest..."
git pull --ff-only

echo "Installing any new deps..."
opam install . --deps-only -y

echo "Building..."
dune build

echo "Installing binary + prebuilt games to system locations..."
# The service runs as a DynamicUser that can't see $HOME, so it runs these
# copies rather than the build output under $HOME. Keep them in sync on update.
sudo install -m 0755 "$BINARY" "$INSTALL_BIN"
sudo install -d -m 0755 "$GAMES_DIR"
sudo install -m 0644 "$REPO_DIR"/game-examples/*.game "$GAMES_DIR"/

echo "Restarting service..."
sudo systemctl restart gamemaster

echo "Done. Status:"
sudo systemctl status gamemaster --no-pager -l | head -15
