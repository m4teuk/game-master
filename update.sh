#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$HOME/game-master"
ENGINE_DIR="$REPO_DIR/engine"

cd "$ENGINE_DIR"
eval "$(opam env)"

echo "Pulling latest..."
git pull --ff-only

echo "Installing any new deps..."
opam install . --deps-only -y

echo "Building..."
dune build

echo "Restarting service..."
sudo systemctl restart gamemaster

echo "Done. Status:"
sudo systemctl status gamemaster --no-pager -l | head -15
