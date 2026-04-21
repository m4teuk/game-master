#!/usr/bin/env bash
set -euo pipefail

REPO_URL="https://github.com/m4teuk/game-master.git"
REPO_DIR="$HOME/game-master"
ENGINE_DIR="$REPO_DIR/engine"
BINARY="$ENGINE_DIR/_build/default/server/server.exe"
OCAML_VERSION="5.2.0"
SERVICE_NAME="gamemaster"

echo "=== 1. Adding 4 GB swap (prevents OOM during OCaml compile) ==="
if ! swapon --show | grep -q swapfile; then
  sudo fallocate -l 4G /swapfile
  sudo chmod 600 /swapfile
  sudo mkswap /swapfile
  sudo swapon /swapfile
  echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
fi

echo "=== 2. Installing system dependencies ==="
sudo apt update
sudo apt install -y build-essential m4 pkg-config git opam unzip curl bubblewrap

echo "=== 3. Initializing opam ==="
if [ ! -d "$HOME/.opam" ]; then
  opam init --bare --disable-sandboxing -y
fi

echo "=== 4. Creating OCaml $OCAML_VERSION switch (this will take ~15 min) ==="
if ! opam switch list --short | grep -q "^$OCAML_VERSION$"; then
  opam switch create "$OCAML_VERSION" -y
fi
eval "$(opam env --switch=$OCAML_VERSION)"

echo "=== 5. Cloning repo ==="
if [ ! -d "$REPO_DIR" ]; then
  git clone "$REPO_URL" "$REPO_DIR"
fi

echo "=== 6. Installing project dependencies ==="
cd "$ENGINE_DIR"
opam install . --deps-only -y

echo "=== 7. Building ==="
dune build

echo "=== 8. Installing systemd unit ==="
sudo tee /etc/systemd/system/${SERVICE_NAME}.service > /dev/null <<EOF
[Unit]
Description=game-master OCaml TCP server
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
User=$USER
WorkingDirectory=$ENGINE_DIR
ExecStart=$BINARY
Restart=always
RestartSec=2
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable --now ${SERVICE_NAME}

echo
echo "=== Done! ==="
echo "Status:  sudo systemctl status ${SERVICE_NAME}"
echo "Logs:    sudo journalctl -u ${SERVICE_NAME} -f"
echo "Test:    nc tcp.kussowski.dev 3301"
