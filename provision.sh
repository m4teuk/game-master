#!/usr/bin/env bash
set -euo pipefail

REPO_URL="https://github.com/m4teuk/game-master.git"
REPO_DIR="$HOME/game-master"
ENGINE_DIR="$REPO_DIR/engine"
BINARY="$ENGINE_DIR/_build/default/server/server.exe"
OCAML_VERSION="5.2.0"
SERVICE_NAME="gamemaster"

# System locations the running service reads. The service runs as a throwaway
# DynamicUser with ProtectHome=yes, so it cannot see $HOME — the binary and the
# prebuilt games must live outside it (root-owned, world-readable). Building
# still happens in $HOME as the invoking user; only these copies are run.
INSTALL_BIN="/usr/local/bin/${SERVICE_NAME}"
GAMES_DIR="/usr/local/share/${SERVICE_NAME}/games"

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

echo "=== 8. Installing binary + prebuilt games to system locations ==="
# The DynamicUser service can't reach $HOME, so copy the artifacts out of it.
sudo install -m 0755 "$BINARY" "$INSTALL_BIN"
sudo install -d -m 0755 "$GAMES_DIR"
sudo install -m 0644 "$REPO_DIR"/game-examples/*.game "$GAMES_DIR"/

echo "=== 9. Installing systemd unit ==="
sudo tee /etc/systemd/system/${SERVICE_NAME}.service > /dev/null <<EOF
[Unit]
Description=game-master OCaml TCP server
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
# Run as a throwaway, unprivileged user that systemd creates on start and tears
# down on stop — never as a real account. The server keeps no on-disk state and
# only needs to read the world-readable games installed above. Port 3301 is
# unprivileged, so no CAP_NET_BIND_SERVICE is required.
DynamicUser=yes
ExecStart=$INSTALL_BIN -f $GAMES_DIR
Restart=always
RestartSec=2
StandardOutput=journal
StandardError=journal

# --- hardening (relax any single line if startup ever fails) ---
# ProtectHome=yes is the one that matters here: it hides /home from the service,
# so it physically cannot read your other files.
NoNewPrivileges=yes
ProtectSystem=strict
ProtectHome=yes
PrivateTmp=yes
ProtectKernelTunables=yes
ProtectKernelModules=yes
ProtectControlGroups=yes
RestrictAddressFamilies=AF_INET AF_INET6
RestrictNamespaces=yes
LockPersonality=yes
SystemCallFilter=@system-service
SystemCallErrorNumber=EPERM

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
