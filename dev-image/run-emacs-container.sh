#!/bin/bash
set -euo pipefail

# Image name can be overridden: IMG_NAME=my-emacs:22.04 ./run-emacs-container.sh
IMG_NAME="${IMG_NAME:-emacs-gptel:gui-22.04}"

# Host user info to mirror inside the image
HOST_USER="$(id -un)"
HOST_UID="$(id -u)"
HOST_GID="$(id -g)"

# Resolve script directory (where Dockerfile lives)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Ensure DISPLAY is set; default to :0 if missing
DISPLAY="${DISPLAY:-:0}"

echo "Building image ${IMG_NAME} for user ${HOST_USER} (${HOST_UID}:${HOST_GID})..."
docker build -t "${IMG_NAME}" -f "${SCRIPT_DIR}/Dockerfile" \
  --build-arg USERNAME="${HOST_USER}" \
  --build-arg UID="${HOST_UID}" \
  --build-arg GID="${HOST_GID}" \
  "${SCRIPT_DIR}"

# Best-effort: allow the host X server to accept connections from this local user
if command -v xhost >/dev/null 2>&1; then
  xhost +SI:localuser:"${HOST_USER}" >/dev/null 2>&1 || true
fi

# Mount current directory into the container user's home
WORKSPACE_MNT="/home/${HOST_USER}/workspace"

# Pass through DISPLAY and X11 socket for GUI
# Extra arguments are forwarded to Emacs (e.g., ./run-emacs-container.sh -Q)
EMACS_ARGS=("$@")



echo "Starting Emacs GUI in container..."
exec docker run --rm -it \
     -e DISPLAY="${DISPLAY}" \
  -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
  -v "$PWD:${WORKSPACE_MNT}" \
  -w "/home/${HOST_USER}" \
  "${IMG_NAME}" emacs "${EMACS_ARGS[@]}"
