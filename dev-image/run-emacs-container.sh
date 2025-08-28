#!/bin/bash
set -euo pipefail


SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
readonly SCRIPT_DIR

# Image name can be overridden: IMG_NAME=my-emacs:22.04 ./run-emacs-container.sh
readonly IMG_NAME="${IMG_NAME:-emacs-gptel:gui-22.04}"

# Host user info to mirror inside the image
HOST_USER=$(id -un)
readonly HOST_USER

HOST_UID=$(id -u)
readonly HOST_UID

HOST_GID=$(id -g)
readonly HOST_GID

# Resolve script directory (where Dockerfile lives)


# Ensure DISPLAY is set; default to :0 if missing
readonly DISPLAY=${DISPLAY:-:0}

echo "Building image ${IMG_NAME} for user ${HOST_USER} (${HOST_UID}:${HOST_GID})..."
docker build -t "${IMG_NAME}" -f "${SCRIPT_DIR}/Dockerfile" \
  --build-arg USERNAME="${HOST_USER}" \
  --build-arg UID="${HOST_UID}" \
  --build-arg GID="${HOST_GID}" \
  "${SCRIPT_DIR}"


# Mount current directory into the container user's home
# WORKSPACE_MNT="/home/${HOST_USER}/workspace"

# Pass through DISPLAY and X11 socket for GUI
# Extra arguments are forwarded to Emacs (e.g., ./run-emacs-container.sh -Q)
# EMACS_ARGS=("$@")



# echo "Starting Emacs GUI in container..."
# exec docker run --rm -it \
#      -e DISPLAY="${DISPLAY}" \
#      --network=host \
#      -w "/home/${HOST_USER}" \
#      "${IMG_NAME}" emacs "${EMACS_ARGS[@]}"



echo "Starting Bash in container..."
exec docker run --rm -it \
     -e DISPLAY="${DISPLAY}" \
     --network=host \
     -v "/home/${HOST_USER}/.emacs.d:/home/${HOST_USER}/.emacs.d" \
     -v "/home/${HOST_USER}/.opam:/home/${HOST_USER}/.opam" \
     -w "/home/${HOST_USER}" \
     "${IMG_NAME}" bash

