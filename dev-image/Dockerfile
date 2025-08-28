FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME
ARG UID
ARG GID
ARG EMACS_TAG=30.0.93
ENV EMACS_TAG=${EMACS_TAG}

# Base tools + GUI Emacs, git, ripgrep, TLS, locales, tzdata, curl
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    make autoconf texinfo libmailutils-dev libgnutls28-dev libx11-dev libgtk-3-dev libtls-dev libtiff-dev libtree-sitter-dev libgif-dev libxpm-dev libncurses-dev gcc g++ libgccjit-11-dev emacs git ripgrep ca-certificates locales tzdata curl \

    && rm -rf /var/lib/apt/lists/*

# Locales
RUN sed -i 's/^# \(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create user and group to match the host user
RUN test -n "${USERNAME}" && test -n "${UID}" && test -n "${GID}"
RUN groupadd -g "${GID}" "${USERNAME}" && \
    useradd -m -u "${UID}" -g "${GID}" -s /bin/bash "${USERNAME}"

# Copy the emacs init file in for the user
COPY emacs-init .emacs

# Default to the created user and their home directory
RUN git clone --depth 1 --branch emacs-30.0.93 https://github.com/emacs-mirror/emacs.git

USER ${USERNAME}
WORKDIR /home/${USERNAME}

RUN git clone --depth 1 --branch emacs-${EMACS_TAG} https://github.com/emacs-mirror/emacs.git
WORKDIR /home/${USERNAME}/emacs
RUN ./autogen.sh
RUN ./configure --prefix=$HOME/.local --with-tree-sitter --with-mailutils --without-native-compilation
RUN make -j
RUN make -j install

WORKDIR /home/${USERNAME}

COPY init.el .emacs
COPY bashrc .bashrc


USER root
RUN chown ${UID}:${GID} .emacs .bashrc

USER ${USERNAME}
RUN mkdir Sandbox
WORKDIR /home/${USERNAME}/Sandbox
RUN git clone https://github.com/sabjohnso/my-emacs.git
WORKDIR /home/${USERNAME}
