FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME
ARG UID
ARG GID

# Base tools + GUI Emacs, git, ripgrep, TLS, locales, tzdata, curl
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      emacs-gtk git ripgrep ca-certificates locales tzdata curl \
    && rm -rf /var/lib/apt/lists/*

# Locales
RUN sed -i 's/^# \(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create user and group to match the host user
RUN test -n "${USERNAME}" && test -n "${UID}" && test -n "${GID}"
RUN groupadd -g "${GID}" "${USERNAME}" && \
    useradd -m -u "${UID}" -g "${GID}" -s /bin/bash "${USERNAME}"

# Default to the created user and their home directory
USER ${USERNAME}
WORKDIR /home/${USERNAME}
