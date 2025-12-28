# OS
ARG VARIANT="ubuntu-22.04"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
# avoid interactive mode when installing packages
ENV DEBIAN_FRONTEND=noninteractive
# avoid interactive mode when installing ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# GHC version
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.8.4
# Cabal version
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.14.2.0
# install stack
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1
# install haskell language server
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
# OS packages for ghcup
RUN apt-get update && \
    apt-get -y install --no-install-recommends git sudo jq bc make automake rsync htop curl build-essential lsb-release pkg-config libffi-dev libgmp-dev software-properties-common libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ wget libncursesw5 libtool autoconf && \
    apt-get clean
# AtCoderCLI / オンラインジャッジツール関連
RUN apt update &&  \
    apt -y upgrade &&  \
    apt install -y nodejs npm python3-pip time
RUN pip3 install online-judge-tools
RUN npm install -g atcoder-cli
# Node.js のバージョン切り替え
RUN npm install -g n
RUN n latest
RUN npm install -g yarn
# change user
USER vscode
# install ghcup
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
# set PATH
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin

# Configure stack and install libraries
# 1. Set system-ghc globally and create a global stack.yaml with resolver, packages and extra-deps
RUN mkdir -p /home/vscode/.stack/global-project && \
    echo "system-ghc: true" > /home/vscode/.stack/config.yaml && \
    echo "resolver: ghc-9.8.4" > /home/vscode/.stack/global-project/stack.yaml && \
    echo "allow-newer: true" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "packages: []" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "extra-deps:" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- ac-library-hs-1.5.3.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- array-0.5.8.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- bytestring-0.12.2.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- containers-0.8.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- deepseq-1.5.1.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- extra-1.8.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- heaps-0.4" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- massiv-1.1.1.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- monad-memo-0.5.4" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- mtl-2.4.1" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- parsec-3.1.16.1" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- primitive-0.9.1.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- psqueues-0.2.7.3" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- random-1.3.1" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- text-2.1.2" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- time-1.12.2" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- transformers-0.6.1.1" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- unordered-containers-0.2.20" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- vector-0.13.2.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- vector-algorithms-0.9.1.0" >> /home/vscode/.stack/global-project/stack.yaml && \
    echo "- vector-split-1.0.0.4" >> /home/vscode/.stack/global-project/stack.yaml

# 2. Build the dependencies
RUN stack build --dependencies-only
