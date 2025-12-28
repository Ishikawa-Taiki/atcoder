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
RUN mkdir -p /home/vscode/.stack && \
    echo "resolver: ghc-9.8.4" > /home/vscode/.stack/config.yaml && \
    echo "system-ghc: true" >> /home/vscode/.stack/config.yaml && \
    stack install ac-library-hs-1.5.3.0 array bytestring containers deepseq extra heaps massiv monad-memo mtl parsec primitive psqueues random text time transformers unordered-containers vector vector-algorithms vector-split
