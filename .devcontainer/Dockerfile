# OS
ARG VARIANT="ubuntu-22.04"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
# avoid interactive mode when installing packages
ENV DEBIAN_FRONTEND=noninteractive

# Install system packages required for AtCoder Haskell environment and other tools
RUN apt-get update && \
    apt-get -y install --no-install-recommends \
    # For ghcup & cabal build
    git sudo jq bc make automake rsync htop curl build-essential lsb-release pkg-config libffi-dev libgmp-dev software-properties-common libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev g++ wget libncursesw5 libtool autoconf \
    # For AtCoder official script (hmatrix etc.)
    llvm-15 libgsl0-dev liblapack-dev libatlas-base-dev libglpk-dev \
    # For atcoder-cli & oj-tools
    nodejs npm python3-pip time \
    && apt-get -y autoremove && apt-get -y clean

# Install python and node tools
RUN pip3 install online-judge-tools
RUN npm install -g atcoder-cli n
RUN n latest
RUN npm install -g yarn

# Switch to vscode user to install Haskell tools in user space
USER vscode

# Install ghcup and set up GHC and Cabal
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_MINIMAL=1 sh"
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin
RUN . ~/.ghcup/env && \
    ghcup install cabal 3.14.2.0 && ghcup set cabal 3.14.2.0 && \
    ghcup install ghc 9.8.4 && ghcup set ghc 9.8.4

# Copy and run the build script to install Haskell dependencies
COPY --chown=vscode:vscode build-haskell-env.sh /tmp/build-haskell-env.sh
RUN chmod +x /tmp/build-haskell-env.sh && /tmp/build-haskell-env.sh

# Configure stack to use the system GHC and its libraries
RUN mkdir -p /home/vscode/.stack && \
    echo "system-ghc: true" > /home/vscode/.stack/config.yaml && \
    echo "allow-newer: true" >> /home/vscode/.stack/config.yaml
