# OS
ARG VARIANT="ubuntu-22.04"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
# avoid interactive mode when installing packages
ENV DEBIAN_FRONTEND=noninteractive
# avoid interactive mode when installing ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# GHC version
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.4.5
# Cabal version
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0
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
RUN cabal update
RUN cabal install \
  'Cabal^>=3.10.1.0' \
  'Cabal-syntax^>=3.10.1.0' \
  'QuickCheck^>=2.14.3' \
  'adjunctions^>=4.4.2' \
  'array==0.5.4.0' \
  'attoparsec^>=0.14.4' \
  'base==4.17.1.0' \
  'bifunctors^>=5.6.1' \
  'binary^>=0.8.9.1' \
  'bitvec^>=1.1.4.0' \
  'bytestring^>=0.11.4.0' \
  'comonad^>=5.0.8' \
  'containers^>=0.6.7' \
  'contravariant^>=1.5.5' \
  'deepseq==1.4.8.0' \
  'directory^>=1.3.7.1 && <1.3.8.0' \
  'distributive^>=0.6.2.1' \
  'exceptions^>=0.10.7' \
  'extra^>=1.7.13' \
  'fgl^>=5.8.1.1' \
  'filepath^>=1.4.2.2 && <1.4.99' \
  'free^>=5.2' \
  'ghc-bignum==1.3' \
  'ghc-boot-th==9.4.5' \
  'ghc-prim==0.9.0' \
  'hashable^>=1.4.2.0' \
  'heaps^>=0.4' \
  'indexed-traversable^>=0.1.2.1' \
  'indexed-traversable-instances^>=0.1.1.2' \
  'integer-gmp^>=1.1' \
  'integer-logarithms^>=1.0.3.1' \
  'kan-extensions^>=5.2.5' \
  'lens^>=5.2.2' \
  'linear-base^>=0.3.1' \
  'list-t^>=1.0.5.6' \
  'massiv^>=1.0.4.0' \
  'megaparsec^>=9.4.1' \
  'mono-traversable^>=1.0.15.3' \
  'mtl^>=2.3.1' \
  'mutable-containers^>=0.3.4.1' \
  'mwc-random^>=0.15.0.2' \
  'parallel^>=3.2.2.0' \
  'parsec^>=3.1.16.1' \
  'parser-combinators^>=1.3.0' \
  'pretty^>=1.1.3.6' \
  'primitive^>=0.8.0.0' \
  'process^>=1.6.17.0' \
  'profunctors^>=5.6.2' \
  'psqueues^>=0.2.7.3' \
  'random^>=1.2.1.1' \
  'reflection^>=2.1.7' \
  'regex-tdfa^>=1.3.2.1' \
  'safe-exceptions^>=0.1.7.3' \
  'scientific^>=0.3.7.0' \
  'semialign^>=1.3' \
  'semigroupoids^>=6.0.0.1' \
  'split^>=0.2.3.5' \
  'stm^>=2.5.1.0' \
  'strict^>=0.5' \
  'strict-lens^>=0.4.0.3' \
  'tagged^>=0.8.7' \
  'template-haskell==2.19.0.0' \
  'text^>=2.0.2' \
  'tf-random^>=0.5' \
  'these^>=1.2' \
  'these-lens^>=1.0.1.3' \
  'time^>=1.12.2' \
  'transformers^>=0.6.1.0' \
  'trifecta^>=2.1.2' \
  'unboxing-vector^>=0.2.0.0' \
  'unix==2.7.3' \
  'unordered-containers^>=0.2.19.1' \
  'utility-ht^>=0.0.17' \
  'vector^>=0.13.0.0' \
  'vector-algorithms^>=0.9.0.1' \
  'vector-stream^>=0.1.0.0' \
  'vector-th-unbox^>=0.2.2' \
  'xhtml^>=3000.2.2.1'
