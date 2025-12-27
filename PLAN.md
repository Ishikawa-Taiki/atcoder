# Haskell開発環境 構築プラン

このドキュメントは、GitHub Codespaces上でHaskellを用いた競技プログラミング環境を構築するための作業計画を記述します。

## 1. 大目標

- GitHub Codespaces を利用して、AtCoderのHaskell開発環境を安定して利用できる状態にする。

## 2. 中期目標

- まずはローカルPC上で、Dockerコンテナとして開発環境を構築し、意図したライブラリが導入された状態を再現する。

## 3. 作業ステップ

- [ ] **ステップ1: 基本のHaskell実行環境の構築**
    - [ ] `Dockerfile`を修正し、ライブラリのビルドやコピーに関する記述を一旦すべて削除する。
    - [ ] 修正した`Dockerfile`でDev Container（Dockerコンテナ）をビルド・起動する。
    - [ ] コンテナ内で`ghc --version`および`cabal --version`コマンドが実行でき、意図したバージョンがインストールされていることを確認する。
    - [ ] 簡単な`Hello.hs`ファイルを作成し、`cabal run` または `ghc` コマンドでコンパイル・実行できることを確認する。
    - [ ] この状態を `docs: Add plan and build minimal Haskell environment` のようなコミットメッセージで保存する。

- [ ] **ステップ2: 主要ライブラリ `ac-library-hs` の導入**
    - [ ] `.devcontainer/haskell-env/` 配下の既存のCabal設定ファイル (`.cabal`, `cabal.project`) を最小構成に修正する（`ac-library-hs`への依存関係のみを記述）。
    - [ ] `Dockerfile`に、`ac-library-hs`の依存関係を解決・ビルドするための`cabal v2-build --only-dependencies`コマンドを追記する。
    - [ ] コンテナをリビルドし、ビルドが成功することを確認する。
    - [ ] コンテナ内で`ghc-pkg list ac-library-hs`などを実行し、ライブラリが利用可能になっていることを確認する。
    - [ ] この状態を `feat: Add ac-library-hs dependency` のようなコミットメッセージで保存する。

- [ ] **ステップ3: その他のライブラリの追加（オプション）**
    - [ ] 必要に応じて、`vector`, `massiv` などの追加ライブラリを `atcoder-env.cabal` に追記する。
    - [ ] ライブラリを追加するごとに、コンテナのリビルドと動作確認、およびコミットを行う。

- [ ] **ステップ4: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
