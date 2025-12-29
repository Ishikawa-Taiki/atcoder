# Haskell開発環境 構築プラン

このドキュメントは、GitHub Codespaces上でHaskellを用いた競技プログラミング環境を構築するための作業計画を記述します。

## 1. 大目標

- GitHub Codespacesを利用して、AtCoderの2025年言語アップデート（GHC 9.8.4）に準拠したHaskell開発環境を構築し、`ac-library-hs`などのライブラリを利用可能にする。

## 2. これまでの経緯と課題の変遷

この環境構築は当初の想定より著しく難航した。将来のメンテナンスのため、失敗の経緯とそこから得られた知見を全て記録する。

### 2.1.【失敗】アプローチ1: Cabalベースでの環境構築

- **当初の計画:** AtCoder公式のGistで公開されている`cabal`ベースのセットアップを参考に、コンテナ内にライブラリをインストールし、`cabal run` や `cabal repl` での開発を目指した。
- **発生した問題:**
    1.  `cabal repl` で `ac-library-hs` などのライブラリが正しく認識されなかった。
    2.  上記はプロジェクト構成（`.cabal`ファイル）を調整することで解決したが、次に`cabal build`で生成した実行ファイルが標準出力に何も表示しない、という深刻な問題に直面した。
    3.  GHCやCabalのバージョン、コンテナ環境に深く依存する問題と推測されたが、原因の特定と解決が困難であった。
- **結論:** `cabal`による実行時の問題解決を断念し、より実績のある`stack`ベースのアプローチへ移行することを決定。

### 2.2.【失敗】アプローチ2: Stackグローバル環境の構築

- **計画:** `stack`をインストールし、`system-ghc: true`を設定して`ghcup`で導入したGHCを利用させつつ、`stack install`や`extra-deps`を用いて全ユーザーが暗黙的に利用できるグローバルなライブラリ環境の構築を目指した。
- **発生した問題（試行錯誤の連鎖）:**
    1.  **`stack config`コマンドのエラー:** `stack config set resolver --global` のようなコマンドが、`stack`のバージョンによって挙動が異なり、ビルドに失敗した。
    2.  **`stack install`の挙動誤解:** `stack install <packages...>` が、ローカルにプロジェクトがないと判断するとエラーになる問題に直面した。
    3.  **Dockerfileの構文・権限エラー:** 上記を回避するために複雑な`RUN`命令やビルドスクリプトを導入したが、`Dockerfile`の`RUN`命令のシェル解釈（`source`が使えない）、ヒアドキュメントの構文エラー、`COPY`命令のファイル所有権（`chmod`が失敗する）など、私のDocker知識不足による初歩的なミスを連発し、ビルド失敗を繰り返した。
    4.  **【根本原因】StackがシステムGHCを無視する:**
        - 上記の失敗を乗り越え、ようやくビルドが通った後も、`stack exec ghc -- --version` を実行すると、`ghcup`で入れたGHC `9.8.4`ではなく、Stackが独自にダウンロードしたGHC `9.10.3`を使い始めることが判明した。
        - これは、`~/.stack/config.yaml`に`system-ghc: true`を設定するだけでは、`stack`がカレントディレクトリに`stack.yaml`を見つけられない場合にフォールバックする「暗黙のグローバルプロジェクト」の挙動を制御するには不十分であったためである。
- **結論:** グローバルな環境を構築し、個々の`.hs`ファイルを独立して扱うという**アプローチ自体が、Stackのプロジェクトベースの設計思想と相性が悪く、不安定である**と判断。このアプローチを完全に破棄する。

## 3. 新しいアプローチと解決の見込み（先行調査済）

これまでの失敗とユーザーからの提案を踏まえ、**Devcontainer Featureを活用し、`hs`ディレクトリを単一のStackプロジェクトとして管理する**という、全く新しいアプローチに方針を転換する。

- **先行調査の結果:**
    1.  **GHC/Stackバージョンの実現性:** `ghcr.io/devcontainers-extra/features/haskell`というFeatureが存在し、オプションで`ghcVersion` (`9.8.4`を指定可) や`cabalVersion`を指定できる。また、`installStackGHCupHook: true` というオプション（デフォルトで有効）により、**StackがghcupでインストールしたGHCを確実に利用するようになる**ことが判明。これにより、これまで直面した最大の問題が解決できる。
    2.  **ライブラリの整合性:** 上記でGHC 9.8.4の環境が確実に構築できるため、`hs/`ディレクトリに配置する`stack.yaml`の`extra-deps`にAtCoder公式のライブラリ群を指定することで、依存関係の解決は可能である。これは`cabal`の公式設定で実績がある組み合わせであり、`stack`でも再現できる見込みが非常に高い。

- **結論:** この新アプローチは、これまでの失敗要因（Dockerfileの自作、ツールの挙動の不確実性）を排除し、ツールの標準的な使い方に沿った、最も確実で安定した方法であると判断。

## 4. 新・作業計画

- [x] **ステップ1: 環境のクリーンアップ**
    - [x] これまでの試行錯誤で作成した`.devcontainer/Dockerfile`および`.devcontainer/build-haskell-env.sh`を削除する。

- [x] **ステップ2: `devcontainer.json`の作成**
    - [x] `.devcontainer/devcontainer.json`を新規に作成する。
    - [x] `image`には`mcr.microsoft.com/devcontainers/base:ubuntu`などを指定。
    - [x] `features`ブロックを追加し、`ghcr.io/devcontainers-extra/features/haskell:2`を指定する。
    - [x] Featureのオプションで、`ghcVersion`を`"9.8.4"`、`cabalVersion`を`"3.14.2.0"`に設定する。`installStackGHCupHook`が`true`であることを確認する。

- [x] **ステップ3: Haskell Stackプロジェクトの作成**
    - [x] `hs/`ディレクトリ直下に`stack.yaml`を作成する。
        - `resolver`には、GHC 9.8.4に対応する`lts`が無いため、`ghc-9.8.4`のようなカスタム設定を行う。
        - `system-ghc: true`を指定する。
        - `packages: ['.']`などを指定する。
        - `extra-deps`に、AtCoder公式Gistから取得したライブラリのリストをバージョン付きで全て記述する。
    - [x] `hs/`ディレクトリ直下に`package.yaml`を作成する。
        - `name: atcoder-haskell-env`のような名前を付ける。
        - `dependencies`セクションに、`extra-deps`に記述したライブラリ名をリストする。
        - `library`セクションを定義し、全てのモジュールを`exposed-modules`に追加するか、あるいはダミーのライブラリとして構成する。これにより、`stack ghci`で全てのライブラリがスコープに入るようにする。

- [ ] **ステップ4: ビルドと動作確認**
    - [x] コンテナをリビルドする。
    - [x] `hs/`ディレクトリに移動し、`stack build`を実行して、全ての依存関係がエラーなくビルドされることを確認する。
        - **Note:** `hmatrix` がCライブラリ (`blas`, `lapack`, `glpk`, `gsl`) に依存しており、ビルドに失敗した。`apt-get install` で `libblas-dev`, `liblapack-dev`, `libglpk-dev`, `libgsl-dev` をコンテナにインストールすることで解決した。この変更は `devcontainer.json` に反映する必要がある。
    - [x] `stack ghci`を起動し、`import AtCoder.FenwickTree`が成功することを確認する。
    - [ ] テスト用のファイル (`hs/_trial/a/Main.hs`) を `stack exec runghc -- hs/_trial/a/Main.hs` で実行し、正常に動作することを確認する。
        - **Note:** 初回実行時、`stack`がプロジェクトを認識できず、無関係なGHCをインストールしようとして失敗した。ワーキングディレクトリを `hs/` に修正して再実行したところ、今度は `Variable not in scope` エラーが発生した。
        - **原因:** `hs/package.yaml` に `_trial/a/Main.hs` を実行ファイルとして定義する `executables` セクションがなかったため、依存関係が解決されなかった。
        - **対応:** `package.yaml` に `executables` セクションを追記した。
        - **失敗:** `runghc`で再実行したが、同じ `Variable not in scope` エラーが発生。`stack exec runghc` は `package.yaml` のコンポーネント定義を解釈しないことが判明した。
        - **次のアクション:** `stack build atcoder-haskell-env:exe:trial-a` のように、コンポーネント名を明示してビルドを試みる。

- [ ] **ステップ5: ワークフローの再整備**
    - [ ] `SETUP_NOTE.md`を更新し、`oj`のテストコマンドを`stack exec runghc Main.hs`のように、`hs`ディレクトリから実行する形に修正する。
    - [ ] `README.md`なども必要に応じて更新し、新しい開発フローを記載する。

## 5. 作業方針と進行方法

- **コンテナ操作:**
    - 開発環境は `vsc-atcoder-***` という名称のDockerコンテナ内に構築されています。
    - 私はホストOSから `docker exec -w /workspaces/atcoder <コンテナ名 or ID> <コマンド>` を実行することで、コンテナ内でのビルド、テスト、ファイル操作を行います。
    - コンテナ上でコマンドを実行する際は、都度ユーザーに確認を求めます。
- **進捗管理:**
    - `PLAN.md` のタスクリストを随時更新し、作業の進捗を明確に記録します。
- **バージョン管理:**
    - 各ステップの作業が完了、または検証が一区切りつくたびに、変更内容をコミットし、リモートリポジトリにプッシュします。