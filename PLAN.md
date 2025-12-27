# Haskell開発環境 構築プラン

このドキュメントは、GitHub Codespaces上でHaskellを用いた競技プログラミング環境を構築するための作業計画を記述します。

## 0. 開発方針

- **コンテナのリビルドとセッション:**
  コンテナのリビルドごとにインタラクティブなセッションは失われます。そのため、作業の進捗、決定事項、参照情報など、必要なすべての情報は常にこの`PLAN.md`に記録し、誰が作業を引き継いでも分かるようにします。この方針自体も、常にこのドキュメントに記載しておくものとします。

- **作業の進め方:**
  - **細やかなコミット:** 動作確認ができた、あるいは論理的にキリの良い単位で、積極的にコミットを作成します。これにより、問題が発生した際に安全に作業を巻き戻せるようにします。
  - **アプローチの記録:** 複数の解決策が考えられる場合は、それぞれの長所・短所を`PLAN.md`に記述し、なぜそのアプローチを選択したかの記録を残します。

- **参照情報:**
  - [Haskeller のための AtCoder 言語アップデート 2025](https://zenn.dev/toyboot4e/articles/haskell-on-atcoder-2025)
  - [AtCoder 2025 language update for Haskell (GHC 9.8.4) - インストールスクリプト](https://gist.github.com/toyboot4e/04b902271997b3c865cc91014720ec40)

## 1. 大目標

- GitHub Codespaces を利用して、AtCoderのHaskell開発環境を安定して利用できる状態にする。

## 2. 中期目標

- まずはローカルPC上で、Dockerコンテナとして開発環境を構築し、意図したライブラリが導入された状態を再現する。

## 3. 作業ステップ

- [x] **ステップ1: 基本のHaskell実行環境の構築**
    - （完了）

- [ ] **ステップ2: AtCoderジャッジ環境の再現**
    - [x] **方針(1):** `ac-library-hs`のみを導入する最小構成を試みたが、`cabal repl`でモジュールが見つからず失敗。
    - [x] **方針(2):** 公式Gistの設定ファイル群をコピーする方針に転換したが、構文エラーで失敗。
    - [x] **方針(3):** `cabal v2-freeze`で`freeze`ファイルを自動生成する方針に転換し、ビルドは成功したが`repl`での`import`は失敗。
    - [x] **方針(4):** プロジェクトを`library`と`executable`に再構成したが、それでも`repl`での`import`は失敗。
    - [x] **最新の状況:** `atcoder-env.cabal`の構文解析エラー(`build-depends`内のコメント)でコンテナビルドが失敗したため、コメントを削除し修正済み。
    - [x] **現状:** `cabal repl lib:atcoder-env`でも`ac-library-hs`は`import`できない。`Data.Vector`は`import`可能。`cabal build`は成功していることから、`cabal repl`でのパッケージ認識の問題と考えられる。
          **次のアクション:**
          1.  `cabal repl lib:atcoder-env`セッション内で`:show packages`を実行し、GHCiが認識しているパッケージ一覧を確認する。
          2.  `ac-library-hs`が一覧に存在するか、存在しない場合はなぜか、を特定する。
    - [ ] この状態を `feat: Create reproducible Haskell environment` のようなコミットメッセージで保存する。

- [ ] **ステップ3: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
