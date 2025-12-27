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

- [x] **ステップ2: AtCoderジャッジ環境の再現**
    - [x] **方針(1) (失敗):** `ac-library-hs`のみを導入する最小構成を試みたが、`cabal repl`でモジュールが見つからず失敗。
          **原因:** `ac-library-hs`は多くの依存ライブラリを持つため、そのバージョン解決が複雑であり、単一の依存追加だけでは不十分だった。また、プロジェクトが`executable`のみの構成では、`cabal repl`が依存関係を適切に読み込めない問題があった。
    - [x] **方針(2) (失敗):** 公式Gistの設定ファイル群（`.cabal`, `.project`, `.freeze`）をコピーする方針に転換。
          **原因:** コピーしたファイル（特に`.project`と`.freeze`）の構文が、使用中の`cabal`バージョン(`3.14.2.0`)と非互換であり、大量の警告と`[no-main-is]`エラーでビルド自体が失敗した。
    - [x] **方針(3) (成功だが`repl`は失敗):** Gistファイルのコピーを断念し、`cabal`自身の依存関係解決機能(`cabal v2-freeze`)を利用して`freeze`ファイルを自動生成する方針に転換。`.cabal`と`.project`を最小化し、`Dockerfile`で`cabal v2-update` -> `cabal v2-freeze` -> `cabal v2-build --only-dependencies`を実行するように変更。
          **結果:** コンテナビルドと`cabal build`は成功。構文エラーは解消された。しかし、`cabal repl`での`import Data.Vector.FenwickTree`は依然失敗。
          **原因:** `cabal repl`が、`executable`のみのプロジェクト構成で、依存ライブラリをGHCiセッションに適切に読み込めていなかった。
    - [x] **方針(4) (最終的な解決策):** `cabal build`は成功し`cabal repl`が失敗することから、プロジェクト構造と`cabal repl`の起動方法に問題があると判断。
          **採用した解決策:**
          1.  プロジェクトを`library`と`executable`を持つ堅牢な構造に再構成。これにより`cabal repl`が`library`コンポーネントを正しくロードするようになった。
          2.  `cabal repl lib:atcoder-env`による明示的なライブラリコンポーネントのロード。
          3.  `ac-library-hs`のモジュール構造を誤解していた点を修正し、`AtCoder.FenwickTree`という正しいモジュールパスで`import`を実行。`import AtCoder.`からの補完機能により、この正しいパスが判明した。
          **結果:** `ac-library-hs`のモジュールが`cabal repl`内で正常にインポート可能になった。

- [ ] **ステップ3: `ac-library-hs`の動作確認と競技プログラミングワークフローへの統合**
    - [x] **フェーズ1: 基本機能の確認 (現在の`app/Main.hs`を利用)**
          - **これまでの試行結果:**
            - `cabal build`は成功し、`ac-library-hs`を利用したコードがコンパイルできることが確認された。
            - `cabal run`で出力がなかったのは、`app/Main.hs`がまだダミーの`main = return ()`であったため。
            - `cabal repl lib:atcoder-env`での`import AtCoder.FenwickTree`は成功した。
            - REPLでのFenwickTreeの利用時に型エラー(`FenwickTree Int`が`* -> *`型であるというエラー)が発生したが、これは`FenwickTree (Sum Int)`のように`Monoid`のインスタンスを渡す必要があるという、**テストコードの記述ミス**であった。
          - **次のアクション:**
            1.  `app/Main.hs`を正しいFenwickTreeの利用例（`FenwickTree (Sum Int)`を使用）で更新する。
            2.  更新後、`cabal build`、`cabal run atcoder-env-exe`、`cabal repl lib:atcoder-env`（REPL内で正しいFenwickTreeのコードを試す）を再度実行し、動作を確認する。
    - [ ] **フェーズ2: 競技プログラミングの実際のワークフローでの確認 (atcoder-cli/online-judge-tools)**
          1.  `hs/`配下に新しいコンテストディレクトリを作成する。
          2.  `atcoder-cli`を使用して問題をフェッチし、テンプレートを生成する。
          3.  生成されたHaskellテンプレートを修正し、`ac-library-hs`（例: FenwickTree）を利用するコードを記述する。
          4.  `online-judge-tools`によるサンプルテスト、および`GHCi`での検証が問題なく動作することを確認する。
    - [ ] この状態を `feat: Verify ac-library-hs functionality and integrate with AtCoderCLI workflow` のようなコミットメッセージで保存する。

- [ ] **ステップ4: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
