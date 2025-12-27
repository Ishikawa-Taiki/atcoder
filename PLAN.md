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
    - [x] **方針(2):** 公式Gistの設定ファイル群（`.cabal`, `.project`, `.freeze`）をコピーする方針に転換。しかし、Gistのファイルの構文が使用中の`cabal`バージョンと互換性がなく、大量の警告と`[no-main-is]`エラーで失敗。
    - [ ] **方針(3) - 現在の方針:** Gistファイルのコピーを断念。`cabal`自身の依存関係解決機能を利用して、クリーンで再現可能な環境を構築する方針に再転換する。
          > **検討した別案:** `cabal`のバージョンを、Gistの`freeze`ファイルが想定しているであろう過去のバージョンに変更する案も検討しました。しかし、`freeze`ファイル自体の構文が特殊である可能性が高く、適切な`cabal`バージョンを探すのが困難と判断し、より確実性の高い`freeze`ファイルの自動生成案を採用しました。
          **次のアクション:**
          1.  `atcoder-env.cabal`を、`ac-library-hs`, `vector`など、必須ライブラリのみに依存する最小限の内容に書き換える。
          2.  `cabal.project`も最小限の`packages: .`のみに書き換える。
          3.  `cabal.project.freeze`は、ビルドプロセスで自動生成させるため、手動で作成したファイルは削除する。
          4.  `Dockerfile`を修正し、`cabal v2-update` → `cabal v2-freeze` → `cabal v2-build --only-dependencies` の順で実行させる。`v2-freeze`が、私たちの環境で有効な`freeze`ファイルを自動生成してくれる。
          5.  これらの修正後、再度コンテナをリビルドし、検証を行う。
    - [ ] コンテナをリビルドし、ビルドが成功することを確認する。
    - [ ] コンテナ内で `/home/vscode/atcoder-env` に移動し、`cabal repl` を起動後、`import qualified Data.Vector.FenwickTree as FT` が成功することを確認する。
    - [ ] この状態を `feat: Create reproducible Haskell environment` のようなコミットメッセージで保存する。

- [ ] **ステップ3: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
