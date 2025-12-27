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

- [x] **ステップ2: AtCoderジャッジ環境の再現 (完了)**
    - これまでの試行錯誤を経て、`ac-library-hs`が`cabal repl`環境で正しく認識・インポートできるようになりました。
    - **主な解決策:**
        1.  **Cabal設定ファイルの互換性問題の解決:** 公式Gistの構文が非互換だったため、最小限の`.cabal`/`.project`ファイルを使用し、`cabal v2-freeze`で環境に合った`freeze`ファイルを自動生成するように変更しました。
        2.  **プロジェクト構造の最適化:** `executable`のみのプロジェクトでは`cabal repl`が依存関係を適切に読み込めない問題があったため、`library`コンポーネントを導入し、`executable`がそれに依存する堅牢な構成に変更しました。
        3.  **明示的なGHCiセッションの起動:** `cabal repl lib:atcoder-env`と明示的にライブラリコンポーネントを指定することで、依存関係が確実にロードされるようになりました。
        4.  **正しいモジュールパスの特定:** `ac-library-hs`のFenwickTreeモジュールは`Data.Vector.FenwickTree`ではなく`AtCoder.FenwickTree`として公開されていることをGHCiの補完機能で確認し、正しいパスでの`import`に成功しました。

- [ ] **ステップ3: `ac-library-hs`の動作確認と競技プログラミングワークフローへの統合**
    - [x] **フェーズ1: 基本機能の確認 (現在の`app/Main.hs`を利用)**
          - **これまでの試行結果:**
            - `cabal build`は成功し、`ac-library-hs`を利用したコードがコンパイルできることは確認された。
            - `cabal run`で出力がなかったのは、`app/Main.hs`がまだダミーの`main = return ()`であったため（私の指示ミスにより`app/Main.hs`の更新がキャンセルされていた）。
            - `cabal repl lib:atcoder-env`での`import AtCoder.FenwickTree`は成功した。
            - REPLでのFenwickTreeの利用時に型エラー(`FenwickTree (Sum Int)`が`* -> *`型であるというエラー)が発生したが、これは`FenwickTree (Sum Int)`の誤用であった。`FenwickTree`は`FenwickTree n a`の形を取るため、`build`関数が`n`を推論し、要素の型`a`が`Monoid`である`Sum Int`のように`map Sum`して渡すのが正しかった。
          - **次のアクション (再試行):**
            1.  `app/Main.hs`を、極めてシンプルな`main = putStrLn "Hello World"`に書き換え、`cabal run`が動作することを確認する（`cabal run`が出力しない問題のデバッグ）。
            2.  上記が成功した場合、`app/Main.hs`を正しいFenwickTreeの利用例（`FenwickTree`の要素型として`Sum Int`を使用し、`map Sum`で値を変換し、`ft`変数への型注釈は**外す**）で更新する。
            3.  更新後、`cabal build`、`cabal run atcoder-env-exe`、`cabal repl lib:atcoder-env`（REPL内で正しいFenwickTreeのコードを試す）を再度実行し、動作を確認する。
    - [ ] **フェーズ2: 競技プログラミングの実際のワークフローでの確認 (atcoder-cli/online-judge-tools)**
          1.  `hs/`配下に新しいコンテストディレクトリを作成する。
          2.  `atcoder-cli`を使用して問題をフェッチし、テンプレートを生成する。
          3.  生成されたHaskellテンプレートを修正し、`ac-library-hs`（例: FenwickTree）を利用するコードを記述する。
          4.  `online-judge-tools`によるサンプルテスト、および`GHCi`での検証が問題なく動作することを確認する。
    - [ ] この状態を `feat: Verify ac-library-hs functionality and integrate with AtCoderCLI workflow` のようなコミットメッセージで保存する。

- [ ] **ステップ4: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
    - [ ] **フェーズ2: 競技プログラミングの実際のワークフローでの確認 (atcoder-cli/online-judge-tools)**
          1.  `hs/`配下に新しいコンテストディレクトリを作成する。
          2.  `atcoder-cli`を使用して問題をフェッチし、テンプレートを生成する。
          3.  生成されたHaskellテンプレートを修正し、`ac-library-hs`（例: FenwickTree）を利用するコードを記述する。
          4.  `online-judge-tools`によるサンプルテスト、および`GHCi`での検証が問題なく動作することを確認する。
    - [ ] この状態を `feat: Verify ac-library-hs functionality and integrate with AtCoderCLI workflow` のようなコミットメッセージで保存する。

- [ ] **ステップ4: GitHub Codespacesでの動作確認**
    - [ ] ローカルでのコンテナ構築が完了した構成をGitHubにプッシュする。
    - [ ] GitHub上でCodespacesを起動し、ローカルと同様に環境が構築されることを確認する。
