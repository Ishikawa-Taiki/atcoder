# 本リポジトリ作業用の覚書

## 前提

基本的には個人の作業記録用として利用している。
本リポジトリは atcoder-cli、online-judge-tools を利用した検証/提出を想定している。
各プログラムを実行する上で必要な環境構築は完了しているものとしている。

VSCode で作業しつつこまめに調整していく予定。

## 初回 clone 後

以下コマンドによるセットアップを実行する。
本リポジトリに登録されている設定ファイルやテンプレートのシンボリックリンクが貼られ、利用可能になる。

`sh setup.sh`

## 言語切り替え時

動作確認を行うターミナルにて各言語のフォルダに移動して、以下コマンドを実行する。
言語別のコマンドの違いを吸収してくれるので、実行方法を意識する必要がなくなる。
以降のコマンドについては、本コマンドが実行できている前提の記載となる。

`source .set`

## コンテストへの参加時

各言語のフォルダにて、対象コンテスト用の定義ダウンロードを行う。
コマンド中で最初の問題を選択することで、問題のサブフォルダが切られる。
コンテストのフォルダ配下へ自動で移動する。

`contest [コンテストのID(URLの末尾)]`

## 作業時

問題のサブフォルダ内にて、テンプレートから生成されたソースコードを編集する。

`edit`

## テスト時

問題のサブフォルダ内にて、テストケースに対する動作確認をする。

`test`

## 提出時

問題のサブフォルダ内にて、ソースコードを提出する。

`submit`

## 次の問題着手時

コンテストのフォルダ配下へ移動して、以下のコマンドを実行する。
コマンド中で次の問題を選択することで、問題のサブフォルダが切られる。
以降は繰り返して進めれば OK。

`next`

# 新言語追加

## 言語用作業フォルダの作成

\_any フォルダをコピーして、新しい言語用のフォルダを作成する。
中にあるファイルそれぞれについて、新言語向けの定義を追加する。

## テンプレート追加

atcoder-cli 配下で新しくテンプレートフォルダを切り、template.json とソースコードを格納する。
その後、setup.sh を再実行することでテンプレートが利用可能になる。

## 動作確認

適当なサンプルコンテストにて動作確認を行い、本リポジトリに登録しておく。

# Web ページリンクの覚書

AtCoder 公式

https://atcoder.jp/home

AtCoder 公式：便利リンク集

https://atcoder.jp/posts/261

AtCoder 公式：テストケース置き場(終了後しばらくしてから)

https://www.dropbox.com/sh/nx3tnilzqz7df8a/AAAYlTq2tiEHl5hsESw6-yfLa?dl=0
