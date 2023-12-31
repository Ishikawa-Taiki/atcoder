# 本リポジトリで作業するときの覚書

## 初回clone後

以下で依存パッケージをインストールする。

`yarn install`

## 言語切り替え時

以下コマンドでテンプレートを切り替えられる。
コンテスト参加や問題切り替え毎に --template オプションをつけなくても良くなるので、言語変えるときに読んでおくと良さそう。

`acc config default-template [使いたいテンプレート名(ts/hs/swift/...)]`

## コンテストへの参加時

以下コマンドでコンテスト用のフォルダが作られる。
その際に問題が質問されるので、選ぶとサブフォルダが切られる。
その中に./testサブフォルダでテストコードがダウンロードされる。

`acc new [コンテストのID(URLの末尾)]`

## 作業時

問題のサブフォルダ内にて、テンプレートから生成されたソースコードを編集する。
標準入力を使わず、単純に動作を見たいだけなら以下が楽。
ファイル名も固定されているため、ほとんど固定のコマンドで行けるはず。

Typescript
`npx ts-node Main.ts`

ts-node を利用するとTypescriptをJavascriptへトランスパイルすることなく実行できる。
npx経由だと事前のインストールも必要ない。

Haskell
`runghc Main.hs`

Swift
`swift Main.swift`

## テスト時
以下でテストケースに対する動作確認をすると良い。

Typescript
`oj test -c 'npx ts-node Main.ts' -N > `date +%Y%m%d-%H%M%S`_log.txt`

Haskell
`oj test -c 'runghc Main.hs' -N > `date +%Y%m%d-%H%M%S`_log.txt`

Swift
`oj test -c 'swift Main.swift' -N > `date +%Y%m%d-%H%M%S`_log.txt`

oj test -c の後ろで、言語別のプログラムを実行する用のコマンド。
-d フォルダ で、テストケースの指定が出来る。(設定で指定省略出来たので消した)
-S は出力のスペース無視、-Nは出力のスペースと改行の両方無視。(どちらかで良さそうなので、今はNのみ)

## 提出時

問題のサブフォルダ内でソースコードを指定して提出する。

Typescript
`acc submit -s -- -l 5058 -y`

Haskell
`acc submit -s -- -y`

Swift
`acc submit -s -- -y`

-sはファイル名指定のスキップ。
-- 以降のオプションはonline-judge-toolsへ渡すものになる。
-yは確認のスキップ。
-lは複数言語に該当するものは、言語種別を指定する必要があるので指定。
```
[ERROR] Matched languages were not narrowed down to one.
[INFO] You have to choose:
5052 (TypeScript 5.1 (Deno 1.35.1))
5058 (TypeScript 5.1 (Node.js 18.16.1))
```

## 次の問題着手時

コンテストのフォルダ配下へ移動して、以下のコマンドを実行する。
その際に問題が質問されるので、選ぶと同様にサブフォルダが切られる。
以降は繰り返して進めればOK。

`acc add`

# セットアップに関する覚書

## 初期設定

以下ページに従って、atcoder-cli,online-judge-toolsを導入
(導入自体はdotfiles側で実施)
https://qiita.com/Adaachill/items/3d4ddad56c5c2cc372cd

node は一旦別で利用してたものをそのまま利用

# Webページリンクの覚書

## コンテスト公式系

AtCoder公式
https://atcoder.jp/home

## ツール公式系

atcoder-cli チュートリアル
http://tatamo.81.la/blog/2018/12/07/atcoder-cli-tutorial/

Github
https://github.com/Tatamo/atcoder-cli

online-judge-tools(oj) コマンド
https://github.com/online-judge-tools/oj/blob/master/docs/getting-started.ja.md

Nodeの.gitignoreのテンプレート(Typescript用)
https://github.com/github/gitignore/blob/main/Node.gitignore

Haskellの.gitignoreのテンプレート
https://github.com/github/gitignore/blob/main/Haskell.gitignore

## 個人ブログ系

atcoder初心者こそ環境構築しよう！(atcoder-cli,online-judge-toolsのインストール、使い方）
https://qiita.com/Adaachill/items/3d4ddad56c5c2cc372cd

TypeScriptでAtCoderを始めた
https://qiita.com/sakananonitsuke/items/12594420253b13c8b93b

TypeScriptでAtCoderをやってみよう!
https://qiita.com/cosocaf/items/255003ecec1d3badfc7b

JavaScriptでAtCoderを始める方法（環境構築～実際のテストまで）Windows編
https://zenn.dev/deen/articles/137bf151b139ef

online-judge-tools と atcoder-cli の環境構築したときのメモ的なアレ
https://idat-50me.hatenadiary.jp/entry/20200622/1592752164

初心者向けatcoder-cliを使った環境構築(Mac編)
https://qiita.com/YUM_3/items/6910b9d14ea544d643f3

競プロ入門者 (Haskell 盆栽 1) → 茶色が遠い (・ω・)#コマンドラインツール導入
https://zenn.dev/link/comments/a5f145d046dd51

AtCoder:Haskellの実行環境の再現
https://scrapbox.io/dragoon8192-main/AtCoder:Haskellの実行環境の再現

コマンドラインから直接Swiftコンパイラを使ってビルドする方法
https://qiita.com/omochimetaru/items/428324ed8bcd2b98dacb#単一のソースを直接実行する

