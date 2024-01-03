# 環境周りの調整時に必要な AtCoder 固有の情報の覚書

## test 関連

```
Typescript
`oj test -c 'npx ts-node Main.ts' -N > `date +%Y%m%d-%H%M%S`_log.txt`

Haskell
`oj test -c 'runghc Main.hs' -N > `date +%Y%m%d-%H%M%S`_log.txt`

Swift
`oj test -c 'swift Main.swift' -N > `date +%Y%m%d-%H%M%S`_log.txt`
```

oj test -c の後ろで、言語別のプログラムを実行する用のコマンド。
-d フォルダ で、テストケースの指定が出来る。(設定で指定省略出来たので消した)
-S は出力のスペース無視、-N は出力のスペースと改行の両方無視。(どちらかで良さそうなので、今は N のみ)

## submit 関連

```
Typescript
`acc submit -s -- -l 5058 -y`

Haskell
`acc submit -s -- -y`

Swift
`acc submit -s -- -y`
```

-s はファイル名指定のスキップ。
-- 以降のオプションは online-judge-tools へ渡すものになる。
-y は確認のスキップ。
-l は複数言語に該当するものは、言語種別を指定する必要があるので指定。

```
[ERROR] Matched languages were not narrowed down to one.
[INFO] You have to choose:
5052 (TypeScript 5.1 (Deno 1.35.1))
5058 (TypeScript 5.1 (Node.js 18.16.1))
```

# セットアップに関する覚書

## 初期設定

以下ページに従って、atcoder-cli,online-judge-tools を導入
(導入自体は dotfiles 側で実施)
https://qiita.com/Adaachill/items/3d4ddad56c5c2cc372cd

# Web ページリンクの覚書

## ツール公式系

atcoder-cli チュートリアル
http://tatamo.81.la/blog/2018/12/07/atcoder-cli-tutorial/

Github
https://github.com/Tatamo/atcoder-cli

online-judge-tools(oj) コマンド
https://github.com/online-judge-tools/oj/blob/master/docs/getting-started.ja.md

## 個人ブログ系

atcoder 初心者こそ環境構築しよう！(atcoder-cli,online-judge-tools のインストール、使い方）
https://qiita.com/Adaachill/items/3d4ddad56c5c2cc372cd

TypeScript で AtCoder を始めた
https://qiita.com/sakananonitsuke/items/12594420253b13c8b93b

TypeScript で AtCoder をやってみよう!
https://qiita.com/cosocaf/items/255003ecec1d3badfc7b

JavaScript で AtCoder を始める方法（環境構築～実際のテストまで）Windows 編
https://zenn.dev/deen/articles/137bf151b139ef

online-judge-tools と atcoder-cli の環境構築したときのメモ的なアレ
https://idat-50me.hatenadiary.jp/entry/20200622/1592752164

初心者向け atcoder-cli を使った環境構築(Mac 編)
https://qiita.com/YUM_3/items/6910b9d14ea544d643f3

競プロ入門者 (Haskell 盆栽 1) → 茶色が遠い (・ω・)#コマンドラインツール導入
https://zenn.dev/link/comments/a5f145d046dd51

AtCoder:Haskell の実行環境の再現
https://scrapbox.io/dragoon8192-main/AtCoder:Haskellの実行環境の再現

コマンドラインから直接 Swift コンパイラを使ってビルドする方法
https://qiita.com/omochimetaru/items/428324ed8bcd2b98dacb#単一のソースを直接実行する

コマンドの実行結果を変数に代入
http://www.ajisaba.net/sh/var_command.html

ls でディレクトリ名のみ表示する(grep は使わない)#ディレクトリ名だけを表示したい
https://qiita.com/github-nakasho/items/1433f6601bb3efc14474#ディレクトリ名だけを表示したい

逆引きシェルスクリプト/引数で渡された最期のスラッシュを削除する方法
https://linux.just4fun.biz/?逆引きシェルスクリプト/引数で渡された最期のスラッシュを削除する方法

Haskell で競技プログラミングをするための環境構築
https://blog.mmmcorp.co.jp/2023/02/27/kyopro-haskell/
