#!/bin/sh

# ユーザー設定
CUSTOM_SETTINGS_DIRECTORY=$PWD/atcoder-cli
APP_SETTINGS_DIRECTORY=`acc config-dir`

# 本ディレクトリ配下の全てのディレクトリをatcoder-cliの設定フォルダ配下へリンクする
for item in `ls -d */ | cut -d' ' -f 1`; do
	ln -nsf "${CUSTOM_SETTINGS_DIRECTORY}/${item%/}"		"${APP_SETTINGS_DIRECTORY}/${item%/}"
done

# 共通の設定ファイル
# 環境固有の情報を持っているので、atcoder-cliの設定フォルダ配下へ直接リンクするのではなく、個別で値設定を行う
# ln -sf "$CUSTOM_SETTINGS_DIRECTORY/config.json"		"$APP_SETTINGS_DIRECTORY/config.json"
acc config default-test-dirname-format test

npm install
