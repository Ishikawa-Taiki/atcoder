#!/bin/sh

# ユーザー設定
CUSTOM_SETTINGS_DIRECTORY=`pwd`/atcoder-cli
APP_SETTINGS_DIRECTORY=`acc config-dir`

# 共通の設定ファイルをatcoder-cliの設定フォルダ配下へリンクする
ln -sf "$CUSTOM_SETTINGS_DIRECTORY/config.json"		"$APP_SETTINGS_DIRECTORY/config.json"

# 本ディレクトリ配下の全てのディレクトリをatcoder-cliの設定フォルダ配下へリンクする
for item in `ls -d */ | cut -d' ' -f 1`; do
	ln -nsf "${CUSTOM_SETTINGS_DIRECTORY}/${item%/}"		"${APP_SETTINGS_DIRECTORY}/${item%/}"
done
