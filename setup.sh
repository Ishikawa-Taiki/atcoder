#!/bin/sh

# Add npm global bin to PATH
export PATH="$(npm root -g)/bin:$PATH"

# ユーザー設定
CUSTOM_SETTINGS_DIRECTORY=$PWD/atcoder-cli
APP_SETTINGS_DIRECTORY=$(acc config-dir)

# Check if acc config-dir succeeded
if [ -z "$APP_SETTINGS_DIRECTORY" ]; then
    echo "Error: Failed to get atcoder-cli config directory. Is atcoder-cli installed correctly?"
    exit 1
fi

# 本ディレクトリ配下の全てのディレクトリをatcoder-cliの設定フォルダ配下へリンクする
for item in $(ls -d */ | cut -d' ' -f 1); do
	ln -nsf "${CUSTOM_SETTINGS_DIRECTORY}/${item%/}"		"${APP_SETTINGS_DIRECTORY}/${item%/}"
done

# 共通の設定ファイル
# 環境固有の情報を持っているので、atcoder-cliの設定フォルダ配下へ直接リンクするのではなく、個別で値設定を行う
# ln -sf "$CUSTOM_SETTINGS_DIRECTORY/config.json"		"$APP_SETTINGS_DIRECTORY/config.json"
acc config default-test-dirname-format test

# Source ghcup environment for bash and zsh
GHCUP_ENV_LINE='[ -f /home/vscode/.ghcup/env ] && source /home/vscode/.ghcup/env'
grep -qF -- "$GHCUP_ENV_LINE" ~/.bashrc || echo "$GHCUP_ENV_LINE" >> ~/.bashrc
grep -qF -- "$GHCUP_ENV_LINE" ~/.zshrc || echo "$GHCUP_ENV_LINE" >> ~/.zshrc

npm install
