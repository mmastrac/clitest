#!/bin/bash
set -e

cd "$(dirname "$0")/.."

mkdir -p ~/.vscode/extensions/functional-tests/
cp highlighting/functional-tests.tmLanguage.json ~/.vscode/extensions/functional-tests/
cp highlighting/package.json ~/.vscode/extensions/functional-tests/
cp highlighting/language-configuration.json ~/.vscode/extensions/functional-tests/

cp -R ~/.vscode/extensions/functional-tests ~/.cursor/extensions/ || echo "No cursor installation found"
cp -R ~/.vscode/extensions/functional-tests ~/.cursor-server/extensions/ || echo "No cursor-server installation found"
cp -R ~/.vscode/extensions/functional-tests ~/.vscode-server/extensions/ || echo "No vscode-server installation found"

echo "Functional Tests language extension installed successfully!"
