#!/bin/bash
set -euxo pipefail

mkdir "day$1"

pushd "day$1"

dotnet new console -lang "F#" 

dotnet new tool-manifest
dotnet tool install --local fantomas-tool

touch test.txt
touch input.txt

mkdir .vscode
popd

cp vscode-settings.json "day$1"/.vscode/settings.json
cp boilerplate.fs "day$1"/Program.fs
