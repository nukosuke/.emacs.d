#!/bin/sh
version=v0.15.1
rm -rf *.ez debugger.sh debugger.bat launch.sh language_server.sh language_server.bat
wget https://github.com/elixir-lsp/elixir-ls/releases/download/$version/elixir-ls-$version.zip
unzip elixir-ls-$version.zip
rm -f elixir-ls-$version.zip
