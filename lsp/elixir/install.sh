#!/bin/sh
rm -rf *.ez debugger.sh debugger.bat launch.sh language_server.sh language_server.bat
wget https://github.com/elixir-lsp/elixir-ls/releases/download/v0.14.3/elixir-ls.zip
unzip elixir-ls.zip
rm -f elixir-ls.zip
