ELIXIR_LSP_VERSION := v0.17.10

.PHONY: elixir clean

elixir:
	mkdir -p elixir && cd elixir \
		&& wget https://github.com/elixir-lsp/elixir-ls/releases/download/$(ELIXIR_LSP_VERSION)/elixir-ls-$(ELIXIR_LSP_VERSION).zip \
		&& unzip elixir-ls-$(ELIXIR_LSP_VERSION).zip

clean:
	rm -rf elixir
