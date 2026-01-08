.PHONY: test-lsp smoke-test help

help:
	@echo "Emacs LSP Server - Test Targets"
	@echo ""
	@echo "  make -f Makefile.lsp test-lsp      Run automated LSP server tests"
	@echo "  make -f Makefile.lsp smoke-test    Quick smoke test"
	@echo "  make -f Makefile.lsp help          Show this help message"
	@echo ""

# Run comprehensive automated tests
test-lsp:
	@echo "Running LSP server automated tests..."
	@python3 test_elisp_lsp_automated.py

# Quick smoke test
smoke-test:
	@echo "Running smoke test..."
	@JSON='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file:///test","capabilities":{}}}' && \
	LEN=$$(printf "%s" "$$JSON" | wc -c | tr -d ' ') && \
	RESULT=$$(printf "Content-Length: %s\r\n\r\n%s" "$$LEN" "$$JSON" | /Users/stefansevelda/bin/elisp-lsp-server 2>/dev/null | tail -1) && \
	echo "$$RESULT" | jq -r '.result.serverInfo.name' && \
	echo "✓ Smoke test passed!"
