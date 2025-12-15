.PHONY: all test compile clean install help

# Emacs command
EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Source files
EL_FILES = config.el $(wildcard autoload/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

# Test files
TEST_FILES = $(wildcard test/*-test.el)

help:
	@echo "Available targets:"
	@echo "  make compile       - Byte-compile all .el files"
	@echo "  make test          - Run all tests"
	@echo "  make clean         - Remove compiled files"
	@echo "  make install       - Install via symlink to Doom modules"
	@echo "  make uninstall     - Remove symlink from Doom modules"
	@echo "  make all           - Compile and test"

all: compile test

compile: $(ELC_FILES)

%.elc: %.el
	@echo "Compiling $<..."
	@$(BATCH) -f batch-byte-compile $<

test: compile
	@echo "Running tests..."
	@$(BATCH) \
		-l test/test-helper.el \
		$(patsubst %,-l %,$(TEST_FILES)) \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(ELC_FILES)
	@rm -f test/*.elc
	@echo "Clean complete."

install:
	@echo "Installing claude-multi-agent.el to Doom Emacs..."
	@mkdir -p ~/.config/emacs/.doom.d/modules/tools
	@if [ -L ~/.config/emacs/.doom.d/modules/tools/claude-multi ]; then \
		echo "Symlink already exists, removing old one..."; \
		rm ~/.config/emacs/.doom.d/modules/tools/claude-multi; \
	fi
	@ln -s $(PWD) ~/.config/emacs/.doom.d/modules/tools/claude-multi
	@echo "Symlink created: ~/.config/emacs/.doom.d/modules/tools/claude-multi"
	@echo ""
	@echo "Next steps:"
	@echo "  1. Add 'claude-multi' to the :tools section in ~/.config/emacs/.doom.d/init.el"
	@echo "  2. Run 'doom sync' to install dependencies"
	@echo "  3. Restart Emacs"

uninstall:
	@echo "Uninstalling claude-multi-agent.el from Doom Emacs..."
	@if [ -L ~/.config/emacs/.doom.d/modules/tools/claude-multi ]; then \
		rm ~/.config/emacs/.doom.d/modules/tools/claude-multi; \
		echo "Symlink removed."; \
	else \
		echo "Symlink not found."; \
	fi
	@echo "Don't forget to remove 'claude-multi' from your init.el and run 'doom sync'"

check-syntax:
	@echo "Checking Emacs Lisp syntax..."
	@$(BATCH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

lint: check-syntax
	@echo "Linting complete."
