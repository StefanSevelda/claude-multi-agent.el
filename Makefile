.PHONY: all test compile clean install install-test-deps help

# Emacs command
EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Source files
EL_FILES = config.el $(wildcard autoload/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

# Test files and dependencies
TEST_FILES = $(wildcard test/*-test.el)
DEPS_DIR = .test-deps
BUTTERCUP_URL = https://github.com/jorgenschaefer/emacs-buttercup.git
BUTTERCUP_DIR = $(DEPS_DIR)/buttercup
DASH_URL = https://github.com/magnars/dash.el.git
DASH_DIR = $(DEPS_DIR)/dash.el
S_URL = https://github.com/magnars/s.el.git
S_DIR = $(DEPS_DIR)/s.el
F_URL = https://github.com/rejeep/f.el.git
F_DIR = $(DEPS_DIR)/f.el

help:
	@echo "Available targets:"
	@echo "  make compile           - Byte-compile all .el files"
	@echo "  make install-test-deps - Install test dependencies (Buttercup)"
	@echo "  make test              - Run all tests (installs deps if needed)"
	@echo "  make clean             - Remove compiled files and test dependencies"
	@echo "  make install           - Install via symlink to Doom modules"
	@echo "  make uninstall         - Remove symlink from Doom modules"
	@echo "  make all               - Compile and test"

all: compile test

compile: $(ELC_FILES)

%.elc: %.el
	@echo "Compiling $<..."
	@$(BATCH) -f batch-byte-compile $<

$(BUTTERCUP_DIR):
	@echo "Installing Buttercup test framework..."
	@mkdir -p $(DEPS_DIR)
	@git clone --depth 1 $(BUTTERCUP_URL) $(BUTTERCUP_DIR)
	@echo "✓ Buttercup installed"

$(DASH_DIR):
	@echo "Installing dash.el..."
	@mkdir -p $(DEPS_DIR)
	@git clone --depth 1 $(DASH_URL) $(DASH_DIR)
	@echo "✓ dash.el installed"

$(S_DIR):
	@echo "Installing s.el..."
	@mkdir -p $(DEPS_DIR)
	@git clone --depth 1 $(S_URL) $(S_DIR)
	@echo "✓ s.el installed"

$(F_DIR):
	@echo "Installing f.el..."
	@mkdir -p $(DEPS_DIR)
	@git clone --depth 1 $(F_URL) $(F_DIR)
	@echo "✓ f.el installed"

install-test-deps: $(BUTTERCUP_DIR) $(DASH_DIR) $(S_DIR) $(F_DIR)
	@echo "✓ All test dependencies installed"

test: $(BUTTERCUP_DIR) $(DASH_DIR) $(S_DIR) $(F_DIR)
	@echo "Running tests with Buttercup (60 second timeout)..."
	@timeout 60 $(EMACS) -batch \
		-L . \
		-L autoload \
		-L test \
		-L $(BUTTERCUP_DIR) \
		-L $(DASH_DIR) \
		-L $(S_DIR) \
		-L $(F_DIR) \
		-l buttercup \
		-l test/test-kitty-integration.el \
		-l test/test-progress-visibility.el \
		-f buttercup-run-discover \
	|| (echo "Tests timed out or failed"; exit 1)

clean:
	@echo "Cleaning compiled files and test dependencies..."
	@rm -f $(ELC_FILES)
	@rm -f test/*.elc
	@rm -rf $(DEPS_DIR)
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
