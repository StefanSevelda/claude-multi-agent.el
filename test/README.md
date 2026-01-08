# Claude Multi-Agent Tests

This directory contains unit tests for the claude-multi-agent plugin using the Buttercup testing framework.

## Running Tests

### From Command Line

```bash
# Run all tests
emacs -batch -l test/run-tests.el

# Run specific test file
emacs -batch -l test/test-vterm-integration.el -f buttercup-run
```

### From Emacs

```elisp
;; Load and run all tests
(load-file "test/run-tests.el")

;; Or run specific test file
(load-file "test/test-vterm-integration.el")
(buttercup-run)
```

### Using Make (if `Makefile` exists)

```bash
make test
```

## Test Files

- `test-kitty-integration.el` - Tests for kitty terminal integration
- `test-progress-visibility.el` - Tests for STATUS drawer visibility features
- `run-tests.el` - Test runner script that loads all test files

## Test Coverage

### Kitty Integration Tests

1. **Window Management**
   - Verifies `claude-multi--launch-agent` creates kitty windows correctly
   - Tests kitty remote control API integration
   - Verifies window ID tracking

2. **Command Sending**
   - Tests `claude-multi--send-to-kitty` for sending initial commands
   - Verifies proper command escaping

3. **Status Monitoring**
   - Tests `claude-multi--kitty-is-alive` for window existence checks
   - Verifies status monitoring timer setup
   - Tests agent completion detection

4. **Agent Cleanup**
   - Tests `claude-multi--kill-agent` for proper resource cleanup
   - Verifies kitty window closing
   - Tests timer cancellation

### Progress Visibility Tests

1. **STATUS Drawer Creation**
   - Verifies `claude-multi--add-agent-section` creates visible drawers
   - Tests drawer structure (`:STATUS:`, marker, `:END:`)
   - Verifies agent ID color formatting in headlines
   - Tests status icon display

2. **Status Updates**
   - Tests `claude-multi--update-agent-status-display` for content updates
   - Verifies auto-expansion when waiting for input
   - Tests handling of missing status.json files
   - Verifies parsed JSON content insertion

3. **Toggle Commands**
   - Tests `claude-multi/show-all-status-drawers` for showing all drawers
   - Tests `claude-multi/hide-all-status-drawers` for hiding all drawers
   - Tests `claude-multi/toggle-all-status-drawers` for toggling visibility
   - Verifies graceful handling of missing progress buffer

4. **Status File Watching**
   - Tests `claude-multi--watch-agent-status-file` for file watch setup
   - Verifies directory watching when status.json doesn't exist
   - Tests watch upgrade from directory to file

### Legacy `vterm` Integration Tests

1. **Command Sending**
   - Verifies `claude-multi--send-to-vterm` correctly sends commands
   - Tests `vterm-send-string` and `vterm-send-return` integration

2. **Process Monitoring**
   - Tests `claude-multi--setup-vterm-output-monitor` filter setup
   - Verifies process filter is correctly attached to `vterm` buffers

3. **Output Processing**
   - Tests `claude-multi--process-vterm-output` for various output types
   - Verifies detection of input requests, completion, and errors
   - Checks agent status updates

4. **Agent Lifecycle**
   - Tests `vterm` buffer creation during agent launch
   - Verifies correct buffer naming (`*vterm-agent-name*`)
   - Tests process and buffer cleanup during agent termination

5. **User Interaction**
   - Tests input sending to waiting agents
   - Verifies status transitions from waiting-input to running

## Writing New Tests

Follow the Buttercup Behavior-Driven Development (BDD) style:

```elisp
(describe "Feature name"
  (it "does something specific"
    (expect (some-function) :to-equal expected-value)))
```

### Mocking Functions

Use `cl-letf` to mock functions:

```elisp
(cl-letf (((symbol-function 'function-to-mock)
           (lambda (args) mock-return-value)))
  ;; Test code here
  )
```

### Cleaning Up

Always clean up buffers and processes after tests:

```elisp
(kill-buffer test-buffer)
(delete-process mock-process)
```

## Dependencies

- **buttercup**: Behavior-Driven Development (BDD) testing framework for Emacs Lisp
- **`vterm`**: Required for integration tests (can be mocked)

## Continuous Integration

Tests should be run before:

- Committing changes
- Creating pull requests
- Merging to main branch

All tests must pass for the build to be considered successful.
