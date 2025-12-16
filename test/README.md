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

- `test-vterm-integration.el` - Tests for `vterm` buffer management and process integration
- `run-tests.el` - Test runner script that loads all test files

## Test Coverage

### `vterm` Integration Tests

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
