# Elisp Verification Skill

**Purpose**: Automatically verify and evaluate Emacs Lisp code using the elisp-eval tool

**When to use**: After writing or modifying any `.el` file, proactively verify the code works correctly

## Automatic Verification Workflow

When you write or modify Emacs Lisp code (`.el` files), you MUST follow this workflow:

### 1. After Writing Code

Immediately after writing or modifying any `.el` file:

```bash
# Verify syntax (check for errors)
/Users/stefansevelda/bin/elisp-eval lint . path/to/file.el
```

If there are errors, fix them before proceeding.

### 2. Verify Functions Load

After ensuring syntax is correct, verify the file loads without errors:

```bash
# This will load the file and report any runtime errors
echo "(load-file \"$(pwd)/path/to/file.el\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

### 3. Test Key Functions

For any new or modified functions, test them with sample inputs:

```bash
# Example: Test a function you just wrote
echo "(your-function-name test-arg1 test-arg2)" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

### 4. Verify Documentation

Check that function documentation is accessible:

```bash
# Get function documentation
/Users/stefansevelda/bin/elisp-eval describe . your-function-name
```

## Common Operations

### Lint a File
```bash
/Users/stefansevelda/bin/elisp-eval lint . autoload/claude-multi-status.el
```

**When to use**: Always after editing any `.el` file

**What it checks**:
- Syntax errors
- Undefined functions
- Style warnings
- Missing docstrings

### Evaluate Expression
```bash
echo "(+ 1 2 3)" | /Users/stefansevelda/bin/elisp-eval eval .
```

**When to use**:
- Test a function with specific arguments
- Verify a calculation
- Check variable values
- Test conditional logic

### Describe Symbol
```bash
/Users/stefansevelda/bin/elisp-eval describe . mapcar
```

**When to use**:
- Check function signature
- Read documentation
- Find where a function is defined
- Understand function behavior

### Find Definition
```bash
/Users/stefansevelda/bin/elisp-eval find-definition . claude-multi--normalize-path
```

**When to use**:
- Locate function implementation
- Find variable definition
- Navigate to source code

### List Symbols
```bash
/Users/stefansevelda/bin/elisp-eval list-symbols . config.el
```

**When to use**:
- Get overview of functions in a file
- See all variables defined
- Review public API
- Find function signatures

## Project-Aware Context

The elisp-eval tool automatically loads the claude-multi-agent.el project:
- All autoload files from `autoload/` directory
- Config from `config.el`
- Dependencies from `.packages/` directory

This means you can test project-specific functions directly:

```bash
echo "(claude-multi--normalize-path \"/some/path/\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

## Verification Checklist

After modifying any `.el` file, verify:

- [ ] **Syntax**: `elisp-eval lint . file.el` reports no errors
- [ ] **Load**: File loads without errors
- [ ] **Functions**: New/modified functions work with test inputs
- [ ] **Documentation**: All public functions have docstrings
- [ ] **Integration**: Functions work with rest of project

## Examples

### Example 1: After Adding a New Function

```bash
# 1. Lint the file
/Users/stefansevelda/bin/elisp-eval lint . autoload/claude-multi-status.el

# 2. Verify it loads
echo "(load-file \"$(pwd)/autoload/claude-multi-status.el\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .

# 3. Test the new function
echo "(claude-multi--normalize-path \"/test/path/\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .

# 4. Verify documentation
/Users/stefansevelda/bin/elisp-eval describe . claude-multi--normalize-path
```

### Example 2: After Fixing a Bug

```bash
# 1. Lint to ensure no new errors
/Users/stefansevelda/bin/elisp-eval lint . autoload/claude-multi-agents.el

# 2. Test the specific scenario that was broken
echo "(let ((agent (make-claude-agent :id \"test\" :working-directory \"/path\")))
        (claude-multi--register-agent-for-status agent)
        t)" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

### Example 3: Verifying Complex Logic

```bash
# Test a complex function with edge cases
echo "(let ((path1 \"/path/with/slash/\")
           (path2 \"/path/with/slash\"))
        (string= (claude-multi--normalize-path path1)
                 (claude-multi--normalize-path path2)))" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

## Error Handling

### If Lint Fails

```bash
/Users/stefansevelda/bin/elisp-eval lint . file.el
```

Output shows:
```json
{
  "status":"success",
  "errors":[
    {"line":42,"message":"Missing docstring","severity":"warning"}
  ]
}
```

**Action**: Fix all errors before proceeding with other changes.

### If Evaluation Fails

```bash
echo "(broken-function)" | /Users/stefansevelda/bin/elisp-eval eval .
```

Output shows:
```json
{
  "status":"error",
  "message":"Symbol's function definition is void: broken-function"
}
```

**Action**:
1. Check if function is defined
2. Verify file is loaded
3. Check for typos in function name

### If Load Fails

```bash
echo "(load-file \"file.el\")" | /Users/stefansevelda/bin/elisp-eval eval .
```

Output shows error with line number and problem description.

**Action**: Fix syntax errors at the reported line.

## Integration with Development Workflow

### Before Creating a Commit

```bash
# Verify all modified .el files
for file in $(git diff --name-only | grep '\.el$'); do
  echo "Checking $file..."
  /Users/stefansevelda/bin/elisp-eval lint . "$file"
done
```

### Before Opening a PR

```bash
# Full verification suite
echo "=== Linting all .el files ==="
find . -name "*.el" -not -path "./.packages/*" -exec \
  /Users/stefansevelda/bin/elisp-eval lint . {} \;

echo "=== Loading main config ==="
echo "(load-file \"$(pwd)/config.el\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

### After Rebasing/Merging

```bash
# Verify project still loads correctly
echo "(progn
        (load-file \"$(pwd)/config.el\")
        \"✓ Project loads successfully\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

## Best Practices

1. **Always lint first** - Catch syntax errors before testing functionality
2. **Test with real data** - Use actual paths, agent structs, etc. from your system
3. **Verify edge cases** - Test with nil, empty strings, trailing slashes, etc.
4. **Check integration** - Ensure new code works with existing functions
5. **Document as you go** - Use `describe` to verify docstrings are complete

## Performance Notes

- Each evaluation is a fresh Emacs process (~0.3-0.5s startup)
- Project files are loaded automatically
- No persistent state between invocations
- Fast enough for interactive development

## Limitations

- Cannot test interactive functions (those requiring user input)
- Cannot test functions that manipulate Emacs UI
- Each test is independent (no shared state)
- Batch mode restrictions apply

## Related Documentation

- `.claude/ELISP-EVAL-TOOL.md` - Complete tool documentation
- `.claude/QUICK-ELISP-GUIDE.md` - Quick reference for Elisp syntax
- `.claude/ELISP-LSP-INVESTIGATION.md` - Why we use this instead of LSP

## Summary

**Always use this tool when working with `.el` files. It's your safety net for catching errors early.**

Key commands to remember:
- `elisp-eval lint . file.el` - Check syntax
- `echo "(expr)" | elisp-eval eval .` - Test code
- `elisp-eval describe . symbol` - Get documentation
