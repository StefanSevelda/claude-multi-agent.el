#!/usr/bin/env python3
"""Simple parenthesis balance checker for Emacs Lisp files."""

import sys
from pathlib import Path

def count_parens(filepath):
    """Count opening and closing parentheses in a file."""
    opens = 0
    closes = 0

    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
        for char in content:
            if char == '(':
                opens += 1
            elif char == ')':
                closes += 1

    return opens, closes

def main():
    root = Path(__file__).parent.parent
    files = (
        [root / "config.el"] +
        list((root / "autoload").glob("*.el"))
    )

    failed = False

    for filepath in sorted(files):
        if not filepath.exists():
            continue

        opens, closes = count_parens(filepath)

        if opens == closes:
            print(f"✓ {filepath.relative_to(root)}: {opens} parens")
        else:
            diff = opens - closes
            print(f"✗ {filepath.relative_to(root)}: {opens} opens, {closes} closes (diff: {diff:+d})")
            failed = True

    return 1 if failed else 0

if __name__ == "__main__":
    sys.exit(main())
