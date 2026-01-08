#!/usr/bin/env python3
"""Test file for Python LSP"""

def test_function(arg1: int, arg2: int) -> int:
    """Test function that adds two arguments.

    Args:
        arg1: The first number
        arg2: The second number

    Returns:
        The sum of arg1 and arg2
    """
    return arg1 + arg2

test_variable = 42

def another_test_function() -> int:
    """Call test_function with some values."""
    return test_function(10, 20)

if __name__ == "__main__":
    result = another_test_function()
    print(f"Result: {result}")
