"""
Example usage of the Structured Logger

Run this file to see the structured logging in action:
    python -m python.structured_logging.example

Or from this directory:
    python example.py
"""

from logger import logger


def main():
    """Demonstrate the structured logger with various examples."""

    print("=== Example 1: Basic usage ===")
    user = "John"
    balance = 42
    logger.log(f"Hello {user}, your balance is {balance}")

    print("\n=== Example 2: Multiple variables of different types ===")
    name = "Alice"
    age = 30
    score = 95.5
    active = True
    logger.log(f"User {name} is {age} years old with score {score} (active: {active})")

    print("\n=== Example 3: Expressions in f-strings ===")
    x = 10
    y = 5
    logger.log(f"The sum of {x} and {y} is {x + y}")

    print("\n=== Example 4: String formatting ===")
    price = 19.99
    item = "widget"
    logger.log(f"The {item} costs ${price}")

    print("\n=== Example 5: Complex objects ===")
    data = {"key": "value", "count": 123}
    items = [1, 2, 3]
    logger.log(f"Data: {data}, Items: {items}")


if __name__ == "__main__":
    main()
