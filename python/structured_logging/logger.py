"""
Structured Logger - Challenge 1: Effortless Structured Logging

This module implements a structured logger that automatically extracts
both the template and variable values from f-strings without compiler plugins.

The logger uses frame introspection to capture the source code at the call site,
then parses the f-string to extract variable names and values.
"""

import inspect
import re
import json
from typing import Any, Dict


class StructuredLogger:
    """
    A logger that extracts template and variable values from f-strings.

    Example:
        logger = StructuredLogger()
        user = "John"
        balance = 42
        logger.log(f"Hello {user}, your balance is {balance}")

        # Outputs:
        # {
        #   "template": "Hello %user%, your balance is %balance%",
        #   "args": {"user": "John", "balance": 42}
        # }
    """

    def log(self, message: str) -> None:
        """
        Log a message with structured template and arguments.

        Args:
            message: The evaluated f-string message
        """
        # Get the caller's frame to extract source code and local variables
        frame = inspect.currentframe()
        assert frame is not None, "Failed to get current frame"

        caller_frame = frame.f_back
        assert caller_frame is not None, "Failed to get caller frame"

        try:
            # Extract the source line
            source_line = self._get_source_line(caller_frame)

            # Extract variable names and values
            template, args = self._extract_template_and_args(
                source_line,
                caller_frame.f_locals,
                caller_frame.f_globals
            )

            # Output structured JSON
            result = {
                "template": template,
                "args": args
            }
            print(json.dumps(result, indent=2))

        finally:
            # Clean up frame reference to avoid reference cycles
            del caller_frame
            del frame

    def _get_source_line(self, frame: Any) -> str:
        """Extract the source code line from the frame."""
        # Get the source code context
        filename = frame.f_code.co_filename
        lineno = frame.f_lineno

        # Try to read the source line
        try:
            with open(filename, 'r') as f:
                lines = f.readlines()
                if 0 <= lineno - 1 < len(lines):
                    return lines[lineno - 1].strip()
        except (IOError, IndexError):
            pass

        # Fallback to inspect if file reading fails
        try:
            source_lines = inspect.getframeinfo(frame).code_context
            if source_lines:
                return source_lines[0].strip()
        except:
            pass

        raise RuntimeError("Failed to extract source line")

    def _extract_template_and_args(
        self,
        source_line: str,
        local_vars: Dict[str, Any],
        global_vars: Dict[str, Any]
    ) -> tuple[str, Dict[str, Any]]:
        """
        Extract template and arguments from an f-string source line.

        Args:
            source_line: The source code line containing the f-string
            local_vars: Local variables from the caller's scope
            global_vars: Global variables from the caller's scope

        Returns:
            A tuple of (template, args_dict)
        """
        # Extract the f-string from the log() call
        # Pattern: logger.log(f"...") or log(f"...")
        fstring_match = re.search(r'\.log\s*\(\s*f["\'](.+?)["\']\s*\)', source_line)
        if not fstring_match:
            # Try without method call
            fstring_match = re.search(r'log\s*\(\s*f["\'](.+?)["\']\s*\)', source_line)

        if not fstring_match:
            raise ValueError(f"Could not find f-string in: {source_line}")

        fstring_content = fstring_match.group(1)

        # Find all variable expressions in the f-string
        # Pattern: {variable} or {expression}
        var_pattern = re.compile(r'\{([^}]+)\}')
        var_names = var_pattern.findall(fstring_content)

        # Build template by replacing {var} with %var%
        template = fstring_content
        for var_expr in var_names:
            # For simple variables, use the variable name
            # For expressions, use the full expression
            var_name = var_expr.strip()
            template = template.replace(f'{{{var_expr}}}', f'%{var_name}%', 1)

        # Evaluate variables and collect their values
        args = {}
        all_vars = {**global_vars, **local_vars}

        for var_expr in var_names:
            var_name = var_expr.strip()
            try:
                # Evaluate the expression in the caller's context
                value = eval(var_name, global_vars, local_vars)
                args[var_name] = value
            except:
                # If evaluation fails, try to get it directly from locals/globals
                args[var_name] = all_vars.get(var_name, f"<unable to resolve: {var_name}>")

        return template, args


# Global logger instance
logger = StructuredLogger()
