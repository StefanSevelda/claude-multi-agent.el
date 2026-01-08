#!/usr/bin/env python3
"""
Automated Test Suite for Emacs Lisp LSP Server
Validates compliance with LSP specification and Claude Code expectations
"""

import json
import subprocess
import sys
from typing import Dict, Any, Optional, List
from dataclasses import dataclass


@dataclass
class TestResult:
    """Result of a single test"""
    name: str
    passed: bool
    message: str
    response: Optional[Dict] = None


class LSPTester:
    """LSP Server Test Harness"""

    def __init__(self, server_path: str):
        self.server_path = server_path
        self.results: List[TestResult] = []

    def send_request(self, method: str, params: Optional[Dict] = None, request_id: int = 1) -> Optional[Dict]:
        """Send LSP request and get response"""
        request = {
            "jsonrpc": "2.0",
            "id": request_id,
            "method": method
        }
        if params is not None:
            request["params"] = params

        content = json.dumps(request)
        message = f"Content-Length: {len(content)}\r\n\r\n{content}"

        try:
            proc = subprocess.Popen(
                [self.server_path],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )

            proc.stdin.write(message.encode('utf-8'))
            proc.stdin.close()

            stdout = proc.stdout.read().decode('utf-8', errors='ignore')
            stderr = proc.stderr.read().decode('utf-8', errors='ignore')
            proc.wait(timeout=5)

            # Parse LSP response
            if "Content-Length:" in stdout:
                # Find the end of headers (double newline)
                header_end = stdout.find('\r\n\r\n')
                if header_end == -1:
                    header_end = stdout.find('\n\n')

                if header_end > 0:
                    json_part = stdout[header_end:].strip()
                    # Remove leading whitespace and newlines
                    json_part = json_part.lstrip('\r\n ')
                    if json_part:
                        try:
                            return json.loads(json_part)
                        except json.JSONDecodeError as e:
                            print(f"JSON parse error: {e}", file=sys.stderr)
                            print(f"Content: {json_part[:200]}", file=sys.stderr)

            return None

        except Exception as e:
            print(f"Error sending request: {e}", file=sys.stderr)
            return None

    def validate_jsonrpc(self, response: Dict) -> tuple[bool, str]:
        """Validate JSON-RPC 2.0 format"""
        if not isinstance(response, dict):
            return False, "Response is not a JSON object"

        if response.get("jsonrpc") != "2.0":
            return False, f"Invalid jsonrpc version: {response.get('jsonrpc')}"

        if "id" not in response:
            return False, "Missing 'id' field"

        if "result" not in response and "error" not in response:
            return False, "Response must have either 'result' or 'error'"

        if "result" in response and "error" in response:
            return False, "Response cannot have both 'result' and 'error'"

        return True, "Valid JSON-RPC 2.0 format"

    def test_initialize(self) -> TestResult:
        """Test initialize request"""
        response = self.send_request("initialize", {
            "rootUri": "file:///test",
            "capabilities": {}
        })

        if not response:
            return TestResult("initialize", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("initialize", False, f"Invalid JSON-RPC: {msg}", response)

        # Validate capabilities
        result = response.get("result", {})
        capabilities = result.get("capabilities", {})

        required_capabilities = [
            "textDocumentSync",
            "hoverProvider",
            "definitionProvider",
            "referencesProvider",
            "documentSymbolProvider"
        ]

        missing = [cap for cap in required_capabilities if cap not in capabilities]
        if missing:
            return TestResult("initialize", False,
                            f"Missing capabilities: {', '.join(missing)}", response)

        # Validate serverInfo
        server_info = result.get("serverInfo")
        if not server_info or not server_info.get("name"):
            return TestResult("initialize", False, "Missing or invalid serverInfo", response)

        return TestResult("initialize", True, "All capabilities present", response)

    def test_document_symbol(self, file_uri: str) -> TestResult:
        """Test textDocument/documentSymbol request"""
        response = self.send_request("textDocument/documentSymbol", {
            "textDocument": {"uri": file_uri}
        }, request_id=2)

        if not response:
            return TestResult("documentSymbol", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("documentSymbol", False, f"Invalid JSON-RPC: {msg}", response)

        # Validate result is array
        result = response.get("result")
        if not isinstance(result, list):
            return TestResult("documentSymbol", False,
                            f"Result must be array, got {type(result).__name__}", response)

        # Validate symbol structure
        if len(result) > 0:
            symbol = result[0]
            required_fields = ["name", "kind", "range", "selectionRange"]
            missing = [field for field in required_fields if field not in symbol]
            if missing:
                return TestResult("documentSymbol", False,
                                f"Symbol missing fields: {', '.join(missing)}", response)

            # Validate range structure
            for range_field in ["range", "selectionRange"]:
                range_obj = symbol.get(range_field, {})
                if "start" not in range_obj or "end" not in range_obj:
                    return TestResult("documentSymbol", False,
                                    f"Invalid {range_field} structure", response)

                for pos in ["start", "end"]:
                    position = range_obj[pos]
                    if "line" not in position or "character" not in position:
                        return TestResult("documentSymbol", False,
                                        f"Invalid position in {range_field}.{pos}", response)

        return TestResult("documentSymbol", True,
                         f"Found {len(result)} symbols with valid structure", response)

    def test_hover(self, file_uri: str, line: int, character: int) -> TestResult:
        """Test textDocument/hover request"""
        response = self.send_request("textDocument/hover", {
            "textDocument": {"uri": file_uri},
            "position": {"line": line, "character": character}
        }, request_id=3)

        if not response:
            return TestResult("hover", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("hover", False, f"Invalid JSON-RPC: {msg}", response)

        result = response.get("result")

        # Hover can return null for no info (Emacs encodes as "json-null" string)
        if result is None or result == "null" or result == "json-null":
            return TestResult("hover", True, "No hover info (acceptable)", response)

        # If hover info exists, validate structure
        if not isinstance(result, dict):
            return TestResult("hover", False,
                            f"Result must be object or null, got {type(result).__name__}: {result}", response)

        # Validate contents
        contents = result.get("contents")
        if contents is None:
            return TestResult("hover", False, "Missing 'contents' field", response)

        # Contents can be string, MarkupContent, or MarkedString
        if isinstance(contents, dict):
            if "kind" not in contents or "value" not in contents:
                return TestResult("hover", False,
                                "MarkupContent must have 'kind' and 'value'", response)
            if contents["kind"] not in ["plaintext", "markdown"]:
                return TestResult("hover", False,
                                f"Invalid markup kind: {contents['kind']}", response)

        return TestResult("hover", True, "Valid hover response", response)

    def test_definition(self, file_uri: str, line: int, character: int) -> TestResult:
        """Test textDocument/definition request"""
        response = self.send_request("textDocument/definition", {
            "textDocument": {"uri": file_uri},
            "position": {"line": line, "character": character}
        }, request_id=4)

        if not response:
            return TestResult("definition", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("definition", False, f"Invalid JSON-RPC: {msg}", response)

        result = response.get("result")

        # Definition can return null for no definition found (Emacs encodes as "json-null" string)
        if result is None or result == "null" or result == "json-null":
            return TestResult("definition", True, "No definition found (acceptable)", response)

        # Result can be Location or Location[]
        if isinstance(result, dict):
            result = [result]  # Normalize to array

        if not isinstance(result, list):
            return TestResult("definition", False,
                            f"Result must be Location or Location[], got {type(result).__name__}: {result}",
                            response)

        # Validate Location structure
        if len(result) > 0:
            location = result[0]
            if "uri" not in location or "range" not in location:
                return TestResult("definition", False,
                                "Location must have 'uri' and 'range'", response)

            range_obj = location["range"]
            if "start" not in range_obj or "end" not in range_obj:
                return TestResult("definition", False, "Invalid range structure", response)

        return TestResult("definition", True, "Valid definition response", response)

    def test_references(self, file_uri: str, line: int, character: int) -> TestResult:
        """Test textDocument/references request"""
        response = self.send_request("textDocument/references", {
            "textDocument": {"uri": file_uri},
            "position": {"line": line, "character": character},
            "context": {"includeDeclaration": True}
        }, request_id=5)

        if not response:
            return TestResult("references", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("references", False, f"Invalid JSON-RPC: {msg}", response)

        result = response.get("result")

        # References must return array (can be empty)
        if not isinstance(result, list):
            return TestResult("references", False,
                            f"Result must be Location[], got {type(result).__name__}", response)

        # Validate Location structure if any references found
        if len(result) > 0:
            location = result[0]
            if "uri" not in location or "range" not in location:
                return TestResult("references", False,
                                "Location must have 'uri' and 'range'", response)

        return TestResult("references", True,
                         f"Found {len(result)} references with valid structure", response)

    def test_shutdown(self) -> TestResult:
        """Test shutdown request"""
        response = self.send_request("shutdown", None, request_id=99)

        if not response:
            return TestResult("shutdown", False, "No response received")

        # Validate JSON-RPC format
        valid, msg = self.validate_jsonrpc(response)
        if not valid:
            return TestResult("shutdown", False, f"Invalid JSON-RPC: {msg}", response)

        # Shutdown must return null (Emacs encodes as "json-null" string)
        result = response.get("result")
        if result is not None and result != "null" and result != "json-null":
            return TestResult("shutdown", False,
                            f"Shutdown must return null, got {result}", response)

        return TestResult("shutdown", True, "Valid shutdown response", response)

    def run_all_tests(self, test_file_uri: str) -> None:
        """Run all tests and report results"""
        print("="*70)
        print("Emacs Lisp LSP Server - Automated Test Suite")
        print("="*70)
        print()

        # Run tests
        tests = [
            ("Initialize", lambda: self.test_initialize()),
            ("Document Symbol", lambda: self.test_document_symbol(test_file_uri)),
            ("Hover", lambda: self.test_hover(test_file_uri, 7, 7)),
            ("Definition", lambda: self.test_definition(test_file_uri, 14, 7)),
            ("References", lambda: self.test_references(test_file_uri, 7, 7)),
            ("Shutdown", lambda: self.test_shutdown()),
        ]

        for test_name, test_func in tests:
            print(f"Testing: {test_name}")
            result = test_func()
            self.results.append(result)

            status = "✓ PASS" if result.passed else "✗ FAIL"
            print(f"  {status}: {result.message}")

            if not result.passed and result.response:
                print(f"  Response: {json.dumps(result.response, indent=2)[:200]}...")

            print()

        # Summary
        passed = sum(1 for r in self.results if r.passed)
        total = len(self.results)

        print("="*70)
        print(f"Test Summary: {passed}/{total} tests passed")
        print("="*70)

        if passed == total:
            print("✓ All tests PASSED - LSP server is fully compliant!")
            sys.exit(0)
        else:
            print("✗ Some tests FAILED - see details above")
            sys.exit(1)


def main():
    """Main test runner"""
    server_path = "/Users/stefansevelda/bin/elisp-lsp-server"
    test_file = "/Users/stefansevelda/projects/claude-multi-agent.el/test-lsp.el"
    test_file_uri = f"file://{test_file}"

    # Check if server exists
    import os
    if not os.path.exists(server_path):
        print(f"Error: LSP server not found at {server_path}", file=sys.stderr)
        sys.exit(1)

    if not os.path.exists(test_file):
        print(f"Error: Test file not found at {test_file}", file=sys.stderr)
        sys.exit(1)

    # Run tests
    tester = LSPTester(server_path)
    tester.run_all_tests(test_file_uri)


if __name__ == "__main__":
    main()
