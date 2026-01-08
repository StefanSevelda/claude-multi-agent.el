#!/usr/bin/env python3
"""
Comprehensive LSP Server Testing Tool
Tests both Python and Emacs Lisp LSP servers for comparison
"""

import json
import subprocess
import sys
import time
import signal
from typing import Optional, Dict, Any


class LSPTester:
    """Test LSP server functionality"""

    def __init__(self, server_command: list, name: str, timeout: int = 10):
        self.server_command = server_command
        self.name = name
        self.timeout = timeout
        self.proc = None

    def start_server(self) -> bool:
        """Start the LSP server process"""
        print(f"\n{'='*60}")
        print(f"Testing {self.name} LSP Server")
        print(f"{'='*60}")
        print(f"Command: {' '.join(self.server_command)}")
        print(f"Timeout: {self.timeout} seconds")

        try:
            self.proc = subprocess.Popen(
                self.server_command,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                bufsize=0
            )
            time.sleep(0.5)  # Give server time to start

            # Check if process is still running
            if self.proc.poll() is not None:
                stderr = self.proc.stderr.read().decode('utf-8', errors='ignore')
                print(f"❌ Server failed to start")
                print(f"   stderr: {stderr}")
                return False

            print(f"✓ Server started (PID: {self.proc.pid})")
            return True
        except Exception as e:
            print(f"❌ Failed to start server: {e}")
            return False

    def send_request(self, request_id: int, method: str, params: Optional[Dict] = None) -> bool:
        """Send an LSP request"""
        if not self.proc or self.proc.poll() is not None:
            print(f"   ❌ Server not running")
            return False

        request = {
            "jsonrpc": "2.0",
            "id": request_id,
            "method": method,
        }
        if params is not None:
            request["params"] = params

        content = json.dumps(request)
        message = f"Content-Length: {len(content)}\r\n\r\n{content}"

        print(f"\n→ Sending: {method} (id={request_id})")
        try:
            self.proc.stdin.write(message.encode('utf-8'))
            self.proc.stdin.flush()
            return True
        except Exception as e:
            print(f"   ❌ Failed to send: {e}")
            return False

    def read_response(self, timeout: Optional[int] = None) -> Optional[Dict[str, Any]]:
        """Read an LSP response with timeout"""
        if not self.proc:
            return None

        timeout = timeout or self.timeout

        def timeout_handler(signum, frame):
            raise TimeoutError("Response timeout")

        try:
            # Set timeout alarm
            signal.signal(signal.SIGALRM, timeout_handler)
            signal.alarm(timeout)

            # Read headers
            content_length = 0
            while True:
                line = self.proc.stdout.readline().decode('utf-8', errors='ignore')
                if not line or line.strip() == '':
                    break
                if line.startswith('Content-Length:'):
                    content_length = int(line.split(':')[1].strip())

            if content_length == 0:
                signal.alarm(0)  # Cancel alarm
                return None

            # Read content
            content = self.proc.stdout.read(content_length).decode('utf-8', errors='ignore')
            signal.alarm(0)  # Cancel alarm

            response = json.loads(content)
            print(f"← Received response (id={response.get('id', 'N/A')})")
            return response

        except TimeoutError:
            signal.alarm(0)  # Cancel alarm
            print(f"   ⏱️  No response within {timeout} seconds")
            return None
        except Exception as e:
            signal.alarm(0)  # Cancel alarm
            print(f"   ❌ Error reading response: {e}")
            return None

    def test_initialize(self, root_uri: str) -> bool:
        """Test initialize request"""
        print(f"\n--- Test 1: Initialize ---")

        if not self.send_request(1, "initialize", {
            "rootUri": root_uri,
            "capabilities": {}
        }):
            return False

        response = self.read_response()
        if not response:
            print(f"   ❌ No initialize response")
            return False

        if "error" in response:
            print(f"   ❌ Error: {response['error']}")
            return False

        capabilities = response.get("result", {}).get("capabilities", {})
        print(f"   ✓ Server capabilities: {list(capabilities.keys())}")
        return True

    def test_hover(self, file_uri: str, line: int, char: int) -> bool:
        """Test hover request"""
        print(f"\n--- Test 2: Hover (line {line}, char {char}) ---")

        if not self.send_request(2, "textDocument/hover", {
            "textDocument": {"uri": file_uri},
            "position": {"line": line, "character": char}
        }):
            return False

        response = self.read_response()
        if not response:
            print(f"   ❌ No hover response")
            return False

        if "error" in response:
            print(f"   ❌ Error: {response['error']}")
            return False

        result = response.get("result")
        if result and result != "null":
            hover_text = result.get("contents", {})
            if isinstance(hover_text, dict):
                value = hover_text.get("value", "")
                preview = value[:100] + "..." if len(value) > 100 else value
                print(f"   ✓ Hover info: {preview}")
            else:
                print(f"   ✓ Hover info: {hover_text}")
            return True
        else:
            print(f"   ⚠️  No hover information available")
            return False

    def test_document_symbol(self, file_uri: str) -> bool:
        """Test document symbol request"""
        print(f"\n--- Test 3: Document Symbols ---")

        if not self.send_request(3, "textDocument/documentSymbol", {
            "textDocument": {"uri": file_uri}
        }):
            return False

        response = self.read_response()
        if not response:
            print(f"   ❌ No document symbol response")
            return False

        if "error" in response:
            print(f"   ❌ Error: {response['error']}")
            return False

        symbols = response.get("result", [])
        if symbols and isinstance(symbols, list):
            print(f"   ✓ Found {len(symbols)} symbols:")
            for symbol in symbols[:5]:  # Show first 5
                name = symbol.get("name", "?")
                kind = symbol.get("kind", "?")
                print(f"      - {name} (kind={kind})")
            if len(symbols) > 5:
                print(f"      ... and {len(symbols) - 5} more")
            return True
        elif symbols:
            print(f"   ⚠️  Symbols result is not a list: {type(symbols)}")
            print(f"      Result: {symbols}")
            return False
        else:
            print(f"   ⚠️  No symbols found")
            return False

    def shutdown(self):
        """Shutdown the server"""
        print(f"\n--- Shutdown ---")

        if self.proc and self.proc.poll() is None:
            self.send_request(99, "shutdown")
            time.sleep(0.2)

            # Try graceful exit
            if self.proc.poll() is None:
                self.proc.terminate()
                try:
                    self.proc.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    self.proc.kill()
                    self.proc.wait()

            print(f"   ✓ Server stopped")

        # Print stderr if any
        if self.proc:
            stderr = self.proc.stderr.read().decode('utf-8', errors='ignore')
            if stderr.strip():
                print(f"\n--- Server stderr ---")
                for line in stderr.strip().split('\n')[:10]:  # First 10 lines
                    print(f"   {line}")

    def run_tests(self, root_uri: str, file_uri: str, hover_line: int, hover_char: int):
        """Run all tests"""
        if not self.start_server():
            return False

        try:
            init_ok = self.test_initialize(root_uri)
            if not init_ok:
                return False

            hover_ok = self.test_hover(file_uri, hover_line, hover_char)
            symbol_ok = self.test_document_symbol(file_uri)

            print(f"\n{'='*60}")
            print(f"Summary for {self.name}:")
            print(f"  Initialize: {'✓' if init_ok else '❌'}")
            print(f"  Hover:      {'✓' if hover_ok else '⚠️'}")
            print(f"  Symbols:    {'✓' if symbol_ok else '⚠️'}")
            print(f"{'='*60}")

            return init_ok

        finally:
            self.shutdown()


def main():
    project_root = "/Users/stefansevelda/projects/claude-multi-agent.el"

    # Test 1: Python LSP (pyright)
    print("\n" + "="*60)
    print("PART 1: Python LSP Server Test")
    print("="*60)

    python_tester = LSPTester(
        server_command=["pyright-langserver", "--stdio"],
        name="Python (pyright)",
        timeout=10
    )

    python_tester.run_tests(
        root_uri=f"file://{project_root}",
        file_uri=f"file://{project_root}/test_python_lsp.py",
        hover_line=4,  # def test_function
        hover_char=5   # on "test_function"
    )

    # Test 2: Emacs Lisp LSP
    print("\n\n" + "="*60)
    print("PART 2: Emacs Lisp LSP Server Test")
    print("="*60)

    elisp_tester = LSPTester(
        server_command=["/Users/stefansevelda/bin/elisp-lsp-server"],
        name="Emacs Lisp",
        timeout=10
    )

    elisp_tester.run_tests(
        root_uri=f"file://{project_root}",
        file_uri=f"file://{project_root}/test-lsp.el",
        hover_line=7,   # def test-function
        hover_char=7    # on "test-function"
    )

    print("\n" + "="*60)
    print("Testing Complete")
    print("="*60)


if __name__ == "__main__":
    main()
