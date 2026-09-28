"""Tiny fake model server speaking the Ollama and OpenAI-compatible chat APIs (test fixture)."""
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

KEY = sys.argv[2] if len(sys.argv) > 2 else ""


def answer(prompt):
    if prompt.startswith("Reply with the word OK."):
        return "OK"
    if "You are the AUDITOR" in prompt:
        return '{"police_service": "Barrie Police Service", "number_of_subject_officers": 1}'
    return ('{"police_service": {"value": "Barrie", "quote": "Barrie Police", "confidence": "high"},'
            ' "number_of_subject_officers": {"value": 1, "quote": "SO", "confidence": "high"}}')


class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def _auth(self):
        if KEY and self.headers.get("Authorization") != "Bearer " + KEY:
            self.send_response(401)
            self.end_headers()
            return False
        return True

    def _send(self, obj):
        b = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def do_GET(self):
        if not self._auth():
            return
        if self.path == "/api/tags":
            self._send({"models": [{"name": "fake-ollama:1b"}]})
        elif self.path == "/v1/models":
            self._send({"object": "list", "data": [{"id": "fake-openai-7b"}]})
        else:
            self.send_response(404)
            self.end_headers()

    def do_POST(self):
        if not self._auth():
            return
        req = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        text = answer(req["messages"][0]["content"])
        if self.path == "/api/chat":
            self._send({"model": req["model"], "message": {"role": "assistant", "content": text}, "done": True})
        elif self.path == "/v1/chat/completions":
            self._send({"id": "x", "choices": [{"index": 0, "message": {"role": "assistant", "content": text}}]})
        else:
            self.send_response(404)
            self.end_headers()


HTTPServer(("127.0.0.1", int(sys.argv[1])), H).serve_forever()
