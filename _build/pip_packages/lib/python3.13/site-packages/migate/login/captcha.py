import os
import json
import time
import base64
import platform
import threading
import webbrowser
import http.server
import socketserver
from dataclasses import dataclass
from pathlib import Path
from typing import Optional
from urllib.parse import unquote, urlparse, parse_qs

from migate.config import BASE_URL, console
from migate.requester import get, post

_TEMPLATE = (Path(__file__).parent / "captcha.html").read_text()


@dataclass
class _CaptchaState:
    b64:   str
    error: bool          = False
    code:  Optional[str] = None
    done:  bool          = False


def _build_html(b64: str, error: bool = False) -> str:
    msg = "<p class='error'>Incorrect code! Try again.</p>" if error else ""
    return _TEMPLATE.replace("{{B64}}", b64).replace("{{MSG}}", msg)


def handle_captcha(send_url, response, payload, capt_key):
    try:
        response_text = json.loads(response.text[11:])
        cap_url       = BASE_URL + response_text["captchaUrl"]
        state         = _CaptchaState(b64=base64.b64encode(get(cap_url).content).decode())

        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self, *a): pass

            def do_GET(self):
                if self.path == "/":
                    body = _build_html(state.b64, state.error).encode()
                    self.send_response(200)
                    self.send_header("Content-Type", "text/html; charset=utf-8")
                    self.send_header("Content-Length", str(len(body)))
                    self.end_headers()
                    self.wfile.write(body)

                elif self.path.startswith("/submit"):
                    code        = parse_qs(urlparse(self.path).query).get("code", [""])[0]
                    state.code  = unquote(code).strip()
                    start       = time.time()
                    while state.code is not None and time.time() - start < 10:
                        time.sleep(0.1)
                    result = b"ok" if state.done else b"retry"
                    self.send_response(200)
                    self.send_header("Content-Type", "text/plain")
                    self.end_headers()
                    self.wfile.write(result)

        httpd  = socketserver.TCPServer(("127.0.0.1", 0), Handler)
        port   = httpd.server_address[1]
        thread = threading.Thread(target=httpd.serve_forever, daemon=True)
        thread.start()

        url = f"http://127.0.0.1:{port}"
        if platform.system() in ("Linux", "Android"):
            os.system(f"xdg-open '{url}' 2>/dev/null")
        else:
            webbrowser.open(url)
        console.print(f"[white]Captcha opened at: [/][orange]{url}[/]")

        while True:
            while state.code is None:
                time.sleep(0.2)

            payload[capt_key] = state.code

            resp      = post(send_url, data=payload)
            resp_text = json.loads(resp.text[11:])

            if resp_text.get("code") == 87001:
                state.b64   = base64.b64encode(get(cap_url).content).decode()
                state.error = True
                state.code  = None
            else:
                state.done = True
                state.code = None
                break

        httpd.shutdown()
        return resp

    except Exception as e:
        return {"error": f"Captcha handling failed: {str(e)}"}
