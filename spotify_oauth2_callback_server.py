try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
    from urllib.parse import urlparse, parse_qs
except ImportError:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
    from urlparse import urlparse, parse_qs

class RequestHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        return

    def do_GET(self):
        print(parse_qs(urlparse(self.path).query)["code"][0])

        self.send_response(200)
        self.send_header('Content-type','text/html')
        self.end_headers()

        self.wfile.write("<h1>Success!</h1><p>You may close this window and go back to Emacs now. :-)</p>".encode("utf-8"))

try:
    server = HTTPServer(('', 8591), RequestHandler)
    server.handle_request()
except KeyboardInterrupt:
    server.socket.close()
