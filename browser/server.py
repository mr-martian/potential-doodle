from http.server import HTTPServer, CGIHTTPRequestHandler
def quit():
    #from https://stackoverflow.com/questions/19040055/how-do-i-shutdown-an-httpserver-from-inside-a-request-handler-in-python/19040484
    import threading
    assassin = threading.Thread(target=httpd.shutdown)
    assassin.daemon = True
    assassin.start()
class exit_handler(CGIHTTPRequestHandler):
    def do_GET(self):
        CGIHTTPRequestHandler.do_GET(self)
        if self.path == '/quit.html':
            quit()
httpd = HTTPServer(('', 8000), exit_handler)
httpd.serve_forever()
