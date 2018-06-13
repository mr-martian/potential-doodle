#!/usr/bin/env python3
print('Content-type:text/html\n\n<html><head><title>YOU SENT IN</title></head>\n\n<body>')
import cgitb
cgitb.enable()
import cgi
v = cgi.FieldStorage()
print('<h1>Name</h1><p>%s</p><h1>Language</h1><p>%s</p><h1>Sentence</h1><p>' % (v.getfirst('name'), v.getfirst('lang')))
def neat(d):
  if 'root' in d:
      return '%s=%s' % (d['ntype'], d['root'])
  else:
      p = []
      for x in ['ntype', 'spec', 'mod', 'head', 'comp']:
          if x not in d:
              pass
          elif isinstance(d[x], dict):
              n = neat(d[x])
              if n != '|[]':
                  p.append(n)
          elif d[x]:
              p.append(d[x])
          else:
              pass
      return '|[%s]' % ' '.join(p)
import json
print(neat(json.loads(v.getfirst('json'))))
print('</p></body></html>')
