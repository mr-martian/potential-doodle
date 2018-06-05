#!/usr/bin/env python3
print('Content-type:text/html\n\n<html><head><title>test</title></head>\n\n<body>')
import sys, os
sys.path.append(os.path.abspath('..'))
from compilelang import loadlang
loadlang(1)
from datatypes import Language
from gentext import make, out
print(out(make(Language.getormake(1))))
print(sys.argv)
#https://stackoverflow.com/questions/3582398/getting-http-get-arguments-in-python
import cgi
print(cgi.FieldStorage())
print('</body></html>')
