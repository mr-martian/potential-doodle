import sys
from doodle import ParseLine
from os.path import isfile
lex = ParseLine.fromfile('langs/%s/lexicon.txt' % sys.argv[1])
transfile = 'langs/%s/translate/%s.txt' % (sys.argv[1], sys.argv[2])
if isfile(transfile):
    trans = ParseLine.fromfile(transfile)
else:
    trans = []
already = [x.label.split('=') for x in trans]
add = []
for r in lex:
    if [r.arg, r.label] not in already:
        add.append('%s=%s: ~' % (r.arg, r.label))
add.sort()
f = open(transfile, 'a')
f.write('\n\n#Generated by lextotrans.py\n' + '\n'.join(add))
f.close()
