import sys, re, subprocess
from gentext import *
f = open(sys.argv[2])
s = ''
for c in f.read():
    if c.lower() in ' abcdefghijklmnopqrstuvwxyz':
        s += c
f.close()
proc = subprocess.Popen(['hfst-lookup', 'langs/%s/.generated/parse.hfst' % sys.argv[1]], stdin=subprocess.PIPE, stdout=subprocess.PIPE, universal_newlines=True)
ls = proc.communicate('\n'.join(s.split()))
tags = ls[0]
print(tags)
langid = int(sys.argv[1])
lang = loadlang(langid)
w = []
for ntype in AllMorphemes[langid]:
    for root in AllMorphemes[langid][ntype]:
        m = AllMorphemes[langid][ntype][root]
        try:
            if re.search(m.tagify(True), tags):
                w.append(m)
        except:
            print(m.tagify(True))
f = open(sys.argv[3], 'w')
check = ' '.join(s.lower().split())
for x in makeall(w):
    if dolinear(movement1(x)).lower() == check:
        f.write(str(x) + '\n\n')
f.close()
