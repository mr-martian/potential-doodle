from datatypes import *
from compilelang import loadlang
from subprocess import Popen
from sys import argv
from collections import defaultdict

def genlexicon(langid):
    lang = loadlang(langid)
    lexs = defaultdict(list)
    for ntype in AllMorphemes[langid]:
        for root in AllMorphemes[langid][ntype]:
            m = AllMorphemes[langid][ntype][root]
            added = False
            for rule in lang.lexc_lexicons:
                if rule['ntype'] != ntype:
                    continue
                ok = True
                for c in rule['conds']:
                    if m.props[c[0]] != c[1]:
                        ok = False
                        break
                if ok:
                    added = True
                    lexs[rule['lexicon-in']].append('%s %s ;' % (m.children[0], rule['lexicon-to']))
    f = open('langs/%d/lexicon.lexc' % langid, 'w')
    for lex in lexs:
        f.write(('LEXICON %s\n' % lex) + '\n'.join(lexs[lex]) + '\n\n')
    f.close()
def buildhfst(langid):
    genlexicon(langid)
    pth = 'langs/%d/' % langid
    proc = Popen(['hfst-lexc', '-v', '-f', 'foma', pth+'morphology.lexc', pth+'lexicon.lexc', '-o', pth+'debug.gen.hfst'])
    proc.wait()
    proc = Popen(['hfst-invert', '-v', pth+'debug.gen.hfst', '-o', pth+'debug.parse.hfst'])
    proc.wait()
    proc = Popen(['hfst-fst2fst', '-v', pth+'debug.parse.hfst', '-f', 'olw', '-o', pth+'parse.hfst'])
    proc.wait()
    proc = Popen(['hfst-fst2fst', '-v', pth+'debug.gen.hfst', '-f', 'olw', '-o', pth+'gen.hfst'])
    proc.wait()
if __name__ == '__main__':
    buildhfst(int(argv[1]))
