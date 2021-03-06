#!/usr/bin/env python3
from doodle import *
from collections import defaultdict
from re import sub

def lexcescape(s):
    return s.replace('<', '%<').replace('>', '%>').replace(' ', '+')
def genlexicon(langid):
    lang = loadlang(langid)
    lexs = defaultdict(list)
    for m in lang.iterlex():
        for rule in lang.lexc_lexicons:
            if rule['ntype'] != m.ntype:
                continue
            for c in rule['conds']:
                if m.props[c[0]] != c[1]:
                    break
            else:
                root = m.root.split('#')[0]
                disp = root
                if 'regex' in rule:
                    root = sub(rule['regex'][0], rule['regex'][1], root)
                if 'root' in m.props:
                    disp = m.props['root']
                lexs[rule['lexicon-in']].append('%s:%s %s "weight: 2" ;' % (disp, root, rule['lexicon-to']))
                for form in m.props['output']:
                    if isinstance(form[0], list):
                        for k in form[0]:
                            m.props[k[0]] = k[1]
                        s = m.tagify()
                    elif not form[0]:
                        s = m.tagify()
                    else:
                        s = form[0]
                    s = lexcescape(s)
                    lexs[rule['lexicon-in']].append('%s:%s %s "weight: %d" ;' % (s, form[1], form[2], 0 if form[2] == '#' else 1))
                break
        else:
            lexs['Bland'].append('%s:%s # "weight: 2" ;' % (lexcescape(m.tagify()), m.root))
    for r in lang.lexc_lexicons:
        if r['bland']:
            lexs['Bland'].append(r['lexicon-in'] + ' ;')
            lexs[r['lexicon-to']].append(lexcescape(r['bland']) + ': # ;')
    f = open('langs/%d/.generated/lexicon.lexc' % langid, 'w')
    for lex in sorted(lexs.keys()):
        f.write(('LEXICON %s\n' % lex) + '\n'.join(lexs[lex]) + '\n\n')
    f.close()
if __name__ == '__main__':
    import sys, os
    genlexicon(int(sys.argv[1]))
    os.system('./maketransducer.sh ' + sys.argv[1])
