f = open('missing_morphemes.txt')
lns = list(set(f.readlines()))
lns.sort()
lang = ''
for line in lns:
    if not line: continue
    s = line.split()
    l = s[0][:-1]
    p,r = s[1].split('=')
    if l != lang:
        f.close()
        f = open('langs/%s/lexicon.txt' % l, 'a')
        f.write('\n\n#Generated from missing_morphemes.txt\n')
        lang = l
        print('Writing to langs/%s/lexicon.txt' % l)
    f.write('%s (%s)\n' % (r,p))
f.close()
