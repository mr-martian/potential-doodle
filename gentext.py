import general, random, itertools, copy
def gen(pats, tree, depth, setvars):
    #print('gen(pats, %s, %s, %s)' % (tree, depth, setvars))
    if isinstance(tree, general.Node):
        r = copy.copy(tree)
        rc = []
        for c in copy.deepcopy(r.children):
            rc.append(gen(pats, c, depth+1, setvars))
        r.children = rc
        return r
    elif isinstance(tree, list):
        return random.choice(tree)
    elif isinstance(tree, general.Variable):
        if not tree.opt or random.randint(1,100) < 10/depth:
            if tree.label in setvars:
                return setvars[tree.label]
            else:
                newtree = pats[tree.value]
                if isinstance(newtree, list):
                    newtree = random.choice(newtree)
                return gen(pats, newtree, depth+1, setvars)
    elif isinstance(tree, general.SyntaxPat):
        vrs = {}
        for v in tree.vrs:
            vrs[v.label] = gen(pats, v, depth, {})
        il = []
        for i, cl in enumerate(tree.conds):
            ad = True
            for c in cl:
                if not c.check(vrs):
                    ad = False
                    break
            if ad:
                il.append(i)
        return gen(pats, tree.opts[random.choice(il)], depth, vrs)
    else:
        return tree
def getroots(tree):
    if isinstance(tree, general.Morpheme):
        return [tree.children[0]]
    elif isinstance(tree, general.Node):
        r = []
        for c in tree.children:
            r += getroots(c)
        return r
    else:
        return []
def transform(tree, pats):
    if len(pats) > 0 and isinstance(tree, general.Node):
        chs = [transform(c, pats) for c in tree.children]
        nodes = [tree.swapchildren(list(cl)) for cl in itertools.product(*chs)]
        ret = []
        retstr = []
        for n in nodes:
            added = False
            for p in pats:
                x = n.trans(p)
                if x:
                    s = str(x)
                    if s not in retstr:
                        ret.append(x)
                        retstr.append(s)
                    added = True
            if not added:
                ret.append(n)
        return ret
    else:
        return [tree]
def make(lang):
    general.loadlexicon(lang)
    l = general.loadlang(lang)
    p = l.getpats()
    return gen(p, p[l.syntaxstart], 1, {})
def translate(sen, tolang): #TODO: add back root-specific pats
    fromlang = sen.lang
    rootpats = list(general.Translation.find(fromlang, tolang, getroots(sen)))
    tr1 = transform(sen, rootpats)
    tr = []
    trstr = []
    pts = list(general.Translation.find(fromlang, tolang, ['syntax', 'morphology']))
    movpts = list(general.Translation.find(tolang, tolang, ['transform']))
    for t in tr1:
        for tt in transform(t, pts + movpts):
            s = str(tt)
            if s not in trstr:
                tr.append(tt)
                trstr.append(s)
    return tr
def movement(sen): #only for gen sen (too slow otherwise)
    rootlist = getroots(sen)
    rootpats = list(general.Translation.find(sen.lang, sen.lang, rootlist))
    tr = transform(sen, list(general.Translation.find(sen.lang, sen.lang, ['transform'])))
    tr2 = []
    tr2str = []
    for t in tr:
        for tt in transform(t, rootpats):
            s = str(tt)
            if s not in tr2str:
                tr2.append(tt)
                tr2str.append(s)
    return tr2
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])
    general.loadlexicon(tl)
    general.loadlang(tl)

    sen = make(fl)
    print(movement(sen)[0].display())
    tr = translate(sen, tl)
    print('\n'.join([x.display() for x in tr]))
