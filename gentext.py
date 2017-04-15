import general2 as general, random, itertools, copy
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
    if isinstance(tree, general.Node):
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
def translate(sen, tolang):
    fromlang = sen.lang
    rootpats = list(general.Translation.find(fromlang, tolang, getroots(sen)))
    tr1 = []
    tr1str = []
    for t in transform(sen, rootpats):
        s = str(t)
        if s not in tr1str:
            tr1.append(t)
            tr1str.append(s)
    tr = []
    trstr = []
    pts = list(general.Translation.find(fromlang, tolang, ['syntax', 'morphology']))
    for t in tr1:
        for tt in transform(t, pts):
            s = str(tt)
            if s not in trstr:
                tr.append(tt)
                trstr.append(s)
    return tr
if __name__ == '__main__':
    sen = make(2)
    print(sen.display())
    print('\n\n\n')
    print('\n'.join([x.display() for x in translate(sen, 1)]))
