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
            for i, p in enumerate(pats):
                x = n.trans(p)
                if x:
                    s = str(x)
                    if s not in retstr:
                        ret.append(x)
                        retstr.append(s)
                    added = True
                    if p.category == 'transform':
                        l = transform(x, pats[:i] + pats[i+1:])
                        ret += l
                        retstr += [str(li) for li in l]
            if not added:
                ret.append(n)
        return ret
    else:
        return [tree]
def make(lang):
    l = general.loadlang(lang)
    p = l.getpats()
    return gen(p, p[l.syntaxstart], 1, {})
def gettransroots(patls):
    r = []
    for p in patls:
        r += p.roots
    return r
def getallrootpats(lang, rootls):
    pats = list(general.Translation.find(lang, lang, rootls))
    done = rootls
    todo = gettransroots(pats)
    find = []
    while len(todo) > 0:
        find = [t for t in todo if t not in done]
        done += find
        todo = []
        if find:
            pts = list(general.Translation.find(lang, lang, find))
            todo = gettransroots(pts)
            pats += pts
            find = []
    return pats
def translate(sen, tolang):
    pats = list(general.Translation.find(sen.lang, tolang, getroots(sen) + ['syntax', 'morphology']))
    pats += getallrootpats(tolang, gettransroots(pats) + ['transform'])
    return transform(sen, pats)
def movement(sen): #only for gen sen (too slow otherwise)
    return transform(sen, getallrootpats(sen.lang, getroots(sen) + ['transform']))
def filterlang(sens, lang):
    for s in sens:
        y = True
        for n in s.iternest():
            if isinstance(n, general.Node) and n.lang != lang:
                y = False
                break
        if y:
            yield s
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])
    general.loadlang(tl)

    sen = make(fl)
    print(movement(sen)[0].display())
    tr = translate(sen, tl)
    ok = False
    for f in filterlang(tr, tl):
        print(f.display())
        ok = True
    #print(tr[0])
    if not ok:
        print('Full translation failed.')
        for t in tr:
            print(t)
