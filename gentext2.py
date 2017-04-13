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
if __name__ == '__main__':
    general.loadlexicon(2)
    l = general.loadlang(2)
    p = l.getpats()
    #print(p)
    #print('\n\n')
    r = gen(p, p[l.syntaxstart], 1, {})
    #r = gen(p, p['Vbar'], 1, {})
    print(r)
