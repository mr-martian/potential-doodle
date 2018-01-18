import random, itertools, copy
from compilelang import loadlang, loadlangset
from datatypes import *
def gen(pats, tree, depth, setvars):
    if isinstance(tree, Node):
        r = copy.copy(tree)
        rc = []
        for c in copy.deepcopy(r.children):
            rc.append(gen(pats, c, depth+1, setvars))
        r.children = rc
        return r
    elif isinstance(tree, list):
        return random.choice(tree)
    elif isinstance(tree, Variable):
        if not tree.opt or random.randint(1,100) < 10/depth:
            if tree.label in setvars:
                return setvars[tree.label]
            else:
                newtree = pats[tree.value]
                if isinstance(newtree, list):
                    newtree = random.choice(newtree)
                return gen(pats, newtree, depth+1, setvars)
    elif isinstance(tree, SyntaxPat):
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
        try: return gen(pats, tree.opts[random.choice(il)], depth, vrs)
        except: print("error with generating tree %s at depth %s" % (tree, depth))
    else:
        return tree
def make(lang):
    p = lang.getpats()
    return gen(p, p[lang.syntaxstart], 1, {})
def gen_and_trans(flang, tlang):
    loadlangset([flang, tlang])
    sen = make(Language.getormake(flang))
    tr = LangLink.getormake(flang, tlang).translate(sen)
    ret = [movement1(s) for s in tr if s.alllang(tlang)]
    return movement1(sen), ret
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])

    sen, tr = gen_and_trans(fl, tl)
    print(sen.display())
    for t in tr:
        print(t.display())
