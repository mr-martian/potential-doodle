import random, itertools, copy
from compilelang import loadlang, loadlangset
from datatypes import *
def gen(pats, tree, depth, setvars):
    #print('gen(pats, %s, %s, %s)' % (tree, depth, setvars))
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
    l = loadlang(lang)
    p = l.getpats()
    return gen(p, p[l.syntaxstart], 1, {})
def getpats(lang, sen):
    fromlang = None
    if sen.lang != lang:
        fromlang = sen.lang
    todo = ['transform']
    if fromlang:
        todo += ['syntax', 'morphology']
    for ch in sen.iternest():
        if isinstance(ch, Morpheme):
            todo.append(ch.children[0])
    done = []
    pats = []
    while len(todo) > 0:
        todo = [x for x in list(set(todo)) if x not in done]
        temp = list(Translation.find(lang, lang, todo))
        if fromlang:
            temp += list(Translation.find(fromlang, lang, todo))
        done += todo
        todo = []
        for tr in temp:
            pats.append(tr)
            todo += tr.roots
        break
    return pats
def translate(sen, tolang):
    roots = ['syntax', 'morphology']
    for ch in sen.iternest():
        if isinstance(ch, str):
            roots.append(ch)
    pats = list(Translation.find(sen.lang, tolang, roots))
    for tr in sen.transform(pats):
        for m in movement(tr):
            yield m
def movement(sen):
    roots = ['transform']
    for ch in sen.iternest():
        if isinstance(ch, str):
            roots.append(ch)
    pats = list(Translation.find(sen.lang, sen.lang, roots))
    return sen.transform(pats) or [sen]
def filterlang(sens, lang):
    for s in sens:
        if s.alllang(lang):
            yield s
def gen_and_trans(flang, tlang):
    loadlangset([flang, tlang])
    sen = make(flang)
    ret = []
    for tr in translate(sen, tlang):
        if tr.alllang(tlang):
            for m in movement(tr):
                ret.append(m)
            #ret.append(tr)
    return movement(sen)[0], ret
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])

    sen, tr = gen_and_trans(fl, tl)
    #mv = movement(sen)
    #print(movement(sen)[0].display())
    #print(sen)
    print(sen.display())
    for t in tr:
        print(t.display())
