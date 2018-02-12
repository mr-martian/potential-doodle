import random, itertools, copy, os.path
from compilelang import loadlang, loadlangset, toobj
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
def gatdebug(flang, tlang, oldsen=None):
    loadlangset([flang, tlang])
    sen = oldsen or make(Language.getormake(flang))
    f = open('trace.txt', 'w')
    f.write(sen.writecompile())
    f.write('\n\n')
    f.write(str(sen))
    f.write('\n\n')
    m = movement1(sen)
    f.write(str(m))
    print(m.display())
    f.write('\n\n' + m.display() + '\n\n\n=====================================\n\n\n')
    tr = LangLink.getormake(flang, tlang).translate(sen)
    for t in tr:
        if t.alllang(tlang):
            m = movement1(t)
            f.write(str(m) + '\n' + m.display() + '\n\n')
            print(m.display())
    f.close()
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])
    sen = None
    if len(sys.argv) > 3:
        if sys.argv[3] == 'reuse':
            f = open('trace.txt')
            s = toobj(f.readline(), fl)
            f.close()
        elif os.path.isfile(sys.argv[3]):
            f = open(sys.argv[3])
            sen = toobj(f.readline(), fl)
            f.close()

    gatdebug(fl, tl, oldsen=sen)
    #sen, tr = gen_and_trans(fl, tl)
    #print(sen)
    #print(sen.display())
    #for t in tr:
    #    print(t.display())
