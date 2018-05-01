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
def out(sen, traceopen='w'):
    lang = Language.getormake(sen.lang)
    m = movement1(sen)
    f = open('trace.txt', traceopen)
    f.write(sen.writecompile() + '\n\n' + str(sen) + '\n\n')
    f.write(m.writecompile() + '\n\n' + str(m) + '\n\n')
    f.write(str(m.display('tags')) + '\n\n' + str(m.display('linear')) + '\n\n')
    r = dolinear(m)
    f.write(m.display() + '\n\n' + r + '\n\n')
    f.close()
    return r
def outls(sens, traceopen='a'):
    if sens:
        out(sens[0], traceopen)
        f = open('trace.txt', traceopen)
        f.write('\n\n'.join([str(s) for s in sens[1:]]) + '\n\n')
        f.close()
    return [dolinear(s) for s in sens]
def trans(sen, tlang):
    tr = LangLink.getormake(sen.lang, tlang).translate(sen)
    return [movement1(s) for s in tr if s.alllang(tlang)]
def full_process(sen, tlang):
    print(out(sen))
    l = outls(trans(sen, tlang))
    for s in l:
        print(s)
if __name__ == '__main__':
    import sys
    fl = int(sys.argv[1])
    tl = int(sys.argv[2])
    sen = None
    if len(sys.argv) > 3:
        if sys.argv[3] == 'reuse':
            f = open('trace.txt')
            sen = toobj(f.readline(), fl)
            f.close()
        elif os.path.isfile(sys.argv[3]):
            f = open(sys.argv[3])
            sen = toobj(f.readline(), fl, '1 of %s' % sys.argv[3])
            f.close()
    loadlangset([fl, tl])
    if not sen:
        sen = make(Language.getormake(fl))

    full_process(sen, tl)
