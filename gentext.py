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
    r = final_output(m)
    f.write(m.display() + '\n\n' + r + '\n\n')
    f.close()
    return r
def outls(sens, traceopen='a'):
    if sens:
        out(sens[0], traceopen)
        f = open('trace.txt', traceopen)
        f.write('\n\n'.join([str(s) for s in sens[1:]]) + '\n\n')
        f.close()
    return [final_output(s) for s in sens]
def trans(sen, tlang):
    tr = LangLink.getormake(sen.lang, tlang).translate(sen)
    return [movement1(s) for s in tr if s.alllang(tlang)]
def full_process(sen, tlang):
    print(out(sen))
    l = outls(trans(sen, tlang))
    for s in l:
        print(s)
def gen_and_trans(flang, tlang):
    loadlangset([flang, tlang])
    sen = make(Language.getormake(flang))
    tr = LangLink.getormake(flang, tlang).translate(sen)
    ret = [movement1(s) for s in tr if s.alllang(tlang)]
    return movement1(sen), ret
def gatdebug(flang, tlang, oldsen=None, args=[]):
    loadlangset([flang, tlang])
    sen = oldsen or make(Language.getormake(flang))
    f = open('trace.txt', 'w')
    f.write(sen.writecompile())
    f.write('\n\n')
    f.write(str(sen))
    f.write('\n\n')
    m = movement1(sen)
    f.write(str(m))
    print(m.display('tags'))
    print(final_output(m))
    f.write('\n\n' + m.display() + '\n\n\n=====================================\n\n\n')
    tr = LangLink.getormake(flang, tlang).translate(sen)
    foundany = False
    for t in tr:
        if t.alllang(tlang):
            foundany = True
            m = movement1(t)
            f.write(str(m) + '\n' + m.display() + '\n\n')
            print(m.display())
    if 'todo' in args and not foundany:
        ls = []
        for t in tr:
            for n in t.iternest():
                if isinstance(n, Node) and n.lang == flang:
                    if isinstance(n.children[0], str):
                        ls.append(n.children[0])
                    f.write(str(n) + '\n')
        s = str(list(sorted(list(set(ls)))))
        f.write('TODO: %s' % s)
        print(s)
    f.close()
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
            sen = toobj(f.readline(), fl)
            f.close()
    loadlangset([fl, tl])
    if not sen:
        sen = make(Language.getormake(fl))

    full_process(sen, tl)
    #gatdebug(fl, tl, oldsen=sen, args=sys.argv[4:])
    #sen, tr = gen_and_trans(fl, tl)
    #print(sen)
    #print(sen.display())
    #for t in tr:
    #    print(t.display())
