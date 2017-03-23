import general, random, itertools
def makesentence(lang):
    x = general.Variable('', general.Language.getorloadlang(lang).syntaxstart, '', '')
    return x.generate(lang)
tracing = False
indent = 0
def syntaxtrans(tolang, sen):
    global indent, tracing
    if tracing: print(' '*indent, 'syntaxtrans(%d, %s)' % (tolang, sen))
    indent += 1
    if isinstance(sen, general.MorphologyNode):
        if sen.lang == tolang:
            trs = syntaxtrans(tolang, sen.stem)
            tra = syntaxtrans(tolang, sen.affix)
            ret = []
            for s in trs:
                for a in tra:
                    ret.append(general.MorphologyNode(sen.lang, s, a, sen.mode))
            indent -= 1
            if tracing: print(' '*indent, 'return20', ret)
            return ret
        ret = []
        print([23, sen.lang, tolang])
        for tr in general.Translation.gettrans(sen.lang, tolang):
            if isinstance(tr.form, general.MorphologyNode):
                t = sen.translate(tr)
                if t:
                    ret += syntaxtrans(tolang, t)
                #for r in syntaxtrans(tolang, t):
                #    a = True
                #    for n in ret:
                #        if r == n:
                #            a = False
                #            break
                #    if a:
                #        ret.append(r)
        indent -= 1
        if tracing: print(' '*indent, 'return35', ret)
        return ret
    if isinstance(sen, general.Morpheme):
        if sen.lang == tolang:
            indent -= 1
            return [sen]
        r = []
        add = False
        for tr in sen.trans:
            if tr.resultlang == tolang:
                if tr.form == '@':
                    r.append(tr.result)
                else:
                    add = True
        if add:
            r.append(sen)
        indent -= 1
        if tracing: print(' '*indent, 'return', r)
        return r
    if not isinstance(sen, general.SyntaxNode):
        indent -= 1
        if tracing: print(' '*indent, 'return', [sen])
        return [sen]
    if sen.lang == tolang:
        ls = [syntaxtrans(tolang, x) for x in sen.children]
        indent -= 1
        if tracing: print(' '*indent, 'swapping')
        return [sen.swap(l) for l in itertools.product(*ls)]
    ret = []
    for tr in general.Translation.gettrans(sen.lang, tolang):
        t = sen.translate(tr)
        if t:
            ret += syntaxtrans(tolang, t)
    indent -= 1
    if tracing: print(' '*indent, 'return', ret)
    return ret
indent = 0
def lexicaltrans(tolang, sen):
    global indent, tracing
    if tracing: print(' '*indent, 'lexicaltrans(%d, %s)' % (tolang, sen))
    indent += 1
    if isinstance(sen, general.Morpheme):
        r = []
        for tr in sen.trans:
            if tr.resultlang == tolang and tr.form != '@':
                for a, d in tr.form.itermorph():
                    if a == '@' or a == sen:
                        r.append((tr, d))
                        break
        indent -= 1
        if tracing: print(' '*indent, 'return [%s], %s' % (sen, r))
        return [sen], r
    elif isinstance(sen, general.MorphologyNode):
        s, ts = lexicaltrans(tolang, sen.stem)
        a, ta = lexicaltrans(tolang, sen.affix)
        trs = [(c[0], c[1]-1) for c in ts+ta]
        ret = [general.MorphologyNode(sen.lang, x[0], x[1], sen.mode) for x in itertools.product(s, a)]
        rettr = []
        for tr, d in trs:
            if d < 0:
                add = []
                for s in ret:
                    m = s.translate(tr)
                    if m:
                        add.append(m)
                ret += add
            else:
                rettr.append((tr, d))
        indent -= 1
        print(' '*indent, 'return %s, %s' % (ret, rettr))
        return ret, rettr
    elif isinstance(sen, general.SyntaxNode):
        nodes = []
        trs = []
        for ch in sen.children:
            n, t = lexicaltrans(tolang, ch)
            nodes.append(n)
            trs += [(c[0], c[1]-1) for c in t]
        ret = [sen.swap(x) for x in itertools.product(*nodes)]
        rettr = []
        for tr, d in trs:
            if d < 0:
                add = []
                for s in ret:
                    m = s.translate(tr)
                    if m:
                        add.append(m)
                ret += add
            else:
                rettr.append((tr, d))
        indent -= 1
        print(' '*indent, 'return %s, %s' % (ret, rettr))
        return ret, rettr
    else:
        indent -= 1
        print(' '*indent, 'return [%s], []' % sen)
        return [sen], []
def translate(tolang, sen):
    ls = syntaxtrans(tolang, sen)
    if tracing: print(len(ls))
    if tracing: print('\n'.join([t.debugdisplay() for t in ls]))
    ret = []
    done = []
    for s in ls:
        if s.islang(tolang) and s not in done:
            ret += lexicaltrans(tolang, s)[0]
            done.append(s)
    if tracing: print(len(ret))
    if tracing: print('\n'.join([t.debugdisplay() for t in ret]))
    r = []
    for s in ret:
        if s.ismorphlang(tolang) and s not in r:
            r.append(s)
    return r
if __name__ == '__main__':
    x = makesentence(2)
    print(x.debugdisplay())
    tr = translate(1, x)
    print(len(tr))
    if len(tr) > 0:
        print('\n'.join([t.debugdisplay() for t in tr]))
    else:
        f = open('failed.txt', 'a')
        f.write(str(x)+'\n')
        f.close()
        tracing = True
        print("TRYING AGAIN, WITH TRACING\n\n\n===================\n\n")
        tr = translate(1, x)
    print('\n\n\n\n\n')
    for t in tr:
        print(t.conjugate())
