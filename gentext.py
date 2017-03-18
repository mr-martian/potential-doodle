import general, random, itertools
def makesentence(lang):
    x = general.Variable('', general.Language.getorloadlang(lang).syntaxstart, '', '')
    return x.generate(lang)
def syntaxtrans(tolang, sen):
    if isinstance(sen, general.Morpheme):
        if sen.lang == tolang:
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
        return r
    if not isinstance(sen, general.SyntaxNode):
        return [sen]
    if sen.lang == tolang:
        ls = [syntaxtrans(tolang, x) for x in sen.children]
        return [sen.swap(l) for l in itertools.product(*ls)]
    ret = []
    for tr in general.Translation.gettrans(sen.lang, tolang):
        t = sen.translate(tr)
        if t:
            ret += syntaxtrans(tolang, t)
    return ret
def lexicaltrans(tolang, sen):
    if isinstance(sen, general.Morpheme):
        r = []
        for tr in sen.trans:
            if tr.resultlang == tolang and tr.form != '@':
                for a, d in tr.form.itermorph():
                    if a == '@' or a == sen:
                        r.append((tr, d))
                        break
        return [sen], r
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
        return ret, rettr
    else:
        return [sen], []
def translate(tolang, sen):
    ls = syntaxtrans(tolang, sen)
    #print(len(ls))
    #print('\n'.join([t.debugdisplay() for t in ls]))
    ret = []
    for s in ls:
        if s.islang(tolang):
            ret += lexicaltrans(tolang, s)[0]
    #print(len(ret))
    #print('\n'.join([t.debugdisplay() for t in ret]))
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
    #print(tr)
    print('\n'.join([t.debugdisplay() for t in tr]))
    #mr = [lexicaltrans(1, s) for s in tr]
    #print(len(mr))
    #print('\n'.join([m.debugdisplay() for m in mr]))
    #print(mr)
