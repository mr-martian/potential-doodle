import random, itertools, loadlang
def evalmorph(cond, morph):
    if cond:
        for c in cond.split():
            if morph.props[c.split('=')[0]] != c.split('=')[1]:
                return False
        return True
    else:
        return morph != None
def pickword(lang, ask):
    if ask.endswith(')'):
        pos, conds = ask[:-1].split('(', 1)
    else:
        pos = ask
        conds = ""
    words = []
    for w in loadlang.Morpheme.morphemelist[lang][pos].values():
        if evalmorph(conds, w):
            words.append(w)
    return random.choice(words)
def gennode(outline, name, depth):
    if name:
        n, a = outline[name]
        if n and a:
            if random.randint(0,100) < 10.0/depth: todo = a
            else: todo = n
        elif n: todo = n
        elif a: todo = a
        else: return None
    else:
        todo = outline[outline.start][0]
        depth = 1
    def dostring(s, lang, depth, outline): #returns value, varname
        if s[0] == '$':
            if ':' in s:
                v, r = dostring(s.split(':', 1)[1], lang, depth, outline)
                return v, s.split(':')[0]
            else:
                return -1, s
        elif s[-1] in '*?':
            if random.randint(0,100) < 10/depth:
                return dostring(s[:-1], lang, depth, outline)[0], None
            else:
                return None, None
        elif s[0] == '#':
            return pickword(lang, s[1:]), None
        else:
            return gennode(outline, s, depth+1), None
    def iterchildren(intodo, depth, outline):
        if isinstance(intodo, loadlang.SyntaxNode):
            todo = intodo
            names = {}
        else: #SyntaxNodeSwap
            names = {}
            for v in intodo.variables:
                l, r = dostring(v, intodo.lang, depth, outline)
                names[r] = l
            for i, conds in enumerate(intodo.conds):
                ok = True
                for c in conds:
                    if '(' in c:
                        ok = evalmorph(c[:-1].split('(')[1], names[c.split('(')[0]])
                    else:
                        ok = evalmorph('', names[c])
                    if not ok:
                        break
                if ok:
                    todo = intodo.ops[i]
                    break
        ret = []
        for ch in todo.children:
            if ch == None:
                ret.append(None)
            elif isinstance(ch, str):
                val, var = dostring(ch, todo.lang, depth, outline)
                if var and val == -1:
                    ret.append(names[var])
                else:
                    ret.append(val)
            else: #SyntaxNode
                ret.append(iterchildren(ch, depth, outline))
        return todo.swapchildren(ret)
    return iterchildren(todo, depth, outline)
def getnames(node, form, callback):
    names = {}
    for i, ch in enumerate(form.children):
        if isinstance(ch, loadlang.SyntaxNode):
            for k, v in getnames(node[i], ch, callback).items():
                names[k] = v
                assert(isinstance(v, list))
        elif ch == None:
            pass
        else:
            names[ch.split(':')[0]] = callback(node[i])
            #TODO: This could result in translating the tree many times
            #maybe try caching it somehow?
    return names
def putnames(names, form):
    if form == None:
        return [None]
    elif isinstance(form, loadlang.SyntaxNode):
        ls = [putnames(names, f) for f in form.children]
        return [form.swapchildren(chs) for chs in itertools.product(*ls)]
    elif isinstance(form, loadlang.Morpheme):
        return [form]
    else:
        return names[form] if isinstance(names[form], list) else [names[form]]
def translatetree(tree, tolang): #general node patterns
    if isinstance(tree, loadlang.Morpheme):
        ret = []
        todo = False
        for tr in tree.gettranslations(tolang):
            if tr.form == '@':
                ret.append(tr.result)
            else:
                todo = True
        if todo:
            ret.append(tree)
        return ret
    elif tree == None:
        return [None]
    else:
        ret = []
        for tr in tree.gettranslations(tolang):
            assert(tree.nodetype == tr.form.nodetype)
            ret += putnames(getnames(tree, tr.form, lambda node: translatetree(node, tolang)), tr.result)
        return ret
def lexicaltransform(tree, tolang): #individual words like tan
    def finddepth(pat):
        if pat == "@":
            return -1 #first node = 0
        elif isinstance(pat, loadlang.SyntaxNode):
            for ch in pat.children:
                x = finddepth(ch)
                if x:
                    return x + 1
        else:
            return False
    if isinstance(tree, loadlang.SyntaxNode):
        pats = []
        nodes = []
        for ch in tree:
            t, p = lexicaltransform(ch, tolang)
            nodes.append(t if isinstance(t, list) else [t])
            pats += p
        trs = [tree.swapchildren(x) for x in itertools.product(*nodes)]
        rettr = []
        retpats = [[x[0]-1, x[1]] for x in pats if x[0] > 0]
        for p in pats:
            if p[0] == 0:
                tls = []
                for tr in trs:
                    tls += putnames(getnames(tr, p[1].form, lambda x: x), p[1].result)
                trs = tls
        return trs, retpats
    elif isinstance(tree, loadlang.Morpheme):
        if tree.lang != tolang:
            ret = []
            for p in tree.gettranslations(tolang):
                if p.form != '@':
                    ret.append([finddepth(p), p])
            return tree, ret
        else:
            return tree, []
    else:
        return tree, []
if __name__ == "__main__":
    loadlang.parselexiconfile(2)
    st = gennode(loadlang.parselangfile(2), None, None)
    #print(st)
    #print('\n\n')
    print(st.display())
    print('\n\n')
    en = translatetree(st, 1)
    #print(en[0])
    #print('\n\n')
    print(en[0].display())
    print('\n\n')
    done = []
    for t in en:
        done += lexicaltransform(t, 1)[0]
    #print(done)
    #print('\n\n')
    print(done[0].display())
