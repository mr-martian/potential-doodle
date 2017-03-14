import random, itertools, loadlang
def pickword(lang, ask):
    return random.choice(list(loadlang.Morpheme.findpos(lang, ask)))
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
    def dostring(var, lang, depth, outline): #returns value, varname
        if not var.nodetype:
            return -1, var.name or None
        s = var.nodetype
        if s[-1] in '*?':
            s = s[:-1]
            if not (random.randint(0,100) < 10/depth):
                return None, var.name or None
        if s[0] == '#':
            return pickword(lang, var), var.name
        else:
            return gennode(outline, s, depth+1), var.name or None
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
                if all([c.findandcheck(names) for c in conds]):
                    todo = intodo.ops[i]
                    break
        ret = []
        for ch in todo.children:
            if ch == None:
                ret.append(None)
            elif isinstance(ch, loadlang.Variable):
                val, var = dostring(ch, todo.lang, depth, outline)
                if var and val == -1:
                    ret.append(names[var])
                else:
                    ret.append(val)
            else: #SyntaxNode
                ret.append(iterchildren(ch, depth, outline))
        return todo.swapchildren(ret)
    return iterchildren(todo, depth, outline)
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
            ret += tree.applytranslation(tr, lambda x: translatetree(x, tolang))
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
        retpats = [[x[0]-1, x[1]] for x in pats if x[0] > 0]
        for p in pats:
            if p[0] == 0:
                trs = list(itertools.chain(*[tr.applytranslation(p[1]) for tr in trs]))
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
