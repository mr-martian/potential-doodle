import random, itertools, loadlang
def pickword(lang, ask):
    conds = []
    pos = ask.split('(')[0]
    if ask.endswith(')'):
        for c in ask[:-1].split('(')[1].split(' '):
            conds.append(c.split('='))
    words = []
    for w in loadlang.Morpheme.morphemelist[lang][pos].values():
        ok = True
        for c in conds:
            if w.props[c[0]] != c[1]:
                ok = False
                break
        if ok:
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
        else: return None, None
    else:
        todo = outline[outline.start][0]
        depth = 1
    def iterchildren(todo, depth, outline):
        ret = []
        for ch in todo.children:
            if ch == None:
                ret.append(None)
            elif isinstance(ch, str):
                s = ch
                if ch[0] == '$':
                    s = ch.split(':', 1)[1]
                if (s[-1] in '*?' and random.randint(0,100) < 10/depth) or (s[-1] not in '*?'):
                    if s[0] == '#':
                        ret.append(pickword(todo.lang, s[1:-1] if s[-1] in '*?' else s[1:]))
                    else:
                        ret.append(gennode(outline, s[:-1] if s[-1] in '*?' else s, depth+1))
                else:
                    ret.append(None)
            else: #SyntaxNode
                ret.append(iterchildren(ch, depth, outline))
        return loadlang.SyntaxNode(todo.lang, todo.nodetype, ret)
    return iterchildren(todo, depth, outline)
if __name__ == "__main__":
    loadlang.parselexiconfile(2)
    st = gennode(loadlang.parselangfile(2), None, None)
    print(st)
    print('\n\n')
    print(st.display())
