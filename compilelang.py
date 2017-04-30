from datatypes import *
###PARSING
def tokenize(s):
    ret = []
    add = False
    for c in s:
        if c in '[]<>$:(){}=@':
            ret.append(c)
            add = False
        elif c == '~':
            ret.append(None)
            add = False
        elif c == '*':
            ret.append(Unknown())
            add = False
        elif c.isspace():
            add = False
        elif add:
            ret[-1] += c
        else:
            ret.append(c)
            add = True
    return ret
def destring(s, lang, at):
    special = '[]<>$:(){}=@'
    def ok(th):
        return isinstance(th, str) and th not in special
    #print('destring("%s", %d, %s)' % (s, lang, at))
    assert(isinstance(lang, int))
    if not isinstance(s[0], str):
        return s[0], s[1:]
    elif s[0].isnumeric(): #number
        return int(s[0]), s[1:]
    elif s[0] == '@':
        return at, s[1:]
    elif s[0] == '$': #Variable
        rest = s[1:]
        l = None
        if len(rest) >= 3 and rest[0] == '(' and rest[1].isnumeric() and rest[2] == ')':
            l = int(rest[1])
            rest = rest[3:]
        label = ''
        if ok(rest[0]):
            label = rest.pop(0)
        value = ''
        if rest and rest[0] == ':':
            rest.pop(0)
            if rest and ok(rest[0]):
                value = rest.pop(0)
        cond = Unknown()
        if rest and rest[0] == '(':
            rest.pop(0)
            if len(rest) >= 2 and rest[1] == ')':
                cond = rest.pop(0)
                rest.pop(0)
            elif len(rest) >= 4 and ok(rest[0]) and rest[1] == '=' and ok(rest[2]) and rest[3] == ')':
                cond = [rest[0], rest[3]]
                rest = rest[4:]
            else:
                cond, rest = destring(rest, lang, at)
                if rest[0] != ')':
                    raise ParseError('Badly formed variable condition.')
                rest.pop(0)
        return Variable(label, value, cond, l or lang), rest
    elif s[0] == '[': #SyntaxNode
        rest = s[1:]
        ntype = rest.pop(0)
        ch = []
        while rest[0] != ']':
            t, rest = destring(rest, lang, at)
            if isinstance(t, Morpheme):
                ch.append(MorphologyNode(lang, t, None, None))
            else:
                ch.append(t)
        return SyntaxNode(lang, ntype, ch), rest[1:]
    elif s[0] == '<': #MorphologyNode
        rest = s[1:]
        l = None
        if rest[0] == '(':
            rest.pop(0)
            l = int(rest.pop(0))
            assert(rest.pop(0) == ')')
        mode = rest.pop(0)
        a, rest = destring(rest, lang, at)
        if rest[0] == '>':
            rest.pop(0)
            return MorphologyNode(l or lang, a, None, mode), rest
        b, rest = destring(rest, lang, at)
        if rest[0] != '>':
            raise ParseError('MorphologyNode with too many elements. %s' % rest)
        rest.pop(0)
        return MorphologyNode(l or lang, a, b, mode), rest
    elif s[0] == '{': #Morpheme pattern
        rest = s[1:]
        d = {}
        while rest[0] != '}':
            p = rest.pop(0)
            assert(rest.pop(0) == '=')
            d[p] = rest.pop(0)
        r = Morpheme(lang, Unknown(), Unknown())
        r.props = d
        rest.pop(0)
        return r, rest
    elif s[0] == '(':
        l = Option()
        rest = s[1:]
        while s[0] != ')':
            o, rest = destring(rest, lang, at)
            l.append(o)
        rest.pop(0)
        return l, rest
    else:
        if s[1] == '=': #Morpheme
            if lang not in Morpheme.langs():
                loadlexicon(lang)
            r = Morpheme.find(lang, s[0], s[2])
            if r == None:
                raise ParseError('Unknown lang %d morpheme %s=%s' % (lang, s[0], s[2]))
            return r, s[3:]
        else:
            return destring(['$', ':'] + s, lang, at)
def toobj(s, lang, at=None):
    ret, rest = destring(tokenize(s), lang, at)
    assert(rest == [])
    return ret
###FILES
class ParseError(Exception):
    pass
class ParseLine:
    def __init__(self, num, label, args, val, children):
        self.num = num
        self.label = label
        self.args = args
        self.val = val
        self.children = children
    def fromstring(fstr, num):
        #label (arg1, arg2): value
        #label: value
        #label (arg1, arg2)
        #label
        i = 0
        r = ParseLine(num, '', [], '', [])
        while i < len(fstr) and fstr[i] not in ' :(':
            r.label += fstr[i]
            i += 1
        r.label = r.label.strip()
        while i < len(fstr) and fstr[i] == ' ':
            i += 1
        p = 0
        if i < len(fstr)-1 and fstr[i] == '(':
            i += 1
            s = ''
            while fstr[i] != ')' or p != 0:
                s += fstr[i]
                if fstr[i] == '(':
                    p += 1
                if fstr[i] == ')':
                    p -= 1
                i += 1
            i += 1
            r.args = [x.strip() for x in s.split(', ')]
        if i < len(fstr)-1 and fstr[i] == ':':
            i += 2
            r.val = fstr[i:].strip()
            i = len(fstr)
        if i != len(fstr):
            raise ParseError('Something is wrong with line %s.\nString was "%s", position: %d' % (num, fstr, i))
        else:
            return r
    def fromfile(fname):
        r = ParseLine(-1, '', [], '', [])
        depth = 0
        with open(fname) as f:
            for i, l in enumerate(f):
                if l.isspace():
                    continue
                while not l.startswith('  '*depth):
                    depth -= 1
                lobj = ParseLine.fromstring(l.rstrip()[depth*2:], i+1)
                at = r
                for d in range(depth):
                    at = at.children[-1]
                at.children.append(lobj)
                depth += 1
        return r.children
    def __str__(self):
        return '%d  %s (%s): %s\n' % (self.num, self.label, ', '.join(self.args), self.val) + ''.join([str(x) for x in self.children])
    def __getitem__(self, key):
        for ch in self.children:
            if ch.label == key:
                yield ch
    def vals(self, key):
        for ch in self[key]:
            yield ch.val
    def first(self, key):
        for ch in self.children:
            if ch.label == key:
                return ch
    def firstval(self, key):
        return self.first(key).val
    def fvo(self, key, lang, at, default=None): #first val object
        f = self.first(key)
        if f:
            return toobj(f.val, lang, at)
        elif default:
            return toobj(default, lang, at)
        else:
            raise ParseError('Key %s not found.' % key)
def blank(l): #for blank contexts
    return Variable(' ', None, Unknown(), l)
def loadlexicon(lang):
    rootslist = ParseLine.fromfile('langs/%s/lexicon.txt' % lang)
    for root in rootslist:
        m = Morpheme(lang, root.label, root.args[0])
        for p in root.children:
            if p.label == 'gloss':
                for g in p.val.split(';'):
                    d = toobj(g.strip(), int(p.args[0]), m)
                    Translation(m, d, root.label)
            elif p.label == 'translate':
                l = int(p.firstval('lang'))
                f = p.fvo('from', lang, m)
                c = p.fvo('context', lang, blank(lang), '@')
                for t in p.vals('to'):
                    Translation(f, toobj(t, l, m), root.label, context=c)
            elif p.label == 'form':
                c = p.fvo('context', lang, blank(lang), '@')
                form = p.fvo('structure', lang, m)
                for f in p['form']:
                    fm = Morpheme(lang, f.val, root.args[0])
                    fm.props['form of'] = m
                    Translation(form, fm, root.label, context=c)
            else:
                m.props[p.label] = p.val
        m.checkkey()
def loadlang(lang):
    loadlexicon(lang)
    things = ParseLine.fromfile('langs/%s/lang.txt' % lang)
    ret = Language(lang)
    for th in things:
        if th.label == 'syntax':
            for ch in th.children:
                if ch.label == 'start-with':
                    ret.syntaxstart = ch.val
                elif ch.label == 'node-types':
                    for ty in ch.children:
                        vrs = [toobj(s, lang) for s in ty.vals('variable')]
                        if not list(ty['option']):
                            ty.children.append(ParseLine(-1, 'option', [], '', list(ty['structure']) + list(ty['translation'])))
                        conds = []
                        ops = []
                        for op in ty['option']:
                            node = toobj(op.firstval('structure'), lang)
                            for tr in op['translation']:
                                res = toobj(tr.val, int(tr.args[0]))
                                Translation(node, res, 'syntax')
                            conds.append([toobj(x, lang) for x in op.args])
                            ops.append(node)
                        ret.syntax[ty.label] = SyntaxPat(conds, ops, vrs)
        if th.label == 'morphology':
            for ch in th.children:
                if ch.label == 'node-types':
                    for ty in ch.children:
                        for op in ty['option']:
                            s = toobj(op.firstval('structure'), lang, None)
                            ret.addmorphopt(ty.label, s)
                            for tr in op['translation']:
                                p = toobj(tr.val, int(tr.args[0]), None)
                                Translation(s, p, 'morphology')
        if th.label == 'transform':
            for ch in th['rule']:
                tc = ch.fvo('context', lang, blank(lang), '@')
                tf = ch.fvo('form', lang, None)
                tr = ch.fvo('result', lang, None)
                ret.transform.append(Translation(tf, tr, 'transform', tc))
    return ret
if __name__ == '__main__':
    loadlexicon(2)
    l = loadlang(2)
    #print([x[0].label for x in l.syntax['NP'].conds])
    #print(destring('$head(<* {transitive=true} *>)', 7, None))
    #print(l)
    print(toobj('<(3) ~ ~ ~>', 7, None))