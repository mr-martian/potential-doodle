from datatypes import *
from os.path import isfile
UNKNOWN_MORPH = "ERROR"
#what to do when parser encounters a morpheme that isn't in the lexicon
#options: "ERROR", "CREATE", "CREATE_AND_LOG"
#"ERROR" is default because loading twice leads to copies that represent the same morpheme, but with different properties. -D.S. 2018-02-11
###PARSING
def tokenize(s):
    ret = []
    add = False
    digraph = False
    for c in s:
        if c in '[]<>$:(){}=@':
            if digraph:
                ret[-1] += c
                digraph = False
            else:
                ret.append(c)
            add = False
        elif c == '|':
            if digraph:
                ret[-1] += c
            else:
                ret.append(c)
            digraph = True
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
                cond = [rest[0], rest[2]]
                rest = rest[4:]
            else:
                cond, rest = destring(rest, lang, at)
                if rest[0] != ')':
                    raise ParseError('Badly formed variable condition.')
                rest.pop(0)
        return Variable(label, value, cond, l or lang), rest
    elif s[0] == '[': #Syntax
        rest = s[1:]
        ntype = rest.pop(0)
        ch = []
        while rest[0] != ']':
            t, rest = destring(rest, lang, at)
            ch.append(t)
        d = {}
        rest.pop(0)
        if rest and rest[0] == '{':
            d, rest = destring(rest, lang, at)
        return Node(lang, ntype, ch, d), rest
    elif s[0] == '|[': #xbar Sytnax
        ntype = s[1]
        rest = s[2:]
        ch = []
        while rest[0] != ']':
            t, rest = destring(rest, lang, at)
            ch.append(t)
        if len(ch) == 1: #just head
            ch.insert(1, None) #insert comp
        if len(ch) == 2: #head and comp
            ch.insert(0, None) #insert spec
        if len(ch) == 3: #spec, head, and comp
            ch.insert(1, None) #insert mod
        name = ntype[:-1]
        bar = Node(lang, name+'bar', ch[2:])
        mod = Node(lang, name+'mod', [ch[1], bar])
        d = {}
        rest.pop(0)
        if rest and rest[0] == '{':
            d, rest = destring(rest, lang, at)
        return Node(lang, ntype, [ch[0], mod], d), rest
    elif s[0] == '<': #Morphology
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
            return Node(l or lang, mode, [a, None]), rest
        b, rest = destring(rest, lang, at)
        if rest[0] != '>':
            raise ParseError('Morphology Node with too many elements. %s' % rest)
        rest.pop(0)
        return Node(l or lang, mode, [a, b]), rest
    elif s[0] == '{': #props pattern
        rest = s[1:]
        d = {}
        while rest[0] != '}':
            p = rest.pop(0)
            assert(rest.pop(0) == '=')
            d[p], rest = destring(rest, lang, at)
        rest.pop(0)
        return d, rest
    elif s[0] == '(':
        l = Option()
        rest = s[1:]
        while rest[0] != ')':
            o, rest = destring(rest, lang, at)
            l.append(o)
        rest.pop(0)
        return l, rest
    else:
        if s[1] == '=': #Morpheme
            if lang not in AllMorphemes:
                loadlexicon(lang)
            r = AllMorphemes[lang][s[0]][s[2]]
            if r == None:
                if UNKNOWN_MORPH == "CREATE_AND_LOG":
                    r = Node(lang, s[0], [s[2]])
                    f = open('missing_morphemes.txt', 'a')
                    f.write(str(lang) + ': ' + s[0] + '=' + s[2] + '\n')
                    f.close()
                if UNKNOWN_MORPH == "CREATE":
                    r = Node(lang, s[0], [s[2]])
                else: #UNKNOWN_MORPH == "ERROR"
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
        self.arg = ','.join(args)
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
            r.arg = s.strip()
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
                if l.isspace() or l[0] == '#':
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
    def __contains__(self, key):
        for ch in self.children:
            if ch.label == key:
                return True
        return False
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
        m = Node(lang, root.arg, [root.label])
        for p in root.children:
            if p.label in ['form', 'conjugation']:
                if p.label == 'form':
                    mode = 'lex'
                else:
                    mode = 'conj'
                c = p.fvo('context', lang, blank(lang), '@')
                form = p.fvo('structure', lang, m, '@')
                for f in p['form']:
                    fm = Node(lang, root.arg, [f.val])
                    Translation(form, fm, root.label, context=c, resultlang=lang, mode=mode)
                for f in p['result']:
                    fm = toobj(f.val, lang, None)
                    Translation(form, fm, root.label, context=c, resultlang=lang, mode=mode)
                for f in p['setdisplay']:
                    Translation(form, ['setdisplay', f.val], root.label, context=c, resultlang=lang, mode=mode)
                for f in p['setprops']:
                    d = {}
                    for prop in f.children:
                        d[prop.label] = prop.val
                    Translation(form, ['set', d], root.label, context=c, resultlang=lang, mode=mode)
                if 'blank' in p:
                    Translation(form, ['setdisplay', ''], root.label, context=c, resultlang=lang, mode=mode)
            else:
                m.props[p.label] = p.val
        register(m)
def condlist(ch): #parse ch.arg of the form "(a=b; c=d)" into [['a', 'b'], ['c', 'd']]
    ret = []
    for s in ch.arg.split(';'):
        if not s or s.isspace():
            continue
        k,v = s.split('=')
        ret.append([k.strip(), v.strip()])
    return ret
def loadlang(lang):
    loadlexicon(lang)
    things = ParseLine.fromfile('langs/%s/lang.txt' % lang)
    ret = Language.getormake(lang)
    for th in things:
        if th.label == 'syntax':
            for ch in th.children:
                if ch.label == 'start-with':
                    ret.syntaxstart = ch.val
                elif ch.label == 'auto-setlang':
                    for n in ch.val.split(';'):
                        s = n.strip()
                        ret.setlang += [s, s[:-1]+'mod', s[:-1]+'bar']
                elif ch.label == 'node-types':
                    for ty in ch.children:
                        vrs = [toobj(s, lang) for s in ty.vals('variable')]
                        if not list(ty['option']):
                            ty.children = [ParseLine(-1, 'option', [], '', ty.children)]
                        conds = []
                        ops = []
                        for op in ty['option']:
                            if 'xbar' in op:
                                line = op.first('xbar')
                                nodes = line.val.split(';')
                                if len(nodes) != 3:
                                    ParseError('Wrong number of nodes given to xbar on line %s, expected 4, got %s' % (line.num, len(nodes)))
                                xargs = []
                                for s_, arg in zip(nodes, ['spec', 'mod', 'head', 'comp']):
                                    s = s_.strip()
                                    if s[0] == '$':
                                        xargs.append(s)
                                    elif s == '~':
                                        xargs.append('~')
                                    else:
                                        xargs.append('$%s:%s'%(arg,s))
                                name = ty.label[:-1] #drop P
                                node = toobj('|[%sP %s]' % (name, ' '.join(xargs)), lang)
                            else:
                                node = toobj(op.firstval('structure'), lang)
                                for tr in op['translation']:
                                    res = toobj(tr.val, int(tr.arg))
                                    Translation(node, res, 'syntax', resultlang=int(tr.arg), mode='syntax')
                            conds.append([toobj(x, lang) for x in op.args])
                            ops.append(node)
                        ret.syntax[ty.label] = SyntaxPat(ty.label, conds, ops, vrs)
        if th.label == 'morphology':
            for ch in th.children:
                Node.addmode(ch.label, list(ch.vals('re')))
        if th.label == 'transform':
            for ch in th['rule']:
                tc = ch.fvo('context', lang, blank(lang), '@')
                tf = ch.fvo('form', lang, None)
                if 'set' in ch:
                    ret.transform.append(Translation(tf, ['set', dict(condlist(ch.first('set')))], 'transform', context=tc, resultlang=lang, mode='syntax'))
                else:
                    tr = ch.fvo('result', lang, None)
                    ret.transform.append(Translation(tf, tr, 'transform', context=tc, resultlang=lang, mode='syntax'))
            for ch in th['rotate']:
                ret.rotate.append(ch.val)
        if th.label == 'metadata':
            pass
        if th.label == 'lexicon-generation':
            for ch in th.children:
                ret.lexc_lexicons.append({'ntype': ch.label, 'conds': condlist(ch), 'lexicon-in': ch.firstval('in'), 'lexicon-to': ch.firstval('to')})
                if 'regex-match' in ch:
                    ret.lexc_lexicons[-1]['regex'] = [ch.firstval('regex-match'), ch.firstval('regex-replace')]
        if th.label == 'tag-order':
            for ch in th.children:
                if ch.label == 'split-root':
                    ret.tags_rootsplit = ch.val
                    continue
                tags = {}
                defaults = {}
                ls = ch.first('tags').children if 'tags' in ch else []
                for tg in ls:
                    if tg.val:
                        tags[tg.label] = tg.val
                    else:
                        tags[tg.label] = []
                        for cs in tg['case']:
                            tags[tg.label].append({'conds': condlist(cs), 'tag': cs.val})
                    defaults[tg.label] = tg.firstval('default')
                ret.tags.append({'format': ch.firstval('format'), 'tags': tags, 'ntype': ch.label, 'conds': condlist(ch), 'defaults': defaults})
    return ret
def loadtrans(lfrom, lto):
    fname = 'langs/%s/translate/%s.txt' % (lfrom, lto)
    if isfile(fname):
        trans = ParseLine.fromfile(fname)
        for lex in trans:
            m = toobj(lex.label, lfrom, None)
            if lex.val:
                for g in lex.val.split(';'):
                    d = toobj(g.strip(), lto, m)
                    Translation(m, d, m.children[0], resultlang=lto, mode='lex')
            for tr in lex.children:
                if tr.label == 'translate':
                    f = tr.fvo('from', lfrom, m)
                    c = tr.fvo('context', lfrom, blank(lfrom), '@')
                    for t in tr.vals('to'):
                        Translation(f, toobj(t, lto, m), m.children[0], context=c, resultlang=lto, mode='lex')
def loadlangset(langs):
    for l in langs:
        loadlang(l)
    for lf in langs:
        for lt in langs:
            loadtrans(lf, lt)
