from datatypes import *
from os.path import isfile
from re import compile
import copy, subprocess
UNKNOWN_MORPH = "ERROR"
#what to do when parser encounters a morpheme that isn't in the lexicon
#options: "ERROR", "CREATE", "CREATE_AND_LOG"
#"ERROR" is default because loading twice leads to copies that represent the same morpheme, but with different properties. -D.S. 2018-02-11
FLAT = False
#when flat is True, |[XP] is read as a [XP a b c d] rather than [XP a [Xmod b [Xbar c d]]]
###PARSING
def tokenize(s):
    ret = []
    add = False
    digraph = False
    for c in s:
        if c in '[]<>$:(){}=@%':
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
def toobj(s, lang, loc, at=None):
    assert(isinstance(lang, int))
    rest = tokenize(s)
    def destring():
        nonlocal rest
        cur = rest.pop(0)
        def ok(th):
            return isinstance(th, str) and th[0] not in '[]<>$:(){}=@|%'
        if not isinstance(cur, str):
            return cur
        elif cur.isnumeric(): #number
            return int(cur)
        elif cur == '@':
            return at
        elif cur == '$': #Variable
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
                    cond = destring()
                    if rest[0] != ')':
                        raise ParseError('Badly formed variable condition on line %s (remainder was %s).' % (loc, rest))
                    rest.pop(0)
            return Variable(label, value, cond, l or lang)
        elif cur == '[': #Syntax
            ntype = rest.pop(0)
            ch = []
            while rest[0] != ']':
                ch.append(destring())
            d = {}
            rest.pop(0)
            if rest and rest[0] == '{':
                d = destring()
            return Node(lang, ntype, ch, d)
        elif cur == '|[': #xbar Sytnax
            if isinstance(rest[0], Unknown):
                rest.pop(0)
                sub = [Unknown(), Unknown(), Unknown(), Unknown()]
                name = rest.pop(0)[:-1]
            elif isinstance(rest[0], str) and len(rest[0]) > 1 and rest[0][0] == '?':
                name = rest.pop(0)[1:-1]
                sub = [Variable(name+'spec?', '', Unknown(), lang),
                       Variable(name+'mod?',  '', Unknown(), lang),
                       Variable(name+'head?', '', Unknown(), lang),
                       Variable(name+'comp?', '', Unknown(), lang)]
            elif rest[0] == '$':
                rest.pop(0)
                name = rest.pop(0)[:-1]
                sub = [Variable(name+'spec', '', Unknown(), lang),
                       Variable(name+'mod',  '', Unknown(), lang),
                       Variable(name+'head', '', Unknown(), lang),
                       Variable(name+'comp', '', Unknown(), lang)]
            else:
                sub = [None, None, None, None]
                if rest[0] == None:
                    rest.pop(0)
                name = rest.pop(0)[:-1]
            ch = []
            while rest[0] != ']':
                ch.append(destring())
            if len(ch) == 0: #nothing
                ch.insert(0, sub[2]) #insert head
            if len(ch) == 1: #just head
                ch.insert(1, sub[3]) #insert comp
            if len(ch) == 2: #head and comp
                ch.insert(0, sub[0]) #insert spec
            if len(ch) == 3: #spec, head, and comp
                ch.insert(1, sub[1]) #insert mod
            d = {}
            rest.pop(0)
            if rest and rest[0] == '{':
                d = destring()
            if FLAT:
                return Node(lang, name+'P', ch, d)
            else:
                bar = Node(lang, name+'bar', ch[2:])
                mod = Node(lang, name+'mod', [ch[1], bar])
                return Node(lang, name+'P', [ch[0], mod], d)
        elif cur == '%': #Text entry
            nodes = []
            while True:
                add = defaultdict(lambda: None)
                add['name'] = rest.pop(0)[:-1]
                add[' props'] = []
                if rest[0] == '(':
                    rest.pop(0)
                    while rest[0] != ')':
                        p = rest.pop(0)
                        v = destring()
                        if p == 's':
                            add['spec'] = v
                        elif p == 'm':
                            add['mod'] = v
                        else:
                            add[p] = v
                            add[' props'].append(p)
                    rest.pop(0)
                add['head'] = destring()
                nodes.append(add)
                if not rest or rest[0] != '>':
                    break
                else:
                    rest.pop(0)
            ret = None
            while nodes:
                n = nodes.pop()
                if FLAT:
                    ret = Node(lang, n['name']+'P', [n['spec'], n['mod'], n['head'], ret])
                else:
                    ret = Node(lang, n['name']+'bar', [n['head'], ret])
                    ret = Node(lang, n['name']+'mod', [n['mod'], ret])
                    ret = Node(lang, n['name']+'P', [n['spec'], ret])
                for k in n[' props']:
                    ret.props[k] = n[k]
            return ret
        elif cur == '<': #Morphology
            l = None
            if rest[0] == '(':
                rest.pop(0)
                l = int(rest.pop(0))
                assert(rest.pop(0) == ')')
            mode = rest.pop(0)
            a = destring()
            if rest[0] == '>':
                rest.pop(0)
                return Node(l or lang, mode, [a, None])
            b = destring()
            if rest[0] != '>':
                raise ParseError('Morphology Node with too many elements on line %s. %s' % (loc, rest))
            rest.pop(0)
            return Node(l or lang, mode, [a, b])
        elif cur == '{': #props pattern
            d = {}
            while rest[0] != '}':
                p = rest.pop(0)
                assert(rest.pop(0) == '=')
                d[p] = destring()
            rest.pop(0)
            return d
        elif cur == '(':
            l = Option()
            while rest[0] != ')':
                l.append(destring())
            rest.pop(0)
            return l
        else:
            if rest[0] == '=': #Morpheme
                pos = cur
                root = rest.pop(1)
                rest.pop(0)
                if lang not in AllMorphemes:
                    loadlexicon(lang)
                r = AllMorphemes[lang][pos][root]
                if r == None:
                    if UNKNOWN_MORPH == "CREATE_AND_LOG":
                        r = Node(lang, pos, [root])
                        f = open(DATA_PATH + 'missing_morphemes.txt', 'a')
                        f.write(str(lang) + ': ' + pos + '=' + root + '\n')
                        f.close()
                    elif UNKNOWN_MORPH == "CREATE":
                        r = Node(lang, pos, [root])
                    else: #UNKNOWN_MORPH == "ERROR"
                        raise ParseError('Unknown lang %d morpheme %s=%s on line %s' % (lang, pos, root, loc))
                return r
            else:
                rest = ['$', ':'] + rest
                return destring()
    ret = destring()
    if rest != []:
        print('problem on line %s, unparsed remainder was %s' % (loc, rest))
    assert(rest == [])
    return ret
###FILES
class ParseError(Exception):
    pass
class ParseLine:
    def __init__(self, num, label, args, val, children):
        self.num = num
        self.label = label
        self.arg = '; '.join(args)
        self.args = args
        self.val = val
        self.vals = [val] if val else []
        self.children = children
    def fromstring(fstr, num):
        #label (arg1; arg2): value
        #label: value
        #label (arg1; arg2)
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
            r.args = [x.strip() for x in s.split(';') if not x.isspace()]
            r.arg = s.strip()
        if i < len(fstr)-1 and fstr[i] == ':':
            i += 2
            r.val = fstr[i:].strip()
            r.vals = [x.strip() for x in r.val.split(';') if not x.isspace()]
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
                if l.isspace() or l.lstrip()[0] == '#':
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
    def tofilestr(self, indent):
        r = '  '*indent + '%s' % self.label
        if self.args:
            r += ' (' + '; '.join(self.args) + ')'
        if self.vals:
            r += ': ' + '; '.join(self.vals)
        r += '\n'
        for c in self.children:
            r += c.tofilestr(indent+1)
        return r
    def tofile(self, fname):
        f = open(fname, 'w')
        f.write(self.tofilestr(0))
        f.close()
    def __str__(self):
        return '%d  %s (%s): %s\n' % (self.num, self.label, '; '.join(self.args), self.val) + ''.join([str(x) for x in self.children])
    def __getitem__(self, key):
        for ch in self.children:
            if ch.label == key:
                yield ch
    def __contains__(self, key):
        for ch in self.children:
            if ch.label == key:
                return True
        return False
    def child_vals(self, key):
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
            return toobj(f.val, lang, f.num, at)
        elif default:
            return toobj(default, lang, self.num, at)
        else:
            raise ParseError('Line %s does not have required child %s.' % (self.num, key))
def blank(l): #for blank contexts
    return Variable(' ', None, Unknown(), l)
def loadlexicon(lang):
    rootslist = ParseLine.fromfile(DATA_PATH + 'langs/%s/lexicon.txt' % lang)
    for root in rootslist:
        m = Node(lang, root.arg, [root.label])
        m.props['output'] = []
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
                    Translation(form, [fm], root.label, [lang, lang], context=c, mode=mode)
                for f in p['result']:
                    fm = toobj(f.val, lang, f.num, None)
                    Translation(form, [fm], root.label, [lang, lang], context=c, mode=mode)
                for f in p['setdisplay']:
                    Translation(form, [['setdisplay', f.val]], root.label, [lang, lang], context=c, mode=mode)
                for f in p['setprops']:
                    d = {}
                    for prop in f.children:
                        d[prop.label] = prop.val
                    Translation(form, [['set', d]], root.label, [lang, lang], context=c, mode=mode)
                if 'blank' in p:
                    Translation(form, [['setdisplay', '']], root.label, [lang, lang], context=c, mode=mode)
            elif p.label == 'output':
                o = [p.arg, p.val, '#']
                if '=' in p.arg:
                    o[0] = condlist(p)
                if 'lexicon' in p:
                    o[2] = p.firstval('lexicon')
                m.props['output'].append(o)
            elif p.label == 'linear':
                con = []
                res = []
                if p.val:
                    res.append([0, toobj(p.val, lang, p.num, None)])
                for ch in p.children:
                    try: idx = int(ch.label)
                    except: continue
                    con.append([idx, toobj(ch.val, lang, ch.num, None)])
                    if 'inaudible' in ch:
                        res.append([idx, 'inaudible'])
                    elif 'to' in ch:
                        res.append([idx, ch.fvo('to', lang, None)])
                Translation(m, res, root.label, [lang, lang], context=con, mode='linear')
            elif p.label == 'linear-text':
                con = []
                for ch in p.children:
                    if ch.label.isnumeric():
                        if ch.val[0] == '/' and ch.val[-1] == '/':
                            con.append([int(ch.label), compile(ch.val[1:-1])])
                        else:
                            con.append([int(ch.label), ch.val])
                Translation(m, p.val, root.label, [lang, lang], context=con, mode='linear-text')
            else:
                m.props[p.label] = p.val
        register(m)
        for pos in root['altpos']:
            m2 = copy.deepcopy(m)
            m2.ntype = pos.val
            for l in pos.children:
                m2.props[l.label] = l.val
            register(m2)
def condlist(ch): #parse ch.arg of the form "(a=b; c=d)" into [['a', 'b'], ['c', 'd']]
    ret = []
    for s in ch.args:
        k,v = s.split('=')
        ret.append([k.strip(), v.strip()])
    return ret
def loadlang(lang):
    loadlexicon(lang)
    things = ParseLine.fromfile(DATA_PATH + 'langs/%s/lang.txt' % lang)
    ret = Language.getormake(lang)
    for th in things:
        if th.label == 'syntax':
            for ch in th.children:
                if ch.label == 'start-with':
                    ret.syntaxstart = ch.val
                elif ch.label == 'auto-setlang':
                    for n in ch.vals:
                        if n[-1] == 'P':
                            ret.setlang += [n, n[:-1]+'mod', n[:-1]+'bar']
                        else:
                            ret.setlang.append(n)
                elif ch.label == 'node-types':
                    for ty in ch.children:
                        vrs = [toobj(s, lang, ty.num) for s in ty.child_vals('variable')]
                        if not list(ty['option']):
                            ty.children = [ParseLine(-1, 'option', [], '', ty.children)]
                        conds = []
                        ops = []
                        require = []
                        for op in ty['option']:
                            if 'xbar' in op:
                                line = op.first('xbar')
                                nodes = line.vals
                                if len(nodes) != 3:
                                    ParseError('Wrong number of nodes given to xbar on line %s, expected 4, got %s' % (line.num, len(nodes)))
                                xargs = []
                                for s, arg in zip(nodes, ['spec', 'mod', 'head', 'comp']):
                                    if s[0] == '$' or s == '~':
                                        xargs.append(s)
                                    else:
                                        xargs.append('$%s:%s'%(arg,s))
                                node = toobj('|[%s %s]' % (ty.label, ' '.join(xargs)), lang, line.num)
                            else:
                                st = op.first('structure')
                                node = toobj(st.val, lang, st.num)
                                for tr in op['translation']:
                                    res = toobj(tr.val, int(tr.arg), tr.num)
                                    Translation(node, [res], 'syntax', [lang, int(tr.arg)], mode='syntax')
                            conds.append([toobj(x, lang, op.num) for x in op.args])
                            ops.append(node)
                            req = []
                            for r in op['require']:
                                req.append(r.val)
                            require.append(req)
                        ret.syntax[ty.label] = SyntaxPat(ty.label, conds, ops, vrs, require)
        if th.label == 'transform':
            for ch in th['rule']:
                tc = ch.fvo('context', lang, blank(lang), '@')
                tf = ch.fvo('form', lang, None)
                res = []
                if 'result' in ch:
                    res.append(ch.fvo('result', lang, None))
                for l in ch['set']:
                    res.append(['set', dict(condlist(l))])
                for l in ch['setprop']:
                    a = ['setprop', ' ', l.arg, False, l.val]
                    if '.' in l.val:
                        v,p = l.val[1:].split('.')
                        a[3] = v
                        a[4] = p
                    if '.' in l.arg:
                        v,p = l.arg[1:].split('.')
                        a[1] = v
                        a[2] = p
                    res.append(a)
                ret.transform.append(Translation(tf, res, 'transform', [lang, lang], context=tc, mode='syntax'))
            for ch in th['rotate']:
                ret.rotate.append(ch.val)
        if th.label == 'metadata':
            if 'creator' in th:
                ret.creator = th.firstval('creator')
            if 'name' in th:
                for ch in th.first('name').children:
                    if ch.label == 'local':
                        ret.name = ch.val
                        ret.names[lang] = ch.val
                    else:
                        ret.names[int(ch.label)] = ch.val
        if th.label == 'lexc':
            ret.morph_mode = th.val
            for ch in th.children:
                if ch.label == 'split-root':
                    ret.tags_rootsplit = ch.val
                    continue
                cases = []
                if 'lexicon' not in ch:
                    cases = [ch]
                else:
                    cases = ch.first('lexicon').children
                for cs in cases:
                    ap = {'ntype': ch.label, 'conds': condlist(cs)}
                    if 'bland' in cs:
                        ap['lexicon-in'] = ch.label + 'Root'
                        ap['lexicon-to'] = ch.label + 'Infl'
                        ap['bland'] = cs.firstval('bland')
                        ch.children.append(ParseLine(-1, 'format', '', '{root[0]}'+ap['bland'], []))
                    else:
                        ap['lexicon-in'] = cs.firstval('in')
                        ap['lexicon-to'] = cs.firstval('to')
                        ap['bland'] = False
                    if 'regex-match' in cs:
                        ap['regex'] = [cs.firstval('regex-match'), cs.firstval('regex-replace')]
                    ret.lexc_lexicons.append(ap)
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
                    if defaults[tg.label] == '_':
                        defaults[tg.label] = ''
                ret.tags.append({'format': ch.firstval('format'), 'tags': tags, 'ntype': ch.label, 'conds': condlist(ch), 'defaults': defaults})
    return ret
def loadtrans(lfrom, lto):
    fname = DATA_PATH + 'langs/%s/translate/%s.txt' % (lfrom, lto)
    if isfile(fname):
        trans = ParseLine.fromfile(fname)
        if trans and trans[0].label != 'stage':
            trans = [ParseLine(-1, 'stage', [], '', trans)]
        for i, stage in enumerate(trans):
            for lex in stage.children:
                if lex.label == 'rule':
                    c = lex.fvo('context', lfrom, blank(lfrom), '@')
                    f = lex.fvo('form', lfrom, None)
                    if 'anylang' in lex and isinstance(f, Node):
                        f.nodelang(Unknown())
                    lgs = [lfrom, lto]
                    if 'samelang' in lex:
                        lgs[1] = lfrom
                    r = lex.fvo('result', lgs[1], None)
                    Translation(f, [r], '', lgs, context=c, mode='lex', stage=i)
                else:
                    m = toobj(lex.label, lfrom, lex.num, None)
                    if lex.val:
                        for g in lex.vals:
                            d = toobj(g.strip(), lto, lex.num, m)
                            Translation(m, [d], m.children[0], [lfrom, lto], mode='lex', stage=i)
                    for tr in lex.children:
                        if tr.label == 'translate':
                            f = tr.fvo('from', lfrom, m)
                            c = tr.fvo('context', lfrom, blank(lfrom), '@')
                            for t in tr.child_vals('to'):
                                Translation(f, [toobj(t, lto, tr.num, m)], m.children[0], [lfrom, lto], context=c, mode='lex', stage=i)
def loadlangset(langs):
    for l in langs:
        loadlang(l)
    for lf in langs:
        for lt in langs:
            loadtrans(lf, lt)
class Sentence:
    def __init__(self, lang, name, trees, gloss):
        self.lang = lang
        self.name = name
        self.trees = trees
        self.gloss = gloss
    def fromparseline(pl, lang):
        trees = {'':None}
        if pl.val:
            trees[''] = toobj(pl.val, lang, pl.num, None)
        for l in pl.children:
            if l.label != 'gloss':
                trees[l.label] = toobj(l.val, lang, l.num, None)
        g = pl.first('gloss')
        return Sentence(lang, pl.label, trees, g.val if g else '')
    def toparseline(self):
        ret = ParseLine(0, self.name, [], None, [])
        if self.gloss:
            ret.children.append(ParseLine(0, 'gloss', [], self.gloss, []))
        for k in sorted(self.trees.keys()):
            if not k:
                ret.val = self.trees[k].writecompile()
            else:
                ret.children.append(ParseLine(0, k, [], self.trees[k].writecompile(), []))
        return ret
    def translate(self, tlang, check=False, normalize=True, keepgloss=True):
        ret = Sentence(self.lang, self.name, {}, self.gloss if keepgloss else '')
        if not self.trees:
            return ret
        tr = LangLink.getormake(self.lang, tlang)
        for k in self.trees:
            for i, s in enumerate(tr.translate(self.trees[k])):
                if normalize:
                    s.nodelang(tlang)
                if not check or s.alllang(tlang):
                    ret.trees[k+'-'+str(i) if k else str(i)] = s
        return ret
    def graph(self):
        for k in sorted(self.trees.keys()):
            self.trees[k].flatten()
            f = open(DATA_PATH + 'test/%s-%s.dot' % (self.name, k), 'w')
            f.write(self.trees[k].graph('n', True))
            f.close()
            yield '<h3>%s</h3>' % (k or '(default)'), '%s-%s.dot' % (self.name, k)
class Text:
    def __init__(self, sens):
        self.sens = sens
    def fromfile(fname, lang):
        ret = Text([])
        for s in ParseLine.fromfile(fname):
            ret.sens.append(Sentence.fromparseline(s, lang))
        return ret
    def tofile(self, fname):
        if isinstance(fname, str):
            f = open(fname, 'w')
        else:
            f = fname
        for s in self.sens:
            f.write(s.toparseline().tofilestr(0))
        f.close()
    def translate(self, tlang, check=False, normalize=True, keepgloss=True):
        return Text([x.translate(tlang, check, normalize, keepgloss) for x in self.sens])
    def graph(self, fname):
        gls = []
        f = open(fname, 'w')
        f.write('<html><head></head><body>\n')
        for s in self.sens:
            f.write('<h1>%s</h1>\n' % s.name)
            for h3, fn in s.graph():
                f.write('%s<img src="%s.svg"></img>\n' % (h3, fn))
                gls.append('test/' + fn)
        f.write('</body></html>')
        f.close()
        pip = subprocess.PIPE
        proc = subprocess.Popen(['dot', '-Tsvg', '-O'] + gls, stdin=pip, stdout=pip, universal_newlines=True)
def translatefile(infile, outfile, slang, tlang, check=False, normalize=True, keepgloss=True):
    Text.fromfile(infile, slang).translate(tlang, check, normalize, keepgloss).tofile(outfile)
if __name__ == '__main__':
    loadlang(1)
    Text.fromfile('texts/2john.pdtxt', 1).graph('test/2john.html')
