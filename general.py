import re, itertools, random, copy
from collections import defaultdict
###VARIABLES
class Variable:
    def __init__(self, label, value, cond):
        self.label = label
        self.value = value
        self.opt = False
        if self.value and self.value[-1] == '?':
            self.opt = True
            self.value = self.value[:-1]
        if self.label and self.label[-1] == '!':
            self.cond = None
            self.label = self.label[:-1]
        elif isinstance(cond, list):
            self.cond = Node(Unknown(), Unknown(), Unknown(), {})
            if cond[0]:
                self.cond.props[cond[0]] = cond[1] or Unknown()
        else:
            self.cond = cond
    def check(self, vrs):
        return match(self.cond, vrs[self.label])
    def __str__(self):
        return '$%s:%s(%s)' % (self.label, self.value, self.cond)
    def __repr__(self):
        return self.__str__()
class Unknown:
    def __init__(self):
        self.n = None
    def __str__(self):
        return 'Unknown()'
    def __repr__(self):
        return 'Unknown()'
###DATA STRUCTURES
class Node:
    def __init__(self, lang, ntype, children, props=defaultdict(list)):
        self.lang = lang
        self.ntype = ntype
        self.children = children
        self.props = props
    def map(self, fn):
        r = copy.deepcopy(self)
        r.children = map(fn, r.children)
        return r
    def swapchildren(self, ls):
        r = copy.deepcopy(self)
        r.children = ls
        return r
    def getvars(self, form, vrs={' failed': False}):
        if isinstance(form, Variable):
            vrs[form.label] = self
        elif type(self) != type(form):
            vrs[' failed'] = 'type'#True
        elif not match(self.lang, form.lang) or not match(self.ntype, form.ntype):
            vrs[' failed'] = 'lang or ntype'#True
        elif match(self, form):
            pass
        elif len(self.children) != len(form.children):
            vrs[' failed'] = 'len(children)'#True
        elif not set(form.props.keys()) <= set(self.props.keys()):
            vrs[' failed'] = 'too many properties: %s not <= %s' % (form.props.keys(), self.props.keys())#True
        else:
            for s, f in zip(self.children, form.children):
                if isinstance(s, Node):
                    s.getvars(f, vrs)
                elif isinstance(f, Variable):
                    vrs[f.label] = s
                elif match(s, f):
                    pass
                else:
                    vrs[' failed'] = 'failed on child %s with form %s' % (s, f)
            for k in form.props.keys():
                if isinstance(form.props[k], Variable):
                    vrs[forms.props[k].label] = self.props[k]
                elif match(self.props[k], form.props[k]):
                    pass
                else:
                    vrs[' failed'] = 'failed on property %s with value %s and form %s' % (k, self.props[k], form.props[k])
        return vrs
    def putvars(self, vrs): #DESTRUCTIVE
        for k in self.props:
            if isinstance(self.props[k], Variable):
                self.props[k] = vrs[self.props[k].label]
        for i, ch in enumerate(self.children):
            if isinstance(ch, Node):
                self.children[i] = ch.putvars(vrs)
            if isinstance(ch, Variable):
                self.children[i] = vrs[ch.label]
        return self
    def trans(self, tr):
        if type(tr.form) != type(self) or tr.form.ntype != self.ntype or tr.form.lang != self.lang:
            return []
        elif tr.hasvars:
            vrs = self.getvars(tr.form, {' failed': False})
            if vrs[' failed']:
                return []
            return copy.deepcopy(tr.result).putvars(vrs)
        elif match(self, tr.form):
            return copy.deepcopy(tr.result)
        else:
            return []
    def __str__(self):
        if isinstance(self.children, list):
            s = ' '.join([str(x) for x in self.children])
        else:
            s = str(self.children)
        return '%s(%s)[%s %s]' % (self.__class__.__name__, self.lang, self.ntype, s)
    def __repr__(self):
        return self.__str__()
    def display(self):
        l = []
        for c in self.children:
            if isinstance(c, Node):
                l.append(c.display())
            elif not c:
                pass
            else:
                l.append(str(c))
        return ' '.join(l)
    def iternest(self):
        for ch in self.children:
            if isinstance(ch, Node):
                for c in ch.iternest():
                    yield c
            else:
                yield ch
def match(a, b):
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return True
    elif isinstance(a, Node) and isinstance(b, Node):
        if type(a) != type(b) and type(a) != Node and type(b) != Node:
            return False
        if not match(a.lang, b.lang):
            return False
        if not match(a.ntype, b.ntype):
            return False
        if not isinstance(a.children, Unknown) and not isinstance(b.children, Unknown) and len(a.children) != len(b.children):
            return False
        if isinstance(a.children, list) and isinstance(b.children, list):
            for ac, bc in zip(a.children, b.children):
                if not match(ac, bc):
                    return False
        ka = set(a.props.keys()) - set(['translations', 'forms'])
        kb = set(b.props.keys()) - set(['translations', 'forms'])
        ks = ka.intersection(kb)
        if len(ks) != len(ka) and len(ks) != len(kb):
            return False
        for k in ks:
            if not match(a.props[k], b.props[k]):
                return False
        return True
    elif isinstance(a, Translation) and isinstance(b, Translation):
        return match(a.form, b.form) and match(a.result, b.result)
    else:
        return a == b
class Morpheme(Node):
    __allmorphs = defaultdict(lambda: defaultdict(dict))
    def __init__(self, lang, root, pos):
        Node.__init__(self, lang, pos, [root], defaultdict(list))
        Morpheme.__allmorphs[lang][pos][root] = self
    def langs():
        return list(Morpheme.__allmorphs.keys())
    def iterpos(lang):
        for p in Morpheme.__allmorphs[lang]:
            yield p, list(Morpheme.__allmorphs[lang][p].values())
    def find(lang, pos, root):
        try:
            return Morpheme.__allmorphs[lang][pos][root]
        except:
            assert(isinstance(lang, int))
            print('Morpheme.find(%d, "%s", "%s") failed.' % (lang, pos, root))
            return None
    def addform(self, form):
        self.props['forms'].append(form)
    def addtrans(self, trans):
        self.props['trans'].append(trans)
class MorphologyNode(Node):
    __modes = {
        'prefix': ['^', '(.*)', '\\1'],
        'suffix': ['$', '(.*)', '\\1'],
        'tri-cons': ['^(.*?)_(.*?)_(.*?)$', '^(.*?)-(.*?)$', '\\\\1\\1\\\\2\\2\\\\3']
    }
    def __init__(self, lang, stem, affix, mode):
        Node.__init__(self, lang, mode, [stem, affix], defaultdict(list))
    def display(self):
        s = self.children[0].display()
        if self.children[1] == None:
            return s
        else:
            a = self.children[1].display()
        pat = MorphologyNode.__modes[self.ntype]
        return re.sub(pat[0], re.sub(pat[1], pat[2], a), s)
class SyntaxNode(Node):
    pass
###TRANSFORMATIONS
class Translation:
    __alltrans = []
    def __init__(self, form, result, category):
        self.form = form
        self.result = result
        self.langs = [form.lang, result.lang]
        self.category = category
        self.hasvars = False
        for c in form.iternest():
            if isinstance(c, Variable):
                self.hasvars = True
                break
        Translation.__alltrans.append(self)
    def find(fromlang, tolang, limit):
        for tr in Translation.__alltrans:
            #try:
            #    if tr.form.lang == fromlang and tr.result.lang == tolang:
            #        yield tr
            #except:
            #    pass
            if tr.langs == [fromlang, tolang] and tr.category in limit:
                yield tr
    def __str__(self):
        return '{%s => %s}' % (self.form, self.result)
    def __repr__(self):
        return self.__str__()
###GENERATION
class SyntaxPat:
    def __init__(self, conds, opts, vrs):
        self.conds = conds
        self.opts = opts
        self.vrs = vrs
class Language:
    def __init__(self, lang):
        self.lang = lang
        self.syntax = {}
        self.morphology = defaultdict(list)
        self.transform = []
        self.syntaxstart = None
    def addmorphopt(self, ntype, struct):
        self.morphology[ntype].append(struct)
    def getpats(self):
        r = {}
        for k in self.syntax:
            r[k] = self.syntax[k]
        for k in self.morphology:
            r[k] = self.morphology[k]
        for k, v in Morpheme.iterpos(self.lang):
            r[k] = v
        return r
###PARSING
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
def destring(s, lang, at):
    #print('destring("%s", %d)' % (s, lang))
    assert(isinstance(lang, int))
    if s[0].isnumeric(): #number
        i = 1
        while s[i].isnumeric():
            i += 1
        return int(s[:i]), s[i:].lstrip()
    elif s[0] == '@':
        return at, s[1:].lstrip()
    elif s[0] == '$': #Variable
        m = re.match('^\\$([\\w\\-\'/!]*)[\t ]*(.*)$', s)
        label = m.group(1)
        rest = m.group(2)
        value = ''
        if rest and rest[0] == ':':
            m = re.match('^:([\\w\\-\'/?]*)[\t ]*(.*)$', rest)
            value = m.group(1)
            rest = m.group(2)
        cond = Unknown()
        if rest and rest[0] == '(':
            m = re.match('^\\(([\\w\\-\'/]+=[\\w\\-\'/]+)\\)[ \t]*(.*)$', rest)
            if m:
                cond = m.group(1).split('=')
                rest = m.group(2)
            else:
                cond, rest = destring(rest[1:], lang, at)
                if rest[0] != ')':
                    raise ParseError('Badly formed variable condition.')
                rest = rest[1:].lstrip()
        return Variable(label, value, cond), rest
    elif s[0] == '[': #SyntaxNode
        ntype, r = s[1:].split(' ', 1)
        ch = []
        while r[0] != ']':
            t, r = destring(r, lang, at)
            if isinstance(t, Morpheme):
                ch.append(MorphologyNode(lang, t, None, None))
            else:
                ch.append(t)
            r = r.lstrip()
        return SyntaxNode(lang, ntype, ch), r[1:]
    elif s[0] == '<': #MorphologyNode
        mode, r = s[1:].split(' ', 1)
        mode = mode.strip()
        if mode == '~':
            mode = None
        if mode == '*':
            mode = Unknown()
        r = r.lstrip()
        a, r = destring(r, lang, at)
        r = r.lstrip()
        if r[0] == '>':
            return MorphologyNode(lang, a, '', mode), r[1:].lstrip()
        b, r = destring(r, lang, at)
        r = r.lstrip()
        if r[0] != '>':
            raise ParseError('MorphologyNode with too many elements.')
        return MorphologyNode(lang, a, b, mode), r[1:].lstrip()
    elif s[0] == '~': #None
        return None, s[1:].lstrip()
    elif s[0] == '*': #Unknown
        return Unknown(), s[1:].lstrip()
    elif s[0] == '{': #Morpheme pattern
        i = 1
        while s[i] != '}':
            i += 1
        d = {}
        for p in s[1:i].split(','):
            k, v = p.split('=')
            d[k.strip()] = v.strip()
        r = Morpheme(lang, Unknown(), Unknown())
        r.props = d
        return r, s[i+1:].lstrip()
    else:
        m = re.match('^([\\w\\-\'/]+)=([\\w\\-\'/]+)[ \t]*(.*)$', s)
        if m: #Morpheme
            if lang not in Morpheme.langs():
                loadlexicon(lang)
            r = Morpheme.find(lang, m.group(1), m.group(2))
            if r == None:
                raise ParseError('Unknown lang %d morpheme %s=%s' % (lang, m.group(1), m.group(2)))
            return r, m.group(3)
        else:
            return destring('$:'+s, lang, at)
def loadlexicon(lang):
    rootslist = ParseLine.fromfile('langs/%s/lexicon.txt' % lang)
    for root in rootslist:
        m = Morpheme(lang, root.label, root.args[0])
        forms = []
        trans = []
        props = {}
        for p in root.children:
            if p.label == 'gloss':
                for g in p.val.split(';'):
                    d, r = destring(g.strip(), int(p.args[0]), m)
                    assert(r == '')
                    Translation(m, d, root.label)
            elif p.label == 'translate':
                l = int(p.firstval('lang'))
                f, r = destring(p.firstval('from'), lang, m)
                assert(r == '')
                for t in p.vals('to'):
                    d, r = destring(t, l, m)
                    assert(r == '')
                    Translation(f, d, root.label)
            elif p.label == 'form':
                s, r = destring(p.first('structure').val.strip(), lang, m)
                assert(r == '')
                for f in p['form']:
                    fm = Morpheme(lang, f.val.strip(), root.args[0])
                    fm.props['form of'] = m
                    Translation(s, fm, root.label)
            else:
                m.props[p.label] = p.val
def loadlang(lang):
    things = ParseLine.fromfile('langs/%s/lang.txt' % lang)
    ret = Language(lang)
    for th in things:
        if th.label == 'syntax':
            for ch in th.children:
                if ch.label == 'start-with':
                    ret.syntaxstart = ch.val
                elif ch.label == 'node-types':
                    for ty in ch.children:
                        vrs = []
                        for s in ty.vals('variable'):
                            v, r = destring(s, lang, None)
                            assert(r == '')
                            vrs.append(v)
                        if not list(ty['option']):
                            ty.children.append(ParseLine(-1, 'option', [], '', list(ty['structure']) + list(ty['translation'])))
                        conds = []
                        ops = []
                        for op in ty['option']:
                            node, r = destring(op.firstval('structure'), lang, None)
                            assert(r == '')
                            for tr in op['translation']:
                                res, r = destring(tr.val, int(tr.args[0]), None)
                                assert(r == '')
                                Translation(node, res, 'syntax')
                            cl = []
                            for x in op.args:
                                c, r = destring(x, lang, None)
                                assert(r == '')
                                cl.append(c)
                            conds.append(cl)
                            ops.append(node)
                        ret.syntax[ty.label] = SyntaxPat(conds, ops, vrs)
        if th.label == 'morphology':
            for ch in th.children:
                if ch.label == 'node-types':
                    for ty in ch.children:
                        for op in ty['option']:
                            s, r = destring(op.firstval('structure'), lang, None)
                            assert(r == '')
                            ret.addmorphopt(ty.label, s)
                            for tr in op['translation']:
                                p, r = destring(tr.val, int(tr.args[0]), None)
                                assert(r == '')
                                Translation(s, p, 'morphology')
        if th.label == 'transform':
            for ch in th['rule']:
                tf, r = destring(ch.firstval('form'), lang, None)
                assert(r == '')
                tr, r = destring(ch.firstval('result'), lang, None)
                assert(r == '')
                ret.transform.append(Translation(tf, tr, 'transform'))
    return ret
if __name__ == '__main__':
    #loadlexicon(2)
    #l = loadlang(2)
    #print([x[0].label for x in l.syntax['NP'].conds])
    print(destring('$head(<* {transitive=true} *>)', 7, None))
