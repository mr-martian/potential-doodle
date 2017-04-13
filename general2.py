import re, itertools, random, copy
from collections import defaultdict
###VARIABLES
class Variable:
    def __init__(self, label, value, prop, cond):
        self.label = label
        self.value = value
        self.prop = prop
        self.cond = cond
        self.mode = False
        self.opt = False
        if self.value and self.value[0] == '#':
            self.mode = True
            self.value = self.value[1:]
        if self.value and self.value[-1] == '?':
            self.opt = True
            self.value = self.value[:-1]
class Unknown(Variable):
    def __init__(self):
        Variable.__init__(self, None, None, None, None)
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
            vrs[' failed'] = True
        elif not match(self.lang, form.lang) or not match(self.ntype, form.ntype):
            vrs[' failed'] = True
        elif match(self, form):
            pass
        elif len(self.children) != len(form.children):
            vrs[' failed'] = True
        elif not set(form.props.keys()) < set(self.props.keys()):
            vrs[' failed'] = True
        else:
            for s, f in zip(self.children, form.children):
                s.getvars(f, vrs)
            for k in form.props.keys():
                if isinstance(form.props[k], Variable):
                    vrs[forms.props[k].label] = self.props[k]
                else:
                    vrs[' failed'] = match(self.props[k], form.props[k])
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
        vrs = self.getvars(tr.form)
        if vrs[' failed']:
            return []
        return copy.deepcopy(tr.result).putvars(vrs)
def match(a, b):
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return True
    elif isinstance(a, Node) and isinstance(b, Node):
        if type(a) != type(b):
            return False
        if not match(a.lang, b.lang):
            return False
        if not match(a.ntype, b.ntype):
            return False
        if len(a.children) != len(b.children):
            return False
        for ac, bc in zip(a.children, b.children):
            if not match(ac, bc):
                return False
        ka = set(a.props.keys())
        kb = set(b.props.keys())
        if kb < ka:
            for k in kb:
                if not match(a.props[k], b.props[k]):
                    return False
        elif ka < kb:
            for k in ka:
                if not match(a.props[k], b.props[k]):
                    return False
        else:
            return False
        return True
    else:
        return a == b
class Morpheme(Node):
    __allmorphs = defaultdict(lambda: defaultdict(dict))
    def __init__(self, lang, root, pos):
        Node.__init__(self, lang, pos, [root])
        Morpheme.__allmorphs[lang][pos][root] = self
    def langs():
        return list(Morpheme.__allmorphs.keys())
    def find(lang, pos, root):
        try:
            return Morpheme.__allmorphs[lang][pos][root]
        except:
            assert(isinstance(lang, int))
            print(Morpheme.__allmorphs)
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
        Node.__init__(self, lang, mode, [stem, affix])
class SyntaxNode(Node):
    pass
###TRANSFORMATIONS
class Translation:
    __alltrans = []
    def __init__(self, form, result):
        self.form = form
        self.result = result
        self.langs = [form.lang, result.lang]
        Translation.__alltrans.append(self)
    def find(fromlang, tolang):
        for tr in Translation.__alltrans:
            try:
                if tr.form.lang == fromlang and tr.result.lang == tolang:
                    yield tr
            except:
                pass
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
    def addmorphopt(self, ntype, struct):
        self.morphology[ntype].append(struct)
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
        m = re.match('^\\$([\\w\\-\'/]*!?)(:#?[\\w\\-\'/]+[*?]?|)(\\([\\w\\-\'/]+=[\\w\\-\'/]+\\)|)[ \t]*(.*)$', s)
        #allow $x:n(c) besides $x:n(c=z)  ??
        if m:
            if m.group(3):
                p, c = m.group(3)[1:-1].split('=', 1)
            else:
                p, c = '', ''
            v = m.group(2)
            return Variable(m.group(1), v[1:] if v else '', p, c), m.group(4)
        else:
            m = re.match('^(\\$[\\w\\-\'/]*!?)(.*)$', s)
            return Variable(m.group(1), '', '', ''), m.group(2).lstrip()
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
            mode =  None
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
                    m.addtrans(Translation(m, d))
            elif p.label == 'translate':
                l = int(p.firstval('lang'))
                f, r = destring(p.firstval('from'), l, m)
                assert(r == '')
                for t in p.vals('to'):
                    d, r = destring(t, l, m)
                    assert(r == '')
                    m.addtrans(Translation(f, d))
            elif p.label == 'form':
                s, r = destring(p.first('structure').val.strip(), lang, m)
                assert(r == '')
                for f in p['form']:
                    fm = Morpheme(lang, f.val.strip(), root.args[0])
                    fm.props['form of'] = m
                    m.addform(Translation(s, fm))
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
                                Translation(node, res)
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
                                Translation(s, p)
        if th.label == 'transform':
            for ch in th['rule']:
                tf, r = destring(ch.firstval('form'), lang, None)
                assert(r == '')
                tr, r = destring(ch.firstval('result'), lang, None)
                assert(r == '')
                ret.transform.append(Translation(tf, tr))
    return ret
if __name__ == '__main__':
    loadlexicon(2)
    l = loadlang(2)
