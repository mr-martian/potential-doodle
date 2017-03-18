import re, itertools, random
###DATA STRUCTURES
class Variable:
    def __init__(self, label, value, prop, cond):
        self.label = label
        self.value = value
        self.prop = prop
        self.cond = cond
    def checkcond(self, names):
        if isinstance(names, Morpheme):
            if self.prop:
                return names.props[self.prop] == self.cond
            else:
                return names != None
        elif isinstance(names[self.label], Morpheme):
            if self.prop:
                return names[self.label].props[self.prop] == self.cond
            else:
                return names[self.label] != None
        else:
            return False #TODO other conds?
    def strip(self):
        r = self.value
        if r and r[0] == '#':
            r = r[1:]
        if r and r[-1] in '*?':
            r = r[:-1]
        return r
    def mode(self):
        if self.value and self.value[0] == '#':
            return '#'
        else:
            return ''
    def opt(self):
        if self.value and self.value[-1] in '*?':
            return self.value[-1]
        else:
            return ''
    def __str__(self):
        return '$%s:%s(%s=%s)' % (self.label, self.value, self.prop, self.cond)
    def __repr__(self):
        return self.__str__()
    def generate(self, lang, depth=1):
        if self.opt() and random.randint(1,100) > 10/depth:
            return None
        elif self.mode():
            return Morpheme.pick(lang, self)
        else:
            #print(' '*depth + str(self))
            swap, alt = Language.getorloadlang(lang).getsyntax(self.strip())
            if alt and random.randint(1,100) < 10/depth:
                swap, alt = alt, swap
            names = VarDict()
            for v in swap.variables:
                names.addvar(v, v.generate(lang, depth+1))
            n = swap.getopt(names)
            for v in n.getvars():
                if v.value:
                    names.addvar(v, v.generate(lang, depth+1))
            return n.rebuild(names)
class VarDict:
    def __init__(self):
        self.vals = {}
    def addvar(self, var, val):
        if isinstance(var, Variable) and var.label:
            self.vals[var.label] = val
        elif isinstance(var, str):
            self.vals[var] = val
        else:
            print([70, 'VarDict.addvar()', var, val])
            assert(False)
    def __getitem__(self, key):
        if isinstance(key, Variable) and key.label:
            return self.vals[key.label]
        elif key == None:
            return None
        elif isinstance(key, str):
            return self.vals[key]
        else:
            print([80, 'VarDict.addvar()', key])
            assert(False)
    def iter(self):
        return self.vals.items()
    def __str__(self):
        return 'VarDict(' + str(self.vals) + ')'
    def __repr__(self):
        return self.__str__()
    def __contains__(self, key):
        if isinstance(key, str):
            return key in self.vals
        elif isinstance(key, Variable):
            return key.label in self.vals
        else:
            print([94, 'VarDict.__contains__()', key])
            return False
class SyntaxNode:
    def __init__(self, lang, ntype, children):
        self.lang = lang
        self.ntype = ntype
        self.children = children
    def swap(self, chs):
        return SyntaxNode(self.lang, self.ntype, chs)
    def iterbuild(self, callback):
        r = SyntaxNode(self.lang, self.ntype, [])
        for ch in self.children:
            if isinstance(ch, SyntaxNode):
                r.children.append(ch.iterbuild(callback))
            else:
                yield callback(ch)
    def getvars(self):
        ret = []
        for ch in self.children:
            if isinstance(ch, SyntaxNode):
                ret += ch.getvars()
            elif ch == None:
                pass
            else:
                ret.append(ch)
        return ret
    def iterfind(self, node, names=VarDict()):
        for sch, nch in zip(self.children, node.children):
            if isinstance(nch, SyntaxNode):
                sch.iterfind(nch, names)
            elif nch == None or nch == '@':
                pass
            else:
                names.addvar(nch, sch)
        return names
    def itermatch(self, node, names=VarDict()):
        if not isinstance(node, SyntaxNode):
            return None
        elif len(self.children) != len(node.children):
            return None
        elif self.ntype != node.ntype:
            return None
        elif self.lang != node.lang:
            return None
        else:
            for sch, nch in zip(self.children, node.children):
                if isinstance(nch, SyntaxNode):
                    x = sch.itermatch(nch, names)
                    if x == None:
                        return None
                elif nch == None:
                    if sch != None:
                        return None
                elif isinstance(nch, Morpheme):
                    if nch != sch:
                        return None
                elif nch in names:
                    if names[nch] != sch:
                        return None
                elif nch == '@':
                    pass
                else:
                    names.addvar(nch, sch)
            return names
    def rebuild(self, names):
        ret = []
        for ch in self.children:
            if isinstance(ch, SyntaxNode):
                ret.append(ch.rebuild(names))
            elif isinstance(ch, Variable):
                ret.append(names[ch])
            else:
                ret.append(ch)
        return SyntaxNode(self.lang, self.ntype, ret)
    def itermorph(self): #yields all non-SyntaxNode non-None items and their depth
        for ch in self.children:
            if isinstance(ch, SyntaxNode):
                for n, d in ch.itermorph():
                    yield n, d+1
            elif ch == None:
                pass
            else:
                yield ch, 0
    def translate(self, trans, inmorph=None):
        if inmorph == None:
            morph = VarDict()
        elif isinstance(inmorph, VarDict):
            morph = inmorph
        else:
            morph = VarDict()
            morph.addvar('@', inmorph)
        x = self.itermatch(trans.form, morph)
        if x:
            return trans.result.rebuild(x)
        else:
            return None
    def itertrans(self, trans, morph=None):
        n = self.translate(trans, morph)
        if n == None:
            n = self
        r = []
        for ch in n.children:
            if isinstance(ch, SyntaxNode):
                r.append(ch.itertrans(trans, morph))
            else:
                r.append(ch)
        return n.swap(r)
    def islang(self, lang):
        if self.lang != lang:
            return False
        for ch in self.children:
            if isinstance(ch, SyntaxNode) and not ch.islang(lang):
                return False
        return True
    def ismorphlang(self, lang):
        if self.lang != lang:
            return False
        for ch in self.children:
            if isinstance(ch, SyntaxNode) and not ch.ismorphlang(lang):
                return False
            if isinstance(ch, Morpheme) and ch.lang != lang:
                return False
        return True
    def debugdisplay(self):
        r = []
        for ch in self.children:
            if ch:
                r.append(ch.debugdisplay())
        return ' '.join(r)
    def __str__(self):
        return '[%s %s]' % (self.ntype, ' '.join([str(x) for x in self.children]))
    def __repr__(self):
        return self.__str__()
    def __eq__(self, other):
        if not isinstance(other, SyntaxNode):
            return False
        if self.lang != other.lang:
            return False
        if self.ntype != other.ntype:
            return False
        if len(self.children) != len(other.children):
            return False
        for sch, nch in zip(self.children, other.children):
            if sch != nch:
                return False
        return True
class MorphologyNode:
    def __init__(self, stem, affix, mode):
        self.stem = stem
        self.affix = affix
        self.mode = mode
    def debugdisplay(self):
        return (self.stem.debugdisplay() if self.stem else '') + '-' + (self.affix.debugdisplay() if self.affix else '')
    def __str__(self):
        return '<%s %s %s>' % (self.mode, self.stem, self.affix)
    def __repr__(self):
        return self.__str__()
    def __eq__(self, other):
        return isinstance(other, MorphologyNode) and self.stem == other.stem and self.affix == other.affix and self.mode == other.mode
class Morpheme:
    __allmorphs = {}
    def __init__(self, lang, root, pos, forms, trans, props):
        self.lang = lang
        self.root = root
        self.pos = pos
        self.forms = forms
        self.trans = trans
        self.props = props
        if lang not in Morpheme.__allmorphs:
            Morpheme.__allmorphs[lang] = {}
        if pos not in Morpheme.__allmorphs[lang]:
            Morpheme.__allmorphs[lang][pos] = {}
        Morpheme.__allmorphs[lang][pos][root] = self
    def find(lang, pos, root):
        try:
            return Morpheme.__allmorphs[lang][pos][root]
        except:
            assert(isinstance(lang, int))
            print('Morpheme.find(%d, "%s", "%s") failed.' % (lang, pos, root))
            return None
    def langs():
        return list(Morpheme.__allmorphs.keys())
    def pick(lang, pos):
        ok = False
        while not ok:
            m = random.choice(list(Morpheme.__allmorphs[lang][pos.strip()].values()))
            ok = pos.checkcond(m)
        return m
    def debugdisplay(self):
        return self.root
    def __str__(self):
        return self.pos + '=' + self.root + ('(%s)' % self.props)
    def __repr__(self):
        return self.__str__()
    def __eq__(self, other):
        if not isinstance(other, Morpheme):
            return False
        return self.lang == other.lang and self.root == other.root
class Translation:
    __gentrans = []
    def __init__(self, form, result, general):
        if isinstance(form, int):
            self.formlang = form
            self.form = '@'
        else:
            self.formlang = form.lang
            self.form = form
        self.resultlang = result.lang
        self.result = result
        if general:
            Translation.__gentrans.append(self)
    def gettrans(flang, tlang):
        ret = []
        for tr in Translation.__gentrans:
            if tr.formlang == flang and tr.resultlang == tlang:
                ret.append(tr)
        return ret
    def __str__(self):
        return '(%d) %s => (%d) %s' % (self.formlang, self.form, self.resultlang, self.result)
    def __repr__(self):
        return self.__str__()
class Form:
    def __init__(self, form, pat):
        self.form = form
        self.pat = pat
class SyntaxPat:
    def __init__(self, conds, ops, variables):
        self.conds = conds
        self.ops = ops
        self.variables = variables
    def getopt(self, names):
        if not self.conds:
            return self.ops[0]
        for i, cl in enumerate(self.conds):
            ok = True
            for c in cl:
                if not c.checkcond(names):
                    ok = False
                    break
            if ok:
                return self.ops[i]
        raise Exception('No option found for conditions %s and variables %s.' % (self.conds, names))
    def __str__(self):
        return 'SyntaxPat(%s, %s, %s)' % (self.conds, self.ops, self.variables)
    def __repr__(self):
        return self.__str__()
class Language:
    __langs = {}
    def __init__(self, langid):
        self.langid = langid
        self.syntax = {}
        self.syntaxstart = ''
        Language.__langs[langid] = self
    def getsyntax(self, key):
        r1 = None
        r2 = None
        if key in self.syntax:
            r1 = self.syntax[key]
        if '-' + key in self.syntax:
            r2 = self.syntax['-' + key]
        if r1:
            return r1, r2
        else:
            return r2, r1
    def getorloadlang(langid):
        if langid not in Language.__langs:
            loadlexicon(langid)
            loadlang(langid)
        return Language.__langs[langid]

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
        m = re.match('^\\$([\\w\\-\'<>]*)(:#?[\\w\\-\'<>]+[*?]?|)(\\([\\w\\-\'<>]+=[\\w\\-\'<>]+\\)|)[ \t]*(.*)$', s)
        #allow $x:n(c) besides $x:n(c=z)  ??
        if m:
            if m.group(3):
                p, c = m.group(3)[1:-1].split('=', 1)
            else:
                p, c = '', ''
            v = m.group(2)
            return Variable(m.group(1), v[1:] if v else '', p, c), m.group(4)
        else:
            m = re.match('^(\\$[\\w\\-\'<>]*)(.*)$', s)
            return Variable(m.group(1), '', '', ''), m.group(2).lstrip()
    elif s[0] == '[': #SyntaxNode
        ntype, r = s[1:].split(' ', 1)
        ch = []
        while r[0] != ']':
            t, r = destring(r, lang, at)
            ch.append(t)
            r = r.lstrip()
        return SyntaxNode(lang, ntype, ch), r[1:]
    elif s[0] == '<': #MorphologyNode
        mode, r = s.split(' ', 1)
        r = r.lstrip()
        a, r = destring(r, lang, at)
        r = r.lstrip()
        if r[0] == '>':
            return MorphologyNode(a, '', mode), r[1:].lstrip()
        b, r = destring(r, lang, at)
        r = r.lstrip()
        if r[0] != '>':
            raise ParseError('MorphologyNode with too many elements.')
        return MorphologyNode(a, b, mode), r[1:].lstrip()
    elif s[0] == '~': #None
        return None, s[1:].lstrip()
    else:
        m = re.match('^([\\w\\-\']+)=([\\w\\-\']+)[ \t]*(.*)$', s)
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
        m = Morpheme(lang, root.label, root.args[0], [], [], {})
        forms = []
        trans = []
        props = {}
        for p in root.children:
            if p.label == 'gloss':
                for g in p.val.split(';'):
                    d, r = destring(g.strip(), int(p.args[0]), m)
                    assert(r == '')
                    m.trans.append(Translation(lang, d, False))
            elif p.label == 'translate':
                l = int(p.firstval('lang'))
                f, r = destring(p.firstval('from'), l, m)
                assert(r == '')
                for t in p.vals('to'):
                    d, r = destring(t, l, m)
                    assert(r == '')
                    m.trans.append(Translation(f, d, False))
            elif p.label == 'form':
                f = list(p['form'])[0].val.strip()
                for s in p['structure']:
                    d, r = destring(s.val, lang, m)
                    assert(r == '')
                    m.forms.append(Form(f, d))
            else:
                m.props[p.label] = p.val
        #Morpheme(lang, root.label, root.args[0], forms, trans, props)
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
                                Translation(node, res, True)
                            cl = []
                            for x in op.args:
                                c, r = destring(x, lang, None)
                                assert(r == '')
                                cl.append(c)
                            conds.append(cl)
                            ops.append(node)
                        ret.syntax[ty.label] = SyntaxPat(conds, ops, vrs)
    return ret
if __name__ == '__main__':
    loadlexicon(2)
    loadlang(2)
    v = destring('$:noun>noun', 2)[0]
    m1 = Morpheme.pick(2, v)
    m2 = Morpheme.pick(2, v)
    print(m1)
    print(m2)
    print(m1 == m2)
    s1 = destring('[NP noun=thefam]', 2)[0]
    s2 = destring('[NP noun=thefam]', 2)[0]
    print(s1)
    print(s2)
    print(s1 == s2)
    print(set([s1, s2]))
