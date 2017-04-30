import re, itertools, random, copy
from collections import defaultdict
###VARIABLES
class Variable:
    def __init__(self, label, value, cond, lang):
        self.label = label
        self.value = value
        self.opt = False
        self.lang = lang
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
    def putvars(self, vrs):
        return vrs[self.label]
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
class Option(list):
    pass
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
        #print('%s.trans(%s)' % (self, tr))
        vrs = self.getvars(tr.context, {' failed': False})
        if vrs[' failed'] or not isinstance(vrs[' '], Node):
            #print('failed on context')
            return []
        subvrs = vrs[' '].getvars(tr.form, {' failed': False})
        if subvrs[' failed']:
            #print('failed on form')
            return []
        vrs[' '] = copy.deepcopy(tr.result).putvars(subvrs)
        return copy.deepcopy(tr.context).putvars(vrs)
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
        yield self
        for ch in self.children:
            yield ch
            if isinstance(ch, Node):
                for c in ch.iternest():
                    yield c
def match(a, b):
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return True
    elif isinstance(a, Option):
        for ac in a:
            if match(ac, b):
                return True
        return False
    elif isinstance(b, Option):
        for bc in b:
            if match(a, bc):
                return True
        return False
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
    def checkkey(self):
        if 'searchkey' in self.props:
            Morpheme.__allmorphs[self.lang][self.ntype][self.props['searchkey']] = self
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
            print('Morpheme.find(%d, "%s", "%s") failed. %s' % (lang, pos, root, Morpheme.__allmorphs[lang].keys()))
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
    def __init__(self, form, result, category, context=None):
        self.form = form
        self.result = result
        self.langs = [form.lang, result.lang]
        self.category = category
        self.roots = [] #roots of all morphemes in result
        if isinstance(result, Node):
            for c in result.iternest():
                if isinstance(c, Morpheme):
                    self.roots.append(c.children[0])
        if not context:
            self.context = Variable(' ', None, Unknown(), form.lang)
        else:
            self.context = context
        Translation.__alltrans.append(self)
    def find(fromlang, tolang, limit):
        for tr in Translation.__alltrans:
            if tr.langs == [fromlang, tolang] and tr.category in limit:
                yield tr
    def __str__(self):
        return '{%s => %s}%s' % (self.form, self.result, self.roots)
    def __repr__(self):
        return self.__str__()
    def entail(self):
        return list(Translation.find(self.langs[1], self.langs[1], self.roots))
###GENERATION
class SyntaxPat:
    def __init__(self, conds, opts, vrs):
        self.conds = conds
        self.opts = opts
        self.vrs = vrs
class Language:
    __alllangs = {}
    def __init__(self, lang):
        self.lang = lang
        self.syntax = {}
        self.morphology = defaultdict(list)
        self.transform = []
        self.syntaxstart = None
        Language.__alllangs[lang] = self
    def addmorphopt(self, ntype, struct):
        self.morphology[ntype].append(struct)
    def isloaded(lang):
        return lang in Language.__alllangs
    def getpats(self):
        r = {}
        for k in self.syntax:
            r[k] = self.syntax[k]
        for k in self.morphology:
            r[k] = self.morphology[k]
        for k, v in Morpheme.iterpos(self.lang):
            r[k] = v
        return r
