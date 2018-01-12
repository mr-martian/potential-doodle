import re, itertools, random, copy
from collections import defaultdict
###VARIABLES
class Variable:
    def __init__(self, label, value, cond, lang):
        self.label = label
        self.value = value
        self.opt = False
        self.lang = lang
        self.blankcond = True
        if self.value and self.value[-1] == '?':
            self.opt = True
            self.value = self.value[:-1]
        if self.label and self.label[-1] == '!':
            self.cond = None
            self.label = self.label[:-1]
            self.blankcond = False
        if self.label and self.label[-1] == '?':
            self.opt = True
            self.label = self.label[:-1]
        elif isinstance(cond, list):
            self.cond = Node(Unknown(), Unknown(), Unknown(), {})
            if cond[0]:
                self.cond.props[cond[0]] = cond[1] or Unknown()
            self.blankcond = False
        else:
            self.cond = cond
            if cond:
                self.blankcond = False
        if self.value and not self.cond:
            self.cond = Node(Unknown(), self.value, Unknown)
            self.blankcond = False
    def check(self, vrs):
        v = vrs[self.label]
        if self.opt:
            if self.blankcond: return True
            else: return v == None or match(v, self.cond)
        else:
            if self.blankcond: return v != None
            else: return match(v, self.cond)
    def putvars(self, vrs):
        return vrs[self.label]
    def __str__(self):
        return '$%s:%s(%s)' % (self.label, self.value, self.cond)
    def __repr__(self):
        return self.__str__()
class Unknown(Variable):
    count = 0
    def __init__(self):
        Variable.__init__(self, ' '+str(Unknown.count), None, None, self)
        Unknown.count += 1
    def __str__(self):
        return 'Unknown(%s)' % self.label
    def __repr__(self):
        return 'Unknown(%s)' % self.label
class Option(list):
    pass
###DATA STRUCTURES
class Node:
    __modes = defaultdict(lambda: [], {
        'prefix': ['^', '(.*)', '\\1'],
        'suffix': ['$', '(.*)', '\\1'],
        'tri-cons': ['^(.*?)_(.*?)_(.*?)$', '^(.*?)-(.*?)$', '\\\\1\\1\\\\2\\2\\\\3']
    })
    def __init__(self, lang, ntype, children, props=None):
        self.lang = lang
        self.ntype = ntype
        self.children = children
        self.props = props
        if not props:
            self.props = defaultdict(list)
    def swapchildren(self, ls):
        return Node(self.lang, self.ntype, ls, copy.deepcopy(self.props))
        r = copy.deepcopy(self)
        r.children = ls
        return r
    def getvars(self, form, vrs={' failed': False}):
        if isinstance(form, Variable):
            vrs[form.label] = self
            if not form.check(vrs):
                vrs[' failed'] = 'variable condition'
        elif isinstance(form, Unknown):
            pass
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
        if isinstance(tr.result, Node):
            vrs[' '] = copy.deepcopy(tr.result).putvars(subvrs)
            return copy.deepcopy(tr.context).putvars(vrs)
        elif tr.result == 'rotate':
            return self.swapchildren(list(reversed(self.children)))
        elif isinstance(tr.result, list):
            if tr.result[0] == 'setlang':
                ret = copy.deepcopy(self)
                ret.lang = tr.result[1]
                return ret
            else:
                return []
        else:
            return []
    def transform(self, pats):
        if len(pats) > 0:
            chs = []
            for c in self.children:
                if isinstance(c, Node):
                    chs.append(c.transform(pats))
                else:
                    chs.append([c])
            nodes = [self.swapchildren(list(cl)) for cl in itertools.product(*chs)]
            ret = []
            retstr = ['[]']
            for n in nodes:
                added = False
                for i, p in enumerate(pats):
                    x = n.trans(p)
                    s = str(x)
                    if s not in retstr:
                        ret.append(x)
                        retstr.append(s)
                    added |= bool(x)
                if not added:
                    ret.append(n)
            return ret
        else:
            return [self]
    def __str__(self):
        if isinstance(self.children, list):
            s = '[' + ' '.join([str(x) for x in self.children]) + ']'
        else:
            s = str(self.children)
        #return '%s(%s)[%s %s]' % (self.__class__.__name__, self.lang, self.ntype, s)
        return '%s(%s)%s' % (self.ntype, self.lang, s)
    def __repr__(self):
        return self.__str__()
    def addmode(name, pats):
        Node.__modes[name] = pats
    def display(self):
        l = []
        for c in self.children:
            if isinstance(c, Node):
                l.append(c.display())
            elif not c:
                pass
            else:
                l.append(str(c))
        mode = Node.__modes[self.ntype]
        if not mode or len(l) != (len(mode) - 1):
            if self.ntype in Language.get(self.lang).rotate:
                l.reverse()
            return ' '.join(l)
        i = len(l) - 1
        s = re.sub(mode[i], mode[i+1], l[i])
        while i > 0:
            i -= 1
            s = re.sub(mode[i], s, l[i])
        return s
    def iternest(self):
        yield self
        for ch in self.children:
            yield ch
            if isinstance(ch, Node):
                for c in ch.iternest():
                    yield c
    def alllang(self, lang):
        for n in self.iternest():
            if isinstance(n, Node) and n.lang != lang:
                return False
        return True
def match(a, b):
    #a is thing, b is pattern
    #only matters for nodes, where b's properties must be a subset of a's
    #otherwise match(b, a) should be identical
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
        if not match(a.lang, b.lang):
            return False
        if not match(a.ntype, b.ntype):
            return False
        if isinstance(a.children, Unknown) or isinstance(b.children, Unknown):
            pass
        elif len(a.children) != len(b.children):
            return False
        elif isinstance(a.children, list) and isinstance(b.children, list):
            for ac, bc in zip(a.children, b.children):
                if not match(ac, bc):
                    return False
        for k in b.props:
            if k not in a.props or not match(a.props[k], b.props[k]):
                return False
        return True
    elif isinstance(a, Translation) and isinstance(b, Translation):
        return match(a.form, b.form) and match(a.result, b.result)
    else:
        return a == b
AllMorphemes = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: None)))
def register(morph):
    AllMorphemes[morph.lang][morph.ntype][morph.children[0]] = morph
    if 'searchkey' in morph.props:
        AllMorphemes[morph.lang][morph.ntype][morph.props['searchkey']] = morph
###TRANSFORMATIONS
class Translation:
    __alltrans = []
    def __init__(self, form, result, category, context=None, resultlang=None):
        self.form = form
        self.result = result
        if resultlang:
            self.langs = [form.lang, resultlang]
        else:
            self.langs = [form.lang, result.lang]
        self.category = category
        self.roots = [] #roots of all morphemes in form
        if isinstance(form, Node):
            for c in form.iternest():
                if isinstance(c, str):
                    self.roots.append(c)
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
    def __init__(self, name, conds, opts, vrs):
        self.name = name
        self.conds = conds
        self.opts = opts
        self.vrs = vrs
    def __str__(self):
        return 'SyntaxPat(%s, %s, %s, %s)' % (self.name, self.conds, self.opts, self.vrs)
    def __repr__(self):
        return self.__str__()
class Language:
    __alllangs = {}
    def __init__(self, lang):
        self.lang = lang
        self.syntax = {}
        self.morphology = defaultdict(list)
        self.transform = []
        self.rotate = []
        self.syntaxstart = None
        Language.__alllangs[lang] = self
    def addmorphopt(self, ntype, struct):
        self.morphology[ntype].append(struct)
    def isloaded(lang):
        return lang in Language.__alllangs
    def get(lang):
        return Language.__alllangs[lang]
    def getpats(self):
        r = {}
        for k in self.syntax:
            r[k] = self.syntax[k]
        for k in self.morphology:
            r[k] = self.morphology[k]
        for k, v in AllMorphemes[self.lang].items():
            r[k] = list(v.values())
        return r
