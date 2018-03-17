import re, itertools, random, copy
from collections import defaultdict
from subprocess import Popen, PIPE
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
            self.blankcond = False
        if self.label and self.label[-1] == '?':
            self.opt = True
            self.label = self.label[:-1]
        self.cond = cond
    def check(self, vrs):
        v = vrs[self.label]
        valmatch = (not self.value) or (isinstance(v, Node) and v.ntype == self.value)
        if v == None:
            return self.opt
        elif not isinstance(v, Node):
            return False
        elif isinstance(self.cond, Unknown):
            return valmatch
        elif isinstance(self.cond, list):
            if self.cond[0] not in v.props:
                return False
            elif len(self.cond) > 1 and v.props[self.cond[0]] != self.cond[1]:
                return False
            else:
                return valmatch
        else:
            return valmatch and match(v, self.cond)
    def putvars(self, vrs):
        return vrs[self.label]
    def putvarscopy(self, vrs):
        return vrs[self.label]
    def __str__(self):
        return '$%s:%s(%s)' % (self.label, self.value, self.cond)
    def __repr__(self):
        return self.__str__()
class Unknown(Variable):
    count = 0
    def __init__(self):
        Variable.__init__(self, ' '+str(Unknown.count)+'?', None, None, self)
        Unknown.count += 1
    def __str__(self):
        return '*'
    def __repr__(self):
        return self.__str__()
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
            self.props = {}
    def swapchildren(self, ls):
        return Node(self.lang, self.ntype, ls, self.props.copy())
    def getvars(self, form, vrs={' failed': False}):
        if isinstance(form, Variable):
            vrs[form.label] = self
            if not form.check(vrs):
                vrs[' failed'] = 'variable condition'
        elif isinstance(form, Unknown):
            pass
        elif type(self) != type(form):
            vrs[' failed'] = 'type'
        elif not match(self.lang, form.lang) or not match(self.ntype, form.ntype):
            vrs[' failed'] = 'lang or ntype'
        elif match(self, form):
            pass
        elif len(self.children) != len(form.children):
            vrs[' failed'] = 'len(children)'
        elif not set(form.props.keys()) <= set(self.props.keys()):
            vrs[' failed'] = 'too many properties: %s not <= %s' % (form.props.keys(), self.props.keys())
        else:
            for s, f in zip(self.children, form.children):
                if isinstance(s, Node):
                    s.getvars(f, vrs)
                elif isinstance(f, Variable):
                    vrs[f.label] = s
                    if not f.check(vrs):
                        vrs[' failed'] = 'variable condition on child'
                elif match(s, f):
                    pass
                else:
                    vrs[' failed'] = 'failed on child %s with form %s' % (s, f)
            for k in form.props.keys():
                if isinstance(form.props[k], Variable):
                    vrs[form.props[k].label] = self.props[k]
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
        vrs = self.getvars(tr.context, {' failed': False})
        if vrs[' failed'] or not isinstance(vrs[' '], Node):
            return []
        vrs = vrs[' '].getvars(tr.form, vrs)
        if vrs[' failed']:
            return []
        if not isinstance(tr.result[0], Node):
            vrs[' '] = copy.deepcopy(vrs[' '])
        for act in tr.result:
            if isinstance(act, Node):
                vrs[' '] = copy.deepcopy(act).putvars(vrs)
            elif isinstance(act, list):
                if act[0] == 'setlang':
                    vrs[' '].lang = act[1]
                elif act[0] == 'setdisplay':
                    vrs[' '].props['display'] = act[1]
                elif act[0] == 'set':
                    vrs[' '].props.update(act[1])
                elif act[0] == 'setprop':
                    if act[3]:
                        vrs[act[1]].props[act[2]] = vrs[act[3]].props[act[4]]
                    else:
                        vrs[act[1]].props[act[2]] = act[4]
        return copy.deepcopy(tr.context).putvars(vrs)
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
        return '%s(%s)%s%s' % (self.ntype, self.lang, s, str(self.props))
    def __repr__(self):
        return self.__str__()
    def debug(self, depth=0):
        ls = [('  '*depth) + ('%s(%s)[' % (self.ntype, self.lang))]
        for c in self.children:
            if isinstance(c, Node):
                l.append(c.debug(depth+1))
            else:
                l.append('  '*(depth+1) + str(c))
        ls.append('  '*depth + ']' + str(self.props))
        return '\n'.join(ls)
    def writecompile(self):
        if len(self.children) == 1 and isinstance(self.children[0], str):
            return self.ntype + '=' + self.children[0]
        l = [self.ntype]
        for c in self.children:
            if isinstance(c, Node):
                l.append(c.writecompile())
            elif not c:
                l.append('~')
            else:
                l.append(str(c))
        return '[' + ' '.join(l) + ']'
    def matchcondlist(self, cndls):
        for c in cndls:
            if c[0] not in self.props:
                return False
            if self.props[c[0]] != c[1]:
                return False
        return True
    def tagify(self):
        lang = Language.get(self.lang)
        format = ''
        tagset = []
        defaults = {}
        for typ in lang.tags:
            if typ['ntype'] != self.ntype:
                continue
            if not self.matchcondlist(typ['conds']):
                continue
            format = typ['format']
            tagset = typ['tags']
            defaults = typ['defaults']
            break
        tags = {'root': self.children[0].split(lang.tags_rootsplit)}
        if 'root' in self.props:
            tags['root'] = self.props['root'].split(lang.tags_rootsplit)
        for tg in tagset:
            if isinstance(tagset[tg], str):
                if tagset[tg] in self.props:
                    t = self.props[tagset[tg]]
                    tags[tg] = '<' + t + '>' if t else ''
            else:
                for cs in tagset[tg]:
                    if self.matchcondlist(cs['conds']):
                        tags[tg] = cs['tag']
                        break
            if tg not in tags:
                tags[tg] = defaults[tg]
        return format.format(**tags) or self.children[0]
    def addmode(name, pats):
        Node.__modes[name] = pats
    def display(self, mode='display'):
        if 'audible' in self.props and self.props['audible'] == 'false':
            return ''
        if mode == 'tags' and isinstance(self.children[0], str):
            return self.tagify()
        if mode == 'linear':
            if isinstance(self.children[0], str):
                return [self]
            else:
                l = []
                for c in self.children:
                    if isinstance(c, Node):
                        l += c.display('linear')
                    elif c:
                        l.append(c)
                return l
        if mode in self.props:
            return self.props[mode]
        l = []
        for c in self.children:
            if isinstance(c, Node):
                d = c.display(mode)
                if d: l.append(d)
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
    def roots(self):
        ret = []
        for ch in self.children:
            if isinstance(ch, str):
                ret.append(ch)
            elif isinstance(ch, Node):
                ret += ch.roots()
        return ret
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
    def __init__(self, form, result, category, context=None, resultlang=None, mode='syntax'):
        self.form = form
        self.result = result
        if resultlang:
            self.langs = [form.lang, resultlang]
        else:
            self.langs = [form.lang, result.lang]
        self.category = category
        self.roots = [] #roots of all morphemes in form
        if isinstance(form, Node):
            self.roots = form.roots()
        self.rootset = set(self.roots)
        self.resultroots = []
        if isinstance(result, Node):
            self.resultroots = result.roots()
        self.resultrootset = set(self.resultroots)
        self.addedroots = self.resultrootset - self.rootset
        if not context:
            self.context = Variable(' ', None, Unknown(), form.lang)
        else:
            self.context = context
        if self.langs[0] == self.langs[1]:
            l = Language.getormake(self.langs[0])
            if mode == 'syntax':
                l.movesyntax.append(self)
            elif mode == 'conj':
                l.moveconj[category].append(self)
            else:
                x = len(l.movelex[category])
                l.movelex[category].append(self)
                assert(len(l.movelex[category]) > x)
        else:
            l = LangLink.getormake(self.langs[0], self.langs[1])
            if mode == 'syntax':
                l.syntax.append(self)
            else:
                l.pats[category].append(self)
    def __str__(self):
        return '{%s => %s}%s' % (self.form, self.result, self.roots)
    def __repr__(self):
        return self.__str__()
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
        self.setlang = []
        #Movement
        self.movelex = defaultdict(list)
        self.movesyntax = []
        self.moveconj = defaultdict(list)
        #Transducer
        self.lexc = ''
        self.lexc_lexicons = []
        self.tags = []
        self.tags_rootsplit = '' #for cases where it's easiest to have tags between parts of the root
        Language.__alllangs[lang] = self
    def addmorphopt(self, ntype, struct):
        self.morphology[ntype].append(struct)
    def isloaded(lang):
        return lang in Language.__alllangs
    def get(lang):
        return Language.__alllangs[lang]
    def getormake(lang):
        if lang in Language.__alllangs:
            return Language.__alllangs[lang]
        else:
            return Language(lang)
    def getpats(self):
        r = {}
        for k in self.syntax:
            r[k] = self.syntax[k]
        for k in self.morphology:
            r[k] = self.morphology[k]
        for k, v in AllMorphemes[self.lang].items():
            r[k] = list(v.values())
        return r
    def movefind(self, roots, conj=False):
        if conj:
            dct = self.moveconj
        else:
            dct = self.movelex
        s = set(roots)
        ret = []
        for r in roots:
            for p in dct[r]:
                if p.rootset < s:
                    ret.append(p)
        return ret
class LangLink:
    __alllinks = {}
    def __init__(self, fromlang, tolang):
        self.fromlang = fromlang
        self.tolang = tolang
        self.syntax = []
        self.pats = defaultdict(list)
        LangLink.__alllinks['%s-%s' % (fromlang, tolang)] = self
        sl = Language.getormake(fromlang).setlang
        for s in sl:
            Translation(Variable('node', s, Unknown(), fromlang), ['setlang', tolang], 'syntax', resultlang=tolang, mode='syntax')
    def find(self, roots):
        s = set(roots)
        ret = []
        for r in roots:
            for p in self.pats[r]:
                if p.rootset < s:
                    ret.append(p)
        return ret
    def getormake(fromlang, tolang):
        s = '%s-%s' % (fromlang, tolang)
        if s in LangLink.__alllinks:
            return LangLink.__alllinks[s]
        else:
            return LangLink(fromlang, tolang)
    def translate(self, sen):
        tr = sen.transform(self.find(sen.roots()))
        ret = []
        for s in tr:
            ret += s.transform(self.syntax)
        return ret
###MOVEMENT
def movement1(sen):
    roots = sen.roots()
    lang = Language.getormake(sen.lang)
    pats = lang.movefind(roots, False)
    lexsen = sen.transform(pats)[0] or sen
    for p in lang.movesyntax:
        lexsen = lexsen.transform([p])[0] or lexsen
    lexsen = lexsen.transform(lang.movefind(lexsen.roots(), True))[0] or lexsen
    return lexsen
def movementall(sen):
    roots = sen.roots()
    rootset = set(roots)
    lang = Language.getormake(sen.lang)
    pats1 = lang.movefind(roots, False)
    sens1 = sen.transform(pats1) or [sen]
    sens2 = []
    for s in sens1:
        l = len(sens1)
        for p in lang.movesyntax:
            sens2 += s.transform([p])
        if len(sens1) == l:
            sens2.append(s)
    sens3 = []
    rootset = set(roots)
    for p in pats1:
        rootset.update(p.addedroots)
    for p in lang.movesyntax:
        rootset.update(p.addedroots)
    roots = list(rootset)
    pats2 = lang.movefind(roots, True)
    for s in sens2:
        sens3 += s.transform(pats2) or [s]
    return sens3
###OUTPUT
def final_output(tree):
    proc = Popen(['hfst-lookup', '-q', '-b', '0', '-i', 'langs/%d/.generated/gen.hfst' % tree.lang], stdin=PIPE, stdout=PIPE, universal_newlines=True)
    ls = proc.communicate('\n'.join(tree.display('tags').split()))
    #print(ls[0])
    print('\n')
    print(ls)
    print('\n')
    print(' '.join([x.split('\t')[1] for x in ls[0].strip().split('\n\n')]))
