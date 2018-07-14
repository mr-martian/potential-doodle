#!/usr/bin/env python3
import shlex, re, itertools, random, copy, os
from collections import defaultdict
from subprocess import Popen, PIPE
from os.path import isfile
DATA_PATH = os.path.abspath(__file__)[:-9]
UNKNOWN_MORPH = "ERROR"
#what to do when parser encounters a morpheme that isn't in the lexicon
#options: "ERROR", "CREATE", "CREATE_AND_LOG"
#"ERROR" is default because loading twice leads to copies that represent the same morpheme, but with different properties. -D.S. 2018-02-11
FLAT = False
#when flat is True, |[XP] is read as a [XP a b c d] rather than [XP a [Xmod b [Xbar c d]]]
NORMALIZE_NODES = True # True: .lang of non-lexical nodes is ignored
###VARIABLES
class Variable:
    pattern = re.compile('^\\$?([^:?!^+]*):?([^:?!^+]*)\\.?([^:?!^+]*)([?!^+]*)$')
    def __init__(self, label, value=None, prop=None, opt=False, neg=False, multi=False, group=False, idx=None, cond=None):
        self.label = label
        self.value = value
        self.prop = prop
        self.opt = opt
        self.neg = neg
        self.multi = multi
        self.group = group
        self.idx = idx
        self.cond = cond
    def fromstring(s):
        m = Variable.pattern.match(s)
        if m:
            g = m.groups()
            return Variable(g[0], g[1], g[2], '?' in g[3], '!' in g[3], '^' in g[3], '+' in g[3])
        else:
            print('no match with %s' % s)
    def checkset(self, vrs):
        if self.label not in vrs:
            return False
        if self.group:
            return all(self.check(x) for x in vrs[self.label])
        elif self.multi:
            return self.check(vrs[self.label][self.idx])
        else:
            return self.check(vrs[self.label])
    def check(self, v):
        if self.neg:
            return v == None
        if v == None:
            return self.opt
        if not isinstance(v, Node):
            return False
        if self.value and v.ntype != self.value:
            return False
        if self.cond:
            if isinstance(self.cond, list):
                if self.cond[0] not in v.props:
                    return False
                if len(self.cond) > 1 and v.props[self.cond[0]] != self.cond[1]:
                    return False
            else:
                if not match(v, self.cond):
                    return False
        return True
    def putvars(self, vrs):
        if self.multi:
            return vrs[self.label][self.idx]
        else:
            return vrs[self.label]
    def __str__(self):
        return '$'+self.label + \
               (':'+self.value) if self.value else '' + \
               ('.'+self.prop) if self.prop else '' + \
               '?' if self.opt else '' + \
               '!' if self.neg else '' + \
               '^' if self.multi else '' + \
               '+' if self.group else '' + \
               ('(' + str(self.cond) + ')') if self.cond else ''
    def __deepcopy__(self, memo):
        return self
        #Variables aren't modified, so we don't care about copying them
class Unknown(Variable):
    count = 0
    def __init__(self):
        Variable.__init__(self, ' '+str(Unknown.count), opt=True)
        Unknown.count += 1
    def __str__(self):
        return '*'
    def __repr__(self):
        return '*'
class Option(list):
    pass
###DATA STRUCTURES
class Node:
    def __init__(self, lang, ntype, children, props=None):
        self.lang = lang
        self.ntype = ntype
        self.children = children
        self.props = props or {}
        self.rotate = False
    def swapchildren(self, ls):
        return Node(self.lang, self.ntype, ls, self.props.copy())
    def getvars(self, form, vrs={' failed': False}):
        if isinstance(form, Unknown):
            vrs[form.label] = self
        elif isinstance(form, Variable):
            if not form.check(self):
                vrs[' failed'] = 'variable condition'
            else:
                vrs[form.label] = self
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
                    if vrs[' failed']:
                        return vrs
                elif isinstance(f, Variable):
                    if not f.check(s):
                        vrs[' failed'] = 'variable condition on child'
                        return vrs
                    else:
                        vrs[f.label] = s
                elif match(s, f):
                    pass
                else:
                    vrs[' failed'] = 'failed on child %s with form %s' % (s, f)
                    return vrs
            for k in form.props.keys():
                if isinstance(form.props[k], Variable):
                    vrs[form.props[k].label] = self.props[k]
                elif match(self.props[k], form.props[k]):
                    pass
                else:
                    vrs[' failed'] = 'failed on property %s with value %s and form %s' % (k, self.props[k], form.props[k])
                    return vrs
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
    def applyrule(self, rule, vrs):
        if isinstance(rule, Node):
            vrs[' '] = copy.deepcopy(rule).putvars(vrs)
        elif isinstance(rule, list):
            if rule[0] == 'setlang':
                vrs[' '].lang = rule[1]
            elif rule[0] == 'setdisplay':
                vrs[' '].props['display'] = rule[1]
            elif rule[0] == 'set':
                vrs[' '].props.update(rule[1])
            elif rule[0] == 'setprop':
                if rule[3]:
                    try:
                        vrs[rule[1]].props[rule[2]] = vrs[rule[3]].props[rule[4]]
                    except KeyError:
                        print('%s does not have key %s' % (vrs[rule[3]], rule[4]))
                        raise
                else:
                    vrs[rule[1]].props[rule[2]] = rule[4]
            elif rule[0] == 'rotate':
                vrs[' '].rotate = True
            elif rule[0] == 'makevar':
                vrs[rule[1]] = copy.deepcopy(rule[2])
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
            self.applyrule(act, vrs)
        return copy.deepcopy(tr.context).putvars(vrs)
    def transmulti(self, tr):
        if tr.ntypelist and self.ntype not in tr.ntypelist:
            return []
        vrs = {' failed': False, ' ': self}
        path = []
        for l in tr.layers:
            ok = False
            for i, f in enumerate(l):
                vrs2 = vrs[' '].getvars(f[0], vrs.copy())
                # don't need deepcopy because the values aren't ever modified
                if not vrs2[' failed']:
                    ok = True
                    vrs = vrs2
                    path.append(f[1:])
                    break
            if not ok:
                return []
        vrs = copy.deepcopy(vrs)
        for result in reversed(path):
            for act in result:
                self.applyrule(act, vrs)
        return vrs[' ']
    def transform(self, pats, returnself=True):
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
                    if isinstance(p, Translation):
                        x = n.trans(p)
                    else:
                        x = n.transmulti(p)
                    s = str(x)
                    if s not in retstr:
                        ret.append(x)
                        retstr.append(s)
                    added |= bool(x)
                if not added:
                    ret.append(n)
            return ret
        elif returnself:
            return [self]
        else:
            return []
    def __str__(self):
        if isinstance(self.children, list):
            if isinstance(self.children[0], str): return '%s=%s' % (self.ntype, self.children[0])
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
    def graph(self, name, ishead=False):
        ret = ''
        if ishead:
            ret += 'digraph {'
        ret += '%s [label="%s"];' % (name, self.ntype)
        for i, c in enumerate(self.children):
            ret += '%s -> %s%d;' % (name, name, i)
            if isinstance(c, Node):
                ret += c.graph(name+str(i))
            else:
                ret += '%s%d [label="%s"];' % (name, i, str(c))
        if ishead:
            ret += '}'
        return ret
    def flatten(self): #DESTRUCTIVE
        for c in self.children:
            if isinstance(c, Node):
                c.flatten()
        if self.ntype[-1] == 'P':
            n = self.ntype[:-1]
            if len(self.children) != 2: return None
            if not isinstance(self.children[1], Node): return None
            m = self.children[1]
            if m.ntype != n+'mod': return None
            if len(m.children) != 2: return None
            if not isinstance(m.children[1], Node): return None
            b = m.children[1]
            if b.ntype != n+'bar': return None
            if len(b.children) != 2: return None
            self.children = [self.children[0], m.children[0], b.children[0], b.children[1]]
    def unflatten(self): #DESTRUCTIVE
        for c in self.children:
            if isinstance(c, Node):
                c.unflatten()
        if self.ntype[-1] == 'P' and len(self.children) == 4:
            ch = self.children
            n = self.ntype[:-1]
            self.children = [ch[0], Node(self.lang, n+'mod', [ch[1], Node(self.lang, n+'bar', [ch[2], ch[3]])])]
    def matchcondlist(self, cndls):
        for c in cndls:
            if c[0] not in self.props:
                return False
            if self.props[c[0]] != c[1]:
                return False
        return True
    def tagify(self, regex=False):
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
        tags = {'root': self.children[0].split('#')[0].split(lang.tags_rootsplit)}
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
                if regex:
                    tags[tg] = '<[^<>]*>'
                else:
                    tags[tg] = defaults[tg]
        ret = format.format(**tags) or self.children[0]
        if regex:
            ret = '\t' + ret.replace('+', '\\+')
        return ret
    def rotated(self):
        return self.ntype in Language.get(self.lang).rotate != self.rotate
    def tagify_all(self):
        if isinstance(self.children[0], str):
            return [self.tagify()]
        else:
            rev = self.rotated()
            ret = []
            for c in self.children:
                if isinstance(c, Node):
                    a = c.tagify_all()
                else:
                    a = [c] if c else []
                if rev:
                    ret = a + ret
                else:
                    ret += a
            return ret
    def linear(self):
        if isinstance(self.children[0], str):
            return [self]
        else:
            l = []
            for c in self.children:
                if isinstance(c, Node):
                    l.append(c.linear())
                elif c:
                    l.append([c])
            if self.rotated():
                l.reverse()
            r = []
            for c in l:
                r += c
            return r
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
    def nodelang(self, lang): #DESTRUCTIVE
        if self.children and isinstance(self.children[0], str):
            pass
        else:
            self.lang = lang
            for c in self.children:
                if isinstance(c, Node):
                    c.nodelang(lang)
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
    roots = [morph.children[0]]
    if 'searchkey' in morph.props:
        roots.append(morph.props['searchkey'])
    pos = [morph.ntype]
    if 'altpos' in morph.props:
        pos.append(morph.props['altpos'])
    for p in pos:
        for r in roots:
            AllMorphemes[morph.lang][p][r] = morph
###TRANSFORMATIONS
class Translation:
    def __init__(self, form, result, category, langs, context=None, mode='syntax', stage=0):
        self.form = form
        self.result = result
        self.stage = stage
        self.langs = langs
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
        self.context = context or Variable(' ')
        if self.langs[0] == self.langs[1]:
            l = Language.getormake(self.langs[0])
            if mode == 'syntax':
                l.movesyntax.append(self)
            elif mode == 'conj':
                l.moveconj[category].append(self)
            elif mode == 'linear':
                l.linear[category].append(self)
            elif mode == 'linear-text':
                l.lineartext[category].append(self)
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
class Layer:
    def __init__(self, form, result):
        self.form = form
        self.result = result
    def applyrule(self, vrs):
        for rule in self.result:
            mode = rule[0]
            args = rule[1:]
            if mode == 'node':
                pass
        if isinstance(rule, Node):
            vrs[' '] = copy.deepcopy(rule).putvars(vrs)
        elif isinstance(rule, list):
            if rule[0] == 'setlang':
                vrs[' '].lang = rule[1]
            elif rule[0] == 'setdisplay':
                vrs[' '].props['display'] = rule[1]
            elif rule[0] == 'set':
                vrs[' '].props.update(rule[1])
            elif rule[0] == 'setprop':
                if rule[3]:
                    try:
                        vrs[rule[1]].props[rule[2]] = vrs[rule[3]].props[rule[4]]
                    except KeyError:
                        print('%s does not have key %s' % (vrs[rule[3]], rule[4]))
                        raise
                else:
                    vrs[rule[1]].props[rule[2]] = rule[4]
            elif rule[0] == 'rotate':
                vrs[' '].rotate = True
            elif rule[0] == 'makevar':
                vrs[rule[1]] = copy.deepcopy(rule[2])
class MultiRule:
    def __init__(self, layers, category, langs, mode='syntax', stage=0):
        self.layers = layers
        self.stage = stage
        self.langs = langs
        self.category = category
        self.roots = [] #roots of all morphemes in form
        self.rootset = set(self.roots)
        self.resultroots = []
        self.resultrootset = set(self.resultroots)
        self.addedroots = self.resultrootset - self.rootset
        self.ntypelist = []
        if all(isinstance(x[0], Node) for x in layers[0]):
            self.ntypelist = [x[0].ntype for x in layers[0]]
        if self.langs[0] == self.langs[1]:
            l = Language.getormake(self.langs[0])
            if mode == 'syntax':
                l.movesyntax.append(self)
            elif mode == 'conj':
                l.moveconj[category].append(self)
            elif mode == 'linear':
                l.linear[category].append(self)
            elif mode == 'linear-text':
                l.lineartext[category].append(self)
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
###GENERATION
class SyntaxPat:
    def __init__(self, name, conds, opts, vrs, require):
        self.name = name
        self.conds = conds
        self.opts = opts
        self.vrs = vrs
        self.require = require
    def __str__(self):
        return 'SyntaxPat(%s, %s, %s, %s)' % (self.name, self.conds, self.opts, self.vrs)
    def __repr__(self):
        return self.__str__()
class Language:
    __alllangs = {}
    def __init__(self, lang):
        #Metadata
        self.name = ''
        self.names = {}
        self.creator = ''
        #General
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
        self.linear = defaultdict(list)
        self.lineartext = defaultdict(list)
        #Transducer
        self.lexc = ''
        self.lexc_lexicons = []
        self.tags = []
        self.tags_rootsplit = '' #for cases where it's easiest to have tags between parts of the root
        self.morph_mode = '' #hfst of lttoolbox
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
            return loadlang(lang)
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
    def allnames():
        return [(x, Language.__alllangs[x].name) for x in sorted(Language.__alllangs.keys())]
    def iterlex(self):
        for ntype in AllMorphemes[self.lang]:
            for root in AllMorphemes[self.lang][ntype]:
                yield AllMorphemes[self.lang][ntype][root]
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
            Translation(Variable('node', value=s), [['setlang', tolang]], 'syntax', [fromlang, tolang], mode='syntax')
    def find(self, _roots):
        roots = _roots + ['']
        s = set(roots)
        ret = defaultdict(list)
        for r in roots:
            for p in self.pats[r]:
                if p.rootset < s:
                    ret[p.stage].append(p)
        return [ret[k] for k in sorted(ret.keys())]
    def getormake(fromlang, tolang):
        s = '%s-%s' % (fromlang, tolang)
        if s in LangLink.__alllinks:
            return LangLink.__alllinks[s]
        else:
            return loadtrans(fromlang, tolang)
    def translate(self, sen):
        pats = self.find(sen.roots())
        #pats.append(self.syntax) #unnecessary due to sen.nodelang()
        tr = [sen]
        for p in pats:
            ntr = []
            for s in tr:
                ntr += s.transform(p, False)
            if ntr:
                tr = ntr
        return tr
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
def hfst(tagstrs, lang):
    mode = Language.getormake(lang).morph_mode
    if mode == 'hfst':
        proc = Popen(['hfst-lookup', '-q', '-b', '0', '-i', DATA_PATH + 'langs/%d/.generated/gen.hfst' % lang], stdin=PIPE, stdout=PIPE, universal_newlines=True)
        ls = proc.communicate('\n'.join(tagstrs))
        if ls[0]:
            ret = [x.split('\t')[1] for x in ls[0].strip().split('\n\n')]
        else:
            ret = []
    elif mode == 'lttoolbox':
        proc = Popen(['lt-proc', '-g', DATA_PATH + 'langs/%d/.generated/gen.bin' % lang], stdin=PIPE, stdout=PIPE, universal_newlines=True)
        ls = proc.communicate('\n'.join(['^%s$' % t for t in tagstrs]))[0]
        ret = ls.split('\n')
    else:
        raise Exception('Unknown morphology mode %s' % mode)
    return ret
def dolinear(sen):
    lin = sen.linear()
    lang = Language.getormake(sen.lang)
    for i, m in enumerate(lin):
        for pat in lang.linear[m.children[0]]:
            if not match(m, pat.form):
                continue
            ok = True
            if isinstance(pat.context, list):
                for d, p in pat.context:
                    if i+d < 0 or i+d >= len(lin):
                        ok = False
                        break
                    if not match(lin[i+d], p):
                        ok = False
                        break
            if ok:
                for d, r in pat.result:
                    if r == 'inaudible':
                        lin[i+d].props['audible'] = 'false'
                    else:
                        lin[i+d] = r
    lintxt = hfst([x.tagify() for x in lin], sen.lang)
    for i, m in enumerate(lin):
        for pat in lang.lineartext[m.children[0]]:
            ok = True
            if isinstance(pat.context, list):
                for d, p in pat.context:
                    if i+d < 0 or i+d >= len(lintxt):
                        ok = False
                        break
                    if isinstance(p, str) and lintxt[i+d] != p:
                        ok = False
                        break
                    if not isinstance(p, str) and not p.match(lintxt[i+d]):
                        ok = False
                        break
            if ok:
                lintxt[i] = pat.result
    final = []
    for i, m in enumerate(lin):
        if 'audible' in m.props and m.props['audible'] == 'false':
            continue
        elif 'display' in m.props:
            final.append(m.props['display'])
        else:
            final.append(lintxt[i])
    return ' '.join(final).replace('+', ' ').replace('- -', '').replace('- ', '').replace(' -', '')
###PARSING
def tokenize(s):
    ret = []
    add = False
    digraph = False
    for c in s:
        if c in '[]<>$(){}=@%~*':
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
        elif c.isspace():
            add = False
            digraph = False
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
            return isinstance(th, str) and th[0] not in '[]<>$(){}=@|%~*'
        if cur == '~':
            return None
        elif cur == '*':
            return Unknown()
        elif cur == '@':
            return at
        elif cur == '$': #Variable
            ret = Variable.fromstring(rest.pop(0))
            if not ret:
                raise ParseError('Badly formed variable at %s' % loc)
            cond = None
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
            ret.cond = cond
            return ret
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
            if rest[0][0] == '?':
                rest = ['?', rest[0][1:]] + rest[1:]
            if rest[0] not in '*?$~':
                rest = ['~'] + rest
            mode = rest.pop(0)
            name = rest.pop(0)[:-1]
            spots = ['spec', 'mod', 'head', 'comp']
            sub = {'*': [Unknown(), Unknown(), Unknown(), Unknown()],
                   '?': [Variable(name+s, opt=True) for s in spots],
                   '$': [Variable(name+s) for s in spots],
                   '~': [None, None, None, None]}[mode]
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
                        raise ParseError('Unknown lang %d morpheme %s=%s at %s' % (lang, pos, root, loc))
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
    def __init__(self, num, label, args=None, val=None, children=None):
        self.num = num
        self.label = label
        self.args = args or []
        self.arg = '; '.join(self.args)
        self.val = val or ''
        self.vals = [val] if val else []
        self.children = children or []
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
                lobj = ParseLine.fromstring(l.rstrip()[depth*2:], 'line %s of %s' % (i+1, fname))
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
    def avo(self, key, lang, at, default=None): #all val objects
        c = 0
        for ch in self.children:
            if ch.label == key:
                c += 1
                yield toobj(ch.val, lang, ch.num, at)
        if c == 0:
            if default:
                yield toobj(default, lang, self.num, at)
            else:
                raise ParseError('Line %s does not have required child(ren) %s.' % (self.num, key))
def condlist(ch): #parse ch.arg of the form "(a=b; c=d)" into [['a', 'b'], ['c', 'd']]
    ret = []
    for s in ch.args:
        k,v = s.split('=')
        ret.append([k.strip(), v.strip()])
    return ret
def readresult(node, lang, at=None):
    ret = []
    for ch in node.children:
        if ch.label == 'result':
            ret.append(toobj(ch.val, lang, ch.num, at))
        if ch.label == 'setprop':
            a = ['setprop', ' ', ch.arg, False, ch.val]
            if '.' in ch.val:
                v,p = ch.val.split('.')
                if v[0] == '$':
                    a[3] = v[1:]
                elif v == '@':
                    a[3] = ' '
                else:
                    raise ParseError('Cannot interpret variable %s on line %s.' % (v, ch.num))
                a[4] = p
            if '.' in ch.arg:
                v,p = ch.arg.split('.')
                if v[0] == '$':
                    a[1] = v[1:]
                elif v == '@':
                    a[1] = ' '
                else:
                    raise ParseError('Cannot interpret variable %s on line %s.' % (v, ch.num))
                a[2] = p
            ret.append(a)
        if ch.label == 'setdisplay':
            ret.append(['setdisplay', ch.val])
        if ch.label == 'setprops':
            d = {}
            for prop in ch.children:
                d[prop.label] = prop.val
            ret.append(['set', d])
        if ch.label == 'blank':
            ret.append(['setdisplay', ''])
        if ch.label == 'set':
            ret.append(['set', dict(condlist(ch))])
        if ch.label == 'rotate':
            ret.append(['rotate'])
    return ret
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
                c = p.fvo('context', lang, Variable(' '), '@')
                form = p.fvo('structure', lang, m, '@')
                for f in p['form']:
                    fm = Node(lang, root.arg, [f.val])
                    Translation(form, [fm], root.label, [lang, lang], context=c, mode=mode)
                res = readresult(p, lang, m)
                if res:
                    Translation(form, res, root.label, [lang, lang], context=c, mode=mode)
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
                            con.append([int(ch.label), re.compile(ch.val[1:-1])])
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
def loadlang(lang):
    things = ParseLine.fromfile(DATA_PATH + 'langs/%s/lang.txt' % lang)
    ret = Language(lang)
    loadlexicon(lang)
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
                                if len(nodes) != 4:
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
            for ch in th.children:
                if ch.label == 'rule':
                    tf = ch.fvo('form', lang, Variable(' '), '@')
                    res = readresult(ch, lang, None)
                    for tc in ch.avo('context', lang, Variable(' '), '@'):
                        ret.transform.append(Translation(tf, res, 'transform', [lang, lang], context=tc, mode='syntax'))
                elif ch.label == 'rotate':
                    ret.rotate.append(ch.val)
                elif ch.label == 'multirule':
                    layers = []
                    for ly in ch.children:
                        if ly.val and 'form' not in ly:
                            ly.children = [ParseLine(ly.num, 'form', [], ly.val, ly.children)]
                        if ly.label == 'layer?':
                            ly.children.append(ParseLine(-1, 'form', [], '@', []))
                            ly.label = 'layer'
                        if ly.label != 'layer':
                            continue
                        l = []
                        for p in ly['form']:
                            op = [toobj(p.val, lang, p.num, Variable(' '))]
                            op += readresult(p, lang, Variable(' '))
                            l.append(op)
                        layers.append(l)
                    ret.transform.append(MultiRule(layers, 'transform', [lang, lang], mode='syntax'))
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
    ret = LangLink(lfrom, lto)
    if isfile(fname):
        trans = ParseLine.fromfile(fname)
        if trans and trans[0].label != 'stage':
            trans = [ParseLine(-1, 'stage', [], '', trans)]
        for i, stage in enumerate(trans):
            for lex in stage.children:
                if lex.label == 'rule':
                    c = lex.fvo('context', lfrom, Variable(' '), '@')
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
                            c = tr.fvo('context', lfrom, Variable(' '), '@')
                            for t in tr.child_vals('to'):
                                Translation(f, [toobj(t, lto, tr.num, m)], m.children[0], [lfrom, lto], context=c, mode='lex', stage=i)
    return ret
def loadlangset(langs):
    loaded = []
    for l in langs:
        if l not in loaded and l != 0:
            loadlang(l)
            loaded.append(l)
    for lf in loaded:
        for lt in loaded:
            loadtrans(lf, lt)
def addmissing():
    f = open('missing_morphemes.txt')
    lns = list(set(f.readlines()))
    lns.sort()
    lang = ''
    for line in lns:
        if not line: continue
        s = line.split()
        l = s[0][:-1]
        p,r = s[1].split('=')
        if l != lang:
            f.close()
            f = open(DATA_PATH + 'langs/%s/lexicon.txt' % l, 'a')
            f.write('\n\n#Generated from missing_morphemes.txt\n')
            lang = l
            print('Writing to langs/%s/lexicon.txt' % l)
        f.write('%s (%s)\n' % (r,p))
    f.close()
def filltrans(lfrom, lto):
    Language.getormake(lfrom)
    Language.getormake(lto)
    LangLink.getormake(lfrom, lto)
    fname = DATA_PATH + 'langs/%s/translate/%s.txt' % (lfrom, lto)
    have = []
    out = '#Automatically generated from langs/%s/lexicon.txt\n' % lfrom
    if isfile(fname):
        pl = ParseLine.fromfile(fname)
        for l in pl:
            if l.label == 'stage':
                have += [x.label for x in l.children]
            else:
                have.append(l.label)
        out = '\n\n'+out
    for pos in sorted(AllMorphemes[lfrom].keys()):
        for root in sorted(AllMorphemes[lfrom][pos].keys()):
            s = pos + '=' + root
            if s not in have:
                out += s + ': ~\n'
    f = open(fname, 'a')
    f.write(out)
    f.close()
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
    def totext(self):
        if '' in self.trees and self.trees['']:
            return dolinear(movement1(self.trees['']))
        else:
            for k in sorted(self.trees.keys()):
                if self.trees[k]:
                    return dolinear(movement1(self.trees[k]))
            return ''
    def graph(self):
        for k in sorted(self.trees.keys()):
            self.trees[k].flatten()
            f = open(DATA_PATH + 'test/%s-%s.dot' % (self.name, k), 'w')
            f.write(self.trees[k].graph('n', True))
            f.close()
            yield '<h3>%s</h3>' % (k or '(default)'), '%s-%s.dot' % (self.name, k)
def readfile(fname):
    pl = ParseLine.fromfile(fname)
    lang = int(pl[0].firstval('lang'))
    return lang, [Sentence.fromparseline(l, lang) for l in pl[1:]]
def graphtext(infile, outfile):
    gls = []
    f = open(outfile, 'w')
    f.write('<html><head></head><body>\n')
    for s in readfile(infile)[1]:
        f.write('<h1>%s</h1>\n' % s.name)
        for h3, fn in s.graph():
            f.write('%s<img src="%s.svg"></img>\n' % (h3, fn))
            gls.append('test/' + fn)
    f.write('</body></html>')
    f.close()
    proc = Popen(['dot', '-Tsvg', '-O'] + gls, stdin=PIPE, stdout=PIPE, universal_newlines=True)
def translatefile(infile, outfile, tlang, check=False, normalize=True, keepgloss=True, keepmeta=True):
    pl = ParseLine.fromfile(infile)
    flang = int(pl[0].firstval('lang'))
    if isinstance(outfile, str):
        f = open(outfile, 'w')
    else:
        f = outfile
    if keepmeta:
        meta = pl[0]
        for x in meta.children:
            if x.label == 'lang':
                x.vals = [str(tlang)]
    else:
        meta = ParseLine(0, 'metadata', children=[ParseLine(7, 'lang', val=str(tlang))])
    f.write(meta.tofilestr(0))
    for l in pl[1:]:
        f.write(Sentence.fromparseline(l, flang).translate(tlang, check, normalize, keepgloss).toparseline().tofilestr(0))
    if isinstance(outfile, str):
        f.close()
class GeneratorError(Exception):
    pass
def gen(pats, tree, depth, setvars):
    if isinstance(tree, Node):
        r = copy.copy(tree)
        rc = []
        for c in copy.deepcopy(r.children):
            rc.append(gen(pats, c, depth+1, setvars))
        r.children = rc
        return r
    elif isinstance(tree, list):
        return random.choice(tree)
    elif isinstance(tree, Variable):
        if not tree.opt or random.randint(1,100) < 10/depth:
            if tree.label in setvars:
                return setvars[tree.label]
            else:
                newtree = pats[tree.value]
                if isinstance(newtree, list):
                    newtree = random.choice(newtree)
                return gen(pats, newtree, depth+1, setvars)
    elif isinstance(tree, SyntaxPat):
        vrs = {}
        for v in tree.vrs:
            vrs[v.label] = gen(pats, v, depth, {})
        il = []
        for i, cl in enumerate(tree.conds):
            ad = True
            for c in cl:
                if not c.checkset(vrs):
                    ad = False
                    break
            if ad:
                il.append(i)
        if not il:
            raise GeneratorError("None of the conditions for generation rule '%s' could be satisfied." % tree.name)
        return gen(pats, tree.opts[random.choice(il)], depth, vrs)
    else:
        return tree
def make(lang):
    p = lang.getpats()
    return gen(p, p[lang.syntaxstart], 1, {})
class LimitList:
    def __init__(self, few, many):
        self.few = few
        self.many = many
    def each(self):
        for i, x in enumerate(self.few):
            yield x, LimitList(self.few[:i]+self.few[i+1:], self.many)
        for x in self.many:
            yield x, self
    def __len__(self):
        return len(self.few)+len(self.many)
    def __str__(self):
        return str(self.few + self.many)
def makeall(words):
    if not words:
        return []
    lang = Language.getormake(words[0].lang)
    pats = lang.getpats()
    for k in pats:
        if isinstance(pats[k], list):
            many = [x for x in pats[k] if 'audible' in x.props and x.props['audible'] == 'false']
            few = [x for x in words if x.ntype == k]
            pats[k] = LimitList(few, many)
    def product(ls):
        if len(ls) == 0:
            yield ()
        else:
            for x in genall(*ls[0]):
                for y in product(ls[1:]):
                    yield (x,) + y
    def genall(tree, setvars):
        nonlocal pats
        if isinstance(tree, Node):
            for ch in product([[c, setvars] for c in tree.children]):
                yield tree.swapchildren(ch)
        elif isinstance(tree, list):
            #for x in tree:
            #    yield x
            yield from tree
        elif isinstance(tree, Variable):
            if tree.label in setvars:
                yield setvars[tree.label]
            elif isinstance(pats[tree.value], LimitList):
                old = pats[tree.value]
                for r, l in old.each():
                    pats[tree.value] = l
                    yield r
                pats[tree.value] = old
            else:
                #for x in genall(pats[tree.value], setvars):
                #    yield x
                yield from genall(pats[tree.value], setvars)
            if tree.opt:
                yield None
        elif isinstance(tree, SyntaxPat):
            idx = []
            for i, req in enumerate(tree.require):
                if all(len(pats[x]) > 0 for x in req):
                    idx.append(i)
            if idx:
                labs = [v.label for v in tree.vrs]
                for vrs in product([[v, {}] for v in tree.vrs]):
                    dct = dict(zip(labs, vrs))
                    for i in idx:
                        if all(c.checkset(dct) for c in tree.conds[i]):
                            for x in genall(tree.opts[i], dct):
                                yield x
        else:
            yield tree
    #for x in genall(pats[lang.syntaxstart], {}):
    #    yield x
    return genall(pats[lang.syntaxstart], {})
def parse(lang, num, text):
    ret = Sentence(lang, str(num), {}, text)
    hfst = DATA_PATH + 'langs/%d/.generated/parse.hfst' % lang
    pip = subprocess.PIPE
    tok = subprocess.Popen(['hfst-proc', '-x', '-w', hfst], stdin=pip, stdout=pip, universal_newlines=True)
    tokplus = subprocess.Popen(['hfst-proc', '-x', '-w', hfst], stdin=pip, stdout=pip, universal_newlines=True)
    tags = tok.communicate(text+'\n')[0].split('\n\n')
    for x in tokplus.communicate(text.replace(' ', '+') + '\n')[0].split('\n\n'):
        if '+' in x:
            tags.append(x)
    w = []
    for m in Target.iterlex():
        r = re.compile(m.tagify(True))
        for t in tags:
            if r.search(t):
                w.append(m)
    n = 0
    for x in makeall(w):
        if dolinear(movement1(x)) == text:
            n += 1
            ret.trees[str(n)] = x
    return ret
def trans(sen, tlang, checklang=True):
    tr = LangLink.getormake(sen.lang, tlang).translate(sen)
    ret = []
    for s in tr:
        if NORMALIZE_NODES:
            s.nodelang(tlang)
        if not checklang or s.alllang(tlang):
            ret.append(s)
    return ret
if __name__ == '__main__':
    import argparse, sys
    parser = argparse.ArgumentParser(description='Generate, translate, and parse sentences.')
    STDINLINE = 1
    class TranslateAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            if len(values) == 0:
                print('Translate single tree: -t L1 [SRC] L2 [DEST]\n' + \
                      '  read a tree from SRC (leave blank for stdin)\n' + \
                      '  translate from L1 to L2\n' + \
                      '  output to DEST (leave blank for stdout)\n' + \
                      'Translate .pdtxt file: -t SRC LANG [DEST]\n' + \
                      '  translate contents of file SRC to LANG\n' + \
                      '  output to DEST (leave blank for stdout)')
            elif values[0].isnumeric():
                flang = int(values.pop(0))
                if values[0].isnumeric():
                    line = sys.stdin.readline()
                    where = 'standard input line %s' % STDINLINE
                    STDINLINE += 1
                else:
                    where = values.pop(0)
                    f = open(where)
                    where += ' line 1'
                    line = f.readline()
                    f.close()
                tree = toobj(line, flang, where)
                tr = trans(tree, int(values.pop(0)))
                if values:
                    f = open(values[0], 'w')
                    f.write(tr.writecompile())
                    f.close()
                else:
                    print(tr.writecompile())
            else:
                if len(values) >= 3:
                    translatefile(values[0], values[2], int(values[1]))
                else:
                    translatefile(values[0], sys.stdout, int(values[1]))
    class GenerateAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            lang = Language.getormake(int(values.pop(0)))
            sen = make(lang)
            if values:
                f = open(values[0], 'w')
                f.write(sen.writecompile())
                f.close()
            else:
                print(sen.writecompile())
    class ParseAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            if values[0].isnumeric():
                lines = [sys.stdin.readline()]
            else:
                f = open(values.pop(0))
                lines = f.readlines()
                f.close()
            lang = int(values.pop(0))
            if values:
                out = open(values[0], 'w')
            else:
                out = sys.stdout
            for i, t in enumerate(lines):
                l = t.strip()
                if l:
                    out.write(parse(lang, i+1, l).toparseline().tofilestr(0))
            if values:
                out.close()
    class DisplayAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            if values[0].isnumeric() or (len(values) > 1 and values[1].isnumeric()):
                if values[0].isnumeric():
                    line = sys.stdin.readline()
                    where = 'standard input line %s' % STDINLINE
                    STDINLINE += 1
                else:
                    where = values.pop(0)
                    f = open(where)
                    where += ' line 1'
                    line = f.readline()
                    f.close()
                lang = int(values.pop(0))
                if values:
                    f = open(values[0], 'w')
                else:
                    f = sys.stdout
                f.write(dolinear(movement1(toobj(line, lang, where))))
                if values:
                    f.close()
            else:
                lines = readfile(values.pop(0))[1]
                if values:
                    f = open(values[0], 'w')
                else:
                    f = sys.stdout
                for l in lines:
                    f.write(l.totext() + '\n')
                if values:
                    f.close()
    class BlankAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            if values:
                filltrans(int(values[0]), int(values[1]))
            else:
                addmissing()
    class SetGlobal(argparse.Action):
        #Horrible? Yes. Effective? Also yes.
        def __init__(self, *args, **kwargs):
            self.command = 'global {0}; {0} = {1}'.format(*kwargs['todo'])
            del kwargs['todo']
            kwargs['nargs'] = 0
            argparse.Action.__init__(self, *args, **kwargs)
        def __call__(self, parser, namespace, values, option_string=None):
            exec(self.command)
    parser.add_argument('-t', '--translate', type=str, nargs='*', action=TranslateAction, metavar='ARG', help="Translate trees (run 'doodle.py -t' for detailed help)")
    parser.add_argument('-g', '--generate', type=str, nargs='+', action=GenerateAction, metavar=('LANG', 'DEST'), help='Randomly generate a tree in LANG and output to DEST or stdout')
    parser.add_argument('-p', '--parse', type=str, nargs='+', action=ParseAction, metavar=('[SRC] LANG', 'DEST'), help='Attempt to parse SRC or next line of std into trees in LANG, output to DEST or stdout')
    parser.add_argument('-d', '--display', type=str, nargs='+', action=DisplayAction, metavar=('SRC [LANG]', 'DEST'), help='Get trees from SRC or stdin, convert to text and output to DEST or stdout')
    parser.add_argument('-F', '--flatten', action=SetGlobal, todo=('FLAT', True), help='Start flattening phrases into single nodes')
    parser.add_argument('-DF', '--dont-flatten', action=SetGlobal, todo=('FLAT', False), help='Stop flattening phrases')
    parser.add_argument('-N', '--normalize', action=SetGlobal, todo=('NORMALIZE_NODES', True), help='Start ignoring the language of non-lexical nodes')
    parser.add_argument('-DN', '--dont-normalize', action=SetGlobal, todo=('NORMALIZE_NODES', False), help='Stop ignoring node language (ignoring is the default)')
    parser.add_argument('-U', '--use-unknown', action=SetGlobal, todo=('UNKNOWN_MORPH', '"CREATE_AND_LOG"'), help='Begin logging unknown morphemes to missing_morphemes.txt, don\'t error')
    parser.add_argument('-am', '--add-missing', nargs=0, action=BlankAction, help='Append everything in missing_morphemes.txt to the relevant lexicon files')
    parser.add_argument('-ft', '--fill-trans', nargs=2, action=BlankAction, metavar=('LANG1', 'LANG2'), help='Add blank entries in translation file from LANG1 to LANG2 for any morpheme not already listed')
    args = parser.parse_args()
