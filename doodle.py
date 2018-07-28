#!/usr/bin/env python3
import re, itertools, random, copy, os
from collections import defaultdict
from subprocess import Popen, PIPE
from os.path import isfile
from types import SimpleNamespace
Globals = SimpleNamespace(path=os.path.abspath(__file__)[:-9], unknown_error=True, flat=False, partial=True, keepmeta=True)
#path: The directory containing the program
#unknown_error: Should an error be raised when trying parse a non-existent morpheme?
#flat: Read |[XP] as [XP a b c d] rather than [XP a [Xmod b [Xbar c d]]]
#partial: Return incomplete translations
#keepmeta: Copy glosses and metadata from input to output

class PatternElement:
    CheckType = False
    def __init__(self, ntype, props=None):
        self.ntype = ntype
        self.props = props
    def __getitem__(self, key):
        return self.props[key]
    def __setitem__(self, key, value):
        self.props[key] = value
    def __contains__(self, key):
        return key in self.props
    def __str__(self):
        return type(self).__name__ + self.ntype + str(self.props)
    def __repr__(self):
        return self.__str__()
    def matchcondlist(self, cndls):
        return all(k in self and self[k] == v for k,v in cndls)
    def getvars(self, tree, vrs):
        if self.CheckType and type(tree) != type(self):
            vrs[' failed'] = 'type'
            return vrs
        if tree == None:
            vrs[' failed'] = 'tree is None'
            return vrs
        if self.ntype and self.ntype != tree.ntype:
            vrs[' failed'] = 'ntype'
            return vrs
        for p in self.props:
            if p not in tree:
                vrs[' failed'] = 'nonexistent property'
                return vrs
            if isinstance(self.props[p], str) or self.props[p] == None:
                if self.props[p] != tree[p]:
                    vrs[' failed'] = 'property value mismatch'
                    return vrs
            else:
                self.props[p].getvars(tree[p], vrs)
                if vrs[' failed']:
                    return vrs
        return vrs
    def putvars(self, vrs):
        return self
    def check(self, tree):
        return self.getvars(tree, {' failed': False}) == False
class DataElement(PatternElement):
    #thing that can occur in a sentence (rather than just a rule)
    CheckType = True
    def trans(self, tr):
        vrs = tr.context.getvars(self, {' failed': False})
        if vrs[' failed'] or not isinstance(vrs[' '], DataElement):
            return []
        vrs = tr.form.getvars(vrs[' '], vrs)
        if vrs[' failed']:
            return []
        for act in tr.result:
            applyrule(act, vrs)
        return copy.deepcopy(tr.context).putvars(vrs)
    def transmulti(self, tr):
        if tr.ntypelist and self.ntype not in tr.ntypelist:
            return []
        vrs = {' failed': False, ' ': self}
        path = []
        for l in tr.layers:
            for i, f in enumerate(l):
                vrs2 = f[0].getvars(vrs[' '], vrs.copy())
                if not vrs2[' failed']:
                    vrs = vrs2
                    path.append(f[1:])
                    break
            else:
                return []
        for result in reversed(path):
            for act in result:
                applyrule(act, vrs)
        return vrs[' ']
    def transform(self, pats, returnself=True):
        if len(pats) > 0:
            nodes = []
            retstr = ['[]']
            for i, p in enumerate(pats):
                if isinstance(p, Translation):
                    x = self.trans(p)
                else:
                    x = self.transmulti(p)
                s = str(x)
                if s not in retstr:
                    nodes.append(x)
                    retstr.append(s)
            if not nodes and returnself:
                nodes = [self]
            return nodes
        elif returnself:
            return [self]
        else:
            return []
###VARIABLES
class Variable(PatternElement):
    pattern = re.compile('^\\$?([^:?!+\\.&]*):?([^:?!+\\.&]*)\\.?([^:?!+\\.&]*)([?!+&]*)$')
    def __init__(self, label, ntype=None, prop=None, opt=False, neg=False, group=False, descend=False, cond=None):
        self.label = label
        self.ntype = ntype
        self.prop = prop
        self.opt = opt
        self.neg = neg
        self.group = group
        self.descend = descend
        self.cond = cond
        self.props = {}
    def fromstring(s):
        m = Variable.pattern.match(s)
        if m:
            g = m.groups()
            return Variable(g[0], g[1], g[2], '?' in g[3], '!' in g[3], '+' in g[3], '&' in g[3])
        else:
            print('no match with %s' % s)
    def checkset(self, vrs):
        if self.label not in vrs:
            return self.neg or self.opt
        if self.group:
            return all(self.check(x) for x in vrs[self.label])
        else:
            return self.check(vrs[self.label])
    def check(self, v):
        if self.neg:
            return v == None
        if v == None:
            return self.opt
        if not PatternElement.check(self, v):
            return False
        if self.cond:
            return self.cond.check(v)
        return True
    def retrieve(self, vrs):
        if self.label in vrs:
            node = vrs[self.label]
            if not node:
                if not self.prop or self.opt:
                    return node
                else:
                    raise Exception('Variable %s cannot retrieve properties from None.' % self)
            if self.descend:
                while True:
                    if node.ntype == 'conjP':
                        node = node.children[0]
                    elif node.ntype[-1] == 'P' and len(node.children) in [2,4]:
                        if len(node.children) == 4:
                            node = node.children[2]
                        else:
                            node = node.children[1].children[1].children[0]
                    else:
                        break
            if self.prop:
                if self.prop in node:
                    return node[self.prop]
                elif self.opt:
                    return None
                else:
                    raise Exception('Error with variable %s and node %s, property does not exist.' % (self, node))
            else:
                return node
        elif self.opt:
            return None
        else:
            print(vrs)
            print(self.label)
            raise Exception('Variable %s does not exist.' % self)
    def place(self, vrs, val):
        if self.label in vrs and vrs[self.label]:
            if self.group:
                for v in vrs[self.label]:
                    v.props[self.prop] = val
            else:
                vrs[self.label].props[self.prop] = val
    def getvars(self, node, vrs):
        PatternElement.getvars(self, node, vrs)
        if not vrs[' failed'] and self.cond:
            self.cond.getvars(node)
        if node == None and (self.opt or self.neg):
            vrs[' failed'] = False
        if self.neg and node:
            vrs[' failed'] = 'node is not None'
        if not vrs[' failed']:
            if self.label in vrs:
                if self.group:
                    vrs[self.label].append(node)
                else:
                    #perhaps overwriting the previous value is the wrong approach
                    #but since this hasn't yet come up in practice I'm inclined to ignore it
                    #  -D.S. 2018-07-27
                    vrs[self.label] = node
            else:
                vrs[self.label] = [node] if self.group else node
        return vrs
    def putvars(self, vrs):
        if self.label not in vrs:
            return None
        else:
            return vrs[self.label]
    def __str__(self):
        return '$'+self.label + \
               ((':'+self.ntype) if self.ntype else '') + \
               (('.'+self.prop) if self.prop else '') + \
               ('?' if self.opt else '') + \
               ('!' if self.neg else '') + \
               ('+' if self.group else '') + \
               ('&' if self.descend else '') + \
               (('(' + str(self.cond) + ')') if self.cond else '')
    def __deepcopy__(self, memo):
        return self
        #Variables aren't modified, so we don't care about copying them
class Unknown(Variable):
    count = 0
    def __init__(self):
        Variable.__init__(self, ' '+str(Unknown.count), opt=True)
        Unknown.count += 1
    def getvars(self, tree, vrs):
        vrs[self.label] = tree
        return vrs
    def check(self, v):
        return True
    def __str__(self):
        return '*'
class Option(list):
    pass
###DATA STRUCTURES
class Morpheme(DataElement):
    __AllMorphemes = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: None)))
    def __init__(self, lang, ntype, root, props=None, isref=False, loc=None):
        self.lang = lang
        self.ntype = ntype
        self.root = root
        self.props = props or {}
        self.isref = isref
        if not isref:
            roots = [root]
            if 'searchkey' in self.props:
                roots.append(self.props['searchkey'])
            pos = [ntype]
            if 'altpos' in self.props:
                pos.append(self.props['altpos'])
            for p in pos:
                for r in roots:
                    Morpheme.__AllMorphemes[lang][p][r] = self
        else:
            try:
                Morpheme.get(lang, ntype, root, loc or '(see stacktrace)')
            except:
                if Globals.unknown_error:
                    raise
                else:
                    f = open(Globals.path + 'missing_morphemes.txt', 'a')
                    f.write(str(lang) + ': ' + ntype + '=' + root + '\n')
                    f.close()
    def __str__(self):
        return self.ntype + '=' + self.root
    def getref(self):
        return Morpheme(self.lang, self.ntype, self.root, isref=True)
    def itermorph(lang):
        return Morpheme.__AllMorphemes[lang]
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
        else:
            format = '{root[0]}<%s>' % self.ntype
            tagset = {}
            defaults = {}
        tags = {'root': self.root.split('#')[0].split(lang.tags_rootsplit)}
        if 'root' in self:
            tags['root'] = self['root'].split(lang.tags_rootsplit)
        for tg in tagset:
            if isinstance(tagset[tg], str):
                if tagset[tg] in self:
                    t = self[tagset[tg]]
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
        ret = format.format(**tags) or self.root
        if regex:
            ret = '\t' + ret.replace('+', '\\+')
        return ret
    def get(lang, ntype, root, loc):
        if lang not in Morpheme.__AllMorphemes:
            raise Exception('Error at %s: Language %s not loaded.' % (loc, lang))
        else:
            d = Morpheme.__AllMorphemes[lang]
            if ntype not in d:
                raise Exception('Error at %s: Non-existent part of speech %s' % (loc, ntype))
            else:
                d = d[ntype]
                if root not in d:
                    raise Exception('Error at %s: Undefined morpheme %s=%s' % (loc, ntype, root))
                else:
                    return d[root]
    def __getitem__(self, key):
        if key in self.props:
            return self.props[key]
        elif self.isref:
            ref = Morpheme.get(self.lang, self.ntype, self.root, None)
            if key in ref.props:
                return ref.props[key]
        else:
            raise KeyError('Morpheme %s does not have property %s.' % (self, key))
    def __contains__(self, key):
        if key in self.props:
            return True
        if self.isref:
            return key in Morpheme.get(self.lang, self.ntype, self.root, None).props
        return False
    def getvars(self, tree, vrs):
        PatternElement.getvars(self, tree, vrs)
        if not vrs[' failed']:
            if self.lang != tree.lang or self.root != tree.root:
                vrs[' failed'] = 'lang or root'
        return vrs
    def putvars(self, vrs):
        return self
class Node(DataElement):
    def __init__(self, ntype, children, props=None):
        self.ntype = ntype
        self.children = children
        self.props = props or {}
        self.rotate = False
    def swapchildren(self, ls):
        return Node(self.ntype, ls, self.props.copy())
    def getvars(self, tree, vrs):
        PatternElement.getvars(self, tree, vrs)
        if not vrs[' failed']:
            if len(self.children) != len(tree.children):
                vrs[' failed'] = 'number of children'
                return vrs
            for s,t in zip(self.children, tree.children):
                if s:
                    s.getvars(t, vrs)
                elif t:
                    vrs[' failed'] = 'non-null child'
                if vrs[' failed']:
                    return vrs
        return vrs
    def putvars(self, vrs):
        ch = []
        for c in self.children:
            try:
                a = c.putvars(vrs)
                if isinstance(a, list):
                    ch += a
                else:
                    ch.append(a)
            except AttributeError:
                ch.append(c)
        return Node(self.ntype, ch, self.props.copy())
    def transform(self, pats, returnself=True):
        chs = []
        for c in self.children:
            if c:
                chs.append(c.transform(pats, True))
            else:
                chs.append([c])
        swap = map(lambda x: self.swapchildren(list(x)), itertools.product(*chs))
        ret = list(itertools.chain.from_iterable(map(lambda x: DataElement.transform(x, pats, True), swap)))
        if returnself and not ret:
            ret = [self]
        return ret
    def __str__(self):
        if isinstance(self.children, list):
            s = '[' + ' '.join([str(x) for x in self.children]) + ']'
        else:
            s = str(self.children)
        return '%s%s%s' % (self.ntype, s, str(self.props))
    def debug(self, depth=0):
        ls = [('  '*depth) + ('%s[' % self.ntype)]
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
            self.children = [ch[0], Node(n+'mod', [ch[1], Node(n+'bar', [ch[2], ch[3]])])]
    def rotated(self, lang):
        return self.ntype in Language.get(lang).rotate != self.rotate
    def tagify_all(self, lang):
        rev = self.rotated(lang)
        ret = []
        for c in self.children:
            if isinstance(c, Node):
                a = c.tagify_all()
            elif isinstance(c, Morpheme):
                a = [c.tagify()]
            else:
                a = [c] if c else []
            if rev:
                ret = a + ret
            else:
                ret += a
        return ret
    def linear(self, lang):
        l = []
        for c in self.children:
            if isinstance(c, Node):
                l.append(c.linear(lang))
            elif c:
                l.append([c])
        if self.rotated(lang):
            l.reverse()
        r = []
        for c in l:
            r += c
        return r
    def iternest(self):
        yield self
        for ch in self.children:
            if isinstance(ch, Node):
                yield from ch.iternest()
            else:
                yield ch
    def roots(self):
        ret = []
        for ch in self.children:
            if isinstance(ch, Morpheme):
                ret.append(ch.root)
            elif isinstance(ch, Node):
                ret += ch.roots()
        return ret
    def alllang(self, lang):
        for n in self.iternest():
            if isinstance(n, Morpheme) and n.lang != lang:
                return False
        return True
class UnorderedCollector(PatternElement):
    def __init__(self, ntype, children):
        self.ntype = ntype
        self.children = children
        self.props = {}
    def getvars(self, tree, vrs):
        PatternElement.getvars(self, tree, vrs)
        if not vrs[' failed']:
            if not isinstance(tree, Node):
                vrs[' failed'] = 'UnorderedCollector only matches Nodes'
                return vrs
            found = set()
            for c in tree.children:
                if not c:
                    continue
                for i, v in enumerate(self.children):
                    v.getvars(c, vrs)
                    if not vrs[' failed']:
                        found.add(i)
                        break
                    else:
                        vrs[' failed'] = False
                else:
                    vrs[' failed'] = 'no matching variables found for %s' % c
                    break
            else:
                for i, v in enumerate(self.children):
                    if isinstance(v, Variable) and v.label not in vrs:
                        if v.opt:
                            vrs[v.label] = None
                            found.add(i)
                        else:
                            vrs[' failed'] = 'unmatched variable'
                            break
            if len(found) < len(self.children):
                vrs[' failed'] = 'unmatched element'
        return vrs
    def putvars(self, vrs):
        ch = []
        for v in self.children:
            a = v.putvars(vrs)
            if isinstance(a, list):
                ch += a
            else:
                ch.append(a)
        return Node(self.ntype, ch)
    def __str__(self):
        return '<%s %s>' % (self.ntype, ' '.join(str(x) for x in self.children))
###TRANSFORMATIONS
class Rule:
    def __init__(self, langs, category='', mode='syntax', stage=0, name=''):
        self.langs = langs
        self.category = category
        self.mode = mode
        self.stage = stage
        self.name = name
        if self.langs[0] == self.langs[1]:
            l = Language.getormake(self.langs[0])
            if mode == 'linear':
                l.linear[category].append(self)
            elif mode == 'linear-text':
                l.lineartext[category].append(self)
            else:
                x = len(l.movement[category])
                l.movement[category].append(self)
                assert(len(l.movement[category]) > x)
        else:
            l = LangLink.getormake(self.langs[0], self.langs[1])
            if mode == 'syntax':
                l.syntax.append(self)
            else:
                l.pats[category].append(self)
class Translation(Rule):
    def __init__(self, form, result, category, langs, context=None, mode='syntax', stage=0, name=''):
        self.form = form
        self.result = result
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
        Rule.__init__(self, langs, category, mode, stage, name)
    def __str__(self):
        return '{%s => %s}%s' % (self.form, self.result, self.roots)
    def __repr__(self):
        return self.__str__()
class MultiRule(Rule):
    def __init__(self, layers, category, langs, mode='syntax', stage=0, name=''):
        self.layers = layers
        self.roots = [] #roots of all morphemes in form
        self.rootset = set(self.roots)
        self.resultroots = []
        self.resultrootset = set(self.resultroots)
        self.addedroots = self.resultrootset - self.rootset
        self.ntypelist = []
        if all(isinstance(x[0], Node) for x in layers[0]):
            self.ntypelist = [x[0].ntype for x in layers[0]]
        Rule.__init__(self, langs, category, mode, stage, name)
def applyrule(rule, vrs):
    if isinstance(rule, DataElement) or isinstance(rule, UnorderedCollector):
        vrs[' '] = rule.putvars(vrs)
    elif isinstance(rule, list):
        if rule[0] == 'setlang':
            vrs[' '].lang = rule[1]
        elif rule[0] == 'setdisplay':
            vrs[' '].props['display'] = rule[1]
        elif rule[0] == 'set':
            vrs[' '].props.update(rule[1])
        elif rule[0] == 'setprop':
            rule[1].place(vrs, rule[2].retrieve(vrs))
        elif rule[0] == 'rotate':
            vrs[' '].rotate = True
        elif rule[0] == 'makevar':
            vrs[rule[1]] = copy.deepcopy(rule[2])
        elif rule[0] == 'order':
            ch = []
            for v in rule[2:]:
                if v.label in vrs and vrs[v.label]:
                    ch.append(vrs[v.label])
            vrs[' '] = Node(rule[1], ch)
        elif rule[0] == 'node':
            vrs[' '] = toobj(*rule[1:], at=vrs[' ']).putvars(vrs)
        elif rule[0] == 'cond':
            for op in rule[1:]:
                if all(v.checkset(vrs) for v in op[0]):
                    for r in op[1:]:
                        applyrule(r, vrs)
                    break
        elif rule[0] == 'distribute':
            src = rule[1]
            dst = rule[2]
            try:
                val = vrs[rule[3].label][src]
            except:
                print(vrs[' '])
                print(rule)
                raise
            for l in rule[4:]:
                nv = None
                for v in (l if isinstance(l, list) else [l]):
                    if v.label in vrs and vrs[v.label]:
                        vrs[v.label].props[dst] = val
                        if src in vrs[v.label]:
                            nv = vrs[v.label][src]
                if nv:
                    val = nv
        elif rule[0] == 'log':
            print(rule[1].retrieve(vrs))
        elif rule[0] == 'print':
            print(rule[1])
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
        self.rotate = []
        self.syntaxstart = None
        #Movement
        self.movement = defaultdict(list)
        self.linear = defaultdict(list)
        self.lineartext = defaultdict(list)
        #Transducer
        self.lexc = ''
        self.lexc_lexicons = []
        self.tags = []
        self.tags_rootsplit = '' #for cases where it's easiest to have tags between parts of the root
        self.morph_mode = '' #hfst or lttoolbox
        Language.__alllangs[lang] = self
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
        r.update(self.syntax)
        for k, v in Morpheme.itermorph(self.lang).items():
            r[k] = list(v.values())
        return r
    def movefind(self, roots):
        s = set(roots + [''])
        ret = defaultdict(list)
        for r in s:
            for p in self.movement[r]:
                if p.rootset < s:
                    ret[p.stage].append(p)
        return [ret[k] for k in sorted(ret.keys())]
    def domovement(self, sen):
        pats = self.movefind(sen.roots())
        tr = [sen]
        for p in pats:
            ntr = []
            for s in tr:
                ntr += s.transform(p, False)
            tr = ntr or tr
        return tr
    def totext(self, sen):
        return dolinear(self.domovement(sen)[0], self.lang)
    def allnames():
        return [(x, Language.__alllangs[x].name) for x in sorted(Language.__alllangs.keys())]
    def iterlex(self):
        dct = Morpheme.itermorph(self.lang)
        for ntype in dct:
            for root in dct[ntype]:
                yield dct[ntype][root]
class LangLink:
    __alllinks = {}
    def __init__(self, fromlang, tolang):
        self.fromlang = fromlang
        self.tolang = tolang
        self.syntax = []
        self.pats = defaultdict(list)
        LangLink.__alllinks['%s-%s' % (fromlang, tolang)] = self
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
        tr = [sen]
        for p in pats:
            ntr = []
            for s in tr:
                ntr += s.transform(p, False)
            if ntr:
                tr = ntr
        return tr
def hfst(tagstrs, lang):
    mode = Language.getormake(lang).morph_mode
    if mode == 'hfst':
        proc = Popen(['hfst-lookup', '-q', '-b', '0', '-i', Globals.path + 'langs/%d/.generated/gen.hfst' % lang], stdin=PIPE, stdout=PIPE, universal_newlines=True)
        ls = proc.communicate('\n'.join(tagstrs))
        if ls[0]:
            ret = [x.split('\t')[1] for x in ls[0].strip().split('\n\n')]
        else:
            ret = []
    elif mode == 'lttoolbox':
        proc = Popen(['lt-proc', '-g', Globals.path + 'langs/%d/.generated/gen.bin' % lang], stdin=PIPE, stdout=PIPE, universal_newlines=True)
        ls = proc.communicate('\n'.join(['^%s$' % t for t in tagstrs]))[0]
        ret = [x[1:] if x[0] == '~' else x for x in ls.split('\n')]
        #print('\n' + ' '.join(t for t,r in zip(tagstrs, ret) if r[0] == '#'))
    else:
        raise Exception('Unknown morphology mode %s' % mode)
    return ret
def dolinear(sen, _lang):
    lin = sen.linear(_lang)
    lang = Language.getormake(_lang)
    for i, m in enumerate(lin):
        for pat in lang.linear[m.root]:
            if not pat.form.check(m):
                continue
            if isinstance(pat.context, list):
                for d, p in pat.context:
                    if i+d < 0 or i+d >= len(lin):
                        break
                    if p.check(lin[i+d]):
                        break
                else:
                    for d, r in pat.result:
                        if r == 'inaudible':
                            lin[i+d]['audible'] = 'false'
                        elif isinstance(r, list) and r[0] == 'display':
                            lin[i+d]['display'] = r[1]
                        else:
                            lin[i+d] = r
    lintxt = hfst([x.tagify() for x in lin], _lang)
    for i, m in enumerate(lin):
        for pat in lang.lineartext[m.root]:
            if isinstance(pat.context, list):
                for d, p in pat.context:
                    if i+d < 0 or i+d >= len(lintxt):
                        break
                    if isinstance(p, str) and lintxt[i+d] != p:
                        break
                    if not isinstance(p, str) and not p.match(lintxt[i+d]):
                        break
                else:
                    lintxt[i] = pat.result
    final = []
    for i, m in enumerate(lin):
        if 'audible' in m and m['audible'] == 'false':
            continue
        elif 'display' in m:
            final.append(m['display'])
        else:
            final.append(lintxt[i])
    return ' '.join(final).replace('+', ' ').replace('- -', '').replace('- ', '').replace(' -', '')
###PARSING
def tokenize(s):
    ret = []
    add = False
    digraph = False
    for c in s:
        if c in '[]<>$(){}=@~*':
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
    Language.getormake(lang)
    rest = tokenize(s)
    def destring():
        nonlocal rest
        cur = rest.pop(0)
        def ok(th):
            return th[0] not in '[]<>$(){}=@|~*'
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
            if rest and rest[0] == '(':
                rest.pop(0)
                if len(rest) >= 2 and rest[1] == ')':
                    ret[rest.pop(0)] = Unknown()
                    rest.pop(0)
                elif len(rest) >= 4 and ok(rest[0]) and rest[1] == '=' and ok(rest[2]) and rest[3] == ')':
                    ret[rest[0]] = rest[2]
                    rest = rest[4:]
                else:
                    ret.cond = destring()
                    if rest[0] != ')':
                        raise ParseError('Badly formed variable condition on line %s (remainder was %s).' % (loc, rest))
                    rest.pop(0)
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
            return Node(ntype, ch, d)
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
            while rest and rest[0] != ']':
                ch.append(destring())
            if rest:
                rest.pop(0)
            else:
                raise ParseError('Syntax mode is missing closing bracket at %s' % loc)
            if len(ch) == 0: #nothing
                ch.insert(0, sub[2]) #insert head
            if len(ch) == 1: #just head
                ch.insert(1, sub[3]) #insert comp
            if len(ch) == 2: #head and comp
                ch.insert(0, sub[0]) #insert spec
            if len(ch) == 3: #spec, head, and comp
                ch.insert(1, sub[1]) #insert mod
            d = {}
            if rest and rest[0] == '{':
                d = destring()
            if Globals.flat:
                return Node(name+'P', ch, d)
            else:
                bar = Node(name+'bar', ch[2:])
                mod = Node(name+'mod', [ch[1], bar])
                return Node(name+'P', [ch[0], mod], d)
        elif cur == '<': #UnorderedCollector
            ntype = rest.pop(0)
            ch = []
            while rest and rest[0] != '>':
                ch.append(destring())
            if not rest:
                raise ParseError('Incomplete Unordered Collector, missing > at %s' % loc)
            rest.pop(0)
            return UnorderedCollector(ntype, ch)
        elif cur == '{': #props pattern
            d = {}
            while rest[0] != '}':
                p = rest.pop(0)
                assert(rest.pop(0) == '=')
                d[p] = rest.pop(0)
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
                d = {}
                if rest and rest[0] == '{':
                    d = destring()
                return Morpheme(lang, pos, root, isref=True, props=d)
            else:
                rest = ['$', ':'+cur] + rest
                return destring()
    try:
        ret = destring()
    except:
        print('original line: %s' % s)
        print('problem on line %s, add more checks, unparsed remainder was %s' % (loc, rest))
        raise
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
        elif ch.label == 'setprop':
            a = ['setprop']
            if '$' in ch.arg or '@' in ch.arg:
                a.append(Variable.fromstring(ch.arg.replace('@', ' ')))
            else:
                a.append(Variable.fromstring('$ .'+ch.arg))
            if a[1] == None:
                raise ParseError('Cannot interpret variable %s on line %s.' % (v, ch.arg))
            if '$' in ch.val or '@' in ch.val:
                a.append(Variable.fromstring(ch.val.replace('@', ' ')))
            else:
                a.append(Variable.fromstring('$ .'+ch.val))
            if a[2] == None:
                raise ParseError('Cannot interpret variable %s on line %s.' % (v, ch.val))
            ret.append(a)
        elif ch.label == 'setval':
            a = ['setprop', None, ch.val]
            if '$' in ch.arg or '@' in ch.arg:
                a[1] = Variable.fromstring(ch.arg.replace('@', ' '))
            else:
                a[1] = Variable.fromstring('$ .'+ch.arg)
            ret.append(a)
        elif ch.label == 'setdisplay':
            ret.append(['setdisplay', ch.val])
        elif ch.label == 'setprops':
            d = {}
            for prop in ch.children:
                d[prop.label] = prop.val
            ret.append(['set', d])
        elif ch.label == 'blank':
            ret.append(['setdisplay', ''])
        elif ch.label == 'set':
            ret.append(['set', dict(condlist(ch))])
        elif ch.label == 'rotate':
            ret.append(['rotate'])
        elif ch.label == 'cond':
            com = ['cond']
            for op in ch.children:
                if op.label == 'option':
                    com.append([[toobj(x, lang, op.num, at) for x in op.args]] + readresult(op, lang, at))
            ret.append(com)
        elif ch.label == 'if':
            ret.append(['cond', [[toobj(x, lang, ch.num, at) for x in ch.args]] + readresult(ch, lang, at)])
        elif ch.label == 'distribute':
            ret.append(['distribute'] + ch.args + [toobj(x, lang, ch.num, at) for x in ch.vals])
        elif ch.label == 'order':
            ret.append(['order', ch.arg] + [toobj(x, lang, ch.num, at) for x in ch.vals])
        elif ch.label == 'log':
            ret.append(['log', toobj(ch.val, lang, ch.num, at)])
        elif ch.label == 'print':
            ret.append(['print', ch.val])
    return ret
def readrule(node, lfrom, _lto, mode, category, _stage):
    if 'samelang' in node:
        lto = lfrom
    else:
        lto = _lto
    if 'stage' in node:
        stage = int(node.firstval('stage'))
    elif node.arg:
        stage = int(node.arg)
    else:
        stage = _stage
    if node.label == 'rule':
        con = node.fvo('context', lfrom, Variable(' '), '@')
        form = node.fvo('form', lfrom, Variable(' '), '@')
        res = readresult(node, lto, None)
        return Translation(form, res, category, [lfrom, lto], context=con, mode=mode, stage=stage, name=node.val)
    elif node.label == 'multirule':
        layers = []
        for ly in node.children:
            if ly.val and 'form' not in ly:
                ly.children = [ParseLine(ly.num, 'form', [], ly.val, ly.children)]
            if ly.label == 'layer?':
                ly.children.append(ParseLine(-1, 'form', [], '@', [ParseLine(-1, 'result', [], '@', [])]))
                ly.label = 'layer'
            if ly.label != 'layer':
                continue
            l = []
            for p in ly['form']:
                op = [toobj(p.val, lfrom, p.num, Variable(' '))]
                op += readresult(p, lfrom, Variable(' '))
                l.append(op)
            layers.append(l)
        return MultiRule(layers, category, [lfrom, lto], mode=mode, stage=stage, name=node.val)
    elif node.label == 'linear':
        pass
    elif node.label == 'linear-text':
        pass
def loadlexicon(lang):
    rootslist = ParseLine.fromfile(Globals.path + 'langs/%s/lexicon.txt' % lang)
    defaults = defaultdict(lambda: defaultdict(dict))
    if rootslist[0].label == 'defaults':
        for pat in rootslist.pop(0).children:
            defaults[pat.label][pat.val] = {ch.label:ch.val for ch in pat.children}
    for root in rootslist:
        m = Morpheme(lang, root.arg, root.label, isref=False, props=defaults[root.arg][root.val].copy())
        if 'output' not in m.props:
            m.props['output'] = []
        for p in root.children:
            if p.label in ['rule', 'multirule']:
                readrule(p, lang, lang, 'lex', root.label, 1)
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
                    elif 'display' in ch:
                        res.append([idx, ['display', ch.firstval('display')]])
                Translation(m.getref(), res, root.label, [lang, lang], context=con, mode='linear')
            elif p.label == 'linear-text':
                con = []
                for ch in p.children:
                    if ch.label.isnumeric() or (ch.label[0] == '-' and ch.label[1:].isnumeric()):
                        if ch.val[0] == '/' and ch.val[-1] == '/':
                            con.append([int(ch.label), re.compile(ch.val[1:-1])])
                        else:
                            con.append([int(ch.label), ch.val])
                Translation(m, p.val, root.label, [lang, lang], context=con, mode='linear-text')
            else:
                m.props[p.label] = p.val
        for pos in root['altpos']:
            p2 = m.props.copy()
            for l in pos.children:
                p2[l.label] = l.val
            Morpheme(lang, pos.val, m.root, props=p2, isref=False)
def loadlang(lang):
    things = ParseLine.fromfile(Globals.path + 'langs/%s/lang.txt' % lang)
    ret = Language(lang)
    loadlexicon(lang)
    for th in things:
        if th.label == 'syntax':
            for ch in th.children:
                if ch.label == 'start-with':
                    ret.syntaxstart = ch.val
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
                            conds.append([toobj(x, lang, op.num) for x in op.args])
                            ops.append(node)
                            req = []
                            for r in op['require']:
                                req.append(r.val)
                            require.append(req)
                        ret.syntax[ty.label] = SyntaxPat(ty.label, conds, ops, vrs, require)
        if th.label == 'transform':
            for ch in th.children:
                if ch.label == 'rotate':
                    ret.rotate.append(ch.val)
                else:
                    readrule(ch, lang, lang, 'syntax', '', 0)
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
    fname = Globals.path + 'langs/%s/translate/%s.txt' % (lfrom, lto)
    ret = LangLink(lfrom, lto)
    if isfile(fname):
        trans = ParseLine.fromfile(fname)
        if trans and trans[0].label != 'stage':
            trans = [ParseLine(-1, 'stage', [], '', trans)]
        for i, stage in enumerate(trans):
            for lex in stage.children:
                if lex.label == 'rule':
                    readrule(lex, lfrom, lto, 'lex', '', i)
                else:
                    m = toobj(lex.label, lfrom, lex.num, None)
                    if lex.val:
                        for g in lex.vals:
                            d = toobj(g, lto, lex.num, m)
                            Translation(m, [d], category=m.root, langs=[lfrom, lto], mode='lex', stage=i)
                    for tr in lex.children:
                        readrule(tr, lfrom, lto, 'lex', m.root, i)
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
    for _line in lns:
        line = _line.strip()
        if not line: continue
        s = line.split()
        l = s[0][:-1]
        p,r = s[1].split('=')
        if l != lang:
            f.close()
            f = open(Globals.path + 'langs/%s/lexicon.txt' % l, 'a')
            f.write('\n\n#Generated from missing_morphemes.txt\n')
            lang = l
            print('Writing to langs/%s/lexicon.txt' % l)
        f.write('%s (%s)\n' % (r,p))
    f.close()
    f = open('missing_morphemes.txt', 'w')
    f.write('\n')
    f.close()
def filltrans(lfrom, lto):
    Language.getormake(lfrom)
    Language.getormake(lto)
    LangLink.getormake(lfrom, lto)
    fname = Globals.path + 'langs/%s/translate/%s.txt' % (lfrom, lto)
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
    def translate(self, tlang):
        ret = Sentence(self.lang, self.name, {}, self.gloss if Globals.keepmeta else '')
        if not self.trees:
            return ret
        tr = LangLink.getormake(self.lang, tlang)
        for k in self.trees:
            for i, s in enumerate(tr.translate(self.trees[k])):
                if Globals.partial or s.alllang(tlang):
                    ret.trees[k+'-'+str(i) if k else str(i)] = s
        return ret
    def totext(self):
        lang = Language.getormake(self.lang)
        for k in sorted(self.trees.keys()):
            #this should default to tree ''
            if self.trees[k]:
                return lang.totext(self.trees[k])
        return ''
    def graph(self):
        for k in sorted(self.trees.keys()):
            self.trees[k].flatten()
            f = open(Globals.path + 'test/%s-%s.dot' % (self.name, k), 'w')
            f.write(self.trees[k].graph('n', True))
            f.close()
            yield '<h3>%s</h3>' % (k or '(default)'), '%s-%s.dot' % (self.name, k)
def readfile(fname):
    pl = ParseLine.fromfile(fname)
    lang = int(pl[0].firstval('lang'))
    Language.getormake(lang)
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
def translatefile(infile, outfile, tlang):
    pl = ParseLine.fromfile(infile)
    flang = int(pl[0].firstval('lang'))
    if isinstance(outfile, str):
        f = open(outfile, 'w')
    else:
        f = outfile
    if Globals.keepmeta:
        meta = pl[0]
        for x in meta.children:
            if x.label == 'lang':
                x.vals = [str(tlang)]
    else:
        meta = ParseLine(0, 'metadata', children=[ParseLine(1, 'lang', val=str(tlang))])
    f.write(meta.tofilestr(0))
    for l in pl[1:]:
        f.write(Sentence.fromparseline(l, flang).translate(tlang).toparseline().tofilestr(0))
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
                newtree = pats[tree.ntype]
                if isinstance(newtree, list):
                    newtree = random.choice(newtree)
                return gen(pats, newtree, depth+1, setvars)
    elif isinstance(tree, SyntaxPat):
        vrs = {}
        for v in tree.vrs:
            vrs[v.label] = gen(pats, v, depth, {})
        il = []
        for i, cl in enumerate(tree.conds):
            for c in cl:
                if not c.checkset(vrs):
                    break
            else:
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
            many = [x for x in pats[k] if 'audible' in x and x['audible'] == 'false']
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
            yield from tree
        elif isinstance(tree, Variable):
            if tree.label in setvars:
                yield setvars[tree.label]
            elif isinstance(pats[tree.ntype], LimitList):
                old = pats[tree.ntype]
                for r, l in old.each():
                    pats[tree.ntype] = l
                    yield r
                pats[tree.ntype] = old
            else:
                yield from genall(pats[tree.ntype], setvars)
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
    return genall(pats[lang.syntaxstart], {})
def parse(lang, num, text):
    ret = Sentence(lang, str(num), {}, text)
    hfst = Globals.path + 'langs/%d/.generated/parse.hfst' % lang
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
    ln = Language.getormake(lang)
    n = 0
    for x in makeall(w):
        if ln.totext(x) == text:
            n += 1
            ret.trees[str(n)] = x
    return ret
def trans(sen, flang, tlang):
    tr = LangLink.getormake(flang, tlang).translate(sen)
    ret = []
    for s in tr:
        if Globals.partial or s.alllang(tlang):
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
                    global STDINLINE
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
                    f.write('\n'.join(t.writecompile() for t in tr))
                    f.close()
                else:
                    print('\n'.join(t.writecompile() for t in tr))
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
                    global STDINLINE
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
                txt = Language.getormake(lang).totext(toobj(line, lang, where))
                f.write(txt + '\n')
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
        def __init__(self, *args, **kwargs):
            self.todo = kwargs['todo']
            del kwargs['todo']
            kwargs['nargs'] = 0
            argparse.Action.__init__(self, *args, **kwargs)
        def __call__(self, parser, namespace, values, option_string=None):
            global Globals
            Globals.__dict__[self.todo[0]] = self.todo[1]
    parser.add_argument('-t', '--translate', type=str, nargs='*', action=TranslateAction, metavar='ARG', help="Translate trees (run 'doodle.py -t' for detailed help)")
    parser.add_argument('-g', '--generate', type=str, nargs='+', action=GenerateAction, metavar=('LANG', 'DEST'), help='Randomly generate a tree in LANG and output to DEST or stdout')
    parser.add_argument('-p', '--parse', type=str, nargs='+', action=ParseAction, metavar=('[SRC] LANG', 'DEST'), help='Attempt to parse SRC or next line of std into trees in LANG, output to DEST or stdout')
    parser.add_argument('-d', '--display', type=str, nargs='+', action=DisplayAction, metavar=('SRC [LANG]', 'DEST'), help='Get trees from SRC or stdin, convert to text and output to DEST or stdout')
    parser.add_argument('-F', '--flatten', action=SetGlobal, todo=('flat', True), help='Start flattening phrases into single nodes')
    parser.add_argument('-DF', '--dont-flatten', action=SetGlobal, todo=('flat', False), help='Stop flattening phrases')
    parser.add_argument('-U', '--use-unknown', action=SetGlobal, todo=('unknown_error', False), help='Begin logging unknown morphemes to missing_morphemes.txt, don\'t error')
    parser.add_argument('-am', '--add-missing', nargs=0, action=BlankAction, help='Append everything in missing_morphemes.txt to the relevant lexicon files')
    parser.add_argument('-ft', '--fill-trans', nargs=2, action=BlankAction, metavar=('LANG1', 'LANG2'), help='Add blank entries in translation file from LANG1 to LANG2 for any morpheme not already listed')
    args = parser.parse_args()
