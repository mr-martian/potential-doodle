import re, itertools
class Morpheme:
    morphemelist = {}
    def __init__(self, lang, root, pos, translations, irregularforms, props):
        self.lang = int(lang)
        self.root = root
        self.pos = pos
        self.translations = translations
        self.irregularforms = irregularforms
        self.props = props
        if lang not in Morpheme.morphemelist:
            Morpheme.morphemelist[lang] = {'%s=%s' % (pos, root): self}
        else:
            Morpheme.morphemelist[lang]['%s=%s' % (pos, root)] = self
    def gettranslations(self, langid):
        return [x for x in self.translations if x.lang == langid]
    def conjugate(self):
        return self.root
        #TODO: Irregular forms
    def find(lang, rootstr):
        if lang not in Morpheme.morphemelist:
            parselexiconfile(lang)
        return Morpheme.morphemelist[lang][rootstr]
    def findpos(lang, var):
        assert(isinstance(var, Variable))
        s = var.strip()
        if s[-1] in '*?':
            s = s[:-1]
        for w, m in Morpheme.morphemelist[lang].items():
            if w.split('=')[0] == s and var.checkcond(m):
                yield m
    def __str__(self):
        #return "Morpheme(%s=%s, properties:%s, translations:%s)" % (self.pos, self.root, self.props, self.translations)
        return "Morpheme(%s=%s)" % (self.pos, self.root)
    def __repr__(self):
        return str(self)
    def display(self):
        return self.root
class Translation:
    def __init__(self, lang, form, result):
        self.lang = int(lang)
        self.form = form
        self.result = result
    def __str__(self):
        return "Translation(%s  =>  %s)" % (self.form, self.result)
    def __repr__(self):
        return str(self)
class SyntaxNode:
    def __init__(self, lang, nodetype, children, translations=[]):
        self.lang = int(lang)
        self.nodetype = nodetype
        self.children = children
        self.translations = translations
    def __getitem__(self, key):
        if key > len(self.children):
            return None
        else:
            return self.children[key]
    def gettranslations(self, langid):
        return [x for x in self.translations if x.lang == langid]
    def __str__(self):
        return "[%s %s]" % (self.nodetype, ' '.join([str(x) for x in self.children]))
        #return "[%s %s %s]" % (self.nodetype, ' '.join([str(x) for x in self.children]), self.translations)
        #translations printed for debugging purposes
    def __repr__(self):
        return str(self)
    def display(self):
        return ' '.join([x.display() for x in self.children if x])
    def swapchildren(self, newch):
        return SyntaxNode(self.lang, self.nodetype, newch, self.translations)
    def extractvariables(self, pat, callback=(lambda x: x)):
        names = {}
        for n, p in zip(self.children, pat.children):
            if isinstance(p, SyntaxNode):
                for k, v in n.extractvariables(p, callback).items():
                    names[k] = v
            elif p == None or p == '@':
                pass
            else:
                r = callback(n)
                names[p.name] = [r] if not isinstance(r, list) else r
                #TODO: This could result in translating the tree many times
                #maybe try caching it somehow?
        return names
    def insertvariables(self, names):
        ret = []
        for ch in self.children:
            if ch == None:
                ret.append([None])
            elif isinstance(ch, SyntaxNode):
                ret.append(ch.insertvariables(names))
            elif isinstance(ch, Morpheme):
                ret.append([ch])
            else:
                r = names[ch.name]
                ret.append(r if isinstance(r, list) else [r])
        return [self.swapchildren(rl) for rl in itertools.product(*ret)]
    def applytranslation(self, trans, callback=(lambda x: x)):
        return trans.result.insertvariables(self.extractvariables(trans.form, callback))
    def fromstring(lang, fstr):
        nodetype = fstr[1:].split(' ', 1)[0]
        children = []
        cur = ""
        l = 0
        r = 0
        for c in fstr[:-1].split(' ', 1)[1]:
            if l != r:
                cur += c
                if c == '[':
                    l += 1
                if c == ']':
                    r += 1
            elif c == ' ':
                children.append(cur)
                cur = ""
            else:
                cur += c
                if c == '[':
                    l += 1
        children.append(cur)
        nodes = []
        for ch in children:
            if ch == '':
                pass
            elif ch == "~":
                nodes.append(None)
            elif ch[0] == '[':
                nodes.append(SyntaxNode.fromstring(lang, ch))
            elif ch[0] != '$' and '=' in ch:
                nodes.append(Morpheme.find(lang, ch))
            elif ch == '@':
                nodes.append(ch)
            else:
                nodes.append(Variable.fromstring(ch)) #Not a SyntaxNode
        return SyntaxNode(lang, nodetype, nodes, [])
class Variable:
    regex = re.compile('^(\\$[A-Za-z0-9\\-]+):(#?[A-Za-z0-9\\-]+[*?]?)\\((([A-Za-z0-9\\-]+)=([A-Za-z0-9\\-]+))\\)$')
    def __init__(self, name, nodetype, prop, val):
        self.name = name
        self.nodetype = nodetype
        self.prop = prop
        self.val = val
    def fromstring(fstr):
        if fstr[0] == '$':
            if ':' not in fstr and '(' not in fstr:
                return Variable(fstr, '', '', '')
            elif ':' not in fstr:
                m = re.match('^(\\$[A-Za-z0-9\\-]+)\\(([A-Za-z0-9\\-]+)=([A-Za-z0-9\\-]+)\\)$', fstr)
                return Variable(m.group(1), '', m.group(2), m.group(3))
            elif '(' not in fstr:
                s = fstr.split(':')
                return Variable(s[0], s[1], '', '')
            else:
                m = re.match('^(\\$[A-Za-z0-9\\-]+):(#?[A-Za-z0-9\\-]+[*?]?)\\(([A-Za-z0-9\\-]+)=([A-Za-z0-9\\-]+)\\)$', fstr)
                return Variable(m.group(1), m.group(2), m.group(3), m.group(4))
        elif '(' not in fstr:
            return Variable('', fstr, '', '')
        else:
            m = re.match('^(#?[A-Za-z0-9\\-]+[*?]?)\\((([A-Za-z0-9\\-]+)=([A-Za-z0-9\\-]+))\\)$', fstr)
            return Variable('', m.group(1), m.group(2), m.group(3))
    def checkcond(self, morph):
        if morph == None:
            return False
        if not self.prop:
            return True
        if self.prop == '':
            return True
        return morph.props[self.prop] == self.val
    def findandcheck(self, names):
        return self.checkcond(names[self.name])
    def __repr__(self):
        return '%s:%s(%s=%s)' % (self.name, self.nodetype, self.prop, self.val)
    def strip(self):
        m = re.match('^#?([A-Za-z0-9\\-]+)[*?]?$', self.nodetype)
        return m.group(1) if m else ''
class MorphologyNode:
    def __init__(self, pos, stem, affix, mode):
        self.pos = pos
        self.stem = stem
        self.affix = affix
        self.mode = mode
    def conjugate(self):
        if self.mode == "suffix":
            return self.stem.conjugate() + self.affix.conjugate()
        elif self.mode == "prefix":
            return self.affix.conjugate() + self.stem.conjugate()
        else:
            raise Exception("MorphologyNode.conjugate() has not been implemented for mode '%s'." % self.mode)
    def __str__(self):
        return self.pos + "=" + self.conjugate()
    def __repr__(self):
        return str(self)
class IrregularForm:
    def __init__(self, lang, form, result):
        self.lang = int(lang)
        self.form = form
        self.result = result
    def conjugate(self, node):
        raise Exception("IrregularForm.conjugate() has not been implemented.")
class SyntaxOutline:
    def __init__(self, lang, start, nodes):
        self.lang = int(lang)
        self.start = start
        self.nodes = nodes
    def __getitem__(self, key):
        return self.nodes[key] if key in self.nodes else None, self.nodes['-'+key] if '-'+key in self.nodes else None
    def __repr__(self):
        return "SyntaxOutline(%s, %s, %s)" % (self.lang, self.start, self.nodes)
class SyntaxNodeSwap:
    def __init__(self, lang, conds, ops, variables):
        self.lang = int(lang)
        assert(not isinstance(conds[0][0], str))
        self.conds = conds
        self.ops = ops
        self.variables = variables
    def __repr__(self):
        return 'SNS(%s)' % self.conds

class parselines:
    withargsval = re.compile('^([A-Za-z0-9\\-]+) \\((.*?)\\): (.*)$')
    withargs = re.compile('^([A-Za-z0-9\\-]+) \\((.*?)\\)$')
    withval = re.compile('^([A-Za-z0-9\\-]+): (.*)$')
    def __init__(self, label, args, children, value):
        self.label = label
        self.args = args
        self.children = children
        self.value = value
    def __str__(self):
        return "parselines(label=%s, args=%s, value=%s, children=%s)" % (self.label, str(self.args), str(self.value), str(self.children))
    def __repr__(self):
        return str(self)
    def fromstring(lines):
        m = parselines.withargsval.match(lines[0])
        if m:
            ret = parselines(m.group(1), m.group(2).split(', '), [], m.group(3))
        else:
            m = parselines.withargs.match(lines[0])
            if m:
                ret = parselines(m.group(1), m.group(2).split(', '), [], None)
            else:
                m = parselines.withval.match(lines[0])
                if m:
                    ret = parselines(m.group(1), [], [], m.group(2))
                else:
                    ret = parselines(lines[0], [], [], None)
        if len(lines) > 1:
            cur = [lines[1]]
            for l in lines[2:]:
                if l.strip() == '':
                    continue
                if not l.startswith('  '):
                    ret.children.append(parselines.fromstring(cur))
                    cur = [l]
                else:
                    cur.append(l[2:])
            ret.children.append(parselines.fromstring(cur))
        return ret
    def fromfile(fname):
        f = open(fname)
        ret = []
        cur = [f.readline().rstrip()]
        l = f.readline().rstrip()
        while l:
            if l.strip() == '':
                pass
            elif not l.startswith('  '):
                ret.append(parselines.fromstring(cur))
                cur = [l]
            else:
                cur.append(l[2:])
            l = f.readline().rstrip()
        ret.append(parselines.fromstring(cur))
        return ret
    def __getitem__(self, key):
        r = []
        for c in self.children:
            if c.label == key:
                r.append(c)
        return r
def parsething(l, s):
    if s[0] == '[':
        return SyntaxNode.fromstring(l, s)
    else:
        return Variable.fromstring(s)
def parselexiconfile(langid):
    wordlist = parselines.fromfile('langs/%s/lexicon.txt' % langid)
    ret = []
    for word in wordlist:
        data = {}
        trans = []
        irreg = []
        for prop in word.children:
            if prop.label not in ['gloss', 'translate', 'irregularforms']:
                data[prop.label] = prop.value
        for gloss in word['gloss']:
            for g in gloss.value.split('; '):
                trans.append(Translation(int(gloss.args[0]), '@', Morpheme.find(int(gloss.args[0]), g)))
        for tr in word['translate']:
            l = int(tr['lang'][0].value)
            trans.append(Translation(l, SyntaxNode.fromstring(l, tr['from'][0].value), SyntaxNode.fromstring(l, tr['to'][0].value)))
        #irregularforms
        ret.append(Morpheme(langid, word.label, word.args[0], trans, irreg, data))
    return ret
def parselangfile(langid):
    #TODO: don't assume order, do other things than syntax
    things = parselines.fromfile('langs/%s/lang.txt' % langid)
    syntax = things[1]
    nodetypes = {}
    for nt in syntax['node-types'][0].children:
        if not nt['option']:
            node = SyntaxNode.fromstring(langid, nt['structure'][0].value)
            for tr in nt['translation']:
                node.translations.append(Translation(tr.args[0], node, SyntaxNode.fromstring(tr.args[0], tr.value)))
        else:
            vrs = [parsething(langid, x.value) for x in nt['variable']]
            conds = []
            ops = []
            for case in nt['option'][0]['case']:
                nd = parsething(langid, case['structure'][0].value)
                for tr in case['translation']:
                    nd.translations.append(Translation(tr.args[0], nd, parsething(tr.args[0], tr.value)))
                conds.append([parsething(langid, x) for x in case.args])
                ops.append(nd)
            node = SyntaxNodeSwap(langid, conds, ops, vrs)
        nodetypes[nt.label] = node
    return SyntaxOutline(langid, syntax['start-with'][0].value, nodetypes)
if __name__ == "__main__":
    #print(parselexiconfile(2))
    print(parselangfile(2))
