import re
class Morpheme:
    morphemelist = {}
    def __init__(self, lang, root, pos, translations, irregularforms, props):
        self.lang = lang
        self.root = root
        self.pos = pos
        self.translations = translations
        self.irregularforms = irregularforms
        self.props = props
        if lang not in Morpheme.morphemelist:
            Morpheme.morphemelist[lang] = {}
        if pos not in Morpheme.morphemelist[lang]:
            Morpheme.morphemelist[lang][pos] = {}
        Morpheme.morphemelist[lang][pos][root] = self
    def gettranslation(self, langid):
        if langid in self.translations:
            return self.translations[langid]
        else:
            return None
    def conjugate(self):
        return self.root
        #TODO: Irregular forms
    def find(lang, rootstr):
        return morphemelist[lang][rootstr.split('=')[0]][rootstr.split('=')[1]]
    def __str__(self):
        #return "Morpheme(%s=%s, properties:%s, translations:%s)" % (self.pos, self.root, self.props, self.translations)
        return "Morpheme(%s=%s)" % (self.pos, self.root)
    def __repr__(self):
        return str(self)
    def display(self):
        return self.root
class Translation:
    def __init__(self, lang, form, result):
        self.lang = lang
        self.form = form
        self.result = result
    def translate(self, node):
        raise Exception("Translation.translate() has not been implemented.")
    def fromstring(lang, form, result):
        return Translation(lang, form, result)
        #NEEDS WORK
    def __str__(self):
        return "Translation(%s  =>  %s)" % (self.form, self.result)
    def __repr__(self):
        return str(self)
class SyntaxNode:
    def __init__(self, lang, nodetype, children, translations={}):
        self.lang = lang
        self.nodetype = nodetype
        self.children = children
        self.translations = translations
    def __getitem__(self, key):
        if key > len(self.children):
            return None
        else:
            return self.children[key]
    def __str__(self):
        return "[%s %s]" % (self.nodetype, ' '.join([str(x) for x in self.children]))
    def __repr__(self):
        return str(self)
    def display(self):
        return ' '.join([x.display() for x in self.children if x])
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
            else:
                nodes.append(ch) #Not a SyntaxNode
        return SyntaxNode(lang, nodetype, nodes)
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
        self.lang = lang
        self.form = form
        self.result = result
    def conjugate(self, node):
        raise Exception("IrregularForm.conjugate() has not been implemented.")
class SyntaxOutline:
    def __init__(self, lang, start, nodes):
        self.lang = lang
        self.start = start
        self.nodes = nodes
    def __getitem__(self, key):
        return self.nodes[key] if key in self.nodes else None, self.nodes['-'+key] if '-'+key in self.nodes else None
class SyntaxNodeSwap:
    def __init__(self, cond, ops, variables):
        self.cond = cond
        self.ops = ops
        self.variables = variables
    def __getitem__(self, val):
        return self.ops[val]

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
                trans.append(Translation(int(gloss.args[0]), '@', g))
        for tr in word['translate']:
            trans.append(Translation(int(tr['lang'][0].value), tr['from'][0].value, tr['to'][0].value))
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
                if tr.args[0] not in node.translations:
                    node.translations[tr.args[0]] = [SyntaxNode.fromstring(tr.args[0], tr.value)]
                else:
                    node.translations[tr.args[0]].append(SyntaxNode.fromstring(tr.args[0], tr.value))
        else:
            #TODO: actually write this part
            #(this is a stopgap method for just loading the first option)
            nt.children += nt['option'][0]['case'][0].children
            node = SyntaxNode.fromstring(langid, nt['structure'][0].value)
            for tr in nt['translation']:
                if tr.args[0] not in node.translations:
                    node.translations[tr.args[0]] = [SyntaxNode.fromstring(tr.args[0], tr.value)]
                else:
                    node.translations[tr.args[0]].append(SyntaxNode.fromstring(tr.args[0], tr.value))
        nodetypes[nt.label] = node
    return SyntaxOutline(langid, syntax['start-with'][0].value, nodetypes)
if __name__ == "__main__":
    print(parselexiconfile(2))
    parselangfile(2)
