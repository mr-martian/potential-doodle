#!/usr/bin/env python3
import random, itertools, copy, re, subprocess, shlex
from compilelang import *
from datatypes import *
NORMALIZE_NODES = True # True: .lang of non-lexical nodes is ignored
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
                if not c.check(vrs):
                    ad = False
                    break
            if ad:
                il.append(i)
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
                        if all(c.check(dct) for c in tree.conds[i]):
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
    parser.add_argument('langs', nargs=2, type=int, help='Languages to convert between, use 0 for plaintext')
    files = parser.add_mutually_exclusive_group(required=True)
    files.add_argument('-t', '--text', type=str, help='Get input from file in texts/')
    files.add_argument('-f', '--file', type=str, help='Get input from file')
    files.add_argument('-r', '--reuse', action='store_true', help='Reuse the sentence in trace.txt')
    files.add_argument('-g', '--generate', action='store_true', help='Randomly generate a new sentence')
    files.add_argument('-p', '--parse', type=str, metavar='FILE', help='Parse a plaintext sentence')
    files.add_argument('-d', '--doc', type=str, metavar='FILE', help='Translate .pdtxt document')
    parser.add_argument('-n', '--notrace', action='store_true', help='Do not output to trace.txt')
    parser.add_argument('-o', '--outfile', type=argparse.FileType('w'), default=sys.stdout, metavar='FILE', help='Output to FILE')
    parser.add_argument('-F', '--flatten', action='store_true', help='Flatten phrases to single nodes')
    parser.add_argument('-N', '--normalize', action='store_true', help='When translating, ignore untranslated syntax nodes')
    parser.add_argument('-U', '--useunknown', action='store_true', help='Log unknown morphemes in missing_morphemes.txt rather than erroring')
    args = parser.parse_args()
    
    import compilelang
    compilelang.FLAT = args.flatten
    NORMALIZE_NODES = args.normalize
    compilelang.UNKNOWN_MORPH = "CREATE_AND_LOG" if args.useunknown else "ERROR"
    
    if 0 not in args.langs:
        loadlangset(args.langs)
        Source = Language.getormake(args.langs[0])
        Target = Language.getormake(args.langs[1])
    elif args.langs[0] == 0:
        Source = None
        Target = loadlang(args.langs[1])
    elif args.langs[1] == 0:
        Source = loadlang(args.langs[0])
        Target = None
    else:
        parser.error('Source and target language must not both be 0.')
    
    if args.parse:
        if not Target:
            parser.error('Target language must not be 0 for parsing text.')
        f = open(args.parse)
        text = Text([])
        for i, l in enumerate(f.readlines()):
            t = l.strip()
            if t:
                text.sens.append(parse(Target.lang, i+1, t))
        f.close()
        text.tofile(args.outfile)
    elif args.doc:
        if Target:
            translatefile(args.doc, args.outfile, Source.lang, Target.lang)
        else:
            args.outfile.write(Text.fromfile(args.doc, Source.lang).totext() + '\n')
        args.outfile.close()
    else:
        if not Source:
            parser.error('Source language must not be 0 for translating text.')
        if args.generate:
            sen = make(Source)
        else:
            if args.text:
                path = DATA_PATH + 'texts/' + args.text + '_tree.txt'
            elif args.file:
                path = args.file
            else:
                path = DATA_PATH + 'trace.txt'
            f = open(path)
            sen = toobj(f.readline(), Source.lang, '1 of %s' % path)
            f.close()
        def out(sen, traceopen, outfile):
            lang = Language.getormake(sen.lang)
            m = movement1(sen)
            r = dolinear(m)
            if traceopen:
                f = open(DATA_PATH + 'trace.txt', traceopen)
                f.write(sen.writecompile() + '\n\n' + str(sen) + '\n\n')
                f.write(m.writecompile() + '\n\n' + str(m) + '\n\n')
                f.write(' '.join(m.tagify_all()) + '\n\n' + str(m.linear()) + '\n\n' + r + '\n\n')
                f.close()
            outfile.write(r + '\n\n')
        out(sen, None if args.notrace else 'w', args.outfile)
        if Target:
            ls = trans(sen, Target.lang, False)
            for s in ls:
                out(s, None if args.notrace else 'a', args.outfile)
        args.outfile.close()
