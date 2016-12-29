from general import loaddict, getname, loadlang
import random, itertools
#TODO
#expand syntax{}
#pass role info to conjugate()
syntax = {
  "clause": {
    "subject": "nounphrase",
    "verb": "verbphrase",
    "object": "nounphrase",
#    "oblique": "nounphrase*",
#    "dependent-clause": "clause*"
    #"tense": "tense",
  },
  "nounphrase": {
    "noun": "$noun",
#    "oblique": "nounphrase*",
#    "adjective": "adjectivephrase*",
#    "relative-clause": "clause*"
    "plural": "plural"
  },
  "verbphrase": {
    "verb": "$verb",
    "adverb": "$adverb*",
    "aspect": "aspect"
  },
  "adjectivephrase": {
    "adjective": "$adjective",
    "adverb": "$adverb*"
  },
  "plural": ["singular", "plural"],
  "aspect": ["continuous", "durative"]
}
def getroots(lang, pos, roots=[]):
    lex = loaddict(lang)
    found = []
    for w in lex:
        if w["pos"] == pos:
            if not roots or w["root"] in roots:
                found.append(w)
    return found
def makenode(innode, genid, outid):
    genret = []
    outret = []
    repeats = 1
    node = innode
    if node[-1] == "*":
        node = node[:-1]
        repeats = max(random.randint(-100, 2), 0) #limit recursion
    elif node[-1] == "?":
        node = node[:-1]
        repeats = random.randint(0, 1)
    else: pass
    for i in xrange(repeats):
        if node[0] == "$":
            root = random.choice(getroots(genid, node[1:]))
            gloss = []
            for g in root["gloss"]:
                if g["lang"] == outid:
                    gloss += g["gloss"]
            genret.append(root)
            outret.append(getroots(outid, node[1:], roots=gloss))
        elif isinstance(syntax[node], dict):
            gennode = {"nodetype": node}
            outnode = {"nodetype": node}
            for n in syntax[node]:
                gennode[n], outnode[n] = makenode(syntax[node][n], genid, outid)
            genret.append(gennode)
            outret.append(outnode)
        else:
            r = random.choice(syntax[node])
            genret.append(r)
            outret.append(r)
    return genret, outret
def mklst(thing):
    if isinstance(thing, list) and len(thing) >= 1:
        return thing
    elif thing == []:
        return [""]
    else:
        return [thing]
def conjugate(tree, lang):
    if isinstance(tree, list):
        if len(tree) > 1:
            return [conjugate(x, lang) for x in tree]
        elif len(tree) == 1:
            return conjugate(tree[0], lang)
        else:
            return [""]
    syntax = loadlang(lang)["grammar"]
    def check(prop, val, place, node):
        if isinstance(node, list):
            for n in node:
                if check(prop, val, place, n):
                    return True
            return False
        elif place == "":
            return node[prop] == val or node == val
        elif "." not in place:
            return check(prop, val, "", node[place])
        else:
            f, r = place.split(".", 1)
            return check(prop, val, r, node[f])
    def checkall(conds, node):
        for c in conds:
            if not check(c["property"], c["value"], c["node"], node):
                return False
        return True
    if "nodetype" not in tree:
        return tree["root"]
        #TODO ignores irregular forms
    for opt in syntax[tree["nodetype"]]:
        if checkall(opt["conditions"], tree):
            ret = []
            for item in opt["order"]:
                if item in tree:
                    ret.append(mklst(conjugate(tree[item], lang)))
                else:
                    ret.append(mklst(item))
            return [" ".join(x) for x in itertools.product(*ret)]
    return "<b>no match found for nodetype \"%s\"</b><br>node:<br>%s" % (tree["nodetype"], tree)
def sandhi(ls, lang):
    ret = []
    sand = loadlang(lang)["sandhi"]
    for sen in ls:
        while "  " in sen:
            sen = sen.replace("  ", " ")
        for rule in sand:
            if rule["regex"]:
                pass
            else:
                sen = sen.replace(rule["find"], rule["replace"])
        ret.append(sen)
    return ret
if __name__ == "__main__":
    import sys
    genid = int(sys.argv[1])
    outid = int(sys.argv[2])
    sen, glosses = makenode("clause", genid, outid)
    print "<table><tbody><tr><td>"
    print sandhi(conjugate(sen, genid), genid)[0]
    print "</td><td><ul><li>"
    print "</li><li>".join(sandhi(conjugate(glosses, outid), outid))
    print "</li></ul></td></tr></tbody></table>"
