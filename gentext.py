from general import loaddict, loadsyntax, getname
import random
def conjugate(lex, langid):
    return lex["root"]
syntax = {
    "sen": ["noun", "verb"],
    "noun": ["adj*", "$noun"],
    "noun2": ["$adjective", "$noun"],
    "adj": ["$adverb?", "$adjective"],
    "verb": ["$verb", "noun"]
}
#TODO
#switch to syntax2
#expand syntax2
#pass role info to conjugate()
syntax2 = {
  "clause": {
    "subject": "noun",
    "verb": "verb",
    "object": "noun?",
    "oblique": "noun*",
    "dependent-clause": "clause*"
  },
  "noun": {
    "noun": "$noun",
    "oblique": "noun*",
    "adjective": "adj*",
    "relative-clause": "clause*"
  },
  "verb": {
    "verb": "$verb",
    "adverb": "$adverb*"
  },
  "adj": {
    "adjective": "$adjective",
    "adverb": "$adverb*"
  }
}
def getroots(lang, pos, roots=[]):
    lex = loaddict(lang)
    found = []
    for w in lex:
        if w["pos"] == pos:
            if not roots or w["root"] in roots:
                found.append(w)
    return found
def gennode(innode, genid, outid):
    genret = []
    outret = []
    repeats = 1
    node = innode
    if node[-1] == "*":
        node = node[:-1]
        repeats = random.randint(0, 5)
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
            genret.append(conjugate(root, genid))
            rts = getroots(outid, node[1:], roots=gloss)
            outret.append([conjugate(r, outid) for r in rts])
        else:
            for n in syntax[node]:
                s, g = gennode(n, genid, outid)
                genret.append(s)
                outret += g
    def mklst(th):
        if type(th) == list: return th
        else: return [th]
    if outret:
        retgloss = mklst(outret[0])
    else:
        retgloss = []
    for opt in outret[1:]:
        t = []
        for r in retgloss:
            t += [r + " " + i for i in mklst(opt)]
        retgloss = t
    return " ".join(genret), retgloss
if __name__ == "__main__":
    import sys
    genid = int(sys.argv[1])
    outid = int(sys.argv[2])
    sen, glosses = gennode("sen", genid, outid)
    print "<table><tbody><tr><td>"
    print sen
    print "</td><td><ul><li>"
    print "</li><li>".join(glosses)
    print "</li></ul></td></tr></tbody></table>"
