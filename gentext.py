from general import loaddict, getname, loadlang
import random, itertools
#TODO
#multiple complements
#movement
#conjugation
#there be bugs in the complex syntax part somewhere
def mklst(thing):
    if isinstance(thing, list) and len(thing) >= 1:
        return thing
    elif thing == []:
        return [""]
    else:
        return [thing]
def getroots(lang, pos, roots=[]):
    lex = loaddict(lang)
    found = []
    for w in lex:
        if w["pos"] == pos:
            if not roots or w["root"] in roots:
                found.append(w)
    if len(found) == 0:
        print "###NOTFOUND", lang, pos, roots
    return found
#"node": [specifier, head, complement, modifiers, complement-oddsnull]
syntax = {
  "CP": ["", "$C", "IP", "", 0],
  "NP": ["$Det", "$N", "PP", "AP", 75],
  "VP": ["AdvP", "$V", ["NP", "PP", "AP", "CP"], "AdvP", 50],
  "PP": ["$Deg", "$P", "NP", "", 10],
  "AP": ["$Deg", "$A", "PP", "", 80],
  "IP": ["NP", "$Aux", "VP", "", 0],
  "AdvP": ["", "$Adv", "", "", 0]
}
def makenode(ntype, gen, out, oddsnull=0): #return gen, out, outreplace
    gengram = loadlang(gen)["grammar"]
    outgram = loadlang(out)["grammar"]
    if ntype == "" or random.randint(0, 100) < oddsnull:
        return {"type": ntype}, [{"type": ntype}], []
    elif isinstance(ntype, list):
        return makenode(random.choice(ntype), gen, out, oddsnull=oddsnull)
    elif ntype[0] == "$":
        gn = random.choice(getroots(gen, gengram["pos"][ntype[1:]]))
        gloss = []
        struct = []
        for gl in gn["gloss"]:
            if gl["lang"] == outid:
                if "gloss" in gl:
                    gloss += gl["gloss"]
                else:
                    struct.append(gl["structure"])
        def traverse(tree, lang):
            if not isinstance(tree, dict):
                return [tree]
            if "type" in tree and tree["type"] == "findroot":
                return getroots(lang, tree["pos"], roots=tree["root"])
            else:
                ret = [{}]
                for key in tree:
                    tr = traverse(tree[key], lang)
                    temp = itertools.product(ret, tr)
                    ret = []
                    for d, i in temp:
                        d[key] = i
                        ret.append(d)
                return ret
        og = []
        os = []
        if len(gloss) > 0:
            og = getroots(out, outgram["pos"][ntype[1:]], roots=gloss)
        if len(struct) > 0:
            for tree in struct:
                os += traverse(tree, out)
        return gn, og, os
    else:
        ls = syntax[ntype]
        det = makenode(ls[0], gen, out)
        head = makenode(ls[1], gen, out)
        if ntype in gengram["complements"]:
            c = gengram["complements"][ntype]
            if "oddsnull" not in c:
                c["oddsnull"] = ls[4]
            if "head-prop" in c:
                comp = makenode(head[ls[1]]["properties"][c["head-prop"]], gen, out, oddsnull=c["oddsnull"])
            else:
                comp = makenode(c["types"], gen, out, oddsnull=c["oddsnull"])
        else:
            comp = makenode(ls[2], gen, out, oddsnull=ls[4])
        mod = makenode(ls[3], gen, out, oddsnull=75)
        genret = {
          "type": ntype,
          "determiner": det[0],
          ntype + "'": {
            "type": ntype + "'",
            ntype[:-1]: head[0],
            "complement": comp[0]
          },
          "modifier": mod[0]
        }
        outret = []
        for d, h, c, m in itertools.product(det[1] or [{"type": "#blank"}],
                                            head[1] or [{"type": "#blank"}],
                                            comp[1] or [{"type": "#blank"}],
                                            mod[1] or [{"type": "#blank"}]):
            outret.append({
              "type": ntype,
              "determiner": d,
              ntype + "'": {
                "type": ntype + "'",
                ntype[:-1]: h,
                "complement": c
              },
              "modifier": m
            })
        def insert(struct, rep, lang):
            if not isinstance(struct, dict):
                return struct
            if "type" in struct and struct["type"] == "insert":
                l = struct["insert"].split(".", 1)
                t = rep
                print "T in",  t
                for k in l:
                    t = t[k]
                print "T, out", t
                return t
            else:
                ret = {}
                for key in struct:
                    ret[key] = insert(struct[key], rep, lang)
                return ret
        #print "pre------reps", det[2], head[2], comp[2], mod[2]
        reps = det[2] + head[2] + comp[2] + mod[2]
        retrep = []
        #print "=======REPS", reps
        for s in reps:
            #print type(s)
            if s == []: continue
            if s["type"] == ntype:
                newout = []
                for tr in outret:
                    newout.append(insert(s, tr, out))
                outret += newout
            elif s["type"] == ntype + "'":
                newout = []
                for tr in outret:
                    x = insert(s, tr[ntype + "'"], out)
                    t = tr
                    t[ntype + "'"] = x
                    newout.append(t)
                outret += newout
            else:
                retrep.append(s)
        if ntype == "VP": print "#CHECK!", ntype, outret[-1]
        return genret, outret, retrep
def move(tree, lang):
    return tree
def conjugate(tree, lang):
    #print tree
    if "root" in tree:
        return tree["root"]
    elif "type" not in tree or tree["type"] == "" or len(tree.keys()) == 1:
        return ""
    elif tree["type"][-1] == "P":
        return conjugate(tree["determiner"], lang) + " " + conjugate(tree["modifier"], lang) + " " + conjugate(tree[tree["type"] + "'"], lang)
    elif tree["type"][-1] == "'":
        return conjugate(tree[tree["type"][:-2]], lang) + " " + conjugate(tree["complement"], lang)
    else:
        return conjugate(tree[tree["type"]], lang)
if __name__ == "__main__":
    import sys
    genid = int(sys.argv[1])
    outid = int(sys.argv[2])
    sen, glosses, extra = makenode("CP", genid, outid)
    print "<table><tbody><tr><td>"
    print conjugate(move(sen, genid), genid)
    print "</td><td><ul><li>"
    print "</li><li>".join(map(lambda x: conjugate(move(x, outid), outid), glosses))
    print "</li></ul></td></tr></tbody></table>"
    print extra
