import json
loaded_meta = {}
def loadmeta(langid):
    if langid in loaded_meta:
        return loaded_meta[langid]
    else:
        l = json.load(open("langs/%s/metadata.json" % langid))
        loaded_meta[langid] = l
        return l
loaded_langs = {}
def loadlang(langid):
    if langid in loaded_langs:
        return loaded_langs[langid]
    else:
        l = json.load(open("langs/%s/lang.json" % langid))
        loaded_langs[langid] = l
        return l
loaded_dicts = {}
def loaddict(langid):
    if langid in loaded_dicts:
        return loaded_dicts[langid]
    else:
        l = json.load(open("langs/%s/lexicon.json" % langid))
        loaded_dicts[langid] = l
        return l
loaded_syntax = {}
def loadsyntax(langid):
    if langid in loaded_syntax:
        return loaded_syntax[langid]
    else:
        l = json.load(open("langs/%s/syntax.json" % langid))
        loaded_syntax[langid] = l
        return l
def getname(langid, displayid):
    n = loadmeta(langid)["name"]
    for t in n["translations"]:
        if t["lang"] == displayid:
            return t["translation"]
    return n["local"]
