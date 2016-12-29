import json
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
def getname(langid, displayid):
    n = loadlang(langid)["name"]
    for t in n["translations"]:
        if t["lang"] == displayid:
            return t["translation"]
    return n["local"]
