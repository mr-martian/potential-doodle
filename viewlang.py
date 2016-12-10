import json, sys
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
def displaylang(langid):
    lex = loaddict(langid)
    langmeta = loadmeta(langid)
    print "<h1>%s</h1>" % langmeta["name"]["local"]
    print "<h2>Morphology</h2>"
    print "<h2>Syntax</h2>"
    print "<h2>Lexicon</h2>"
    lex.sort(key=lambda x: x["root"])
    cur = ""
    for word in lex:
        if word["root"] != cur:
            if cur != "": print "</div>"
            cur = word["root"]
            print "<div class=\"root\"><h3>%s</h3>" % cur
        print "<div class=\"def\">"
        print "<h4>%s</h4>" % word["pos"]
        for gl in word["gloss"]:
            print "<div class=\"gloss\">"
            glname = loadmeta(gl["lang"])["name"]
            if langid in glname:
                disp = glname[langid]
            else:
                disp = glname["local"]
            print "<b><a href=\"viewlang.php?lang=%s\">%s</a></b>" % (gl["lang"], disp)
            print "<ul>"
            for glw in gl["gloss"]:
                print "<li>%s</li>" % glw
            print "</ul>"
        print "</div>"
    print "</div>"
if __name__ == "__main__":
    langid = sys.argv[1]
    try:
        displaylang(langid)
    except:
        print "<h1>No language with id %s</h1>" % langid
