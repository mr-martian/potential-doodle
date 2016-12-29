from general import loaddict, loadlang, getname
def displaylang(langid):
    lex = loaddict(langid)
    langmeta = loadlang(langid)
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
            print "<b><a href=\"viewlang.php?lang=%s\">%s</a></b>" % (gl["lang"], getname(gl["lang"], langid))
            print "<ul>"
            for glw in gl["gloss"]:
                print "<li>%s</li>" % glw
            print "</ul>"
        print "</div>"
    print "</div>"
if __name__ == "__main__":
    import sys
    langid = sys.argv[1]
    try:
        displaylang(langid)
    except:
        print "<h1>No language with id %s</h1>" % langid
