from sys import argv
import json
mode = argv[1]
lang = argv[2]
langdata = json.load(open(argv[3]))
f = open('langs/%s/lang.json' % lang)
olddata = json.load(f)
f.close()
for k in langdata:
    olddata[k] = langdata[k]
f = open('langs/%s/lang.json' % lang, 'w')
json.dump(olddata, f)
f.close()
print('<h2><i>' + olddata['display name'] + '</i> %s Received Successfully!</h2>' % mode)
#if langdata['id'] == 2:
    #print('<a href="../editlang.php?lang=2">Return to Editting</a>')
    #I have no idea why the .. is necessary, but it evidently is. -DS 2017-05-06
