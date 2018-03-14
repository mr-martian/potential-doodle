from sys import argv
import json
print(argv)
mode = argv[1]
lang = argv[2]
langdata = json.load(open(argv[3], encoding='utf-8'))
f = open('../langs/%s/lang.json' % lang, encoding='utf-8')
olddata = json.load(f)
f.close()
for k in langdata:
    olddata[k] = langdata[k]
f = open('langs/%s/lang.json' % lang, 'w')
json.dump(olddata, f)
f.close()
print('<h2><i>' + olddata['display name'] + '</i> %s Received Successfully!</h2>' % mode)
print('<a href="../%s.php?lang=%s">Return to Editting</a>' % (mode, lang))
#I have no idea why the .. is necessary, but it evidently is. -DS 2017-05-06
