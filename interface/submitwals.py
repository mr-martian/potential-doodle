from sys import argv
import json
print(argv)
lang = argv[1]
langdata = json.load(open(argv[2], encoding='utf-8'))
f = open('../langs/%s/lang.json' % lang, encoding='utf-8')
olddata = json.load(f)
f.close()
wals = {}
for k in langdata:
    if k != 'id' and langdata[k] != '0':
        wals[k] = int(langdata[k])
olddata['wals'] = wals
f = open('../langs/%s/lang.json' % lang, 'w')
json.dump(olddata, f)
f.close()
print('<h2><i>' + olddata['display name'] + '</i> WALS features Received Successfully!</h2>')
print('<a href="../walsfeatures.php?lang=%s">Return to Editting</a>' % lang)
