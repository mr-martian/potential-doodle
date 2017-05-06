from sys import argv
import json
langdata = json.load(open(argv[1]))
print('<h2><i>' + langdata['display name'] + '</i> Received Successfully!</h2>')
if langdata['id'] == 2:
    print('<a href="../editlang.php?lang=2">Return to Editting</a>')
    #I have no idea why the .. is necessary, but it evidently is. -DS 2017-05-06
