from collections import defaultdict
places = ['bimanual', 'faciomanual', 'bilabial', 'labiodental', 'dental', 'alveolar', 'postalveolar', 'retroflex', 'palatal', 'velar', 'uvular', 'pharyngeal', 'epiglottal', 'glottal']
manners = ['stop', 'fricative', 'trill', 'tap', 'approximant', 'click']
modifiers = ['lateral', 'ejective', 'ingressive', 'aspirated', 'nasal']
consid = defaultdict(lambda: defaultdict(lambda: False))
consch = defaultdict(lambda: defaultdict(lambda: False))
def getcid(place, voice, manner, mods):
    "getcid(place, voice, manner, mods) -> unique consonant id"
    cid = int(voice)
    cid += places.index(place) << 1
    cid += manners.index(manner) << 5
    for i, m in enumerate(mods):
        cid += 1 << (modifiers.index(mods[i])+8)
    return cid
def cgetname(cid):
    "cgetname(cid) -> human-readable name for a consonant (e.g. 'voiced velar stop')"
    voiced = cid & 1
    cid >>= 1
    place = places[cid % 16]
    cid >>= 4
    manner = manners[cid % 8]
    cid >>= 3
    mods = []
    for i, m in enumerate(modifiers):
        if cid & 1:
            mods.append(m)
        cid >>= 1
    l = ['voice' + ('d' if voiced else 'less')] + mods
    return ' '.join(l) + ' ' + place + ' ' + manner
def addc(mode, cid, ch):
    "addc(mode, cid, ch) associates character ch with id cid in mode mode"
    consid[cid][mode] = ch
    consch[mode][ch] = cid
def c(mode, ch, place, voice, manner, mods):
    "c(mode, ch, place, voice, manner, mods) determines id and calls addc()"
    cid = getcid(place, voice, manner, mods)
    addc(mode, cid, ch)
def cfillgaps():
    "iterate through consonants and fill gaps, such as aspirated versions, returns number of additions"
    gaps = [['ipa', 1, '̥'], #voiceless
            ['ipa', -1, '̬'], #voiced
            ['ipa', -2048, 'ʰ'], #aspirated
            ['ipa', -512, 'ʼ'], #ejective
            ['cxs', 1, '_0'], #voiceless
            ['cxs', -1, '_v'], #voiced
            ['cxs', -2048, '_h'], #aspirated
            ['cxs', -512, '_>'], #ejective
           ]
    add = []
    keys = list(consid.keys())
    r = 0
    for cid in keys:
        for g in gaps:
            mode = g[0]
            flag = g[1]
            ch = g[2]
            cond = cid & abs(flag)
            if (flag < 0):
                cond = not cond
            if cond and consid[cid][mode] and not consid[cid-flag][mode]:
                addc(mode, cid-flag, consid[cid][mode] + ch)
                r += 1
    return r
def addmanner(mode, manner, voice, modifiers, chars):
    """addmanner(mode, manner, voice, modifiers, chars)
given a string of chars, either sequentially add each one, skipping spaces
or split on |, either way adding them by place of articulation from front to back"""
    if len(chars) > len(places):
        return addmanner(mode, manner, voice, modifiers, chars.split('|'))
    for i, p in enumerate(places):
        if i < len(chars) and chars[i] != ' ':
            c(mode, chars[i], p, voice, manner, modifiers)
addmanner('ipa', 'stop', False, [], '  p  tʈckq ʡʔ');
addmanner('ipa', 'stop', True, [], '  b  dɖɟgɢ');
addmanner('ipa', 'stop', True, ['nasal'], '  mɱ n ɳɲŋɴ');
addmanner('ipa', 'trill', True, [], '  ʙ  r    ʀ');
addmanner('ipa', 'tap', True, [], '   ⱱ  ɾ ɽ');
addmanner('ipa', 'fricative', False, [], '  ɸfθsʃʂçxχħʜh');
addmanner('ipa', 'fricative', True, [], '  βvðzʒʐʝɣʁʕʢɦ');
c('ipa', 'ɬ', 'alveolar', False, 'fricative', ['lateral']);
c('ipa', 'ɮ', 'alveolar', True, 'fricative', ['lateral']);
addmanner('ipa', 'approximant', True, [], '   ʋ ɹ  ɻjɰ');
addmanner('ipa', 'approximant', True, ['lateral'], '     l ɭʎʟ');
addmanner('ipa', 'stop', True, ['ingressive'], '  ɓ  ɗ  ʄɠʛ');
addmanner('ipa', 'click', False, [], '  ʘ ǀ ǃ ǂ');
c('ipa', 'ǁ', 'alveolar', False, 'click', ['lateral']);

addmanner('cxs', 'stop', False, [], ' | |p| | |t|t`|c|k|q| |>\\|?');
addmanner('cxs', 'stop', True, [], ' | |b| | |d|d`|J\\|g|G\\');
addmanner('cxs', 'stop', True, ['nasal'], ' | |m|F| |n| |n`|J|N|N\\');
addmanner('cxs', 'trill', True, [], ' | |B\\| | |r| | | | |R\\');
addmanner('cxs', 'tap', True, [], ' | | | | | |4| |r`');
addmanner('cxs', 'fricative', False, [], ' | |p\\|f|T|s|S|s`|C|x|X|X\\|H\\|h');
addmanner('cxs', 'fricative', True, [], ' | |B|v|D|z|Z|z`|j\\|G|R|?\\|<\\|h\\');
c('cxs', 'K', 'alveolar', False, 'fricative', ['lateral']);
c('cxs', 'K\\', 'alveolar', True, 'fricative', ['lateral']);
addmanner('cxs', 'approximant', True, [], ' | | |P| |r\\| | |r\\`|j|M\\');
addmanner('cxs', 'approximant', True, ['lateral'], ' | | | | |l| |l`|L|L\\');
addmanner('cxs', 'stop', True, ['ingressive'], ' | |b_<| | |d_<| | |J\\_<|g_<|G\\_<');
addmanner('cxs', 'click', False, [], ' | |0\\| | | |!\\|!\\`|=\\');
c('cxs', '|\\', 'dental', False, 'click', []);
c('cxs', '|\\|\\', 'alveolar', False, 'click', ['lateral']);

x = 1
while x > 0:
    x = cfillgaps()

backs = ['front', 'central', 'back']
heights = ['high', 'mid', 'low']
vmodifiers = ['voiceless', 'nasalized', 'lax', 'reduced']
vowid = defaultdict(lambda: defaultdict(lambda: False))
vowch = defaultdict(lambda: defaultdict(lambda: False))
def getvid(back, height, rounded, mods):
    "getvid(back, height, rounded, mods) -> unique vowel id"
    vid = int(rounded)
    vid += backs.index(back) << 1
    vid += heights.index(height) << 3
    for m in mods:
        vid += 1 << (vmodifiers.index(m)+5)
    return vid
def vgetname(vid):
    "vgetname(vid) -> human-readable name for a vowel (e.g. 'high back unrounded vowel')"
    rounded = vid & 1
    vid >>= 1
    back = backs[vid % 4]
    vid >>= 2
    height = heights[vid % 4]
    vid >>= 2
    mods = []
    for i, m in enumerate(vmodifiers):
        if vid & 1:
            mods.append(m)
        vid >>= 1
    l = [height, back, ('' if rounded else 'un') + 'rounded'] + mods
    return ' '.join(l) + ' vowel'
def v(mode, ch, height, back, rounded, mods):
    "v(mode, ch, height, back, rounded, mods) determine vowel id and register character"
    if ch == ' ':
        return False
    vid = getvid(back, height, rounded, mods)
    vowid[vid][mode] = ch
    vowch[mode][ch] = vid
def vrow(mode, chs, height, mods):
    """vrow(mode, chs, height, mods) given 6 vowels (1 character or separated by |)
add as front unrounded, front rounded, central unrounded, ... back rounded"""
    if len(chs) != 6:
        return vrow(mode, chs.split('|'), height, mods)
    v(mode, chs[0], height, 'front', False, mods)
    v(mode, chs[1], height, 'front', True, mods)
    v(mode, chs[2], height, 'central', False, mods)
    v(mode, chs[3], height, 'central', True, mods)
    v(mode, chs[4], height, 'back', False, mods)
    v(mode, chs[5], height, 'back', True, mods)
vrow('ipa', 'iyɨʉɯu', 'high', [])
vrow('ipa', 'ɪʏ   ʊ', 'high', ['lax'])
vrow('ipa', 'eøɘɵɤo', 'mid', [])
vrow('ipa', 'ɛœɜɞʌɔ', 'mid', ['lax'])
vrow('ipa', 'aɶ  ɑɒ', 'low', [])
vrow('ipa', 'æ ɐ   ', 'low', ['lax'])
v('ipa', 'ə', 'mid', 'central', False, ['lax', 'reduced'])

vrow('cxs', 'i|y|1|u\\|M|u', 'high', [])
vrow('cxs', 'I|Y|I\\|U\\| |U', 'high', ['lax'])
vrow('cxs', 'e|2|@\\|8|7|o', 'mid', [])
vrow('cxs', 'E|9|3|3\\|V|O', 'mid', ['lax'])
vrow('cxs', 'a|&\\| | |A|Q', 'low', [])
vrow('cxs', '& 6   ', 'low', ['lax'])
v('cxs', '@', 'mid', 'central', False, ['lax', 'reduced'])

if __name__ == '__main__':
    import json
    s = '/*generated by ipa.py*/'
    s += 'var consid=' + json.dumps(consid).replace(' ', '') + ';'
    s += 'var consch=' + json.dumps(consch).replace(' ', '') + ';'
    s += 'var vowid=' + json.dumps(vowid).replace(' ', '') + ';'
    s += 'var vowch=' + json.dumps(vowch).replace(' ', '') + ';'
    print(s)
