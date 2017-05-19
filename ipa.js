var places = ['bimanual', 'faciomanual', 'bilabial', 'labiodental', 'dental', 'alveolar', 'postalveolar', 'retroflex', 'palatal', 'velar', 'uvular', 'pharyngeal', 'epiglottal', 'glottal'];
var manners = ['stop', 'fricative', 'trill', 'tap', 'approximant', 'click'];
var modifiers = ['lateral', 'ejective', 'ingressive', 'aspirated', 'nasal'];
var consid = {};
var consch = {};
var getcid = function(place, voice, manner, mods) {
  id = voice ? 1 : 0;
  id += places.indexOf(place) << 1;
  id += manners.indexOf(manner) << 5;
  for (var i = 0; i < mods.length; i++) {
    id += 1 << (modifiers.indexOf(mods[i])+8);
  }
  return id;
};
var cgetchr = function(mode, id) {
  if (consid.hasOwnProperty(id)) {
    if (consid[id].hasOwnProperty(mode)) {
      return consid[id][mode];
    }
  }
  return false;
};
var cgetname = function(id) {
  var voiced = id & 1;
  id >>= 1;
  var place = places[id % 16];
  id >>= 4;
  var manner = manners[id % 8];
  id >>= 3;
  var mods = [];
  for (var i = 0; i < modifiers.length; i++) {
    if (id & 1) {
      mods.push(modifiers[i]);
    }
    id >>= 1;
  }
  var l = ['voice' + (voiced ? 'd' : 'less')].concat(mods);
  return l.join(' ') + ' ' + place + ' ' + manner;
};
var addc = function(mode, id, chr) {
  if (consid.hasOwnProperty(id)) {
    consid[id][mode] = chr;
  } else {
    consid[id] = {};
    consid[id][mode] = chr;
  }
  if (consch.hasOwnProperty(mode)) {
    consch[mode][chr] = id;
  } else {
    consch[mode] = {};
    consch[mode][chr] = id;
  }
};
var c = function(mode, chr, place, voice, manner, mods) {
  var id = getcid(place, voice, manner, mods);
  addc(mode, id, chr);
};
var cfillgaps = function() {
  var gaps = [['ipa', 1, '̥'], //voiceless
              ['ipa', -1, '̬'], //voiced
              ['ipa', -2048, 'ʰ'], //aspirated
              ['ipa', -512, 'ʼ'], //ejective
              ['cxs', 1, '_0'], //voiceless
              ['cxs', -1, '_v'], //voiced
              ['cxs', -2048, '_h'], //aspirated
              ['cxs', -512, '_>'], //ejective
             ];
  var add = [];
  for (var id in consid) {
    for (var g = 0; g < gaps.length; g++) {
      var mode = gaps[g][0];
      var flag = gaps[g][1];
      var chr = gaps[g][2];
      var cond = id & Math.abs(flag);
      if (flag < 0) {
        cond = !cond;
      }
      if (cond && !cgetchr(mode, id-flag) && cgetchr(mode, id)) {
        add.push([id-flag, cgetchr(mode, id) + chr, mode]);
      }
    }
  }
  for (var i = 0; i < add.length; i++) {
    addc(add[i][2], add[i][0], add[i][1]);
  }
  return add.length;
};
var addmanner = function(mode, manner, voice, modifiers, chars) {
  if (chars.length > places.length) {
    return addmanner(mode, manner, voice, modifiers, chars.split('|'))
  }
  for (var i = 0; i < places.length; i++) {
    if (i < chars.length && chars[i] != ' ') {
      c(mode, chars[i], places[i], voice, manner, modifiers);
    }
  }
};
addmanner('ipa', 'stop', false, [], '  p  tʈckq ʡʔ');
addmanner('ipa', 'stop', true, [], '  b  dɖɟgɢ');
addmanner('ipa', 'stop', true, ['nasal'], '  mɱ n ɳɲŋɴ');
addmanner('ipa', 'trill', true, [], '  ʙ  r    ʀ');
addmanner('ipa', 'tap', true, [], '   ⱱ  ɾ ɽ');
addmanner('ipa', 'fricative', false, [], '  ɸfθsʃʂçxχħʜh');
addmanner('ipa', 'fricative', true, [], '  βvðzʒʐʝɣʁʕʢɦ');
c('ipa', 'ɬ', 'alveolar', false, 'fricative', ['lateral']);
c('ipa', 'ɮ', 'alveolar', true, 'fricative', ['lateral']);
addmanner('ipa', 'approximant', true, [], '   ʋ ɹ  ɻjɰ');
addmanner('ipa', 'approximant', true, ['lateral'], '     l ɭʎʟ');
addmanner('ipa', 'stop', true, ['ingressive'], '  ɓ  ɗ  ʄɠʛ');
addmanner('ipa', 'click', false, [], '  ʘ ǀ ǃ ǂ');
c('ipa', 'ǁ', 'alveolar', false, 'click', ['lateral']);

addmanner('cxs', 'stop', false, [], ' | |p| | |t|t`|c|k|q| |>\\|?');
addmanner('cxs', 'stop', true, [], ' | |b| | |d|d`|J\\|g|G\\');
addmanner('cxs', 'stop', true, ['nasal'], ' | |m|F| |n| |n`|J|N|N\\');
addmanner('cxs', 'trill', true, [], ' | |B\\| | |r| | | | |R\\');
addmanner('cxs', 'tap', true, [], ' | | | | | |4| |r`');
addmanner('cxs', 'fricative', false, [], ' | |p\\|f|T|s|S|s`|C|x|X|X\\|H\\|h');
addmanner('cxs', 'fricative', true, [], ' | |B|v|D|z|Z|z`|j\\|G|R|?\\|<\\|h\\');
c('cxs', 'K', 'alveolar', false, 'fricative', ['lateral']);
c('cxs', 'K\\', 'alveolar', true, 'fricative', ['lateral']);
addmanner('cxs', 'approximant', true, [], ' | | |P| |r\\| | |r\\`|j|M\\');
addmanner('cxs', 'approximant', true, ['lateral'], ' | | | | |l| |l`|L|L\\');
addmanner('cxs', 'stop', true, ['ingressive'], ' | |b_<| | |d_<| | |J\\_<|g_<|G\\_<');
addmanner('cxs', 'click', false, [], ' | |0\\| | | |!\\|!\\`|=\\');
c('cxs', '|\\', 'dental', false, 'click', []);
c('cxs', '|\\|\\', 'alveolar', false, 'click', ['lateral']);

var x;
do {
  x = cfillgaps();
} while (x > 0)

var backs = ['front', 'central', 'back'];
var heights = ['high', 'mid', 'low'];
var vmodifiers = ['voiceless', 'nasalized', 'lax', 'reduced'];
var vowid = {};
var vowch = {};
var getvid = function(back, height, rounded, mods) {
  var id = rounded ? 1 : 0;
  id += backs.indexOf(back) * 2;
  id += heights.indexOf(height) * 8;
  for (var i = 0; i < mods.length; i++) {
    id += Math.pow(2, vmodifiers.indexOf(mods[i])+5);
  }
  return id;
};
var vgetchr = function(mode, id) {
  if (vowid.hasOwnProperty(id)) {
    if (vowid[id].hasOwnProperty(mode)) {
      return vowid[id][mode];
    }
  }
  return false;
};
var vgetname = function(id) {
  var rounded = id & 1;
  id >>= 1;
  var back = backs[id % 4];
  id >>= 2;
  var height = heights[id % 4];
  id >>= 2;
  var mods = [];
  for (var i = 0; i < vmodifiers.length; i++) {
    if (id & 1) {
      mods.push(vmodifiers[i]);
    }
    id >>= 1;
  }
  var l = [height, back, (rounded ? '' : 'un') + 'rounded'];
  return l.concat(mods).join(' ') + ' vowel';
};
var v = function(mode, ch, height, back, rounded, mods) {
  if (ch == ' ') {
    return false;
  }
  var id = getvid(back, height, rounded, mods);
  if (vowid.hasOwnProperty(id)) {
    vowid[id][mode] = ch;
  } else {
    vowid[id] = {};
    vowid[id][mode] = ch;
  }
  if (vowch.hasOwnProperty(mode)) {
    vowch[mode][ch] = id;
  } else {
    vowch[mode] = {};
    vowch[mode][ch] = id;
  }
};
var vrow = function(mode, chs, height, mods) {
  if (chs.length != 6) {
    return vrow(mode, chs.split('|'), height, mods);
  }
  v(mode, chs[0], height, 'front', false, mods);
  v(mode, chs[1], height, 'front', true, mods);
  v(mode, chs[2], height, 'central', false, mods);
  v(mode, chs[3], height, 'central', true, mods);
  v(mode, chs[4], height, 'back', false, mods);
  v(mode, chs[5], height, 'back', true, mods);
}
vrow('ipa', 'iyɨʉɯu', 'high', []);
vrow('ipa', 'ɪʏ   ʊ', 'high', ['lax']);
vrow('ipa', 'eøɘɵɤo', 'mid', []);
vrow('ipa', 'ɛœɜɞʌɔ', 'mid', ['lax']);
vrow('ipa', 'aɶ  ɑɒ', 'low', []);
vrow('ipa', 'æ ɐ   ', 'low', ['lax']);
v('ipa', 'ə', 'mid', 'central', false, ['lax', 'reduced']);

vrow('cxs', 'i|y|1|u\\|M|u', 'high', []);
vrow('cxs', 'I|Y|I\\|U\\| |U', 'high', ['lax']);
vrow('cxs', 'e|2|@\\|8|7|o', 'mid', []);
vrow('cxs', 'E|9|3|3\\|V|O', 'mid', ['lax']);
vrow('cxs', 'a|&\\| | |A|Q', 'low', []);
vrow('cxs', '& 6   ', 'low', ['lax']);
v('cxs', '@', 'mid', 'central', false, ['lax', 'reduced']);
