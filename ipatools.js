var places = ['bimanual', 'faciomanual', 'bilabial', 'labiodental', 'dental', 'alveolar', 'postalveolar', 'retroflex', 'palatal', 'velar', 'uvular', 'pharyngeal', 'epiglottal', 'glottal'];
var manners = ['stop', 'fricative', 'trill', 'tap', 'approximant', 'click'];
var modifiers = ['lateral', 'ejective', 'ingressive', 'aspirated', 'nasal'];
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

var backs = ['front', 'central', 'back'];
var heights = ['high', 'mid', 'low'];
var vmodifiers = ['voiceless', 'nasalized', 'lax', 'reduced'];
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
