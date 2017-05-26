///////////IPA Tools
var places = ['bimanual', 'faciomanual', 'bilabial', 'labiodental', 'dental', 'alveolar', 'postalveolar', 'retroflex', 'palatal', 'velar', 'uvular', 'pharyngeal', 'epiglottal', 'glottal'];
var manners = ['stop', 'fricative', 'trill', 'tap', 'approximant', 'click'];
var modifiers = ['lateral', 'ejective', 'ingressive', 'aspirated', 'nasal'];
//given the properties of a consonant return its id as an int
var getcid = function(place, voice, manner, mods) {
  id = voice ? 1 : 0;
  id += places.indexOf(place) << 1;
  id += manners.indexOf(manner) << 5;
  for (var i = 0; i < mods.length; i++) {
    id += 1 << (modifiers.indexOf(mods[i])+8);
  }
  return id;
};
//given a consonant's id (int), return registered char or false
var cgetchr = function(mode, id) {
  if (consid.hasOwnProperty(id)) {
    if (consid[id].hasOwnProperty(mode)) {
      return consid[id][mode];
    }
  }
  return false;
};
//given a consonant id (int) return a human-readable description of properties
var cgetname = function(id, unplace) {
  var voiced = id & 1;
  id >>= 1;
  if (unplace) {
    var place = '';
  } else {
    var place = ' '+places[id % 16];
  }
  id >>= 4;
  var manner = ' '+manners[id % 8];
  id >>= 3;
  var mods = [];
  for (var i = 0; i < modifiers.length; i++) {
    if (id & 1) {
      mods.push(modifiers[i]);
    }
    id >>= 1;
  }
  var l = ['voice' + (voiced ? 'd' : 'less')].concat(mods);
  return l.join(' ') + place + manner;
};

var backs = ['front', 'central', 'back'];
var heights = ['high', 'mid', 'low'];
var vmodifiers = ['voiceless', 'nasalized', 'lax', 'reduced'];
//given the properties of a vowel return its id as an int
var getvid = function(back, height, rounded, mods) {
  var id = rounded ? 1 : 0;
  id += backs.indexOf(back) * 2;
  id += heights.indexOf(height) * 8;
  for (var i = 0; i < mods.length; i++) {
    id += Math.pow(2, vmodifiers.indexOf(mods[i])+5);
  }
  return id;
};
//given a vowel's id (int), return registered char or false
var vgetchr = function(mode, id) {
  if (vowid.hasOwnProperty(id)) {
    if (vowid[id].hasOwnProperty(mode)) {
      return vowid[id][mode];
    }
  }
  return false;
};
//given a vowel id (int) return a human-readable description of properties
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
//a phone id (string) return char (if available) or name
var getname = function(mode, n) {
  if (n[0] == 'c') {
    var i = parseInt(n.slice(1));
    return cgetchr(mode, i) || cgetname(i);
  } else if (n[0] == 'v') {
    var i = parseInt(n.slice(1));
    return vgetchr(mode, i) || vgetname(i);
  }
};
/////////////General Utilities
//abbreviate document.getElementById()
var getel = function(id) {
  return document.getElementById(id);
};
//retrieve first child node with a particular class name
var firstclass = function(el, cls) {
  return el.getElementsByClassName(cls)[0];
};
//make an element, set innerHTML and return
var mkel = function(nam, inner) {
  var r = document.createElement(nam);
  r.innerHTML = inner;
  return r;
};
//returns a function to be assigned as onchange handler for number <input>s
//get number from input
//if chls has more than that many children, delete the extras
//otherwise create more with fn() and append them
var setchcount = function(chls, fn) {
  return function(event) {
    var input = event.target || event.srcElement;
    var n = Math.max(parseInt(input.value), 0);
    while (chls.children.length > n) {
      chls.removeChild(chls.children[chls.children.length-1]);
    }
    while (chls.children.length < n) {
      chls.appendChild(fn());
    }
  };
};
//make and label a checkbox, append as child to parent
//will be checked if among is an array containing value or == value
var mkchk = function(parent, value, label, change, among) {
  var ch = document.createElement('input');
  ch.type = 'checkbox';
  ch.id = value;
  ch.value = value;
  if (change) {
    ch.onchange = change;
  }
  if (value == among || (Array.isArray(among) && among.includes(value))) {
    ch.checked = true;
  }
  parent.appendChild(ch);
  var lab = document.createElement('label');
  lab.setAttribute('for', value);
  lab.innerText = label || value;
  parent.appendChild(lab);
};
