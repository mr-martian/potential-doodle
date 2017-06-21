//
//Any page using this file must define getmode() which returns the character display mode (currently 'ipa' or 'cxs').
//
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
var cgetchr = function(id) {
  if (consid.hasOwnProperty(id)) {
    var mode = getmode();
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
var vgetchr = function(id) {
  if (vowid.hasOwnProperty(id)) {
    var mode = getmode();
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
var getname = function(n) {
  if (n[0] == 'c') {
    var i = parseInt(n.slice(1));
    return cgetchr(i) || cgetname(i);
  } else if (n[0] == 'v') {
    var i = parseInt(n.slice(1));
    return vgetchr(i) || vgetname(i);
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
//make and element, append a child, and return
var mkelch = function(nam, ch) {
  var r = document.createElement(nam);
  r.appendChild(ch);
  return r;
};
//create and a return an <input> with type, value, onchange, id
var mkinput = function(type, val, id, onchange) {
  var r = document.createElement('input');
  r.type = type;
  r.value = val || '';
  r.id = id || '';
  r.onchange = onchange || function() {};
  return r;
};
//create and return an element with id and/or className set
var mkname = function(type, id, cls) {
  var r = document.createElement(type);
  r.id = id || '';
  r.className = cls || '';
  return r;
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
//capitalize the first letter of a string
var tocap = function(x) {
  return x[0].toUpperCase() + x.slice(1);
};
//get values of all checked checkboxes in a <div>
var getchecks = function(el) {
  var ls = el.getElementsByTagName('input');
  var ret = [];
  for (var i = 0; i < ls.length; i++) {
    if (ls[i].checked) {
      ret.push(ls[i].value);
    }
  }
  return ret;
};
//create and return an <option>
var mkop = function(lab, val) {
  var ret = mkel('option', lab);
  ret.value = val;
  return ret;
};
//create a <select> for use in phoneme structure inputs
var make_phone_select = function(val, none, phones, cats, andblank) {
  var ret = document.createElement('select');
  ret.appendChild(mkel('option', '---'));
  var op;
  if (none) {
    ret.appendChild(mkop(none, 'null'));
  }
  for (var i = 0; i < phones.length; i++) {
    ret.appendChild(mkop(getname(phones[i]), phones[i]));
  }
  for (var i = 0; i < cats.length; i++) {
    ret.appendChild(mkop(cats[i], cats[i]));
  }
  if (andblank) {
    ret.appendChild(mkop('Enter Other Phone', 'new'));
    ret.onchange = function() {
      if (ret.value == 'new') {
        ret.outerHTML = '<input type="text" class="altphone"></input>';
      }
    };
  }
  if (val == null) {
    ret.selectedIndex = -1;
  } else if (phones.includes(val) || cats.includes(val)) {
    ret.value = val;
  } else {
    var el = document.createElement('input');
    el.type = 'text';
    el.className = 'altphone';
    el.value = val;
    return el;
  }
  return ret;
};
//returns a function to be assigned as onchange handler
//for a phoneme structure input
//get number from input
//if the .phones <div> has more than that many children, delete the extras
//otherwise create more with make_phone_select(..) and append them
var setchcount = function(none, phones, cats, andblank) {
  return function(event) {
    console.log([none, phones, cats, andblank]);
    var input = event.target || event.srcElement;
    var chls = input.parentNode.children[3];
    var n = Math.max(parseInt(input.value), 0);
    while (chls.children.length > n) {
      chls.removeChild(chls.children[chls.children.length-1]);
    }
    while (chls.children.length < n) {
      chls.appendChild(make_phone_select(null, none, phones, cats, andblank));
    }
  };
};
//create and return a phoneme structure input
var mkphin = function(val, none, phones, cats, withblanks) {
  val = val || [];
  var ret = mkel('div', '<span>Elements: </span><input type="number"></input><br><div class="phones"></div>');
  ret.className = 'ph-in';
  ret.children[1].value = val.length;
  ret.children[1].onchange = setchcount(none, phones, cats, withblanks);
  var sel;
  for (var i = 0; i < val.length; i++) {
    ret.children[3].appendChild(make_phone_select(val[i], none, phones, cats, withblanks));
  }
  return ret;
};
//read the value of a phoneme structure input
var readphin = function(el) {
  var ret = [];
  var ins = el.children[3].children;
  for (i = 0; i < ins.length; i++) {
    if (ins[i].value) {
      ret.push(ins[i].value);
    }
  }
  return ret;
};
