<html>
  <!-- TODO:
    1. metathesis
    2. degeminnation
  -->
  <head>
    <meta charset="utf-8" />
    <?php
      $langid = 2;//intval($_GET["lang"]);
      $s = file_get_contents("../langs/$langid/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Phonotactics, Allophony, and Romanization</title>\n";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = $langid;";
      echo file_get_contents("../ipadata.js");
      echo file_get_contents("../ipatools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit { display: none; }
      div { display: inline-block; }
      .phones { margin-left: 20px; }
      li { margin: 20px; border: 1px solid gray; }
      .del { margin-left: 20px; color: red;}
      #rules > li > span { font-size: 35; }
      .altphone { width: 4em; }
    </style>
  </head>
  <body>
    <form id="submit" method="post" action="../submitlang.php/">
      <input type="text" id="mode" name="mode" value="allophony"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <h2>Categories</h2>
    <ul id="cats"></ul>
    <button onclick="makecat(null, null);">Add Category</button>
    <h2>Phonotactics</h2>
    <ul id="syls"></ul>
    <button onclick="makesyl(null, 'word');">Add Syllable Type</button>
    <h2>Allophonic Rules</h2>
    <ol id="rules"></ol>
    <button onclick="makerule(null);">Add Rule</button>
    <p>If your category name is v or c followed by numbers, it will end up being interpreted as some random vowel or consonant (respectively). If your category name is an IPA or CXS symbol, it could be interpreted as that phone. Naming a category "new" can also have messy results. To use a category in a rule already on the page, you may have to delete the rule and recreate it.</p>
    <button onclick="validate_all();">Save</button>
    <p id="errors"></p>
    <script>
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
      //retrieve all values from a <div> of <input>s or <select>s
      var getvals = function(div) {
        var r = [];
        for (var i = 0; i < div.children.length; i++) {
          r.push(div.children[i].value);
        }
        return r;
      };
      //create a delete button
      var mkdel = function() {
        var r = mkel('span', 'X');
        r.className = 'del';
        r.onclick = function() {
          r.parentNode.parentNode.removeChild(r.parentNode);
        };
        return r;
      };
      //read data from all categories
      var readcats = function() {
        var cats = getel('cats').children;
        var ret = {};
        var nam, phon;
        for (var i = 0; i < cats.length; i++) {
          nam = firstclass(cats[i], 'name').value;
          ret[nam] = getvals(firstclass(cats[i], 'phones'));
        }
        return ret;
      };
      //list all phonemes in langdata as source for <option>s
      //set as "selected" if id == sel
      //subsequently list all categories if withcats == true
      var listphones = function(sel, withcats) {
        var mode = getel('charmode').value;
        var s = '';
        var id;
        for (var i = 0; i < langdata.consonants.length; i++) {
          id = langdata.consonants[i];
          if ('c' + id == sel) {
            s += '<option value="c'+id+'" selected="selected">';
          } else {
            s += '<option value="c'+id+'">';
          }
          s += cgetchr(mode, id) || cgetname(id);
          s += '</option>';
        }
        for (var i = 0; i < langdata.vowels.length; i++) {
          id = langdata.vowels[i];
          if ('v' + id == sel) {
            s += '<option value="v'+id+'" selected="selected">';
          } else {
            s += '<option value="v'+id+'">';
          }
          s += vgetchr(mode, id) || vgetname(id);
          s += '</option>';
        }
        if (withcats) {
          var cats = Object.keys(readcats());
          for (var c = 0; c < cats.length; c++) {
            s += '<option value="' + cats[c];
            if (sel == cats[c]) {
              s += '" selected="selected';
            }
            s += '">' + cats[c] + '</option>';
          }
        }
        return s;
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
      //add category
      var makecat = function(nam, val) {
        if (!nam) {
          nam = '';
        }
        if (!val) {
          val = [];
        }
        var s = '<div><span>Elements: </span><input type="number"></input>';
        s += '<br><span>Name: </span><input type="text" class="name"></input></div>';
        s += '<div class="phones">';
        for (var i = 0; i < val.length; i++) {
          s += '<select>' + listphones(val[i], false) + '</select>';
        }
        s += '</div>';
        cat = mkel('li', s);
        cat.appendChild(mkdel());
        cat.children[0].children[4].value = nam;
        cat.children[0].children[1].value = val.length;
        cat.children[0].children[1].onchange = setchcount(cat.children[1], function() { return mkel('select', listphones(null, false)); });
        getel('cats').appendChild(cat);
      };
      //display a character
      var getname = function(n) {
        var mode = getel('charmode').value;
        if (n[0] == 'c') {
          var i = parseInt(n.slice(1));
          return cgetchr(mode, i) || cgetname(i);
        } else if (n[0] == 'v') {
          var i = parseInt(n.slice(1));
          return vgetchr(mode, i) || vgetname(i);
        }
      };
      //make a category input
      var makesel = function(cls, val) {
        var s = '<option value="null"'
        if (val == null) {
          s += ' selected="selected"';
        }
        s += '>';
        switch (cls) {
          case 'from':
          case 'to': s += 'Nothing'; break;
          case 'before': s += 'Beginning of Word'; break;
          case 'after': s += 'End of Word'; break;
        }
        s += '</option>' + listphones(val, true);
        if (cls == 'to') {
          s += '<option value="new">Enter Other Phone</option>';
        }
        if (!s.match('selected="selected"')) {
          sel = document.createElement('input');
          sel.type = "text";
          sel.className = "altphone";
          sel.value = getname(val);
          return sel;
        }
        var sel = mkel('select', s);
        if (cls == 'to') {
          sel.onchange = function() {
            if (sel.value == 'new') {
              sel.outerHTML = '<input type="text" class="altphone"></input>';
            }
          };
        }
        return sel;
      };
      //add a syllable structure input
      var makesyl = function(struct, mode) {
        if (!struct) {
          struct = [];
        }
        var l = [['word', 'Whole Word'], ['init', 'Word-Initial Syllable'], ['mid', 'Middle Syllable'], ['end', 'Word-Final Syllable']];
        var s = '<select class="mode">';
        for (var i = 0; i < l.length; i++) {
          s += '<option value="' + l[i][0] + '"';
          if (l[i][0] == mode) {
            s += ' selected="selected"';
          }
          s += '>' + l[i][1] + '</option>';
        }
        var el = mkel('li', s + '</select><span>Parts: </span>');
        var n = document.createElement('input');
        el.appendChild(n);
        el.appendChild(document.createElement('br'));
        var s = '';
        for (var i = 0; i < struct.length; i++) {
          s += '<select>' + listphones(struct[i], true) + '</select>';
        }
        var d = mkel('div', s);
        d.className = 'parts';
        el.appendChild(d);
        n.type = "number";
        n.value = struct.length;
        n.onchange = setchcount(d, function() { return mkel('select', listphones(null, true)); });
        el.appendChild(mkdel());
        getel('syls').appendChild(el);
      };
      //make a rule input
      var makeinput = function(cls, vals) {
        var s = mkel('div', '<span>Count: </span><input type="number" min="0"></input><br><div class="phones"></div>');
        s.className = cls;
        var sch = s.children;
        for (var i = 0; i < vals.length; i++) {
          sch[3].appendChild(makesel(cls, vals[i]));
        }
        sch[1].value = vals.length;
        sch[1].onchange = setchcount(sch[3], function() { return makesel(cls, null); });
        return s;
      };
      //make an allophonic rule input
      var makerule = function(rule) {
        if (!rule) {
          rule = {from: [], to: [], before: [], after: []};
        }
        var el = document.createElement('li');
        el.appendChild(mkel('span', '/'));
        el.appendChild(makeinput('from', rule.from));
        el.appendChild(mkel('span', '/ → ['));
        el.appendChild(makeinput('to', rule.to));
        el.appendChild(mkel('span', '] / '));
        el.appendChild(makeinput('before', rule.before));
        el.appendChild(mkel('span', ' ___ '));
        el.appendChild(makeinput('after', rule.after));
        el.appendChild(mkdel());
        getel('rules').appendChild(el);
      };
      //find any erroneous input in l, given category list cats
      var geterr = function(l, cats) {
        var mode = getel('charmode').value;
        var cons = consch[mode];
        var vows = vowch[mode];
        var err = [];
        var ret = [];
        for (var i = 0; i < l.length; i++) {
          if (l[i] == 'null') {
            ret.push(null);
          } else if (l[i].match(/^[vc]\d+$/)) {
            ret.push(l[i]);
          } else if (cats.hasOwnProperty(l[i])) {
            ret.push(l[i]);
          } else if (cons.hasOwnProperty(l[i])) {
            ret.push('c' + cons[l[i]]);
          } else if (vows.hasOwnProperty(l[i])) {
            ret.push('v' + vows[l[i]]);
          } else {
            err.push(l[i]);
          }
        }
        return [ret, err];
      }
      //get input from an allophonic rule
      var readrule = function(li, cats) {
        var ret = {from: [], to: [], before: [], after: []};
        var err = [];
        var e;
        for (k in ret) {
          e = geterr(getvals(firstclass(firstclass(li, k), 'phones')), cats);
          ret[k] = e[0];
          err = err.concat(e[1]);
        }
        return [ret, err];
      };
      var validate_all = function() {
        var pass = {allophony: [], phonotactics: []};
        getel('id').value = langdata.id;
        var l = getel('rules').children;
        var cats = readcats();
        pass['allophone categories'] = cats;
        var err = [];
        for (var i = 0; i < l.length; i++) {
          var res = readrule(l[i], cats);
          pass.allophony.push(res[0]);
          for (var e = 0; e < res[1].length; e++) {
            err.push('Unknown phone or category "'+res[1][e]+'" in rule '+(i+1));
          }
        }
        l = getel('syls').children;
        var m, e;
        for (var i = 0; i < l.length; i++) {
          m = {mode: firstclass(l[i], 'mode').value};
          e = geterr(getvals(firstclass(l[i], 'parts')), cats);
          m.parts = e[0];
          pass.phonotactics.push(m);
          for (var r = 0; r < e[1].length; r++) {
            err.push('Unknown phone or category "'+e[1][r]+'" in syllable structure '+(i+1));
          }
        }
        if (err.length == 0) {
          getel('langdata').value = JSON.stringify(pass);
          getel('submit').submit();
        } else {
          getel('errors').innerText = err.join('<br>');
        }
      };
      if (langdata.hasOwnProperty('allophone categories')) {
        for (k in langdata['allophone categories']) {
          makecat(k, langdata['allophone categories'][k]);
          console.log(k);
        }
      }
      if (langdata.hasOwnProperty('phonotactics')) {
        for (var i = 0; i < langdata.phonotactics.length; i++) {
          makesyl(langdata.phonotactics[i].parts, langdata.phonotactics[i].mode);
          console.log(i);
        }
      }
      if (langdata.hasOwnProperty('allophony')) {
        for (var i = 0; i < langdata.allophony.length; i++) {
          makerule(langdata.allophony[i]);
          console.log(i);
        }
      }
    </script>
  </body>
</html>
