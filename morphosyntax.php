<html>
  <!-- TODO
    1. add section for predicting word-forms
    2. add more features and values
    3. word order
    4. lexical property specification in table header?
  -->
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("langs/".$_GET["lang"]."/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Morphosyntax</title>";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = ".$_GET["lang"].";";
      echo file_get_contents("ipadata.js");
      echo file_get_contents("tools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit { display: none; }
      .featlist > div { display: inline-block; vertical-align: top; }
      table { border-collapse: collapse }
      .newgroup { border-top: 3px solid black; }
      td { border: 1px solid black; }
      .catval { margin-left: 15px; }
    </style>
  </head>
  <body>
    <form id="submit" method="post" action="submitlang.php/">
      <input type="text" id="mode" name="mode" value="morphosyntax"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="phoneupdate();">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <p>In all conjugational forms, <b>%</b> represents an unchanged stem. It serves as a wildcard in unconjugated forms.</p>
    <div id="addtype">
      <p>Add a category or value to be marked</p>
      <span>Part of Speech: </span>
      <select id="addpos"></select>
      <span>Category: </span>
      <input type="text" id="addcat"></input>
      <span>Value Name: </span>
      <input type="text" id="addval"></input>
      <button onclick="addcat();">Add</button>
    </div>
    <div id="pos"></div>
    <h2>Word Orders</h2>
    <div id="order"></h2>
    <button onclick="validate_all();">Save</button>
    <p id="errors"></p>
    <script>
      var getmode = function() { return getel('charmode').value; };
      var pos = ['noun', 'verb', 'adjective', 'adverb', 'article', 'preposition'];
      var defaultcats = {
        noun: [
          ['plurality',
            'singular', 'plural', 'pacual', 'singulative'],
          ['case',
            'nominative', 'accusative', 'dative', 'genitive', 'ergative', 'absolutive', 'vocative', 'instrumental'],
          ['animacy',
            'animate', 'inanimate'],
        ],
        verb: [
          ['tense',
            ],
          ['aspect',
            ],
          ['mood/modality',
            ],
          ['subject person',
            ],
          ['subject number',
            ],
        ],
        adjective: [
          ['noun number',
            'singular', 'plural', 'pacual', 'singulative'],
          ['noun class/gender',
            'male', 'female', 'neuter', 'animate', 'inanimate', 'fire', 'earth', 'water', 'air'],
          ['noun case',
            'nominative', 'accusative', 'dative', 'genitive', 'ergative', 'absolutive', 'vocative', 'instrumental'],
        ],
        adverb: [
        ],
        article: [
          ['definiteness',
            'definite', 'indefinite'],
          ['noun number',
            'singular', 'plural', 'pacual', 'singulative'],
          ['noun class/gender',
            'male', 'female', 'neuter', 'animate', 'inanimate', 'fire', 'earth', 'water', 'air'],
          ['noun case',
            'nominative', 'accusative', 'dative', 'genitive', 'ergative', 'absolutive', 'vocative', 'instrumental'],
        ],
        preposition: [
        ]
      };
      //make an event listener for a category or value checkbox
      var catlisten = function(pos) {
        return function() { table(pos, read_table(pos)); };
      };
      //add more checkboxes for grammatical categories
      var addcat = function() {
        var p = getel('addpos').value;
        var c = getel('addcat').value;
        var v = getel('addval').value;
        var el = null;
        var chls = getel(p).children;
        for (var i = 0; i < chls.length; i++) {
          if (chls[i].firstChild.value == c) {
            el = chls[i].lastChild;
            break;
          }
        }
        if (!el) {
          var div = document.createElement('div');
          mkchk(div, c, c, catlisten(p), c);
          el = document.createElement('div');
          el.className = 'catval';
          div.appendChild(el);
          getel(p).appendChild(div);
        }
        mkchk(el, v, v, catlisten(p), v);
        el.appendChild(document.createElement('br'));
        table(p, read_table(p));
      };
      //display the options for a grammatical category
      var makecat = function(div, nam, vals, checked, selected) {
        var el = document.createElement('div');
        mkchk(el, nam, nam, catlisten(div), checked ? [nam] : []);
        var ls = document.createElement('div');
        ls.className = 'catval';
        for (var i = 0; i < vals.length; i++) {
          mkchk(ls, vals[i], null, catlisten(div), selected);
          ls.appendChild(mkel('br', ''));
        }
        el.appendChild(ls);
        getel(div).appendChild(el);
      };
      //load all the defaults and the contents of langdata
      var alldefs = function(type) {
        var namls = [];
        var ls = defaultcats[type];
        for (var i = 0; i < ls.length; i++) {
          namls.push(ls[i][0]);
        }
        var ch = langdata[type+' categories'] || {};
        for (var k in ch) {
          if (!namls.includes(k)) {
            ls.push([k].concat(ch[k]));
          }
        }
        for (var i = 0; i < ls.length; i++) {
          var n = ls[i][0]; // category name
          var r = ls[i].slice(1); // options
          if (ch.hasOwnProperty(n)) {
            for (var j = 0; j < ch[n].length; j++) {
              if (!r.includes(ch[n][j])) {
                r.push(ch[n][j]);
              }
            }
            makecat(type, n, r, true, ch[n]);
          } else {
            makecat(type, n, r, false, []);
          }
        }
      };
      var wordtype = function(pat) {
        var d = document.createElement('div');
        d.className = 'wordpat';
        d.innerHTML = '<select><option value="default">Default</option><option value="string">String Pattern</option><option value="array">Complex Pattern</option></select>';
        if (!pat) {
          d.children[0].value = 'default';
        } else if (Array.isArray(pat)) {
          d.children[0].value = 'array';
          d.appendChild(mkphin(pat, 'anything', langdata.phonemes, langdata['allophone categories'], false));
        } else {
          d.children[0].value = 'string';
          var ap = document.createElement('input');
          ap.type = 'text';
          ap.value = pat;
          d.appendChild(ap);
        }
        d.firstChild.onchange = function() {
          var f = d.firstChild.value;
          var p = (f == 'default') ? false : ((f == 'array') ? [] : '%');
          d.parentNode.replaceChild(wordtype(p), d);
        };
        return d;
      };
      var readword = function(el) {
        switch (el.firstChild.value) {
          case 'default': return false;
          case 'array': return readphin(el.children[1]);
          case 'string': return el.children[1].value;
        }
      };
      var iterls = function(ops) {
        if (ops.length == 0) {
          return [[]];
        }
        var ret = [];
        var temp;
        for (var i = 0; i < ops[0].length; i++) {
          temp = iterls(ops.slice(1));
          for (var j = 0; j < temp.length; j++) {
            ret.push([ops[0][i]].concat(temp[j]));
          }
        }
        return ret;
      };
      //forms = return value from read_table()
      //cats = list of categories
      //ops = list of values
      //find best corresponding value
      var findform = function(forms, cats, ops) {
        var key = [];
        var x;
        for (var i = 0; i < forms.cats.length; i++) {
          x = cats.indexOf(forms.cats[i]);
          if (x == -1 || !forms.ops[i].includes(ops[x])) {
            key.push(forms.ops[i][0]);
          } else {
            key.push(ops[x]);
          }
        }
        return forms.data[key.join(' ')] || [];
      };
      var read_table = function(pos) {
        var tab = getel(pos+'forms');
        var cats = JSON.parse(tab.getAttribute('data-cats') || '[]');
        var ops = JSON.parse(tab.getAttribute('data-ops') || '[]');
        var ret = {cats: cats, ops: ops, data: {}, roots: []};
        var rows = iterls(ops);
        var nam;
        var rowel = tab.firstChild;
        for (var i = 1; i < rowel.children.length; i++) {
          ret.roots.push(readword(rowel.children[i].firstChild));
        }
        for (var i = 0; i < rows.length; i++) {
          if (rows[i].length == 0) {
            continue;
          }
          nam = rows[i].join(' ');
          ret.data[nam] = [];
          rowel = document.getElementById(pos + ' ' + nam);
          for (var j = 1; j < rowel.children.length; j++) {
            ret.data[nam].push(readword(rowel.children[j].firstChild));
          }
        }
        return ret;
      };
      var table = function(pos, old) {
        old = old || {cats: [], ops: [], data: {}, roots: []};
        var rep = document.createElement('tbody');
        var ct = Math.max(parseInt(getel(pos + 'numroots').value), 1);
        getel(pos+'numroots').value = ct;
        while (old.roots.length < ct) {
          old.roots.push(false);
        }
        var first = mkel('tr', '<td></td>');
        var td;
        for (var i = 0; i < ct; i++) {
          td = document.createElement('td');
          td.appendChild(wordtype(old.roots[i]));
          first.appendChild(td);
        }
        rep.appendChild(first);
        var ops = [];
        var cats = [];
        var ch = getel(pos).children;
        var reset = 1;
        var opadd;
        for (var i = 0; i < ch.length; i++) {
          if (ch[i].children[0].checked) {
            opadd = getchecks(ch[i]);
            if (opadd.length > 1) {
              ops.push(opadd.slice(1));
              cats.push(opadd[0]);
              if (i > 0) {
                reset *= opadd.length-1;
              }
            }
          }
        }
        rep.setAttribute('data-cats', JSON.stringify(cats));
        rep.setAttribute('data-ops', JSON.stringify(ops));
        var rowlabs = iterls(ops);
        var lab, row, oldrow, td, vals, ls;
        for (var i = 0; i < rowlabs.length; i++) {
          lab = rowlabs[i].join(' ');
          row = mkel('tr', '<td>'+lab+'</td>');
          row.id = pos + ' ' + lab;
          if (i % reset == 0) {
            row.className = 'newgroup';
          }
          ls = findform(old, cats, rowlabs[i]);
          while (ls.length < ct) {
            ls.push(false);
          }
          for (var n = 0; n < ct; n++) {
            td = document.createElement('td');
            td.appendChild(wordtype(ls[n]));
            row.appendChild(td);
          }
          rep.appendChild(row);
        }
        getel(pos+'forms').outerHTML = '';
        rep.id = pos+'forms';
        getel(pos+'cont').appendChild(rep);
      };
      var el;
      var posel = getel('pos');
      var psel = getel('addpos');
      for (var i = 0; i < pos.length; i++) {
        psel.appendChild(mkop(pos[i], pos[i]));
        posel.appendChild(mkel('h2', tocap(pos[i])+'s'));
        el = document.createElement('div');
        el.className = 'featlist';
        el.id = pos[i];
        posel.appendChild(el);
        posel.appendChild(mkel('span', 'Number of conjugational patterns: '));
        posel.appendChild(mkel('input', ''));
        posel.lastChild.id = pos[i]+'numroots';
        posel.lastChild.type = 'number';
        posel.lastChild.value = 1;
        posel.lastChild.onchange = catlisten(pos[i]);
        el = mkel('table', '<tbody id="'+pos[i]+'forms"></tbody>');
        el.id = pos[i]+'cont';
        posel.appendChild(el);
        alldefs(pos[i]);
        if (langdata.hasOwnProperty('morphology') && langdata.morphology.hasOwnProperty(pos[i])) {
          getel(pos[i]+'numroots').value = langdata.morphology[pos[i]].roots.length || 1;
          table(pos[i], langdata.morphology[pos[i]]);
        } else {
          table(pos[i], null);
        }
      }
      var validate_all = function() {
        var pass = {morphology: {}};
        var d, tab;
        for (var i = 0; i < pos.length; i++) {
          tab = read_table(pos[i]);
          pass.morphology[pos[i]] = tab;
          d = {};
          for (var k = 0; k < tab.cats.length; k++) {
            d[tab.cats[k]] = tab.ops[k];
          }
          pass[pos[i]+' categories'] = d;
        }
        getel('langdata').value = JSON.stringify(pass);
        getel('id').value = langdata.id;
        getel('submit').submit();
      };
    </script>
  </body>
</html>
