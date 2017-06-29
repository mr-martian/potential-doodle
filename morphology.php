<html>
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("langs/".$_GET["lang"]."/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Morphology</title>";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = ".$_GET["lang"].";";
      echo file_get_contents("ipadata.js");
      echo file_get_contents("tools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit { display: none; }
      .catls { display: inline-block; vertical-align: top; }
      .featlist { border: 1px dashed grey; }
      table { border-collapse: collapse }
      .newgroup { border-top: 3px solid black; }
      td { border: 1px solid black; }
      .catval { margin-left: 15px; }
      #addtype { border: 1px solid black; }
    </style>
  </head>
  <body>
    <form id="submit" method="post" action="submitlang.php/">
      <input type="text" id="mode" name="mode" value="morphology"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="phoneupdate();">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <p>In all conjugational forms, <b>%</b> represents an unchanged stem. It serves as a wildcard in unconjugated forms.</p>
    <p>For more agglutinating languages, forms are duplicated when you add more categories. So if you have, for example, a past tense form of <i>%or</i> and you add aspect marking, every aspect will have <i>%or</i> for the past tense version.</p>
    <p>To have agreement, the category being agreed with must be labeled as marked, even if it isn't. So if adjectives agree with the gender of the noun but gender doesn't change the form of the noun in any way, say that nouns mark gender. Incidentally, you have to select the thing being agreed with before the category, so in this instance clicking on "gender" doesn't work until you click on "noun".</p>
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
    <button onclick="validate_all();">Save</button>
    <script>
      var getmode = function() { return getel('charmode').value; };
      var Pos = ['noun', 'verb', 'adjective', 'adverb', 'article', 'adposition', 'pronoun', 'conjunction', 'auxilliary', 'particle', 'clitic'];
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
            'past', 'present', 'future'],
          ['aspect',
            'perfect', 'imperfect', 'continuous', 'habitual'],
          ['mood/modality',
            'indicative', 'subjunctive'],
        ],
        adjective: [
        ],
        adverb: [
        ],
        article: [
          ['definiteness',
            'definite', 'indefinite'],
        ],
        adposition: [
        ],
        pronoun: [
          ['person',
            'first', 'second', 'third'],
        ],
        conjunction: [
          ['part of speech'].concat(Pos)
        ],
        particle: [],
        clitic: []
      };
      defaultcats.pronoun = defaultcats.pronoun.concat(defaultcats.noun);
      defaultcats.auxilliary = defaultcats.verb;
      var agree = [
        ['verb', 'subject', 'noun'],
        ['verb', 'object', 'noun'],
        ['auxilliary', 'subject', 'noun'],
        ['auxilliary', 'object', 'noun'],
        ['adjective', 'noun', 'noun'],
        ['article', 'noun', 'noun']
      ];
      //make an event listener for a category or value checkbox
      var catlisten = function(pos) {
        return function() {
          table(pos, read_table(pos), update_agree());
          //should see if there's a more efficient way of updating agreement
          //maybe check if the checkbox changed was a category?
          //-D.S. 02017-06-24
        };
      };
      //read grammatical category checkboxes
      var readcats = function(pos) {
        var ret = {cats: [], ops: []};
        var ch = getel(pos).children;
        var opadd;
        for (var i = 0; i < ch.length; i++) {
          opadd = readlist(ch[i]);
          if (opadd.checked && opadd.vals.length > 0) {
            ret.cats.push(opadd.cat);
            ret.ops.push(opadd.vals);
          }
        }
        return ret;
      };
      //add more checkboxes for grammatical categories
      var addcat = function() {
        var p = getel('addpos').value;
        var c = getel('addcat').value;
        var v = getel('addval').value;
        var chls = getel(p).children;
        for (var i = 0; i < chls.length; i++) {
          if (chls[i].firstChild.value == c) {
            addlist(chls[i], v, true);
            table(p, read_table(p));
            return;
          }
        }
        getel(p).appendChild(mklist(c, true, [v], [v], catlisten(p)));
        table(p, read_table(p));
      };
      //load all the defaults and the contents of langdata
      var alldefs = function(type) {
        var namls = [];
        var ls = defaultcats[type];
        for (var i = 0; i < ls.length; i++) {
          namls.push(ls[i][0]);
        }
        var ch = langdata[type+' categories'] || {cats: [], ops: []};
        for (var i = 0; i < ch.cats.length; i++) {
          if (!namls.includes(ch.cats[i])) {
            ls.push([ch.cats[i]].concat(ch.ops[i]));
          }
        }
        var div = getel(type);
        var n, r, x;
        for (var i = 0; i < ls.length; i++) {
          n = ls[i][0]; // category name
          r = ls[i].slice(1); // options
          x = ch.cats.indexOf(n);
          if (x != -1) {
            for (var j = 0; j < ch.ops[x].length; j++) {
              if (!r.includes(ch.ops[x][j])) {
                r.push(ch.ops[x][j]);
              }
            }
            div.appendChild(mklist(n, true, r, ch.ops[x], catlisten(type)));
          } else {
            div.appendChild(mklist(n, false, r, [], catlisten(type)));
          }
        }
      };
      var wordtype = function(pat) {
        var d = mkel('div', '<select><option value="default">Default</option><option value="string">String Pattern</option><option value="array">Complex Pattern</option></select>');
        d.className = 'wordpat';
        if (!pat) {
          d.children[0].value = 'default';
        } else if (Array.isArray(pat)) {
          d.children[0].value = 'array';
          d.appendChild(mkphin(pat, 'anything', langdata.phonemes, Object.keys(langdata['allophone categories']), false));
        } else {
          d.children[0].value = 'string';
          d.appendChild(mkinput('text', pat, null, null));
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
      //read agreement category checkboxes
      var readagree = function(pos) {
        var ret = {};
        var ch = getel(pos+'agree').children;
        if (ch.length == 0) {
          return langdata[pos+' agreement'];
        }
        var opadd, nam;
        for (var i = 0; i < ch.length; i++) {
          opadd = readlist(ch[i]);
          ret[opadd.cat] = opadd;
        }
        return ret;
      };
      //update agreement options
      //return the data to be passed to table()
      var update_agree = function() {
        var cats = {};
        var agg = {};
        for (var i = 0; i < Pos.length; i++) {
          cats[Pos[i]] = readcats(Pos[i]);
          agg[Pos[i]] = readagree(Pos[i]);
        }
        for (var i = 0; i < Pos.length; i++) {
          getel(Pos[i]+'agree').innerHTML = '';
        }
        var a, h, l;
        for (var i = 0; i < agree.length; i++) {
          a = agree[i];
          if (agg[a[0]].hasOwnProperty(a[1])) {
            h = agg[a[0]][a[1]].checked;
            l = agg[a[0]][a[1]].vals;
            agg[a[0]][a[1]].pos = a[2];
          } else {
            h = false;
            l = [];
          }
          getel(a[0]+'agree').appendChild(mklist(a[1], h, cats[a[2]].cats, l, catlisten(a[0])));
        }
        return [cats, agg];
      };
      //transform the return value of update_agree() into something usable by table()
      var extract_agree = function(data, pos) {
        var ret = {cats: [], ops: []};
        var obj = data[1][pos];
        var cat, ls, key, ind;
        for (var k in obj) {
          if (obj[k].checked) {
            cat = data[0][obj[k].pos];
            for (var i = 0; i < obj[k].vals.length; i++) {
              key = obj[k].vals[i];
              ind = cat.cats.indexOf(key);
              ret.cats.push(k+' '+cat.cats[ind]);
              ls = [];
              for (var s = 0; s < cat.ops[ind].length; s++) {
                ls.push(cat.ops[ind][s]+' '+k);
              }
              ret.ops.push(ls);
            }
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
        return forms.data[JSON.stringify(key)] || [];
      };
      //read all morphemes for a particular part of speech
      var read_table = function(pos) {
        var tab = getel(pos+'forms');
        var cats = JSON.parse(tab.getAttribute('data-cats') || '[]');
        var ops = JSON.parse(tab.getAttribute('data-ops') || '[]');
        var ret = {cats: cats, ops: ops, data: {}, roots: []};
        var nam;
        var rowel = tab.firstChild;
        for (var i = 1; i < rowel.children.length; i++) {
          ret.roots.push(readword(rowel.children[i].firstChild));
        }
        for (var i = 1; i < tab.children.length; i++) {
          rowel = tab.children[i];
          nam = rowel.getAttribute('data-key');
          ret.data[nam] = [];
          for (var j = 1; j < rowel.children.length; j++) {
            ret.data[nam].push(readword(rowel.children[j].firstChild));
          }
        }
        return ret;
      };
      //(re)build a part of speech's table of morphemes
      var table = function(pos, old, agree) {
        old = old || {cats: [], ops: [], data: {}, roots: []};
        agree = extract_agree(agree || [{}, {}], pos);
        var cats = readcats(pos);
        var rep = mkel('tbody', '<tr></tr>');
        var nmroots = getel(pos+'numroots');
        if (nmroots) {
          var ct = Math.max(parseInt(nmroots.value), 1);
        } else {
          var ct = old.roots.length || 1;
        }
        while (old.roots.length < ct) {
          old.roots.push(false);
        }
        rep.firstChild.appendChild(mkelch('td', mkinput('number', ct, pos+'numroots', catlisten(pos))));
        for (var i = 0; i < ct; i++) {
          rep.firstChild.appendChild(mkelch('td', wordtype(old.roots[i])));
        }
        cats.cats = cats.cats.concat(agree.cats);
        cats.ops = cats.ops.concat(agree.ops);
        rep.setAttribute('data-cats', JSON.stringify(cats.cats));
        rep.setAttribute('data-ops', JSON.stringify(cats.ops));
        rep.setAttribute('data-agree', JSON.stringify(agree));
        var rowlabs = iterls(cats.ops);
        if (cats.ops.length > 0) {
          var reset = rowlabs.length / cats.ops[0].length;
        } else {
          var reset = 1;
        }
        var lab, row, ls;
        for (var i = 0; i < rowlabs.length; i++) {
          lab = rowlabs[i].join(' ');
          row = mkel('tr', '<td>'+lab+'</td>');
          row.id = pos + ' ' + lab;
          row.setAttribute('data-key', JSON.stringify(rowlabs[i]));
          if (i % reset == 0) {
            row.className = 'newgroup';
          }
          ls = findform(old, cats.cats, rowlabs[i]);
          while (ls.length < ct) {
            ls.push(false);
          }
          for (var n = 0; n < ct; n++) {
            row.appendChild(mkelch('td', wordtype(ls[n])));
          }
          rep.appendChild(row);
        }
        getel(pos+'forms').outerHTML = '';
        rep.id = pos+'forms';
        getel(pos+'cont').appendChild(rep);
      };
      //build the page
      var posel = getel('pos');
      var psel = getel('addpos');
      var nullstr = langdata['display name']+' does not have ';
      for (var i = 0; i < Pos.length; i++) {
        psel.appendChild(mkop(Pos[i], Pos[i]));
        posel.appendChild(mkel('h2', tocap(Pos[i])+'s'));
        mkchk(posel, Pos[i]+'null', nullstr+Pos[i]+'s', null, langdata[Pos[i]+' is null'] ? nullstr+Pos[i]+'s' : null);
        posel.appendChild(mkname('div', Pos[i], 'featlist'));
        posel.appendChild(mkname('div', Pos[i]+'agree', 'featlist'));
        posel.appendChild(mkelch('table', mkname('tbody', Pos[i]+'forms', null)));
        posel.lastChild.id = Pos[i]+'cont';
        alldefs(Pos[i]);
      }
      update_agree();
      for (var i = 0; i < Pos.length; i++) {
        if (langdata.hasOwnProperty('morphology') && langdata.morphology.hasOwnProperty(Pos[i])) {
          table(Pos[i], langdata.morphology[Pos[i]], update_agree());
        } else {
          table(Pos[i], null, update_agree());
        }
      }
      var validate_all = function() {
        var pass = {morphology: {}};
        var d, tab;
        for (var i = 0; i < Pos.length; i++) {
          pass.morphology[Pos[i]] = read_table(Pos[i]);
          pass[Pos[i]+' categories'] = readcats(Pos[i]);
          pass[Pos[i]+' agreement'] = readagree(Pos[i]);
          pass[Pos[i]+' is null'] = getel(Pos[i]+'null').checked;
        }
        getel('langdata').value = JSON.stringify(pass);
        getel('id').value = langdata.id;
        getel('submit').submit();
      };
    </script>
  </body>
</html>
