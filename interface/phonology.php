<html>
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("../langs/".$_GET["lang"]."/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Phonology</title>";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = ".$_GET["lang"].";";
      echo file_get_contents("ipadata.js");
      echo file_get_contents("tools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit { display: none; }
      #consonantstable, #vowelstable {
        border-collapse: collapse;
        border: 1px solid black;
      }
      td, tr { border: 1px solid black; }
      .cboldedge { border-top-width: 3px; }
      .mlab { text-transform: capitalize; }
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
    <form id="submit" method="post" action="submitlang.php/">
      <input type="text" id="mode" name="mode" value="phonology"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="consupdate(); vowupdate();">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <br />
    <div id="consphonefeatures">
      <span>Include features:</span>
    </div>
    <table id="consonantstable">
      <tbody id="consonants"></tbody>
    </table>
    <div id="vowphonefeatures">
      <span>Include features:</span>
    </div>
    <table id="vowelstable">
      <tbody id="vowels"></tbody>
    </table>
    <h2>Categories</h2>
    <ul id="cats"></ul>
    <button onclick="makecat(null, null);">Add Category</button>
    <p>If your category name is v or c followed by numbers, it will end up being interpreted as some random vowel or consonant (respectively). If your category name is an IPA or CXS symbol, it could be interpreted as that phone. Naming a category "new" can also have messy results. To use a category in a rule already on the page, you may have to delete the rule and recreate it.</p>
    <h2>Phonotactics</h2>
    <ul id="syls"></ul>
    <button onclick="makesyl(null, 'word');">Add Syllable Type</button>
    <h2>Allophonic Rules</h2>
    <ol id="rules"></ol>
    <button onclick="makerule(null, false);">Add Rule</button>
    <h2>Romanization</h2>
    <ol id="romrules"></ol>
    <button onclick="makerule(null, true);">Add Character</button>
    <input type="checkbox" id="irreg"></input>
    <label for="irreg">Language has irregular spellings</label>
    <p>Phonological representations will be transformed in exactly the same manner as with allophony. Any unmodified phonemes will be displayed in IPA.</p>
    <br>
    <button onclick="validate_all();">Save</button>
    <p id="errors"></p>
    <script>
      var Consfeat = getel('consphonefeatures');
      var Constab = getel('consonants');
      var Vowfeat = getel('vowphonefeatures');
      var Vowtab = getel('vowels');
      var Mode = getel('charmode');
      var getmode = function() { return Mode.value; };
      
      var modsubsets = function(ml) {
        if (ml.length == 0) {
          return [[]];
        } else {
          var r = modsubsets(ml.slice(1));
          var ret = [];
          for (var i = 0; i < r.length; i++) {
            ret.push(r[i]);
            ret.push(r[i].concat(ml[0]));
          }
          return ret;
        }
      };
      var consupdate = function() {
        var mods = modsubsets(getchecks(Consfeat));
        var cons = getchecks(Constab);
        Constab.innerHTML = '<tr><td></td><td>'+places.join('</td><td>')+'</td></tr>';
        var voices = [false, true];
        var id, s;
        for (var j = 0; j < manners.length; j++) {
          for (var i = 0; i < voices.length; i++) {
            for (var m = 0; m < mods.length; m++) {
              id = getcid(places[0], voices[i], manners[j], mods[m]);
              s = mkel('tr', '<td>' + cgetname(id, true) + '</td>');
              if (i == 0 && m == 0) {
                s.className = 'cboldedge';
              }
              for (var k = 0; k < places.length; k++) {
                s.appendChild(mkel('td', ''));
                mkchk(s.children[k+1], 'c'+id, cgetchr(id) || ' ', null, cons);
                id += 2;
              }
              Constab.appendChild(s);
            }
          }
        }
      };
      for (var i = 0; i < modifiers.length; i++) {
        mkchk(Consfeat, modifiers[i], tocap(modifiers[i]), consupdate, langdata['consonant features']);
      }
      consupdate();
      var vowupdate = function() {
        var mods = modsubsets(getchecks(Vowfeat));
        var vows = getchecks(Vowtab);
        Vowtab.innerHTML = '<tr><td></td><td>'+backs.join('</td><td>')+'</td></tr>';
        for (var j = 0; j < heights.length; j++) {
          for (var m = 0; m < mods.length; m++) {
            s = mkel('tr', '<td>'+mods[m].join(' ')+' '+heights[j]+'</td>');
            if (m == 0) {
              s.className = 'cboldedge';
            }
            for (var k = 0; k < backs.length; k++) {
              s.appendChild(mkel('td', ''));
              id = getvid(backs[k], heights[j], false, mods[m]);
              mkchk(s.children[k+1], 'v'+id, vgetchr(id)||' ', null, vows);
              mkchk(s.children[k+1], 'v'+(id+1), vgetchr(id+1)||' ', null, vows);
            }
            Vowtab.appendChild(s);
          }
        }
      };
      for (var i = 0; i < vmodifiers.length; i++) {
        mkchk(Vowfeat, vmodifiers[i], tocap(vmodifiers[i]), vowupdate, langdata['vowel features']);
      }
      vowupdate();
      if (langdata.hasOwnProperty('phonemes')) {
        for (var i = 0; i < langdata.phonemes.length; i++) {
          getel(langdata.phonemes[i]).checked = true;
        }
      }
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
          ret[nam] = readphin(firstclass(cats[i], 'ph-in'));
        }
        return ret;
      };
      //return the names of all the categories
      var listcats = function() {
        var cats = getel('cats').children;
        var ret = [];
        for (var i = 0; i < cats.length; i++) {
          ret.push(firstclass(cats[i], 'name').value);
        }
        return ret;
      };
      //get the ids of all checked phones
      var allphones = function() {
        return getchecks(Constab).concat(getchecks(Vowtab));
      };
      //add category
      var makecat = function(nam, val) {
        var cat = mkel('li', '<span>Name: </span><input type="text" class="name"></input>');
        if (nam) {
          cat.children[1].value = nam;
        }
        cat.appendChild(mkphin(val, null, allphones(), [], false));
        cat.appendChild(mkdel());
        getel('cats').appendChild(cat);
      };
      //add a syllable structure input
      var makesyl = function(struct, mode) {
        var el = mkel('li', '<select class="mode"></select>');
        el.firstChild.appendChild(mkop('Whole Word', 'word'));
        el.firstChild.appendChild(mkop('Word-Initial Syllable', 'init'));
        el.firstChild.appendChild(mkop('Middle Syllable', 'mid'));
        el.firstChild.appendChild(mkop('Word-Final Syllable', 'end'));
        if (mode) {
          el.firstChild.value = mode;
        }
        el.appendChild(mkphin(struct || [], null, allphones(), listcats(), false));
        el.appendChild(mkdel());
        getel('syls').appendChild(el);
      };
      //make an allophonic rule input
      var makerule = function(rule, isrom) {
        rule = rule || {from: [], to: (isrom ? [""] : []), before: [], after: []};
        var phls = allphones();
        var cats = listcats();
        var el = document.createElement('li');
        el.appendChild(mkel('span', '/'));
        el.appendChild(mkphin(rule.from, 'Nothing', phls, cats, false));
        el.appendChild(mkel('span', '/ → ['));
        el.appendChild(mkphin(rule.to, 'Nothing', phls, cats, true));
        el.appendChild(mkel('span', '] / '));
        el.appendChild(mkphin(rule.before, 'Beginning of Word', phls, cats, false));
        el.appendChild(mkel('span', ' ___ '));
        el.appendChild(mkphin(rule.after, 'End of Word', phls, cats, false));
        el.appendChild(mkdel());
        getel(isrom ? 'romrules' : 'rules').appendChild(el);
      };
      //find any erroneous input in l, given category list cats
      var geterr = function(l) {
        var cons = consch[Mode.value];
        var vows = vowch[Mode.value];
        var cats = listcats();
        var err = [];
        var ret = [];
        for (var i = 0; i < l.length; i++) {
          if (l[i] == 'null') {
            ret.push(null);
          } else if (l[i].match(/^[vc]\d+$/)) {
            ret.push(l[i]);
          } else if (cats.includes(l[i])) {
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
      var readrule = function(li, isrom) {
        var ret = {};
        var err = [];
        var e = geterr(readphin(li.children[1]));
        ret.from = e[0];
        err = err.concat(e[1]);
        if (isrom) {
          ret.to = readphin(li.children[3]);
        } else {
          e = geterr(readphin(li.children[3]));
          ret.to = e[0];
          err = err.concat(e[1]);
        }
        e = geterr(readphin(li.children[5]));
        ret.before = e[0];
        err = err.concat(e[1]);
        e = geterr(readphin(li.children[7]));
        ret.after = e[0];
        err = err.concat(e[1]);
        return [ret, err];
      };
      var validate_all = function() {
        var pass = {allophony: [], phonotactics: [], romanization: []};
        getel('id').value = langdata.id;
        pass.phonemes = allphones();
        pass['consonant features'] = getchecks(Consfeat);
        pass['vowel features'] = getchecks(Vowfeat);
        var l = getel('rules').children;
        var cats = readcats();
        pass['allophone categories'] = cats;
        var err = [];
        for (var i = 0; i < l.length; i++) {
          var res = readrule(l[i], false);
          pass.allophony.push(res[0]);
          for (var e = 0; e < res[1].length; e++) {
            err.push('Unknown phone or category "'+res[1][e]+'" in rule '+(i+1));
          }
        }
        l = getel('syls').children;
        var m, e;
        for (var i = 0; i < l.length; i++) {
          m = {mode: firstclass(l[i], 'mode').value};
          e = geterr(readphin(firstclass(l[i], 'ph-in')));
          m.parts = e[0];
          pass.phonotactics.push(m);
          for (var r = 0; r < e[1].length; r++) {
            err.push('Unknown phone or category "'+e[1][r]+'" in syllable structure '+(i+1));
          }
        }
        l = getel('romrules').children;
        for (var i = 0; i < l.length; i++) {
          var res = readrule(l[i], true);
          pass.romanization.push(res[0]);
          for (var e = 0; e < res[1].length; e++) {
            err.push('Unknown phone or category "'+res[1][e]+'" in romanization rule '+(i+1));
          }
        }
        pass['has irregular spellings'] = getel('irreg').checked;
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
        }
      }
      if (langdata.hasOwnProperty('phonotactics')) {
        for (var i = 0; i < langdata.phonotactics.length; i++) {
          makesyl(langdata.phonotactics[i].parts, langdata.phonotactics[i].mode);
        }
      }
      if (langdata.hasOwnProperty('allophony')) {
        for (var i = 0; i < langdata.allophony.length; i++) {
          makerule(langdata.allophony[i], false);
        }
      }
      if (langdata.hasOwnProperty('has irregular spellings')) {
        getel('irreg').checked = langdata['has irregular spellings'];
      }
      if (langdata.hasOwnProperty('romanization')) {
        for (var i = 0; i < langdata.romanization.length; i++) {
          makerule(langdata.romanization[i], true);
        }
      }
    </script>
  </body>
</html>
