<html>
  <!-- TODO:
    1. add phonotactics input (different page?)
    2. metathesis? degeminnation?
    3. decide between Python pregenerated IPA (~45kB) or JS client-side generation (~15kB)
  -->
  <head>
    <meta charset="utf-8" />
    <?php
      $langid = 2;//intval($_GET["lang"]);
      $s = file_get_contents("../langs/$langid/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Phonotactics and Allophony</title>\n";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = $langid;";
      if ($_GET["preload"] == 'true') {
        $output = array();
        exec('python3 ../ipa.py', $output);
        echo $output[0]."\n";
        echo file_get_contents("../ipatools.js");
      } else {
        echo file_get_contents("../ipa.js");
      }
      echo "</script>";
    ?>
    <style type="text/css">
      #submit, #holder { display: none; }
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
    <h2>Allophonic Rules</h2>
    <ol id="rules"></ol>
    <button onclick="makerule(null);">Add Rule</button>
    <p>If your category name is v or c followed by numbers, it will end up being interpreted as some random vowel or consonant (respectively). Naming a category "new" can also have messy results. To use a category in a rule already on the page, you may have to delete the rule and recreate it.</p>
    <button onclick="validate_all();">Save</button>
    <p id="errors"></p>
    <div id="holder"></div>
    <script>
      var getel = function(id) {
        return document.getElementById(id);
      };
      var firstclass = function(el, cls) {
        return el.getElementsByClassName(cls)[0];
      };
      var listphones = function(sel) {
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
        return s;
      };
      var catcount = 0;
      var catlen = function(id) {
        var el = getel(id);
        var ct = parseInt(firstclass(el, 'count').value);
        var div = firstclass(el, 'phones');
        if (ct < 1) {
          div.innerHTML = '';
          return null;
        }
        while (div.children.length > ct) {
          div.removeChild(div.children[div.children.length-1]);
        }
        while (div.children.length < ct) {
          var add = document.createElement('select');
          add.innerHTML = listphones();
          div.appendChild(add);
        }
      };
      var makecat = function(nam, val) {
        if (!nam) {
          nam = '';
        }
        if (!val) {
          val = [];
        }
        var cat = document.createElement('li');
        var id = 'cat' + catcount;
        cat.id = id;
        var s = '<div><span>Elements: </span>';
        s += '<input type="number" class="count" onchange="catlen(\''+id+'\');" value="'+val.length+'"></input>';
        s += '<br><span>Name: </span><input type="text" class="name" value="'+nam+'"></input></div>';
        s += '<div class="phones">';
        for (var i = 0; i < val.length; i++) {
          s += '<select>' + listphones(val[i]) + '</select>';
        }
        s += '</div>';
        s += '<span class="del" onclick="deleterule(\''+id+'\');">X</span>';
        cat.innerHTML = s;
        getel('cats').appendChild(cat);
        catcount++;
      };
      var readcats = function() {
        var cats = getel('cats').children;
        var ret = {};
        var nam, phon;
        for (var i = 0; i < cats.length; i++) {
          nam = firstclass(cats[i], 'name').value;
          ret[nam] = [];
          phon = firstclass(cats[i], 'phones').children;
          for (var c = 0; c < phon.length; c++) {
            ret[nam].push(phon[c].value);
          }
        }
        return ret;
      };
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
        s += '</option>' + listphones(val);
        var cats = Object.keys(readcats());
        for (var c = 0; c < cats.length; c++) {
          s += '<option value="' + cats[c];
          if (val == cats[c]) {
            s += '" selected="selected';
          }
          s += '">' + cats[c] + '</option>';
        }
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
        var sel = document.createElement('select');
        sel.innerHTML = s;
        if (cls == 'to') {
          sel.onchange = function() {
            if (sel.value == 'new') {
              sel.outerHTML = '<input type="text" class="altphone"></input>';
            }
          };
        }
        return sel;
      };
      var count = 0;
      var makeinput = function(cls, vals) {
        var s = '<div class="' + cls + '" id="s'+count+'">';
        s = document.createElement('div');
        s.className = cls;
        s.id = 's' + count;
        s.innerHTML = '<span>Count: </span>';
        s.innerHTML += '<input type="number" min="0" class="phcount" value="'+vals.length+'" onchange="update(\'s'+count+'\');"></input>';
        s.innerHTML += '<br><div class="phones"></div>';
        for (var i = 0; i < vals.length; i++) {
          s.children[3].appendChild(makesel(cls, vals[i]));
        }
        count++;
        return s;
      };
      var readinput = function(div) {
        var ls = firstclass(div, 'phones').children;
        var ret = [];
        for (var i = 0; i < ls.length; i++) {
          ret.push(ls[i].value);
        }
        return ret;
      };
      var update = function(id) {
        var div = getel(id);
        var ls = firstclass(div, 'phones');
        var ct = Math.max(parseInt(firstclass(div, 'phcount').value), 0);
        while (ls.children.length > ct) {
          ls.removeChild(ls.children[ls.children.length-1]);
        }
        while (ls.children.length < ct) {
          ls.appendChild(makesel(div.className, null));
        }
      };
      var deleterule = function(id) {
        var d = getel(id);
        d.parentNode.removeChild(d);
      };
      var makerule = function(rule) {
        if (!rule) {
          rule = {from: [], to: [], before: [], after: []};
        }
        var id = 'i' + count;
        var el = document.createElement('li');
        el.id = id;
        var add = document.createElement('span');
        add.innerText = '/';
        el.appendChild(add);
        el.appendChild(makeinput('from', rule.from));
        add = document.createElement('span');
        add.innerText = '/ → [';
        el.appendChild(add);
        el.appendChild(makeinput('to', rule.to));
        add = document.createElement('span');
        add.innerText = '] / ';
        el.appendChild(add);
        el.appendChild(makeinput('before', rule.before));
        add = document.createElement('span');
        add.innerText = ' ___ ';
        el.appendChild(add);
        el.appendChild(makeinput('after', rule.after));
        add = document.createElement('span');
        add.onclick = function() { deleterule(id); };
        add.className = 'del';
        add.innerText = 'X';
        el.appendChild(add);
        getel('rules').appendChild(el);
      };
      var readrule = function(li, cats) {
        var mode = getel('charmode').value;
        var cons = consch[mode];
        var vows = vowch[mode];
        var ret = {from: [], to: [], before: [], after: []};
        var err = [];
        var kl = ['from', 'to', 'before', 'after'];
        for (k in ret) {
          var l = readinput(firstclass(li, k));
          for (var i = 0; i < l.length; i++) {
            if (l[i] == 'null') {
              ret[k].push(null);
            } else if (l[i].match(/^[vc]\d+$/)) {
              ret[k].push(l[i]);
            } else if (cats.hasOwnProperty(l[i])) {
              ret[k].push(l[i]);
            } else if (cons.hasOwnProperty(l[i])) {
              ret[k].push('c' + cons[l[i]]);
            } else if (vows.hasOwnProperty(l[i])) {
              ret[k].push('v' + vows[l[i]]);
            } else {
              err.push([k, l[i]]);
            }
          }
        }
        return [ret, err];
      };
      var validate_all = function() {
        var pass = {allophony: []};
        getel('id').value = langdata.id;
        var l = getel('rules').children;
        var cats = readcats();
        pass['allophone categories'] = cats;
        var err = [];
        for (var i = 0; i < l.length; i++) {
          var res = readrule(l[i], cats);
          pass.allophony.push(res[0]);
          for (var e = 0; e < res[1].length; e++) {
            err.push(res[1][e].concat(i+1));
          }
        }
        if (err.length == 0) {
          getel('langdata').value = JSON.stringify(pass);
          getel('submit').submit();
        } else {
          var s = [];
          for (var i = 0; i < err.length; i++) {
            s.push('Unknown phone or category "'+err[i][1]+'" in rule '+err[i][2]);
          }
          getel('errors').innerText = s.join('<br>');
        }
      };
      if (langdata.hasOwnProperty('allophone categories')) {
        for (k in langdata['allophone categories']) {
          makecat(k, langdata['allophone categories'][k]);
        }
      }
      if (langdata.hasOwnProperty('allophony')) {
        var l = [];
        for (var i = 0; i < langdata.allophony.length; i++) {
          makerule(langdata.allophony[i]);
        }
      }
    </script>
  </body>
</html>
