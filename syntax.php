<html>
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("langs/".$_GET["lang"]."/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Syntax</title>";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = ".$_GET["lang"].";";
      echo file_get_contents("ipadata.js");
      echo file_get_contents("tools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit { display: none; }
      .catval { margin-left: 15px; }
      .catls, .phrasepart { display: inline-block; vertical-align: top; }
    </style>
  </head>
  <body>
    <form id="submit" method="post" action="submitlang.php/">
      <input type="text" id="mode" name="mode" value="syntax"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="phoneupdate();">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <div id="syntax"></div>
    <button onclick="validate_all();">Save</button>
    <script>
      var getmode = function() { return getel('charmode').value; };
      var Pos = ['noun', 'verb', 'adjective', 'adverb', 'article', 'adposition', 'pronoun', 'conjunction', 'auxilliary'];
      var r = [];
      for (var i = 0; i < Pos.length; i++) {
        if (!(langdata.hasOwnProperty(Pos[i]+' is null') && langdata[Pos[i]+' is null'])) {
          r.push(Pos[i]);
        }
      }
      Pos = r;
      var Phrases = ['Adjective Phrase', 'Adpositional Phrase', 'Clause', 'Conjunction', 'Noun Phrase', 'Verb Phrase'];
      var phrase_parts = {
        'Adjective Phrase': ['adjective', 'adverb'],
        'Adpositional Phrase': ['adposition', 'Noun Phrase'],
        'Clause': ['Noun Phrase', 'Verb Phrase', 'auxilliary']
      };
      //display the checkboxes for a phrase_element
      var display_pos = function(parent, pos, settings) {
        settings = settings || {};
        var ret = mkel('div', '');
        if (!pos) {
        } else if (Pos.includes(pos)) {
          var cats = langdata[pos+' categories'];
          var check, valcheck;
          for (var i = 0; i < cats.cats.length; i++) {
            if (settings.hasOwnProperty(cats.cats[i])) {
              check = true;
              valcheck = settings[cats.cats[i]];
            } else {
              check = false;
              valcheck = [];
            }
            ret.appendChild(mklist(cats.cats[i], check, cats.ops[i], valcheck, function() {}));
          }
        } else {
          var phls = read_phrases(pos);
          var ls;
          for (var i = 0; i < phls.length; i++) {
            ls = [];
            for (var p = 0; p < phls[i].parts.length; p++) {
              ls.push(phls[i].parts[p].pos);
            }
            ret.appendChild(mklist(phls[i].opname || pos+' '+(i+1), true, ls, ls, null));
          }
        }
        parent.appendChild(ret);
      };
      //create an item in a phrase
      var phrase_element = function(phrase, data) {
        data = data || {pos: null, cats: {}};
        var ret = mkname('div', null, 'phrasepart');
        ret.appendChild(mksel(Pos.concat(Phrases), null, null, data.pos,
                              function() {
                                ret.removeChild(ret.lastChild);
                                display_pos(ret, ret.firstChild.value, null);
                              }));
        display_pos(ret, data.pos, data);
        return ret;
      };
      //create a phrase
      var make_phrase = function(type, data) {
        data = data || {nam: '', els: []};
        var ret = mkname('div', null, 'phrase');
        ret.appendChild(mkel('span', 'Name: '));
        ret.appendChild(mkinput('text', data.nam, null, null));
        ret.appendChild(mkel('span', 'Count: '));
        ret.appendChild(mkinput('number', data.els.length, null,
                                function() {
                                  var n = Math.max(parseInt(ret.children[3].value), 0);
                                  while (ret.lastChild.children.length > n) {
                                    ret.lastChild.removeChild(ret.lastChild.lastChild);
                                  }
                                  while (ret.lastChild.children.length < n) {
                                    ret.lastChild.appendChild(phrase_element(type, null));
                                  }
                                }));
        ret.appendChild(mkel('div', ''));
        for (var i = 0; i < data.els.length; i++) {
          ret.lastChild.appendChild(phrase_element(type, data.els[i]));
        }
        getel(type).appendChild(ret);
      };
      //read the data for all the options of a particular phrase
      var read_phrases = function(phrase) {
        var ret = [];
        var ls = getel(phrase).children;
        var parts;
        for (var i = 0; i < ls.length; i++) {
          ret.push({opname: ls[i].children[1].value, parts: []});
          parts = ls[i].lastChild.children;
          for (var p = 0; p < parts.length; p++) {
            ret[i].parts.push({pos: parts[p].firstChild.value, ops: []});
            if (parts[p].children.length == 2) { //handle the issue of a phrase trying to read itself
              for (var o = 0; o < parts[p].lastChild.children.length; o++) {
                ret[i].parts[p].ops.push(readlist(parts[p].lastChild.children[o]));
              }
            }
          }
        }
        return ret;
      };
      //create an event listener that can be set on the buttons that add phrases
      var make_add_button_listener = function(ph) {
        return function() { make_phrase(ph, null); };
      };
      //build the page
      var synel = getel('syntax');
      for (var i = 0; i < Phrases.length; i++) {
        synel.appendChild(mkel('h2', Phrases[i]));
        synel.appendChild(mkname('div', Phrases[i], null));
        if (langdata.hasOwnProperty('syntax') && langdata.syntax.hasOwnProperty(Phrases[i])) {
          for (var p = 0; p < langdata.syntax[Phrases[i]]; p++) {
            make_phrase(Phrases[i], langdata.syntax[Phrases[i]][p]);
          }
        } else {
          make_phrase(Phrases[i], null);
        }
        synel.appendChild(mkel('button', 'Add '+Phrases[i]));
        synel.lastChild.onclick = make_add_button_listener(Phrases[i]);
      }
      var validate_all = function() {
        var pass = {syntax: {}};
        for (var i = 0; i < Phrases.length; i++) {
          pass.syntax[Phrases[i]] = read_phrases(Phrases[i]);
        }
        getel('langdata').value = JSON.stringify(pass);
        getel('id').value = langdata.id;
        //getel('submit').submit();
      };
    </script>
  </body>
</html>
