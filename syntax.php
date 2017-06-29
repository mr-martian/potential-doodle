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
      .node { border: 1px solid black; display: inline-block; }
      .invis { display: none; }
      #syntax { width: 500%; }
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
    <div id="props"></div>
    <div id="syntax"></div>
    <button onclick="validate_all();">Save</button>
    <script>
      var getmode = function() { return getel('charmode').value; };
      var nodels = {
        CP: ['Complementizer Phrase', 'CPspec', 'Cbar'],
        Cbar: ['Complementizer Bar Phrase', 'C', 'IP'],
        IP: ['Inflection Phrase', 'IPspec', 'Ibar'],
        Ibar: ['Inflection Bar Phrase', 'auxilliary', 'VP'],
        VP: ['Verb Phrase', 'subject', 'Vmod1'],
        Vmod1: ['Verb Phrase Locative', 'Vmod2', 'PP'],
        Vmod2: ['Verb Phrase Adverb', 'Vbar', 'adverb'],
        Vbar: ['Verb Bar Phrase', 'verb', 'object', 'indirect object'],
        subject: 'NPp',
        object: 'NP',
        'indirect object': 'NP',
        NPp: ['Noun Phrase with possessor', 'NP', 'Nmod1'],
        NP: ['Noun Phrase', 'determiner', 'Nmod1'],
        Nmod1: ['Noun Phrase Relative Clause', 'Nmod2', 'clause'],
        Nmod2: ['Noun Phrase Locative', 'Nmod3', 'PP'],
        Nmod3: ['Noun Phrase Adjective', 'noun', 'AP'],
        NPs: ['Noun Phrase (abbreviated)', 'noun', 'AP'],
        PP: ['Adpositional Phrase', 'degree', 'Pbar'],
        Pbar: ['Adpositional Bar Phrase', 'adposition', 'NPs'],
        AP: ['Adjective Phrase', 'degree', 'Abar'],
        Abar: ['Adjective Bar Phrase', 'adjective', 'PPs'],
        PPs: ['Adpositional Phrase (abbreviated)', 'adposition', 'noun']
      };
      var mknodeobj = function(key) {
        if (nodels.hasOwnProperty(key)) {
          var ls = nodels[key];
          if (typeof ls == 'string') {
            var ret = mknodeobj(ls);
            ret.nam = key;
            return ret;
          }
          return {type: 'node', id: key, nam: ls[0], left: mknodeobj(ls[1]), right: mknodeobj(ls[2])};
        } else {
          return {type: 'leaf', nam: key};
        }
      };
      var nodes = mknodeobj('CP');
      var swap_children = function(node) {
        var div = node.lastChild;
        div.insertBefore(div.lastChild, div.firstChild);
      };
      //update the page
      var swap = function() {
      };
      var make_leaf = function(data) {
        return mkel('span', data.nam);
      };
      //create and return a syntax node <div>
      checkbox_id = 0;
      var make_node = function(data) {
        var ret = mkname('div', null, 'node');
        ret.appendChild(mkel('span', data.nam+' (<b>'+data.id+'</b>)'));
        ret.appendChild(mkinput('checkbox', '', 'chk'+checkbox_id, swap));
        ret.lastChild.className = 'invis';
        if (data.swapped) {
          ret.lastChild.checked = true;
        }
        ret.appendChild(mkel('label', 'switch order'));
        ret.lastChild.setAttribute('for', 'chk'+(checkbox_id++));
        ret.appendChild(mkel('div', ''));
        if (data.left.type == 'node') {
          ret.lastChild.appendChild(make_node(data.left));
        } else {
          ret.lastChild.appendChild(make_leaf(data.left));
        }
        if (data.right.type == 'node') {
          ret.lastChild.appendChild(make_node(data.right));
        } else {
          ret.lastChild.appendChild(make_leaf(data.right));
        }
        return ret;
      };
      getel('syntax').appendChild(make_node(nodes));
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
