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
        NP: ['NP', 'Noun Phrase',
          ['Determiner'],
          ['Nmod1', '',
            ['Nmod2', '',
              ['Nmod3', '', 'noun', 'adjective'],
              'adpositional phrase'],
            'relative clause']],
        clause: ['CP', 'clause',
          'CPspec',
          ['Cbar', '',
            'Complementizer',
            ['IP', '',
              'IPspec',
              ['Ibar', '',
                'auxilliary',
                ['VP', 'verb phrase',
                  'subject',
                  ['Vmod1', '',
                    ['Vmod2', '',
                      ['Vindir', '',
                        ['Vbar', '', 'verb', 'object'],
                        'indirect object'],
                      'adverb'],
                    'adpositional phrase']]]]]],
        AP: ['AP', 'adjective phrase',
          'degree',
          ['Abar', '', 'adjective', 'adpositional phrase']],
        PP: ['PP', 'adpositional phrase',
          'degree',
          ['Pbar', '', 'adposition', 'noun phrase']],
        conj: ['conj', 'conjunction phrase',
          'left',
          ['conjbar', '', 'conjunction', 'right']]
      };
      var mknodeobj = function(ls) {
        if (Array.isArray(ls) && ls.length == 4) {
          return {type: 'node', id: ls[0], nam: ls[1], left: mknodeobj(ls[2]), right: mknodeobj(ls[3])};
        } else {
          return {type: 'leaf', nam: ls};
        }
      };
      var nodes = {
        NP: mknodeobj(nodels.NP),
        clause: mknodeobj(nodels.clause),
        AP: mknodeobj(nodels.AP),
        PP: mknodeobj(nodels.PP),
        conj: mknodeobj(nodels.conj)
      };
      //log a change in tree structure in the tree's data-path
      var log_move = function(node, op) {
        var par = getel(node.getAttribute('data-store'));
        var ls = JSON.parse(par.getAttribute('data-path'));
        ls.push(op);
        par.setAttribute('data-path', JSON.stringify(ls));
      };
      //apply movement to a tree
      var movement = function(tree, src_cls, dest_cls) {
        var src = firstclass(tree, src_cls);
        var dest = firstclass(tree, dest_cls);
        if (!src || !dest) { return; }
        var t = mkel('span', '(moved away)');
        src.parentNode.replaceChild(t, src);
        dest.parentNode.replaceChild(src, dest);
        log_move(src, [src.getAttribute('data-id'), dest.getAttribute('data-id')]);
      };
      //reverse the order of a node's children
      var swap_children = function(node) {
        var div = node.lastChild;
        div.insertBefore(div.lastChild, div.firstChild);
        log_move(node, node.getAttribute('data-id'));
      };
      //create and return a leaf node for a syntax tree
      var node_id = 0;
      var make_leaf = function(data, store) {
        var ret = mkel('span', data.nam);
        ret.className = data.nam;
        ret.id = 'node'+(node_id++);
        ret.setAttribute('data-store', store);
        ret.setAttribute('data-id', data.nam);
        return [ret, [data.nam]];
      };
      var nodeorleaf = function(data, store) {
        if (data.type == 'node') {
          return make_node(data, store);
        } else {
          return make_leaf(data, store);
        }
      };
      //create and return a syntax node <div>
      var make_node = function(data, store) {
        var ret = mkname('div', 'node'+node_id, 'node '+data.id);
        if (!store) {
          store = ret.id;
          ret.setAttribute('data-path', '[]');
        }
        ret.setAttribute('data-store', store);
        ret.setAttribute('data-id', data.id);
        ret.appendChild(mkel('span', data.nam+' (<b>'+data.id+'</b>)'));
        ret.appendChild(mkinput('checkbox', '', 'chk'+node_id,
                                function() { swap_children(ret); }));
        ret.lastChild.className = 'invis';
        if (data.swapped) {
          ret.lastChild.checked = true;
        }
        ret.appendChild(mkel('label', 'switch order'));
        ret.lastChild.setAttribute('for', 'chk'+(node_id++));
        ret.appendChild(mkel('div', ''));
        var l = nodeorleaf(data.left, store);
        ret.lastChild.appendChild(l[0]);
        var r = nodeorleaf(data.right, store);
        ret.lastChild.appendChild(r[0]);
        return [ret, l[1].concat(r[1])];
      };
      var read_tree = function(tree) {
        var ret = {move: JSON.parse(tree.firstChild.getAttribute('data-path'))};
        //get any special properties (is a question, has pronoun possessor, etc.)
        return ret;
      };
      var move_button = function(div) {
        return function(e) {
          var s = div.children[3].value;
          var d = div.children[5].value;
          if (s && d && s != d) {
            movement(div, s, d);
          }
        };
      };
      var load_tree = function(type) {
        var ret = document.createElement('div');
        getel('syntax').appendChild(ret);
        var ls = seek(langdata, ['syntax', type]) || [];
        if (ls.length == 0) {
          ls.push({move: []});
        }
        var div, ndls, but, mov;
        for (var i = 0; i < ls.length; i++) {
          div = document.createElement('div');
          ndls = make_node(nodes[type]);
          div.appendChild(ndls[0]);
          div.appendChild(mkel('br', ''));
          div.appendChild(mkel('span', 'Move '));
          div.appendChild(mksel(ndls[1], null, null, null, null));
          div.appendChild(mkel('span', ' to '));
          div.appendChild(mksel(ndls[1], null, null, null, null));
          but = mkel('button', 'move');
          but.onclick = move_button(div);
          div.appendChild(but);
          ret.appendChild(div);
          for (var m = 0; m < ls[i].move.length; m++) {
            mov = ls[i].move[m];
            if (Array.isArray(mov)) {
              movement(div, mov[0], mov[1]);
            } else {
              swap_children(firstclass(div, mov));
            }
          }
        }
      };
      var syn = getel('syntax');
      var trees = {};
      syn.appendChild(mkel('h2', 'Noun Phrase'));
      load_tree('NP');
      trees.NP = syn.lastChild;
      syn.appendChild(mkel('h2', 'Sentence/Clause'));
      load_tree('clause');
      trees.clause = syn.lastChild;
      syn.appendChild(mkel('h2', 'Adjective Phrase'));
      load_tree('AP');
      trees.AP = syn.lastChild;
      syn.appendChild(mkel('h2', 'Adpositional Phrase'));
      load_tree('PP');
      trees.PP = syn.lastChild;
      syn.appendChild(mkel('h2', 'Conjunction Phrase'));
      load_tree('conj');
      trees.conj = syn.lastChild;
      var validate_all = function() {
        var pass = {syntax: {}};
        var treels;
        for (k in trees) {
          pass.syntax[k] = [];
          treels = trees[k];
          for (var i = 0; i < treels.children.length; i++) {
            pass.syntax[k].push(read_tree(treels.children[i]));
          }
        }
        getel('langdata').value = JSON.stringify(pass);
        getel('id').value = langdata.id;
        getel('submit').submit();
      };
    </script>
  </body>
</html>
