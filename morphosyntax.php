<html>
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("langs/".$_GET["lang"]."/lang.json");
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
      #cats > div { display: inline-block; }
    </style>
  </head>
  <body>
    <form id="submit" method="post" action="submitlang.php/">
      <input type="text" id="mode" name="mode" value="phonology"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="phoneupdate();">
      <option value="ipa">International Phonetic Alphabet (IPA)</option>
      <option value="cxs">Conlang X-Sampa (CXS)</option>
    </select>
    <h2>Categories Marked Morphologically</h2>
    <div id="cats"></div>
    <h2>Inflectional Forms</h2>
    <div id="forms"></div>
    <h2>Word Orders</h2>
    <div id="order"></h2>
    <button onclick="validate_all();">Save</button>
    <p id="errors"></p>
    <script>
      var defaultcats = {
        noun: [
          ['plurality',
            'singular', 'plural', 'pacual', 'singulative'],
          ['case',
            'nominative', 'accusative', 'dative', 'genitive', 'ergative', 'absolutive', 'vocative', 'instrumental'],
          ['class/gender',
            'male', 'female', 'neuter', 'animate', 'inanimate', 'fire', 'earth', 'water', 'air'],
        ],
        verb: [
        ],
        adjective: [
        ],
        adverb: [
        ],
      ];
      var makecat = function(nam, vals, checked, selected) {
        var el = document.createElement('div');
        mkchk(el, nam, nam, function() {}, checked ? [nam] : []);
        var ls = document.createElement('div');
        ls.style['margin-left'] = '15px';
        for (var i = 0; i < vals.length; i++) {
          mkchk(ls, vals[i], null, function() {}, selected);
          ls.appendChild(mkel('br', ''));
        }
        el.appendChild(ls);
        getel('cats').appendChild(el);
      };
      var validate_all = function() {
        var pass = {allophony: [], phonotactics: []};
        getel('id').value = langdata.id;
      };
    </script>
  </body>
</html>
