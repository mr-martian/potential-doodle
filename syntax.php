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
      var display_pos = function(parent, pos, settings) {
        settings = settings || {};
        var cats = langdata[pos+' categories'];
        var ret = mkel('div', '<span>'+pos+'</span>');
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
        parent.appendChild(ret);
      };
      display_pos(getel('syntax'), 'noun', null);
      var validate_all = function() {
        var pass = {syntax: {}};
        getel('langdata').value = JSON.stringify(pass);
        getel('id').value = langdata.id;
        //getel('submit').submit();
      };
    </script>
  </body>
</html>
