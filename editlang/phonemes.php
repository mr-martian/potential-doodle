<html>
  <head>
    <meta charset="utf-8" />
    <?php
      $s = file_get_contents("../langs/".$_GET["lang"]."/lang.json");
      $lang = json_decode($s, true);
      echo "<title>Editting ".htmlspecialchars($lang["display name"])." Phonemic Inventory</title>";
      echo "<script>var langdata = ".$s.";";
      echo "langdata.id = ".$_GET["lang"].";";
      echo file_get_contents("../ipadata.js");
      echo file_get_contents("../ipatools.js");
      echo "</script>";
    ?>
    <style type="text/css">
      #submit {
        display: none;
      }
      #consonantstable, #vowelstable {
        border-collapse: collapse;
        border: 1px solid black;
      }
      td, tr {
        border: 1px solid black;
      }
      .voiceless {
        border-top-width: 3px;
      }
      .voiced {
        border-top-width: 0px;
      }
      .cboldedge {
        border-top-width: 3px;
      }
      #consphonefeatures label, #vowphonefeatures label {
        text-transform: capitalize;
      }
    </style>
    <script src="../ipa.js"></script>
  </head>
  <body>
    <form id="submit" method="post" action="../submitlang.php/">
      <input type="text" id="mode" name="mode" value="phonemes"></input>
      <input type="number" id="id" name="id"></input>
      <input type="text" id="langdata" name="langdata"></input>
    </form>
    <span>Display characters in: </span>
    <select id="charmode" onchange="phoneupdate();">
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
    <button onclick="validate_all();">Save</button>
    <script>
      var getel = function(id) {
        return document.getElementById(id);
      };
      var checks = getel('consphonefeatures');
      for (var i = 0; i < modifiers.length; i++) {
        checks.innerHTML += '<input type="checkbox" id="'+modifiers[i]+'" value="'+modifiers[i]+'" onchange="phoneupdate();"></input><label for="'+modifiers[i]+'">'+modifiers[i]+'</label>';
      }
      var checks = getel('vowphonefeatures');
      for (var i = 0; i < vmodifiers.length; i++) {
        checks.innerHTML += '<input type="checkbox" id="'+vmodifiers[i]+'" value="'+vmodifiers[i]+'" onchange="phoneupdate();"></input><label for="'+vmodifiers[i]+'">'+vmodifiers[i]+'</label>';
      }
      var getchecks = function(elname, nums) {
        var els = document.querySelectorAll('#' + elname + ' input');
        var ch = [];
        for (var i = 0; i < els.length; i++) {
          if (els[i].checked) {
            if (nums) {
              ch.push(parseInt(els[i].value));
            } else {
              ch.push(els[i].value);
            }
          }
        }
        return ch;
      };
      var cmakecheck = function(mode, manner, place, voice, mods, conlist) {
        var id = getcid(place, voice, manner, mods);
        var s = '<input type="checkbox" value="' + id + '" id="c' + id + '"';
        if (conlist.includes(id)) {
          s += ' checked="checked"';
        }
        s += '></input>';
        var ch = cgetchr(mode, id);
        if (ch) {
          s += '<label for="c' + id + '">' + ch + '</label>';
        }
        return s;
      };
      var vmakecheck = function(mode, back, height, rounded, mods, vowlist) {
        var id = getvid(back, height, rounded, mods);
        var s = '<input type="checkbox" value="' + id + '" id="v' + id + '"';
        if (vowlist.includes(id)) {
          s += ' checked="checked"';
        }
        s += '></input>';
        var ch = vgetchr(mode, id);
        if (ch) {
          s += '<label for="v' + id + '">' + ch + '</label>';
        }
        return s;
      };
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
      var getrowname = function(voice, manner, mods) {
        return 'voice' + (voice ? 'd' : 'less') + ' ' + mods.join(' ') + ' ' + manner;
      };
      var phoneupdate = function() {
        var alpha = getel('charmode').value;
        var mods = [];
        for (var i = 0; i < modifiers.length; i++) {
          if (getel(modifiers[i]).checked) {
            mods.push(modifiers[i]);
          }
        }
        mods = modsubsets(mods);
        var s = '<tr><td></td>';
        for (var i = 0; i < places.length; i++) {
          s += '<td>' + places[i] + '</td>';
        }
        s += '</tr>';
        var cons = getchecks('consonants', true);
        var voices = [false, true];
        for (var j = 0; j < manners.length; j++) {
          for (var i = 0; i < voices.length; i++) {
            var vstr = 'voice' + (voices[i] ? 'd' : 'less');
            for (var m = 0; m < mods.length; m++) {
              s += '<tr';
              if (i == 0 && m == 0) {
                s += ' class="cboldedge"';
              }
              s += '><td>' + getrowname(voices[i], manners[j], mods[m]) + '</td>';
              for (var k = 0; k < places.length; k++) {
                var ch = cmakecheck(alpha, manners[j], places[k], voices[i], mods[m], cons);
                s += '<td>' + ch + '</td>';
              }
              s += '</tr>';
            }
          }
        }
        getel('consonants').innerHTML = s;
        
        var mods = [];
        for (var i = 0; i < vmodifiers.length; i++) {
          if (getel(vmodifiers[i]).checked) {
            mods.push(vmodifiers[i]);
          }
        }
        mods = modsubsets(mods);
        var s = '<tr><td></td>';
        for (var i = 0; i < backs.length; i++) {
          s += '<td>' + backs[i] + '</td>';
        }
        s += '</tr>';
        var vows = getchecks('vowels', true);
        for (var j = 0; j < heights.length; j++) {
          for (var m = 0; m < mods.length; m++) {
            s += '<tr';
            if (m == 0) {
              s += ' class="cboldedge"';
            }
            s += '><td>' + mods[m].join(' ') + ' ' + heights[j] + '</td>';
            for (var k = 0; k < backs.length; k++) {
              var ch = vmakecheck(alpha, backs[k], heights[j], false, mods[m], vows);
              s += '<td>' + ch;
              var ch = vmakecheck(alpha, backs[k], heights[j], true, mods[m], vows);
              s += ch + '</td>';
            }
            s += '</tr>';
          }
        }
        getel('vowels').innerHTML = s;
      };
      if (langdata.hasOwnProperty('consonant features')) {
        var l = langdata['consonant features'];
        for (var i = 0; i < l.length; i++) {
          getel(l[i]).checked = true;
        }
      }
      if (langdata.hasOwnProperty('vowel features')) {
        var l = langdata['vowel features'];
        for (var i = 0; i < l.length; i++) {
          getel(l[i]).checked = true;
        }
      }
      phoneupdate();
      if (langdata.hasOwnProperty('consonants')) {
        for (var i = 0; i < langdata.consonants.length; i++) {
          getel('c' + langdata.consonants[i]).checked = true;
        }
      }
      if (langdata.hasOwnProperty('vowels')) {
        for (var i = 0; i < langdata.vowels.length; i++) {
          getel('v' + langdata.vowels[i]).checked = true;
        }
      }
      //form validation
      var validate_all = function() {
        var pass = {};
        getel('id').value = langdata.id;
        pass.consonants = getchecks('consonants', true);
        pass.vowels = getchecks('vowels', true);
        pass['consonant features'] = getchecks('consphonefeatures', false);
        pass['vowel features'] = getchecks('vowphonefeatures', false);
        getel('langdata').value = JSON.stringify(pass);
        getel('submit').submit();
      };
    </script>
  </body>
</html>
