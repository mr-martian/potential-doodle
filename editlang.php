<html>
  <head>
    <meta charset="utf-8" />
    <?php
      if ($_GET["lang"] == "new") {
        echo "<title>Creating Language</title>";
      } else {
        $s = file_get_contents("langs/".$_GET["lang"]."/lang.json");
        $n = json_decode($s, true)["display name"];
        echo "<title>Editting ".htmlspecialchars($n)."</title>";
      }
    ?>
    <style>
      #header {
        position: fixed;
        background-color: grey;
        width: 100%;
        height: 50px;
      }
      #phoneheader {
        padding-top: 50px;
      }
      #submit {
        display: none;
      }
      #consonantstable {
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
    </style>
  </head>
  <body>
    <div id="header">
      <span id="name">(lang name)</span>
      <form id="submit" method="post" action="submitlang.php/">
        <input type="text" id="langdata" name="langdata"></input>
      </form>
      <button onclick="validate_all();">Save</button>
    </div>
    <h1 id="phoneheader">Phonology</h1>
    <h2>Phonetic Inventory</h2>
    <div id="phonefeatures">
      <span>Include features:</span>
      <input type="checkbox" id="voicing" checked="checked" onchange="phoneupdate();"></input>
      <label for="voicing">Voicing</label>
      <input type="checkbox" id="aspirated" onchange="phoneupdate();"></input>
      <label for="aspiration">Aspiration</label>
      <input type="checkbox" id="lateral" onchange="phoneupdate();"></input>
      <label for="lateral">Laterals</label>
      <input type="checkbox" id="nasal" onchange="phoneupdate();" checked="checked"></input>
      <label for="nasal">Nasals</label>
      <input type="checkbox" id="ejective" onchange="phoneupdate();"></input>
      <label for="ejective">Ejectives</label>
      <input type="checkbox" id="ingressive" onchange="phoneupdate();"></input>
      <label for="ingressive">Ingressives</label>
    </div>
    <table id="consonantstable">
      <tbody id="consonants"></tbody>
    </table>
    <table id="vowelstable">
      <tbody id="vowels"></tbody>
    </table>
    <h1>Morphology</h1>
    <h1>Syntax</h1>
    <?php
      if ($_GET["lang"] == "new") {
        $lang = array();
        echo "<script>var langdata = {'display name': null};";
        echo "langdata.id = null;</script>";
      } else {
        $str = file_get_contents("langs/".$_GET["lang"]."/lang.json");
        $lang = json_decode($str, true);
        echo "<script>var langdata = ".$str.";";
        echo "langdata.id = ".$_GET["lang"].";</script>";
      }
      //form generation
    ?>
    <!-- GENERAL USE -->
    <script>
      var getel = function(id) {
        return document.getElementById(id);
      };
    </script>
    <!-- PHONOLOGY -->
    <script>
      var places = ['bimanual', 'faciomanual', 'bilabial', 'labiodental', 'dental', 'alveolar', 'postalveolar', 'retroflex', 'palatal', 'velar', 'uvular', 'pharyngeal', 'epiglottal', 'glottal'];
      var manners = ['stop', 'fricative', 'trill', 'tap', 'approximant', 'click'];
      var modifiers = ['lateral', 'ejective', 'ingressive', 'aspirated', 'nasal'];
      var consid = {};
      var getcid = function(place, voice, manner, mods) {
        id = voice ? 4096 : 0;
        id += places.indexOf(place) * 256;
        id += manners.indexOf(manner) * 32;
        for (var i = 0; i < mods.length; i++) {
          id += Math.pow(2, modifiers.indexOf(mods[i]));
        }
        return id;
      };
      var cgetchr = function(mode, id) {
        if (consid.hasOwnProperty(id)) {
          if (consid[id].hasOwnProperty(mode)) {
            return consid[id][mode];
          }
        }
        return false;
      };
      var cfillgaps = function() {
        var gaps = [['ipa', 4096, '̥'], //voiceless
                    ['ipa', -4096, '̬'], //voiced
                    ['ipa', -8, 'ʰ'], //aspirated
                    ['ipa', -2, 'ʼ'], //ejective
                    ['cxs', 4096, '_0'], //voiceless
                    ['cxs', -4096, '_v'], //voiced
                    ['cxs', -8, '_h'], //aspirated
                    ['cxs', -2, '_>'], //ejective
                   ];
        var add = [];
        for (var id in consid) {
          for (var g = 0; g < gaps.length; g++) {
            var mode = gaps[g][0];
            var flag = gaps[g][1];
            var chr = gaps[g][2];
            var cond = id & Math.abs(flag);
            if (flag < 0) {
              cond = !cond;
            }
            if (cond && !cgetchr(mode, id-flag) && cgetchr(mode, id)) {
              add.push([id-flag, cgetchr(mode, id) + chr, mode]);
            }
          }
        }
        for (var i = 0; i < add.length; i++) {
          var id = add[i][0];
          var chr = add[i][1];
          var mode = add[i][2];
          if (consid.hasOwnProperty(id)) {
            consid[id][mode] = chr;
          } else {
            consid[id] = {};
            consid[id][mode] = chr;
          }
        }
        return add.length;
      };
      var c = function(mode, chr, place, voice, manner, mods) {
        var id = getcid(place, voice, manner, mods);
        if (consid.hasOwnProperty(id)) {
          consid[id][mode] = chr;
        } else {
          consid[id] = {};
          consid[id][mode] = chr;
        }
      }
      var addmanner = function(mode, manner, voice, modifiers, chars) {
        if (chars.length > places.length) {
          return addmanner(mode, manner, voice, modifiers, chars.split('|'))
        }
        for (var i = 0; i < places.length; i++) {
          if (i < chars.length && chars[i] != ' ') {
            c(mode, chars[i], places[i], voice, manner, modifiers);
          }
        }
      };
      addmanner('ipa', 'stop', false, [], '  p  tʈckq ʡʔ');
      addmanner('ipa', 'stop', true, [], '  b  dɖɟgɢ');
      addmanner('ipa', 'stop', true, ['nasal'], '  mɱ n ɳɲŋɴ');
      addmanner('ipa', 'trill', true, [], '  ʙ  r    ʀ');
      addmanner('ipa', 'tap', true, [], '   ⱱ  ɾ ɽ');
      addmanner('ipa', 'fricative', false, [], '  ɸfθsʃʂçxχħʜh');
      addmanner('ipa', 'fricative', true, [], '  βvðzʒʐʝɣʁʕʢɦ');
      c('ipa', 'ɬ', 'alveolar', false, 'fricative', ['lateral']);
      c('ipa', 'ɮ', 'alveolar', true, 'fricative', ['lateral']);
      addmanner('ipa', 'approximant', true, [], '   ʋ ɹ  ɻjɰ');
      addmanner('ipa', 'approximant', true, ['lateral'], '     l ɭʎʟ');
      addmanner('ipa', 'stop', true, ['ingressive'], '  ɓ  ɗ  ʄɠʛ');
      addmanner('ipa', 'click', false, [], '  ʘ ǀ ǃ ǂ');
      c('ipa', 'ǁ', 'alveolar', false, 'click', ['lateral']);

      addmanner('cxs', 'stop', false, [], ' | |p| | |t|t`|c|k|q| |>\\|?');
      addmanner('cxs', 'stop', true, [], ' | |b| | |d|d`|J\\|g|G\\');
      addmanner('cxs', 'stop', true, ['nasal'], ' | |m|F| |n| |n`|J|N|N\\');
      addmanner('cxs', 'trill', true, [], ' | |B\\| | |r| | | | |R\\');
      addmanner('cxs', 'tap', true, [], ' | | | | | |4| |r`');
      addmanner('cxs', 'fricative', false, [], ' | |p\\|f|T|s|S|s`|C|x|X|X\\|H\\|h');
      addmanner('cxs', 'fricative', true, [], ' | |B|v|D|z|Z|z`|j\\|G|R|?\\|<\\|h\\');
      c('cxs', 'K', 'alveolar', false, 'fricative', ['lateral']);
      c('cxs', 'K\\', 'alveolar', true, 'fricative', ['lateral']);
      addmanner('cxs', 'approximant', true, [], ' | | |P| |r\\| | |r\\`|j|M\\');
      addmanner('cxs', 'approximant', true, ['lateral'], ' | | | | |l| |l`|L|L\\');
      addmanner('cxs', 'stop', true, ['ingressive'], ' | |b_<| | |d_<| | |J\\_<|g_<|G\\_<');
      addmanner('cxs', 'click', false, [], ' | |0\\| | | |!\\|!\\`|=\\');
      c('cxs', '|\\', 'dental', false, 'click', []);
      c('cxs', '|\\|\\', 'alveolar', false, 'click', ['lateral']);
      
      var x;
      do {
        x = cfillgaps();
      } while (x > 0)

      vowellist = {}
      var v = function(mode, ch, height, back, round, tense) {
        if (ch == ' ') {
          return false;
        }
        if (!vowellist.hasOwnProperty(mode)) {
          vowellist[mode] = {};
        }
        if (!vowellist[mode].hasOwnProperty(height)) {
          vowellist[mode][height] = {};
        }
        if (!vowellist[mode][height].hasOwnProperty(back)) {
          vowellist[mode][height][back] = {};
        }
        if (!vowellist[mode][height][back].hasOwnProperty(round)) {
          vowellist[mode][height][back][round] = {};
        }
        if (!vowellist[mode][height][back][round].hasOwnProperty(tense)) {
          vowellist[mode][height][back][round][tense] = [];
        }
        vowellist[mode][height][back][round][tense].push(ch);
        //Add various modifiers? -DS 02017-05-08
      };
      var vrow = function(mode, chs, height, tense) {
        if (chs.length != 6) {
          return vrow(mode, chs.split('|'), height, tense);
        }
        v(mode, chs[0], height, 'front', false, tense);
        v(mode, chs[1], height, 'front', true, tense);
        v(mode, chs[2], height, 'central', false, tense);
        v(mode, chs[3], height, 'central', true, tense);
        v(mode, chs[4], height, 'back', false, tense);
        v(mode, chs[5], height, 'back', true, tense);
      }
      vrow('ipa', 'iyɨʉɯu', 'high', true);
      vrow('ipa', 'ɪʏ   ʊ', 'high', false);
      vrow('ipa', 'eøɘɵɤo', 'mid', true);
      vrow('ipa', 'ɛœɜɞʌɔ', 'mid', false);
      vrow('ipa', 'aɶ  ɑɒ', 'low', true);
      vrow('ipa', 'æ ɐ   ', 'low', false);
      v('ipa', 'ə', 'mid', 'central', false, false);

      vrow('cxs', 'i|y|1|u\\|M|u', 'high', true);
      vrow('cxs', 'I|Y|I\\|U\\| |U', 'high', false);
      vrow('cxs', 'e|2|@\\|8|7|o', 'mid', true);
      vrow('cxs', 'E|9|3|3\\|V|O', 'mid', false);
      vrow('cxs', 'a|&\\| | |A|Q', 'low', true);
      vrow('cxs', '& 6   ', 'low', false);
      v('cxs', '@', 'mid', 'central', false, false);
      var getcons = function() {
        var cons = [];
        var l = document.querySelectorAll('#consonants input');
        for (var i = 0; i < l.length; i++) {
          if (l[i].checked) {
            cons.push(parseInt(l[i].value));
          }
        }
        return cons;
      };
      var getvows = function() {
        var vows = [];
        var l = document.querySelectorAll('#vowels input');
        for (var i = 0; i < l.length; i++) {
          if (l[i].checked) {
            vows.push(parseInt(l[i].value));
          }
        }
        return vows;
      };
      var makecheck = function(mode, manner, place, voice, mods, conlist) {
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
        var cons = getcons();
        console.log(cons);
        var voices = getel('voicing').checked ? [false, true] : [false];
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
                var ch = makecheck('ipa', manners[j], places[k], voices[i], mods[m], cons);
                s += '<td>' + ch + '</td>';
              }
              s += '</tr>';
            }
          }
        }
        getel('consonants').innerHTML = s;
      };
      phoneupdate();
    </script>
    <!-- SETUP AND SUBMISSION -->
    <script>
      if (langdata['display name']) {
        getel('name').innerText = 'Editting ' + langdata['display name'];
      } else {
        getel('name').innerText = 'Creating New Language';
      }
      //form validation
      var validate_all = function() {
        getel('langdata').value = JSON.stringify(langdata);
        getel('submit').submit();
      };
    </script>
  </body>
</html>
