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
      }
      #submit {
        display: none;
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
    <script>
      var getel = function(id) {
        return document.getElementById(id);
      };
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
