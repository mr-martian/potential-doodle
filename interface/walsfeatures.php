<html>
  <head>
    <style>
      td, table {
        border: 1px solid black;
      }
    </style>
  </head>
  <body>
    <?php
      $langid = intval($_GET["lang"]);
      if ($langid != 0) {
        $sid = strval($langid);
        echo file_get_contents('walsform.html');
        $jsonstr = file_get_contents("../langs/".$sid."/lang.json");
        $jsondata = json_decode($jsonstr, true);
        echo '<script type="text/javascript">var data='.$jsonstr.'; var go=true; var lang='.$sid.'</script>';
      } else {
        echo "<h1>No language with that ID</h1>";
        echo '<script type="text/javascript">var go = false;</script>';
      }
    ?>
    <script>
      if (go) {
        document.getElementById('id').value = lang;
        for (var k in data.wals) {
          document.getElementById(k).value = data.wals[k];
        }
      }
    </script>
  </body>
</html>
