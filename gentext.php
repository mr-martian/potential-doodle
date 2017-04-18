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
      $f = intval($_GET["from"]);
      $t = intval($_GET["to"]);
      if ($f != 0 & $t != 0 & $f != $t) {
        $sf = strval($f);
        $st = strval($t);
        $output = array();
        exec('python3 gentext.py '.$sf.' '.$st, $output);
        echo "<h2>Sentence:</h2><p>".$output[0]."</p><h2>Translations:</h2><ul>";
        foreach(array_slice($output,1) as $line) {
          echo "<li>".$line."</li>";
        }
        echo "</ul>";
      } else {
        echo "<h1>No language with that ID</h1>";
      }
    ?>
  </body>
</html>
