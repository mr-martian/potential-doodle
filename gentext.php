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
        exec('python gentext.py '.$sf.' '.$st, $output);
        foreach($output as $line) {
          echo $line;
        }
      } else {
        echo "<h1>No language with that ID</h1>";
      }
    ?>
  </body>
</html>
