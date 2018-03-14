<html>
  <head></head>
  <body>
    <?php
      $i = intval($_GET["lang"]);
      if ($i != 0) {
        $s = strval($i);
        $output = array();
        exec('python viewlang.py '.$s, $output);
        foreach($output as $line) {
          echo $line;
        }
      } else {
        echo "<h1>No language with that ID</h1>";
      }
    ?>
  </body>
</html>
