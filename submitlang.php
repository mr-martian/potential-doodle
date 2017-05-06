<?php
  $name = tempnam(getcwd().'/submit/', '');
  $file = fopen($name, "w");
  fwrite($file, $_POST["langdata"]);
  fclose($file);
  $output = array();
  exec('python3 submitlang.py '.$name, $output);
  foreach($output as $line) {
    echo $line;
  }
?>
