<?php
  $name = tempnam(getcwd().'/submit/', '');
  $file = fopen($name, "w");
  fwrite($file, json_encode($_POST));
  fclose($file);
  $output = array();
  exec('python3 submitwals.py '.$_POST["id"].' '.$name.' 2>&1', $output);
  foreach($output as $line) {
    echo $line;
  }
?>
