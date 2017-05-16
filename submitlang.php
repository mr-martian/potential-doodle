<?php
  //send by POST:
  //langdata (JSON object of updates)
  //mode (string, name of sending page)
  //id (number, language id)
  $name = tempnam(getcwd().'/submit/', '');
  $file = fopen($name, "w");
  fwrite($file, $_POST["langdata"]);
  fclose($file);
  $output = array();
  exec('python3 submitlang.py '.$_POST["mode"].' '.$_POST["id"].' '.$name.' 2>&1', $output);
  foreach($output as $line) {
    echo $line;
  }
?>
