<?php
$file= $argv[1];
$f = file_get_contents($file);
$f = preg_replace("/<!--.*?-->/s","",$f);
$f = preg_replace("/\s*\n(\s*\n)+?/s","\n\n",$f);

if(isset($argv[2]))
  file_put_contents($argv[2], $f);
else
  echo $f;
