#!/bin/bash

set -e

prev=$(tar -cf - src | md5sum)
echo "Previous hash: $prev"
check_equal_hashes () {
  curr=$(tar -cf - src | md5sum)
  echo "New hash: $curr"
  if [ "$curr" != "$prev" ]; then
    echo "Source code changed. Run bash ./scripts/scala.sh"
    exit 1
  fi
}
echo "Compiling Scala, fixing intentation..."
sbt -Dscala.rewrite=indent "compile; test:compile; scalafmtAll"
echo "Compiling Scala, fixing syntax..."
sbt -Dscala.rewrite=new-syntax "compile; test:compile; scalafmtAll"
check_equal_hashes
