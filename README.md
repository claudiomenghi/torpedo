# thrive
Partial kripke structure LTL model checker

## Compile

Run `sbt assembly`.

## Launch

Install the requried Docker containers running `./prepare.sh`
from the `docker` folder.

After compilation, 
run `java -jar target/scala-2.12/thrive.jar`.

