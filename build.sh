#!/bin/bash

rm -f torpedo/torpedo.jar
rm -f torpedo.zip
sbt clean assembly
cp target/scala-2.12/torpedo.jar torpedo/
zip -r torpedo.zip torpedo

