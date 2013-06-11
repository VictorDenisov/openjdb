#!/bin/bash

banner "Open JDB"

./dist/build/openjdb/openjdb -h localhost -p 2044 --class-path sample/Main.class --source-path sample/Main.java
