#!/bin/bash

java -cp . -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=2044 Main
