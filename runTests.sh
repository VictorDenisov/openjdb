#!/bin/bash

rm -f Jdwp/ConfigurationTest
rm -f Jdwp/ProtocolTest
ghc Jdwp/ConfigurationTest.hs
ghc Jdwp/ProtocolTest.hs
./Jdwp/ConfigurationTest
./Jdwp/ProtocolTest
