#!/bin/bash
echo -n 'module Version(build) where
build::String
build="' > src/Version.purs
date --iso-8601="ns" | tr -d "\r\n" >> src/Version.purs
echo '"' >> src/Version.purs

