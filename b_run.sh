#!/bin/sh
exec dune exec --profile=release -- benchs/bin/run.exe $@
