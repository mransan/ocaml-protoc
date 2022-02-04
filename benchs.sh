#!/bin/sh
exec dune exec --profile=release benchs/benchs.exe -- $@

