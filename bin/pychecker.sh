#!/bin/bash
/usr/local/bin/pyflakes $1
echo "## pyflakes above, pep8 below ##"
/usr/local/bin/pep8 --repeat $1
