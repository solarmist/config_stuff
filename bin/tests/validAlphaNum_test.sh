#!/bin/sh

. validAlphaNum.sh


# echo -n doesn't work on MacOS 10.7 inside this script
echo "Enter input: \c"
read input

if ! validAlphaNum "$input" ; then
    echo "Your input must consist of only letters and numbers." >&2
    exit 1
else
    echo "Input is valid."
fi

exit 0

