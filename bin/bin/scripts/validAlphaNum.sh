#!/bin/sh
# validAlphaNum - Ensures that input consists only of alphabetical
# and numeric characters.

validAlphaNum()
{
    # Validate arg: returns 0 if all upper+lower+digits, 1 otherwise

    #Remove all unacceptable chars
    compressed="$(echo $1 | sed -e 's/[^[:alnum:]]//g')"

    if [ "$compressed" != "$1" ] ; then
	return 1
    else
	return 0
    fi
}