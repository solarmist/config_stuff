#!/bin/sh
# nicenumber - Given a number, show it in a comma-separated form.
# Expects DD, GS and TD to be instantiated. Instantiates nicenum
# or, if a second arg is specified, the output is echoed to stdout.

nicenumber()
{
    # note that we assume that '.' is the decimal separator in 
    # the INPUT value to this script. The decimal separator in the output value
    # is '.' unless specified by the user with the -d flag

    integer=$(echo $1 | cut -d. -f1)	# left of the decimal
    decimal=$(echo $1 | cut -d. -f2)	# right of the decimal

    if [ $decimal != $1 ]; then
	# There's a fractional part, so let's include it.
	result="${DD:="."}$decimal"
    fi

    thousands=$integer

    while [ $thousands -gt ${group:=999} ]; do
	remainder=$(($thousands % $((group+1)) ))	# the GS least significant digits

	while [ ${#remainder} -lt ${GS:=3} ]; do	# force leading zeros as needed
	    remainder="0$remainder"
	done

	thousands=$(($thousands / $((group+1)) ))	# to left of remainder, if any
	result="${TD:=","}${remainder}${result}"	# builds right to left
    done

    nicenum="${thousands}${result}"
    if [ ! -z $2 ]; then
	echo $nicenum
    fi
}

DD="."	# decimal point delimiter, to separate integer and fractional values
TD=","	# thousands delimiter, to separate every GS digits
GS=3	# group size

while getopts "d:g:t:" opt; do
    case $opt in
	d ) DD="$OPTARG";;
	g ) GS="$OPTARG";;
	t ) TD="$OPTARG";;
    esac
done
shift $(($OPTIND - 1))	# Shift off all the options

while [ ${#group} -lt $GS ]; do
    group="9${group:=9}"
done

if [ $# -eq 0 ]; then
    echo "Usage: $(basename $0) [-d c] [-t c] [-g n] numeric value"
    echo "  -d specifies the decimal point delimiter (default '.')"
    echo "  -g specifies the grouping size (default 3)"
    echo "  -t specifies the thousands delimiter (default ',')"
    exit 0
fi

nicenumber $1 1	# second arg forces nicenumber to 'echo' output

exit 0