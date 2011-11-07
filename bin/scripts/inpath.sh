#!/bin/sh
# inpath - Verifies that a specified program is either valid as is,
#  or that it can be found in the PATH directory list.

in_path()
{
    # Given a command and the PATH, try to find the command. Returns
    # 0 if found and executable, 1 if not. Not that this temporarily modifies
    # the IFS (input field separator) but restores it upon completion.

    cmd=$1
    path=$2
    retval=1
    oldIFS=$IFS
    IFS=":"

    for dir in $path; do
	if [ -x $dir/$cmd ] ; then
	    retval=0  # if we're here, we cound $cmd in $dir
	fi
    done
    IFS=$oldIFS
    return $retval
}

checkForCmdInPath()
{
    var=$1
    
    # The variable slicing notation in the following conditional
    # needs some explaination: ${var#expr} returns everything after
    # the match for 'expr' in the variable value (if any), and
    # ${var%expr} returns everything that doesn't match (in this
    # case, just the very first character. You can also do this in
    # Bash with ${var:0:1}, and you could use cut too: cut -c1.

    if [ "$var" != "" ] ; then
	if [ "${var%{var#?}}" = "/" ] ; then
	    if [ ! -x $var ] ; then
		return 1
	    fi
	elif ! in_path $var $PATH ; then
	    return 2
	fi
    fi
}
