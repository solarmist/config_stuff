#!/bin/sh
# inpath - Verifies that a specified program is either valid as is,
#  or that it can be found in the PATH directory list.

in_path()
{
    # Given a command and the PATH, try to find the command. Re