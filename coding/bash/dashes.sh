#!/usr/bin/env bash
# Begin bash scripting exercise
# from the Book Bash Cookbook by Carl V Chapter 12
# dashes.sh - print a line of dashes as default
# use -c X argument to change the default character to char X

# create a usage message function
function usagexit ()
{
    printf "usage: %s [ -c X ] [#]\n" ${0##*/}
    exit 2
} >&2

LEN=72
CHAR='-'
while (( $# > 0 ))
do
    case $1 in
        [0-9*]) LEN=$1;;
        -c) shift;
            CHAR=${1:--};;
        *) usagexit;;
    esac
    shift
done

if (( LEN > 4096 ))
then
    echo "too large!" >&2
    exit 3
fi

# build the string to the exact length
DASHES=""
for ((i=0; i<LEN; i++))
do
    DASHES="${DASHES}${CHAR}"
done
printf "%s\n" "$DASHES"
