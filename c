#!/bin/sh

erl -smp disable +K true -run endsrename start -noshell
#erl +K true -run endsrename main . -noshell
