#!/bin/sh

erl -smp disable +K true -run endsrename main . -noshell
#erl +K true -run endsrename main . -noshell
