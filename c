#!/bin/sh

erl -smp disable +K true -run endsrename start -s init stop -noshell
#erl +K true -run endsrename main . -noshell
