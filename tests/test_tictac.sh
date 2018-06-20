#!/bin/bash

./tictac -d 6 <<< 233 &> out.log 

diff -wq out.log ../tests/tictac.log > /dev/null || diff -wq out.log ../tests/loss.log
