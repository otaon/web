#!/bin/zsh

notnums=2
nums=4

for num in {1..${notnums}}
do
	dot -T png NOT-DI${num}.dot -o NOT-DI${num}.png
done


for num in {1..${nums}}
do
	dot -T png DI${num}.dot -o DI${num}.png
done

sleep 1

open NOT-DI{1..${notnums}}.png DI{1..${nums}}.png
