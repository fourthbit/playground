#!/bin/bash

MAIN_FILE=$1

if [[ ! -e src ]]; then mkdir src; fi
cd src
echo $MAIN_FILE
if [[ ! -e $MAIN_FILE ]]; then echo "Compilation failed: main file doesn't exist"; exit 1; fi
bh exe --features=android -C $MAIN_FILE 2> ../jni/c-files.dat
if [ "$?" -ne 0 ]; then echo "Compilation failed"; exit 1; fi 
mv *.c ../jni
