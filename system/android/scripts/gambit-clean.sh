#!/bin/bash

if [[ ! -z `ls jni/*.c 2> /dev/null` ]]; then rm -R jni/*.c; fi
if [[ ! -z `ls src/*.c 2> /dev/null` ]]; then rm -R src/*.c; fi
