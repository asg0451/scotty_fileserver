#!/usr/bin/env bash
f=$(which one || find .stack-work/dist -name one -type f)
PORT=4242 AUTH=True $f
