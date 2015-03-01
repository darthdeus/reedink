#!/bin/bash

set -e

if [ "$1" = "stop" ]; then
    echo "Trying to stop"
    if [ -f tmp/pids/reedink.pid ]; then
        kill $(tmp/pids/reedink.pid)
        rm tmp/pids/reedink.pid
        echo "Stopped"
    else
        echo "Not running"
    fi
else
    if [ -f tmp/pids/reedink.pid ]; then
        echo "Already running"
    else
        nohup ./dist/build/reedink/reedink &
        disown %1
        echo $! > tmp/pids/reedink.pid
        echo "Reedink started"
    fi
fi
