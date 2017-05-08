#!/bin/bash

echo 'Building App' && cabalH new-build --flags=+awspi-dev && echo 'Finished build'
