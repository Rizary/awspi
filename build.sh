#!/bin/bash

set -e

echo 'Building App' && cabal new-build --flags=+awspi-dev && echo 'Finished build'
