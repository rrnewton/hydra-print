#!/bin/bash

# Here's the simple approach, bring up the view and then as its
# "continuation", call the parallel Make.
hydra-view -- make -j
