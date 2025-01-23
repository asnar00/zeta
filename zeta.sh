#!/bin/zsh
find src -not -name "*_classes.py" | entr -r python3 src/zeta.py