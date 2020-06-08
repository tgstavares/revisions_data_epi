#!/bin/bash

if [ "$1" == "all" ] || [ "$1" == "1" ]; then
    echo "Running part 1 - python to export data"
    python3 export_data_to_fortran.py
fi

if [ "$1" == "all" ] || [ "$1" == "2" ]; then
    echo "Running part 2 - estimation fortran"
    make main && ./main
fi

if [ "$1" == "all" ] || [ "$1" == "3" ]; then
    echo "Running part 3 - python to recover fortran data"
    python3 export_data_to_python.py
fi

