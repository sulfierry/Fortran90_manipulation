#!/bin/bash

# Compile
gfortran -o cartesian cartesian_coord.f90
gfortran -o matrix matrix_operations.f90
gfortran -o verify verify_sequence_from_binary.f90

# Execute
./cartesian
./matrix
./verify
