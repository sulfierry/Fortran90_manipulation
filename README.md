# Fortran Mathematical Suite

A Fortran codebase containing utilities for:

- **2D Cartesian Coordinates Manipulation**
- **Matrix Operations**
- **Mathematical Sequence Verification from Binary Files**

---

## 1. `cartesian_coord.f90`
### Module: `Cartesian2DModule`
- `Point2D`: A data structure to represent 2D Cartesian coordinates.
- Functions and Procedures:
  - `calculateDistance`: Computes distance between two 2D points.
  - `assignValues`: Assigns x and y values to a Point2D type.
  - `displayContent`: Prints the x and y values of a Point2D type.
  - `translatePoint`: Translates a point by x and y offsets.
  - `angleWithXAxis`: Computes the angle a point makes with the x-axis.
  
### Program: `DerivedTypeExperiment`
Demonstrates various operations using the `Cartesian2DModule`.

---

## 2. `matrix_operations.f90`
### Module: `arrayMod`
- Functions and Procedures:
  - `matVet`: Multiplies a matrix by a vector.
  - `somaElement`: Sums elements of a vector.
  - `normaEuclidiana`: Computes the Euclidean norm of a vector.
  - `matrixScalarProduct`: Multiplies a matrix by a scalar.
  - `mostrarConteudo`: Prints a vector's content.
  - `mostrarConteudoM`: Prints a matrix's content.

### Program: `comModule`
Demonstrates matrix and vector operations using the `arrayMod`.

---

## 3. `verify_sequence_from_binary.f90`
### Module: `SequenceUtilities`
Contains mathematical constants and utilities for sequence operations:
- `phi`: Golden ratio constant.
- `root`: Square root of 5.
- Functions and Procedures:
  - `readFile`: Reads a sequence from a text file.
  - `displayContent`: Prints the sequence and its number of terms.
  - `identifySequence`: Determines the type of mathematical sequence and validates it.
  - `writeBinary`: Writes a sequence to a binary file.
  - `readBinary`: Reads a sequence from a binary file.

### Program: `VerifySequenceFromBinary`
Demonstrates sequence reading from a binary file and its subsequent identification and validation.

---

These utilities are instrumental for basic mathematical operations, matrix manipulations, and sequence validations from binary files.
