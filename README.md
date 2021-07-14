# `rwlock` -- Readers-Writer Lock in Fortran+OpenMP #

This implements a simple readers-writer lock in Fortran using OpenMP `atomic`
constructs. This can be used to protect a memory location which may be read by
many threads simultaneously, but only modified by a single thread.

Compilation requires OpenMP 5.0+. Supported compilers include
`gfortran>=11.0.0` and `ifort>=2021.1`.
