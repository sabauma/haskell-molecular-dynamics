haskell-molecular-dynamics
==========================

Haskell Implementation of Generic N-Body Computations

This project aims to provide some simple tools for doing N-Body computations in
Haskell while leveraging the parallelism facilities of GHC.
Currently, the following is provided

* A generic API for the Barnes-Hut algorithm. This code is intentionally agnostic
  to the force being calculated. The goal being that these facilities could be
  easily plugged into an N-Body simulation regardless of whether the system
  being simulated is governed by gravity, Coulomb's law, van der Waal's forces,
  etc. The user need only supply object positions and the per object force
  quantity (mass for gravity, charge for Coulomb's law, etc.).

* A simple parallel integrator for a gravitational system using Verlet
  integration. Parallelism for this step is provided by Repa.

* A relatively high level represenation of objects and vectors in the system.
  The implementation of the gravitational system explicitly uses parallel arrays
  of vectors, though it is possible to use a single array of "particles" as the
  system representation without performance impact. Keeping them separate is
  easier for Verlet integration. This relies heavily on GHC's optimizer to
  achieve good performance.

* Some functions to load simple CSV and binary data formats of systems.

* Sample program which populates a cubical system based on the given input. The
  command line argument is interpreted as particles per edge, so the total
  particle count is the cube of the input.

