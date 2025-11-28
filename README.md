# Coding in Chemistry
Teaching material for the course Coding in Chemistry (Sergio Rampino,
University of Padua).

Authors: Sergio Rampino, Erwan Privat.

*Note that this is a working-in-progress repository: draft, messy, unfinished sections may appear from time to time. Final polishing is planned for late December 2025.*


## Setup:

- bash (or bash-like) shell
- Git
  - For windows you can use
    [Git for Windows](https://git-scm.com/downloads/win)
    that comes with both Git and bash.
- Vim text editor ([gVim](https://www.vim.org/download.php) for Windows)
- [gfortran](https://fortran-lang.org/learn/os_setup/install_gfortran/)
  compiler
- [gnuplot](http://gnuplot.info/download.html)
- [VMD](https://www.ks.uiuc.edu/Development/Download/download.cgi?PackageName=VMD)

## Contents

- `breviary.md` -- Basic usage of the above software
- `verlet/` -- Write a program for calculating classical trajectories
  of molecular systems solving Newton's equation through the Verlet
algorithm
- `cda/` -- Write a program for performing charge-displacement (CD)
  analysis of an electron charge redistribution
