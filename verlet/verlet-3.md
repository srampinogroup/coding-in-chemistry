# Assignment

Write a Fortran program that implements the Velocity Verlet algorithm
for calculating classical trajectories for the reaction A + BC
$\rightarrow$ AB + C, with A, B, and C being hydrogen atoms.
Use the subroutine `jpca15` in file `jpca15.f` (Rampino S, The
Journal of Physical Chemistry A 120,
[4683-4692](http://dx.doi.org/10.1021/acs.jpca.5b10018), 2016) to
calculate the interaction potential between the three atoms. Try
different initial conditions and collect at least one non-reactive
and one reactive trajectory.

## Guidelines and tips

### The `jpca15` subroutine

The forces can easily be calculated as the derivative of the
potential (see `verlet-1.md`). The subroutine `jpca15.f` takes as
input argument a vector with the three interatomic distances (AB, AC,
and BC) and returns as output the potential energy and the vector of
the derivatives of the potential with respect to the interatomic
distances (AB, AC, and BC). Distances are in bohr and energies are in
eV.

### Visualizing the trajectories.

Save the trajectories in XYZ format (see `verlet-2.md`) and
visualized them through a molecular visualize (e.g.,
[VMD](https://www.ks.uiuc.edu/Development/Download/download.cgi?PackageName=VMD)).
