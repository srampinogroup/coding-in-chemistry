# Hands-on session 1.
# Classical trajectories via the velocity Verlet algorithm

In finite-difference schemes, time is discretized using a constant
time step   $\Delta t = \tau$.
Let us denote with subscript $k$ the quantities at the $k$-th time
step: the time $t_k=k\tau$, the position of the particle $x_k =
x(t_k)$, its velocity $v_  k =v(t_k)$, and the force acting on it
$f_k = f(x_k)$.

For one particle of mass $m$ and one dimension $x$, the velocity Verlet
algorithm is based on the following recursive scheme:

$$
x_{k+1} = x_k + \tau v_k + \frac{\tau^2}{2} \frac{f_k}{m}
$$

$$
v_{k+1} = v_k + \frac{\tau}{2m} (f_k + f_{k+1})
$$

Such scheme can be easily generalized to the many-atom, higher
dimensional case. For a system of $N$ particles in three-dimensional
space, the potential is a function
of the positions of the particles $\\{\boldsymbol{r}_a\\}$.
The force acting on the $a$-th atom will then be:

$$
\boldsymbol{f}^{(a)} = - \nabla^{(a)} V (\\{\boldsymbol{r}_a\\})
$$

where superscript $a$ on the gradient operator indicates that the
derivatives should be taken with respect to the coordinates of the
$a$-th atom.

For each separate Cartesian component, the velocity Verlet recursive
scheme will apply, with the $x$, $y$, and $z$ components of the
forces being, respectively,

$$
f^{(a,x)} = - \frac{~\mathrm{d}}{~\mathrm{d} x^{(a)}} V (\{\boldsymbol{r}_a\})
$$

$$
f^{(a,y)} = - \frac{~\mathrm{d}}{~\mathrm{d} y^{(a)}} V (\{\boldsymbol{r}_a\})
$$

$$
f^{(a,z)} = - \frac{~\mathrm{d}}{~\mathrm{d} z^{(a)}} V (\{\boldsymbol{r}_a\})
$$

Thus, a distinct recursion relation rooted in the Velocity Verlet
algorithm can be used for each coordinate-velocity pair but these are
all coupled through the force, which depends on the coordinates of
all the atoms.

Focusing on the $x$ component, for instance, the final algorithm,
with given initial $x_k$ and $v_k$ and a known expression for $f(x)$,
is:

1. Calculate $x^{(a)}_{k+1}$:

$$
x^{(a)}_{k+1} = x^{(a)}_k + \tau v^{(a,x)}_k + \tau^2
\frac{f^{(a,x)}_k}{2m_a}
$$

2. Evaluate $f^{(a,x)}_{k+1}$

3. Calculate $v^{(a,x)}_{k+1}$:

$$
v^{(a,x)}_{k+1} = v^{(a,x)}_k + \frac{\tau}{2m_a} \left( f^{(a,x)}_k + f^{(a,x)}_{k+1} \right)
$$

4. Assign the value of $x^{(a)}_{k+1}$ to $x^{(a)}_k$ and go back go
back to step 1.

Analogous schemes can be written for the $y$ and $z$ components.

## Exercise 1

Write a Fortran program that implements the Verlet algorithm with $k$
ranging from 1 to 200 and $\tau$ = 0.2 s for one
particle of mass 1 kg in 3D space subject to a constant force
expressed by components $f^{(a,x)}$ = 0 kg m s<sup>-2</sup>,
$f^{(a,y)}$ = 0.1 kg m s<sup>-2</sup>,
$f^{(a,z)}$ = 0 kg m s<sup>-2</sup>.

## Guidelines

Create the file `verlet.f95` and type the following:

```
PROGRAM verlet
  IMPLICIT NONE

  ! write your program here

END PROGRAM verlet
```

Lines starting with `!` are comments (not part of your Fortran
program, will be ignored by the compiler). To test your program, you
need to compile it:

```
gfortran -o verlet verlet.f95
```

and execute it:

```
./verlet
```

Typically, you implement a small portion of code, and you test the
program in order to verify that it behaves as expected (this is
called debugging).

## Relevant examples of Fortran code
