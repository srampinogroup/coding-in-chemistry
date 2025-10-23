# Hands-on session 1.
# Classical trajectories via the Velocity Verlet algorithm

In finite-difference schemes, time is discretized using a constant
time step   $\Delta t = \tau$.
Let us denote with subscript $k$ the quantities at the $k$-th time
step: the time $t_k=k\tau$, the position of the particle $x_k =
x(t_k)$, its velocity $v_  k =v(t_k)$, and the force acting on it
$f_k = f(x_k)$.

For one particle of mass $m$ in one dimension $x$, the Velocity Verlet
algorithm is based on the following recursive scheme:

$$
x_{k+1} = x_k + \tau v_k + \frac{\tau^2}{2} \frac{f_k}{m}
$$

$$
v_{k+1} = v_k + \frac{\tau}{2m} (f_k + f_{k+1})
$$

Such scheme can be easily generalized to the many-particle, higher
dimensional case. For a system of $N$ particles in three-dimensional
space, the potential is a function
of the positions of the particles $\\{\boldsymbol{r}_a\\}$.
The force acting on the $a$-th particle will then be:

$$
\boldsymbol{f}^{(a)} = - \nabla^{(a)} V (\\{\boldsymbol{r}_a\\})
$$

where superscript $a$ on the gradient operator indicates that the
derivatives should be taken with respect to the coordinates of the
$a$-th particle.

For each separate Cartesian component, the Velocity Verlet recursive
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
all the particles.

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
ranging from 1 to 10 and $\tau$ = 0.2 s for one
particle of mass 1 kg in 3D space subject to a constant force
expressed by components $f^{(a,x)}$ = 0 kg m s<sup>-2</sup>,
$f^{(a,y)}$ = 0.1 kg m s<sup>-2</sup>,
$f^{(a,z)}$ = 0 kg m s<sup>-2</sup>.

Use the following initial conditions: still particle in the origin of
the reference frame.
Make the program print the values of $x$, $y$, and $z$ of the
particle at each time iteration.

## Guidelines and tips

Create the file `verlet.f95` and type the following:

```
PROGRAM verlet
  IMPLICIT NONE
  ! declare your variable here

  ! write your instructions here

END PROGRAM verlet
```

Language keywords will be typeset uppercase (they are not case
sensitive, but we will stick to this convention).
We will use two blank spaces for indenting blocks of code.
Lines starting with `!` are comments (not part of your Fortran
program, will be ignored by the compiler).

To test your program, you
need to compile it:

```
gfortran -o verlet verlet.f95
```

(this will generate the `verlet` executable) and execute it:

```
./verlet
```

Typically, you implement a small portion of code, and soon after you
test the program in order to verify that it behaves as expected (this
is called debugging). Don't forget to commit and push when you
implemented something that works!

## Relevant examples of Fortran code

### Variable declaration

```
IMPLICIT NONE
INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
INTEGER :: i, j, k                                                 
INTEGER :: l, m                                               
CHARACTER (LEN=72) :: str1, str2, str3       
REAL (KIND=wp) :: a, b, c                                          
REAL, DIMENSION(:), ALLOCATABLE :: x
REAL, DIMENSION(:,:), ALLOCATABLE :: xmat
```

$x$ will be an one-index array, `xmat` will be an two-index array.
Both will be of undefined dimensions, until their size is defined and
the related memory is allocated.

### Memory allocation

```
ALLOCATE ( x (300) )
ALLOCATE ( xmat (200,300) )
```

Now `x` is a vector with 300 elements. The `i`-th element (with `i`
referencing an integer between 1 and 300) is referenced with `x(i)`.
`xmat` is instead a rectangular matrix with 200 rows and 300 columns.
Element $ij$ is referenced with `xmat(i,j)` with `i` and `j`
referencing integer numbers in the proper ranges.

When you don't need `x` and `xmat` any more, you free up the
allocated memory:

```
DEALLOCATE ( x, xmat )
```

### Assignement

Examples:
```
a = b * c

a = a + ( b * c)

a = 3.0_wp

x(i) = 3.5_wp

x(3) = 1.0E-12_wp

xmat(i,j) = 9.2E6_wp

```
 
### I/O

#### Read from file

```
OPEN (UNIT=11, FILE="<input file>", STATUS="old", ACTION="read")

READ (UNIT=11, FMT=*) a, b, c
READ (UNIT=11, FMT=*) i, j, k [every READ corresponds to a line]
READ (UNIT=11, FMT=*) x(1), x(2), x(3)
```

When reading from a given unit, every `READ` corresponds to a line.
When reading multiple variables (e.g. `a, b, c`) in one `READ` line,
the expected delimiter in the corresponding finle line is one or more
blank spaces.
A compatible `<input file>` with the above code would be for instance:

```
3.0 5.0 1.0E-2
4 200 350
0.0 0.0 0.0
```

Note that you do not need to append `_wp` when you read real values.

When you don't need the file any more:

```
CLOSE (11)
```

#### Write to file

```
OPEN (UNIT=12, FILE="<output file>", STATUS="replace", ACTION="write")

WRITE (UNIT=12, FMT=*) a, b, c
WRITE (UNIT=12, FMT=*) x(1), x(2), x(3)
```

Again, every `WRITE` corresponds to a line.

When you don't need the file any more:

```
CLOSE (12)
```

### Iteration

```
INTEGER :: i, n
! ...
n = 10

DO i = 1, n
  <block of instructions>
ENDDO
```

corresponds to:

```
i = 1
```
If `i > n`, exit loop. Otherwise:
```
<block of instructions>
i = i + 1
```
If `i > n`, exit loop. Otherwise:
```
<block of instructions>
i = i + 1
```
Etc...

### Alternative

Basic alternative construct:

```
IF ( i > 0) THEN
  <block A>
ELSEIF ( i < 0) THEN
  <block B>
ELSE
  <block C>
ENDIF
```

We will come to this later on: not strictly necessary to complete the
exercies.

### Misellanea

In addition to `INTEGER`, `REAL`, and `CHARACTER`, variables can be
`COMPLEX`, `LOGICAL`, or of user-defined (so-called `derived`) data
type ( `TYPE`), i.e. composite data structures
resulting from the aggregation of simple (or other derived) data
types.

#### Main operators

##### Arithmetic
`+`, `-`, `*`, `/`, `**`: addition, subtraction,
multiplication, division and exponentiation operator, respectively.

##### Relational
`==`, `/=`, `<`, `<=`, `>`, `>=`: equal to, not equal to, less than,
less than or equal to, greater than, greater than or equal to,
respectively)

##### Logical
`.AND.`, `.OR.`, `.NOT.`, `.EQV.`, and `.NEQV.`.
