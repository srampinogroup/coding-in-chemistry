# Hands-on session 2.

#### Objectives
1. Devise algorithms and coding strategies for more complex
computational problems
2. Write and compile a composite Fortran program using modules and
external procedures
3. Learn how to use array syntax, modules, functions, and subroutines

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


## Exercise 2

Write a Fortran program that implements the Velocity Verlet algorithm
for a system of $N$ Neon ($m_\text{Ne}$ = 20.1797 amu) atoms in 3D space
interacting through pairwise additive Lennard-Jones potentials.

Use the following parameters: $\sigma_{\text{Ne}-\text{Ne}}$ = 5.2186
$a_0$ and $\epsilon_{\text{Ne}-\text{Ne}}$ = 0.000112991 $E_\text{h}$
(taken from [JCP 138, 134502
(2013)](https://doi.org/10.1063/1.4796144)).

Write the trajectory in XYZ format for visualizaton with
[VMD](https://www.ks.uiuc.edu/Development/Download/download.cgi?PackageName=VMD).

## Guidelines and tips

For a system of $N$ particles in 3D space interacting through
pairwise additive Lennard-Jones potentials:

$$
V (\{\boldsymbol{r}_a\})
 = \sum_a^N \sum_{b<a}^N V_\text{LJ} (r_{ab})
 %= \sum_{a>b} V_\text{LJ} (r_{ab})
$$

where $r_{ab}$ is the distance between atoms $a$ and $b$

$$
r_{ab} = \sqrt{x_{ab}^2 + y_{ab}^2 + z_{ab}^2}
$$

with $x_{ab} = x_a - x_b$, and the Lennard-Jones potential is given by:

$$
V_\text{LJ} (r) = 4 \epsilon
\left[
	\left( \frac{\sigma}{r} \right)^{12}
	- \left( \frac{\sigma}{r} \right)^{6}
\right]
$$

In this case, the
derivative of the pair potential with respect to each Cartesian component of the position of an atom can be written in terms of $V'$, i.e. the derivative of the Lennard-Jones potential with respect to the argument of the same function.
For the $x$ component, for example:

$$
\frac{\partial V_\text{LJ} (r_{ab})}{\partial x_{ab}} = V_\text{LJ}'(r_{ab})
\frac{\partial r_{ab}}{\partial x_{ab}} = \frac{x_{ab}}{r_{ab}}
V_\text{LJ}'(r_{ab})
$$

where $V_\text{LJ}'(r)$ is easily evaluated as:

$$V_\text{LJ}'(r) = 4 \epsilon
\left[
	- 12 \frac{\sigma^{12}}{r^{13}}
	+  6 \frac{\sigma^{6}}{r^{7}}
\right]
$$
Accordingly, the $x$ component of the force acting on the $a$-th atom to be used in step 2
of the algorithm is:
$$
f^{(a,x)}_{k+1} = - \sum_{b \neq a} \frac{x_{ab}}{r_{ab}} V_\text{LJ}'(r_{ab})
$$

### Array syntax

The following code example may be useful to understand and exploit array syntax in Fortran.

```
PROGRAM arrays                                                        
  IMPLICIT NONE                                                       
  INTEGER, DIMENSION(3) :: vec                                        
  INTEGER, DIMENSION(3,3) :: mat                                      
                                                                      
  vec = (/ 1, 2, 3 /)                                                 
                                                                      
  mat(1,:) = (/ 1, 2, 3 /)                                            
  mat(2,:) = (/ 4, 5, 6 /)                                            
  mat(3,:) = (/ 7, 8, 9 /)                                            
                                                                      
  PRINT *, vec                                                        
  PRINT *, "---"                                                      
                                                                      
  PRINT *, mat(3,2:3)                                                 
  PRINT *, "---"                                                      
                                                                      
  vec = vec * 2                                                       
                                                                      
  mat(:,1) = mat(:,1) + vec(:)                                        
                                                                      
  PRINT *, mat(1,:)                                                   
  PRINT *, mat(2,:)                                                   
  PRINT *, mat(3,:)                                                   
END PROGRAM arrays
```

The output will be:
```
           1           2           3
 ---
           8           9
 ---
           3           2           3
           8           5           6
          13           8           9
```


### External procedures

Use a subroutine to evaluate the forces acting on a given particle. Subroutines and functions typically go into a 'module' in a separate file, which is imported in the main program. The following code illustrates how this works. Note that we are also putting into a module (named `kinds`, in file `kinds.f95`) the definition of the parateters relating to machine precision (`sp` for single, `db` for double). We then opt to use a working precision `wp` equal to double precision when importing the module (see `USE` statements in the code examples below).

File `kinds.f95`:
```
MODULE kinds                                                          
  IMPLICIT NONE                                                       
  INTEGER, PARAMETER, PUBLIC :: sp = SELECTED_REAL_KIND (p=6, r=37)   
  INTEGER, PARAMETER, PUBLIC :: dp = SELECTED_REAL_KIND (p=13, r=300) 
END MODULE kinds 
```

File `module.f95`:
```
MODULE mymodule                                                       
  USE kinds, ONLY: wp => dp                                           
  IMPLICIT NONE                                                       
                                                                      
  PUBLIC :: myexpsubroutine                                           
                                                                      
  CONTAINS                                                            
                                                                      
  SUBROUTINE myexpsubroutine(x, i, y)                                 
    INTEGER, INTENT(IN) :: i                                          
    REAL (KIND=wp), DIMENSION(:), INTENT(IN) :: x                     
    REAL (KIND=wp), DIMENSION(:), INTENT(OUT) :: y                    
    y = x**i                                                          
  END SUBROUTINE myexpsubroutine                                      
                                                                      
END MODULE mymodule
```

File `main.f95`:
```
PROGRAM main                                                          
  USE kinds, ONLY: wp => dp                                           
  USE mymodule                                                        
  IMPLICIT NONE                                                       
  INTEGER :: i                                                        
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: x, y                   
  i = 3                                                               
  ALLOCATE ( x(3), y(3) )                                             
  x = (/ 1.0, 2.0, 5.0 /)                                             
  CALL myexpsubroutine(x, i, y)                                       
  PRINT *, y                                                          
  DEALLOCATE (x, y)                                                   
END PROGRAM main
```

To compile, execute:
```
gfortran -o myprogram kinds.f95 module.f95 main.f95
```

And to execute:
```
./myprogram
```

### XYZ format

The XYZ format for storing trajectories is:
```
<number of atoms>
<time at k = 1>
<atom 1 symbol> <x> <y> <z>
<aotm 2 symbol> <x> <y> <z>
[...]
<aotm N symbol> <x> <y> <z>
<number of atoms>
<time at k = 2>
<atom 1 symbol> <x> <y> <z>
<aotm 2 symbol> <x> <y> <z>
[...]
<aotm N symbol> <x> <y> <z>
[...]
```
where coordinates are given in Angstrom.
Note that strictly speaking the lines `<time at k = ...>` would be
comment lines in ordinary XYZ format but we will use them to store
the value of the time at each iteration.
