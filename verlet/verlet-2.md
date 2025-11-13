# Hands-on session 2.

#### Objectives
1. Devise algorithms and coding strategies for more complex
computational problems
2. Write and compile a composite Fortran program using modules and
external procedures
3. Learn how to use array syntax, modules, functions, and subroutines

## Systems of $N$ particles interactive through pairwise additive Lennard-Jones potentials

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
of the algorithm (see Hands-on Session 1.) is:
$$
f^{(a,x)}_{k+1} = - \sum_{b \neq a} \frac{x_{ab}}{r_{ab}} V_\text{LJ}'(r_{ab})
$$


## Exercise 2

Write a Fortran program that implements the Velocity Verlet algorithm
for a system of $N$ Neon ($m_\text{Ne}$ = 20.1797 amu) atoms in 3D space
interacting through pairwise additive Lennard-Jones potentials.

Use the following parameters: $\sigma_{\text{Ne}-\text{Ne}}$ = 5.2186
$a_0$ and $\epsilon_{\text{Ne}-\text{Ne}}$ = 0.000112991 $E_\text{h}$
(taken from [JCP 138, 134502
(2013)](https://doi.org/10.1063/1.4796144)).

Read input values from a file structured as follows (with distnces in Bohr and energies in Hartree):

```
600 0.2                                     ! nk, tau
5.2186, 0.000112991                         ! sigma, epsilon
2                                           ! n, number of atoms
20.1797 0.0  0.0  4.0  0.0  0.0  0.0        ! m, x, y, z, vx, vy, vz
20.1797 0.0  0.0  0.0  0.0  0.0  0.0        ! m, x, y, z, vx, vy, vz 
```

Write the trajectory in XYZ format for visualizaton with
[VMD](https://www.ks.uiuc.edu/Development/Download/download.cgi?PackageName=VMD).

## Guidelines and tips

### Coding and variables

Use an allocatable two-index array `x(:,:)` for storing the values of the coordinates of the particles at the current iteration.
Use the first index for the particle id, and the second index ranging from 1 to 3 for $x$, $y$, and $z$.
Do the same for the velocities `v(:,:)`, the forces `f(:,:)`, and the forces at the next iteration (see algorithm description in Hands-on Session 1.) `fnext(:,:)`.
In so doing, you can benefit from syntax array (see below) for avoiding code replication due to the fact that the equation for $x$, $y$, and $z$ are exactly the same.

### Array syntax

The following code example may be useful to understand and exploit array syntax in Fortran.

```
PROGRAM arrays                                                        
  IMPLICIT NONE                                                       
  INTEGER, DIMENSION(3) :: vec                                        
  INTEGER, DIMENSION(3,3) :: mat                                      

  ! array construction                                                                    
  vec = (/ 1, 2, 3 /)                                                 
                                                                      
  mat(1,:) = (/ 1, 2, 3 /)                                            
  mat(2,:) = (/ 4, 5, 6 /)                                            
  mat(3,:) = (/ 7, 8, 9 /)                                            

  ! print all elements of the array                                                            
  PRINT *, vec                                                        
  PRINT *, "---"                                                      

  ! array slicing                                                                    
  PRINT *, mat(3,2:3)                                                 
  PRINT *, "---"
                                                     
  ! array multiplication with scalar                                                                    
  vec = vec * 2                                                       

  ! element-wise operation without explicit iteration (loop) constructs                                                                    
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

Use a subroutine to evaluate the forces acting on a given particle. Subroutines and functions go into a 'module' in a separate file, which is imported in the main program. The following code illustrates how this works. Note that we are also putting into a module (named `kinds`, in file `kinds.f95`) the definition of the parateters relating to machine precision (`sp` for single, `db` for double). We then opt to use a working precision `wp` equal to double precision when importing the module (see `USE` statements in the code examples below).

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
<atom 2 symbol> <x> <y> <z>
[...]
<atom N symbol> <x> <y> <z>
<number of atoms>
<time at k = 2>
<atom 1 symbol> <x> <y> <z>
<atom 2 symbol> <x> <y> <z>
[...]
<atom N symbol> <x> <y> <z>
[...]
```
where coordinates are given in Angstrom.
Note that strictly speaking the lines `<time at k = ...>` would be
comment lines in ordinary XYZ format but we will use them to store
the value of the time at each iteration.

## Exercise 2.1

Check the convergence of your trajectory with respect to the
time-step value. How small has the time step to be in order to get a
trajectory converged within 1 cm after 2 minutes of simulation?

## Guidelines and tips

To check the convergence, 'nest' your Velocity Verlet algorithm
inside a loop construct that, at each iteration, halves the timestep,
calculates the final positions after 2 minutes of simulation, and checks the
convergence of these with respect to the values obtained in the
previous iteration.

