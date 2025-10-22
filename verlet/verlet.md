# Hands-on session 1. Classical trajectories via the velocity Verlet
# algorithm

In a finite-difference scheme, time is discretized using a constant time step   $\Delta t = \tau$.
Let us denote with subscript $k$ the quantities at the $k$-th time step: the t  ime $t_k=k\tau$, the position of the particle $x_k = x(t_k)$, its velocity $v_  k =v(t_k)$, and the force acting on it $f_k = f(x_k)$.


$x_{k+1} = x_k + \tau v_k + \frac{\tau^2}{2} \frac{f_k}{m}$

$v_{k+1} = v_k + \frac{\tau}{2m} (f_k + f_{k+1})$

Such scheme can be easily generalized to the many-atom, higher dimensional case.
For a system of
$N$ particles in three-dimensional space, the potential is a function
of the positions of the particles $\{\boldsymbol{r}_a\}$.

The force acting on the $a$-th atom will then be:

$\vf^{(a)} = - \nabla^{(a)} V (\{\boldsymbol{r}_a\})$
where superscript $a$ on the gradient operator indicates that the derivatives should be taken with respect to the coordinates of the $a$-th atom.
For each separate Cartesian component, the above discussed Verlet recursive scheme will apply, with the $x$, $y$, and $z$ components of the forces being, respectively,
%
\begin{equation}
%f^{(a,x)} = - \frac{\dd}{\dd x^{(a)}} V (\boldsymbol{r}_1, \boldsymbol{r}_2, \ldots, \boldsymbol{r}_N)
f^{(a,x)} = - \frac{\dd}{\dd x^{(a)}} V (\{\boldsymbol{r}_a\})
\end{equation}
\begin{equation}
%f^{(a,y)} = - \frac{\dd}{\dd y^{(a)}} V (\boldsymbol{r}_1, \boldsymbol{r}_2, \ldots, \boldsymbol{r}_N)
f^{(a,y)} = - \frac{\dd}{\dd y^{(a)}} V (\{\boldsymbol{r}_a\})
\end{equation}
\begin{equation}
%f^{(a,z)} = - \frac{\dd}{\dd z^{(a)}} V (\boldsymbol{r}_1, \boldsymbol{r}_2, \ldots, \boldsymbol{r}_N)
f^{(a,z)} = - \frac{\dd}{\dd z^{(a)}} V (\{\boldsymbol{r}_a\})
\; .
\end{equation}
Thus, a distinct recursion relation rooted in the Velocity Verlet algorithm can be used for each coordinate-velocity pair but these are all coupled through the force, which depends on the coordinates of all the atoms.

Focusing on the $x$ component, for instance, the final algorithm, with given initial $x_k$ and $v_k$ and a known expression for $f(x)$, is:
%
\begin{enumerate}

\item\label{it-1} Calculate $x^{(a)}_{k+1}$:
\begin{equation}
x^{(a)}_{k+1} = x^{(a)}_k + \tau v^{(a,x)}_k + \tau^2 \frac{f^{(a,x)}_k}{2m_a}
\end{equation}

\item\label{it-2} Evaluate $f^{(a,x)}_{k+1}$

\item Calculate $v^{(a,x)}_{k+1}$:
\begin{equation}
v^{(a,x)}_{k+1} = v^{(a,x)}_k + \frac{\tau}{2m_a} \left( f^{(a,x)}_k + f^{(a,x)}_{k+1} \right)
\end{equation}

\item assign the value of $x^{(a)}_{k+1}$ to $x^{(a)}_k$ and go back go back to step \ref{it-1}
\end{enumerate}
%
Analogous schemes can be written for the $y$ and $z$ components.

%\subsection*{Potential-energy functions}

An easy and fruitful exercise for the eager student is to implement the Velocity Verlet algorithm for a system of $N$ atoms interacting through pairwise additive Lennard-Jones potentials:
%
\begin{equation}
%V (\boldsymbol{r}_1, \boldsymbol{r}_2, \ldots, \boldsymbol{r}_N)
V (\{\boldsymbol{r}_a\})
 = \sum_a^N \sum_{b<a}^N V_\rmLJ (r_{ab})
 %= \sum_{a>b} V_\rmLJ (r_{ab})
\; ,
\end{equation}
% chk
%\red{check consistency with notation of sums of couples}
%
where $r_{ab}$ is the distance between atoms $a$ and $b$
%
\begin{equation}
r_{ab} = \sqrt{x_{ab}^2 + y_{ab}^2 + z_{ab}^2}
\; ,
\end{equation}
%
with $x_{ab} = x_a - x_b$, and the Lennard-Jones potential is given by:
%
\begin{equation}
V_\rmLJ (r) = 4 \epsilon
\left[
	\left( \frac{\sigma}{r} \right)^{12}
	- \left( \frac{\sigma}{r} \right)^{6}
\right]
\; ,
\end{equation}
featuring the well known short-range repulsion term and long-range attraction term.
%
%Force on atom $i$ is a vector pointing in the direction of the steepest descent of the potential energy:
%\begin{equation}
%\vf^{(i)} = - \nabla^{(i)} V (\boldsymbol{r}_1, \boldsymbol{r}_2, \ldots, \boldsymbol{r}_N) =
%- \sum_{j \neq i} \nabla^{(i)} V_\rmLJ (r_{ij})
%\end{equation}
%
%\begin{equation}
%\vf^{(i)} = - \sum_{j \neq i}
%\left(
%	\hat{x} \frac{\partial}{\partial x^{(i)}} +
%	\hat{y} \frac{\partial}{\partial y^{(i)}} +
%	\hat{z} \frac{\partial}{\partial z^{(i)}}
%\right)
%V_\rmLJ (r_{ij})
%\end{equation}
%
%(uniform notation, with Bader)
%
%
%[using the chain rule for differentiation], $\frac{\partial}{\partial x^{(i)}} = \frac{\partial}{\partial x_{ij}}$, since $\frac{\partial x_{ij}}{\partial x^{(i)}} = 1$.
In this case, the
derivative of the pair potential with respect to each Cartesian component of the position of an atom can be written in terms of $V'$, i.e. the derivative of the Lennard-Jones potential with respect to the argument of the same function.
%Indicate $v'(r_{ij}) = \frac{\dd V_\rmLJ (r_{ij})}{\dd r_{ij}}$.
%
For the $x$ component, for example:
%
\begin{equation}
\frac{\partial V_\rmLJ (r_{ab})}{\partial x_{ab}} = V_\rmLJ'(r_{ab}) \frac{\partial r_{ab}}{\partial x_{ab}} = \frac{x_{ab}}{r_{ab}} V_\rmLJ'(r_{ab})
\; ,
\end{equation}
%
where $V_\rmLJ'(r)$ is easily evaluated as:
%
%LJ, simple:
%
\begin{equation}
V_\rmLJ'(r) = 4 \epsilon
\left[
	- 12 \frac{\sigma^{12}}{r^{13}}
	+  6 \frac{\sigma^{6}}{r^{7}}
\right]
\; .
\end{equation}
%
Accordingly, the $x$ component of the force acting on the $a$-th atom to be used in step
%\ref{it-2}
2
of the above summarized algorithm is:
% 
\begin{equation}
f^{(a,x)}_{k+1} = - \sum_{b \neq a} \frac{x_{ab}}{r_{ab}} V_\rmLJ'(r_{ab})
\; .
\end{equation}


## Variable declaration

```fortran
  INTEGER :: i, j, k                                                 
  REAL (KIND=wp) :: a, b, c                                          
  CHARACTER (LEN=72) :: str1, str2, str3                             
```

NB: The following instruction should be used when declaring an array as an attribute of a derived datatype

```fortran
  REAL (KIND=wp), DIMENSION(:), POINTER :: array                     
```
                                                                      
## I/O                                                                
                                                                      
```fortran
  OPEN (UNIT=11, FILE=infile, STATUS="old", ACTION="read")           
                                                                      
  READ (UNIT=11, FMT=*) mycube%str1
  READ (UNIT=11, FMT=*) mycube%str2
  READ (UINT=11, FMT=*) mycube%natom, mycube%xmin, mycube%ymin, mycube%zmin
  ! when you don't need the file any more:
  CLOSE (11)                                                         
```

## Memory allocation                                                  
                                                                      
```fortran
  ALLOCATE ( mycube%zahl(mycube%natoms) )                            
```
                                                                      
## Reading the array...                                               
                                                                      
```
    ALLOCATE ( mycube%array(mycube%nx*mycube%ny*mycube%nz) )          

    READ (UNIT=11, FMT=*) mycube%array                                
```
