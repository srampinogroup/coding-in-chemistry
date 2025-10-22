# Hands-on session 1. Classical trajectories via the velocity Verlet
# algorithm

bla



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
