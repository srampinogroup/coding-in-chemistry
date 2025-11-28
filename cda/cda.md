# Hands-on session 3.

#### Objectives
1. Translate formulae into algorithms 
2. Write and compile a composite Fortran program using a Makefile
3. Familiarize with advanced Fortran programming features: derived
datatypes, object-based programming, operator overloading.

# Charge Displacement Analysis via object-based programming

Read Chapter on Charge Displacement Analysis on the Moodle page of
the course.

Create a Fortarn module handling `.cube` files and a Fortran program
computing the CD function along $z$ associated with the charge
redistriubtion occuring upon bond formation of two molecular
fragments A and B.

# Guidelines and tips
                                                                      
1. Create the 'cube' derived datatype in `cubes.f95`
2. Fill the procedures in `cubes.f95`
3. Fill the proper instructions in `cda.f95`
                                                                      
How to compile:                                                       
                                                                      
In `cda-files/cda/src`:
```
make                                                                  
```
(if you get an error try `make clean` first and then `make`).

How to execute:
                                                                      
`../bin/cda`

More information on the `cube` file format:                                                      
                                                                      
cube file format: http://paulbourke.net/dataformats/cube/             
see also: http://gaussian.com/cubegen/ 

NB: The following instruction should be used when declaring an array as an attribute of a derived datatype

```
  REAL (KIND=wp), DIMENSION(:), POINTER :: array                     
```
                                                                      
## I/O                                                                
                                                                      
```
  OPEN (UNIT=11, FILE=infile, STATUS="old", ACTION="read")           
                                                                      
  READ (UNIT=11, FMT=*) mycube%str1
  READ (UNIT=11, FMT=*) mycube%str2
  READ (UINT=11, FMT=*) mycube%natom, mycube%xmin, mycube%ymin, mycube%zmin
  ! when you don't need the file any more:
  CLOSE (11)                                                         
```

## Memory allocation
                                                                      
```
  ALLOCATE ( mycube%zahl(mycube%natoms) )                            
```
                                                                      
## Reading the array
                                                                      
```
    ALLOCATE ( mycube%array(mycube%nx*mycube%ny*mycube%nz) )          

    READ (UNIT=11, FMT=*) mycube%array                                
```
