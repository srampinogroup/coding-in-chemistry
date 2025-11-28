PROGRAM cda
  USE kinds, ONLY: wp => dp
  USE cubes
  IMPLICIT NONE

  ! 1. Insantiate cube objects with data from cube files ../test/CuCO+/ab.cube, ../test/CuCO+/a.cube, and ../test/CuCO+/b.cube

  ! 2. Generate a cube object containing the charge redistribution \Delta \rho = \rho_{AB} - \rho_{A} - \rho_{B}

  ! 3. Use an ad hoc defined external procedures in module cubes for extracting the charge-displacement function from the charge redistribution

END PROGRAM cda
