MODULE cubes
  USE kinds, ONLY: wp => dp
  IMPLICIT NONE

  TYPE, PUBLIC :: cube
    PRIVATE
    CHARACTER (LEN=72) :: str1
    ! fill
  END TYPE cube

  PUBLIC :: cube_get, &
            cube_add, &
            cube_sub, &
            cube_int, &
            cube_cdz, &
            cube_del

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE cube_add
  END INTERFACE
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE cube_sub
  END INTERFACE


  CONTAINS


  SUBROUTINE cube_get (mycube, infile)
    CHARACTER(LEN=*), INTENT(IN) :: infile
    TYPE (cube), INTENT(OUT) :: mycube
    ! ...
    ! access attributes as follows:
    ! mycube%str1 = ...
  END SUBROUTINE cube_get


  FUNCTION cube_add (mycube1, mycube2)
    TYPE(cube) :: cube_add
    TYPE(cube), INTENT(IN) :: mycube1, mycube2
    ! ...
  END FUNCTION cube_add


  FUNCTION cube_sub (mycube1, mycube2)
    TYPE(cube) :: cube_sub
    TYPE(cube), INTENT(IN) :: mycube1, mycube2
    ! ...
  END FUNCTION cube_sub


  FUNCTION cube_int (mycube)
    REAL (KIND=wp) :: cube_int
    TYPE (cube), INTENT(IN) :: mycube
    ! ...
  END FUNCTION cube_int


  SUBROUTINE cube_del (mycube)
    TYPE (cube), INTENT(IN) :: mycube
    ! ...
  END SUBROUTINE cube_del


END MODULE cubes
