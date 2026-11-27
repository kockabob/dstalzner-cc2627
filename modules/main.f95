PROGRAM main

        USE kinds, ONLY: wp => dp
        USE mymodule

        IMPLICIT NONE

        INTEGER :: i
        REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: x, y

        i = 3

        ALLOCATE ( x(3), y(3))
        
        x = (/1.0, 2.0, 5.0/)
        CALL myexpsubroutine(x,i,y)
        PRINT *, y
        DEALLOCATE (x, y)

END PROGRAM main
        
