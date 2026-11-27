MODULE mymodule
        USE KINDS, ONLY: wp => dp
        IMPLICIT NONE

        PUBLIC :: myexpsubroutine

        CONTAINS

                SUBROUTINE myexpsubroutine(x,i,y)
                        INTEGER, INTENT(IN) :: i
                        REAL (KIND=wp), DIMENSION(:), INTENT(IN) :: x
                        REAL (KIND=wp), DIMENSION(:), INTENT(OUT) :: y
                        y = x**i
                END SUBROUTINE

        END MODULE mymodule


