PROGRAM circle
        IMPLICIT NONE
        real :: r, A = 0, d, pi = 3.14159

        write (*,*) "Enter the diameter of the circle"
        read (*,*) d
        r = d/2
        A = (r**2)*pi
        write (*,*) "The area is:", A
        END PROGRAM

      
