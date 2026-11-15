PROGRAM verlet
        implicit none
        
        ! variable declaration
        
        integer, parameter :: wp = SELECTED_REAL_KIND (p=13, r=300)
        
        real :: m, s, actualtime, tol
        
        real (kind=wp), dimension(3) :: F, V, pos, Fnext !attribute matrices

        real, dimension(2,2) :: inpt ! input file
        
        integer :: i, n, j !total steps for docycle
        
        !array construction
        F = (/ 0.0, 0.1, 0.0 /)
        pos = (/ 0.0, 0.0, 0.0 /)
        V = (/ 0.0, 0.0, 0.0 /)
        







        !initializing variables
        
        n = 6000
        s = 0.02
        m = 1.0
        ! velocity verlet algorithm

        do i = 1, n
                actualtime = i*s
                
                do j = 1, 3 ! 3 x y z vectors
                !update positions
  
                pos(j) = pos(j)+s*V(j)+s**2*F(j)/(2*m)

                !update forces
                Fnext(j) = F(j)

                !update velocities
                V(j) = V(j)+s/(2*m)*(F(j)+Fnext(j))
                enddo
                write (*,*) actualtime, "seconds"

                !write data in 10s intervals
                tol = 10
                if(mod(actualtime, tol) == 0) then
                        write (*,*) "SpeedXYZ = ", V(1), "m/s, ", V(2), "m/s, ", V(3), "m/s"
                        write (*,*) "PosXYZ = ", pos(1), "m[x] | ", pos(2), "m[y] |", pos(3), "m[z]"
                        write (*,*) "time:", actualtime, "s"
                else
                endif
        
        enddo
                
END PROGRAM verlet

