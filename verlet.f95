PROGRAM verlet
        implicit none
        real :: m, s, Fx, Fy, Fz, Vx, Vy, Vz, x, y, z, actualtime, Fx2, Fy2, Fz2, tol
        integer :: i, n, j !total steps for docycle
        Vx = 0
        Vy = 0
        Vz = 0
        n = 600
        s = 0.2
        x = 0
        y = 0
        z = 0
        Fx = 0
        Fy = 0.1
        Fz = 0

        write (*,*) "Type in the details of the particle: (mass) "
        read (*,*) m
        do i = 1, n
                actualtime = i*s
                x = x+s*Vx+s**2*Fx/(2*m)
                y = y+s*Vy+s**2*Fy/(2*m)
                z = z+s*Vz+s**2*Fz/(2*m)
                Fx2 = Fx
                Fy2 = Fy
                Fz2 = Fz
                Vx = Vx+s/(2*m)*(Fx+Fx2)
                Vy = Vy+s/(2*m)*(Fy+Fy2)
                Vz = Vz+s/(2*m)*(Fz+Fz2)
                write (*,*) actualtime, "seconds"
                tol = 10
                if(mod(actualtime, tol) == 0) then
                        write (*,*) "SpeedXYZ = ", Vx, "m/s, ", Vy, "m/s, ", Vz, "m/s"
                        write (*,*) "PosXYZ = ", x, "m[x] | ", y, "m[y] |", z, "m[z]"
                        write (*,*) "time:", actualtime, "s"
                else
                endif
        
        enddo
                
END PROGRAM verlet
