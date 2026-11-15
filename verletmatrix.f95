PROGRAM verlet
        implicit none
        
        ! variable declaration
        
        integer, parameter :: wp = SELECTED_REAL_KIND (p=13, r=300)
        
        integer :: p !particlenumber
        
        
        integer :: i, n, j, pstart, matcol = 12, ios !total steps for docycle, matrix size data
        
        real :: m, s, actualtime, tol
        
        real (kind=wp), dimension(3) :: F, V, pos, Fnext !attribute matrices
        
        real, dimension(:,:), allocatable :: inputmat ! input file
        
        !array construction
        F = (/ 0.0, 0.1, 0.0 /)
        pos = (/ 0.0, 0.0, 0.0 /)
        V = (/ 0.0, 0.0, 0.0 /)
        
        !read into array "inputmat" to store data in RAM
        
        OPEN(UNIT = 11,  FILE="input.txt", STATUS="old", ACTION="read")
         
        !counting rows (particles)
        do
        read(11, *, iostat=ios)
        if (ios /=0) exit
        p = p + 1
        enddo
        
        if (p == 0) then
                print *, "No data found"
        stop
        endif

        allocate (inputmat(p, matcol))

        rewind(11)

        do i = 1, p
        read(11, *, iostat = ios) inputmat(p,:)
        if (ios /= 0) then
                print *, "Error reading row", i, ". IOSTAT=", ios
                stop
        endif
        enddo

        close(11)
        print *, "Successfully created particledata-matrix. Particle number=", p

        !initializing variables
        
        n = 6000
        s = 0.02
        m = 1.0

        ! velocity verlet algorithm
        
        do pstart = 1, p
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

        enddo
        open(unit = 12, file = "output.txt", status="replace", action="write")
        write(12, *) inputmat
        close(12)
                
END PROGRAM verlet

