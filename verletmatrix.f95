PROGRAM verlet
        implicit none
        
        ! variable declaration
        
        integer, parameter :: wp = SELECTED_REAL_KIND (p=13, r=300)
        
        integer :: p = 0, ncell !particlenumber (row), cellnumber

        character(len=512) :: line
        
        integer :: i, n, j, pstart, matcol = 10, ios !total steps for docycle, matrix size data, iostatus
        
        real :: m, s, actualtime, tol
        
        real (kind=wp), dimension(3) :: F, V, pos, Fnext !attribute matrices
        
        real, dimension(:,:), allocatable :: inputmat ! input file
        
        !array construction
        F = (/ 0.0, 0.1, 0.0 /)
        pos = (/ 0.0, 0.0, 0.0 /)
        V = (/ 0.0, 0.0, 0.0 /)
    
        !txt into array
        OPEN(UNIT = 11,  FILE="input.txt", STATUS="old", ACTION="read")
        
         
        !counting non-empty rows (particles)
        do
        read(11, *, iostat=ios) line
        if (ios /=0) exit
        if (len_trim(line) == 0) cycle
        p = p + 1
        enddo
        if (p == 0) then
                print *, "No data found"
        stop
        endif
        print *, p
        !creating data-matrix same size as input-matrix
        allocate (inputmat(p, matcol))

        rewind(11)
        !allocating values into matrix

        do i = 1, p
        read(11, *, iostat = ios) inputmat(i,:)
        print *, inputmat(i,:)
        if (ios /= 0) then
                print *, "Error reading row", i, ". IOSTAT=", ios
                exit
        
        endif
        enddo

        CLOSE(11)
        print *, "Successfully created particledata-matrix. Particle number=", p

        !initializing variables
        
        n = 6000
        s = 0.02
        m = 1.0

        ! velocity verlet algorithm
        
        !do pstart = 1, p
        !do i = 1, n
        
        !actualtime = i*s
                
                !do j = 1, 3 ! 3 x y z vectors
                !update positions
  
                !pos(j) = pos(j)+s*V(j)+s**2*F(j)/(2*m)

                !update forces
                !Fnext(j) = F(j)

                !update velocities
                !V(j) = V(j)+s/(2*m)*(F(j)+Fnext(j))
                !enddo
                !write (*,*) actualtime, "seconds"

                !write data in 10s intervals
                !tol = 10
                !if(mod(actualtime, tol) == 0) then
                !        write (*,*) "SpeedXYZ = ", V(1), "m/s, ", V(2), "m/s, ", V(3), "m/s"
                !        write (*,*) "PosXYZ = ", pos(1), "m[x] | ", pos(2), "m[y] |", pos(3), "m[z]"
                !        write (*,*) "time:", actualtime, "s"
                !else
                !endif
                 
        !enddo

        !enddo
        open(unit = 12, file = "output.txt", status="replace", action="write")
        do i = 1, p
        write(12, *) inputmat(i,:)
        enddo
        close(12)
                
END PROGRAM verlet

