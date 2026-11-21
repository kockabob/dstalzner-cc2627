PROGRAM verlet
        implicit none
        
        ! variable declaration
        
        integer, parameter :: wp = SELECTED_REAL_KIND (p=13, r=300)
        
        integer :: p = 0, ncell !particlenumber (row), cellnumber

        character(len=512) :: line
        
        integer :: i, n, j, l, matcol = 10, ios !total steps for docycle, matrix size data, iostatus
        
        real :: s, actualtime, tol
        
        real (kind=wp), dimension(:,:), allocatable :: F, V, pos, Fnext, m !attribute matrices
        
        real, dimension(:,:), allocatable :: inputmat ! input file
        

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
        !creating property-arrays for each particle
        allocate (F(p, 3))
        allocate (V(p, 3))
        allocate (pos(p, 3))
        allocate (m(p,1))
        allocate (Fnext(p, 3)) 


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

        ! initializing variables
        ! timer
        n = 6000
        s = 0.02


        !initializing properties  for particle p
        do i = 1, p
                m(i,:) = inputmat(i,1)
                F(i,1:3) = inputmat(i,8:10)
                pos(i,1:3) = inputmat(i,2:4)
                V(i,1:3) = inputmat(i,5:7)
        enddo
        ! velocity verlet algorithm

        do i = 1, n
        actualtime = i*s

                do l = 1, p
                
                do j = 1, 3 ! 3 x y z vectors
                        
                        !update positions
                        
                        pos(l,j) = pos(l,j)+s*V(l,j)+s**2*F(l,j)/(2*m(l,1))

                        !update forces
                        Fnext(l,j) = F(l,j)

                        !update velocities
                        V(l,j) = V(l,j)+s/(2*m(l,1))*(F(l,j)+Fnext(l,j))
                enddo

                !write (*,*) actualtime, "seconds"
                
                if (actualtime == 1 .OR. actualtime == n*s) then
                        write (*,*) "SpeedXYZ = ", V(l,1), "m/s, ", V(l,2), "m/s, ", V(l,3), "m/s"
                        write (*,*) "PosXYZ = ", pos(l,1), "m[x] | ", pos(l,2), "m[y] |", pos(l,3), "m[z]"
                        write (*,*) "time:", actualtime, "s"
                else
                endif    
        enddo
        enddo

        open(unit = 12, file = "iodebug.txt", status="replace", action="write")
        do i = 1, p
        write(12, *) inputmat(i,:)
        enddo
        close(12)

        open(unit = 13, file = "verletresults.txt", status ="replace", action="write")
        do i = 1, p
        write(13,*) m(i,1), pos(i,1:3), V(i,1:3)
        enddo
        close(13)
                
END PROGRAM verlet

