PROGRAM verlet
        implicit none
        
        ! variable declaration
        
        integer, parameter :: wp = SELECTED_REAL_KIND (p=13, r=300)
        
        integer :: p !particlenumber, cellnumber

        integer :: i, nk, j, l !total steps (nk) for docycle, matrix size data, iostatus
        
        real (kind=wp) :: tau, actualtime, sigma, epsilon !step length (s), + actual time
        
        real (kind=wp), dimension(:,:), allocatable :: F, V, x, Fnext, m ! attribute matrices
        

        !txt into array
        OPEN(UNIT = 11,  FILE="input.txt", STATUS="old", ACTION="read")

                !allocating values into matrix
                read(11,*) nk, tau
                read(11,*) sigma, epsilon
                read(11,*) p

                !creating property-arrays where row number is particle number
                allocate (F(p, 3))
                allocate (V(p, 3))
                allocate (x(p, 3))
                allocate (m(p,1))
                allocate (Fnext(p, 3)) 

                !initializing properties  for particle p

                do i = 1, p
                        read(11, *) m(i,:), x(i,1:3), V(i,1:3)              
                        F(i,1:3) = (/0.0, 0.1, 0.0/)
                enddo

        CLOSE(11)

        !checking input-output process
        OPEN(UNIT=12, FILE="iodebug.txt", STATUS="replace", ACTION="write")
               
                write(12,*) nk, tau
                write(12,*) p
               
        
                do i = 1, p
                        write(12,*) m(i,:), x(i,1:3), V(i,1:3)
                enddo


        CLOSE(12)

        print *, "Successfully created particledata-matrix. Particle number=", p

        ! initializing variables


        ! velocity verlet algorithm

        do i = 1, nk
                actualtime = i*tau

                do l = 1, p
                
                        do j = 1, 3 ! 3 x y z vectors
                        
                                !update positions
                        
                                 x(l,j) = x(l,j)+tau*V(l,j)+tau**2*F(l,j)/(2*m(l,1))

                                !update forces
                                Fnext(l,j) = F(l,j)

                                !update velocities
                                V(l,j) = V(l,j)+tau/(2*m(l,1))*(F(l,j)+Fnext(l,j))
                         enddo

                enddo
        enddo

        !calculation results
        open(unit = 13, file = "verletresults.txt", status ="replace", action="write")

                do i = 1, p

                        write(13,'(*(G12.4))') m(i,1), x(i,1:3), V(i,1:3)

                enddo

        close(13)
                
END PROGRAM verlet

