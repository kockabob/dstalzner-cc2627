PROGRAM arrays
        implicit none
        
        INTEGER, DIMENSION(3) :: vec !vector 3 components
        INTEGER, DIMENSION(3,3) :: mat !matrix 3 rows, 3 columns

        
        !array construction
        
        
        vec = (/1, 2, 3 /)
        mat(1,:) = (/1, 2, 3 /) ! ":" is used to refer to all the components in the series
        mat(2,:) = (/4, 5, 6 /)
        mat(3,:) = (/7, 8, 9 /)
        !print all elements of array
        PRINT *, vec
        PRINT *, "---"

        !array slicing
        
        PRINT *, mat(3,2:3)
        PRINT *, "---"

        !array multiplication with scalar
        
        vec = vec*2

        !element-wise operation without explicit iteration (loop) constructs using ":"
        mat(:,1) = mat(:,1) + vec(:)
        PRINT *, mat(1,:)
        PRINT *, mat(2,:)
        PRINT *, mat(3,:)

        END PROGRAM arrays
