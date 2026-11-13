PROGRAM arraysundefined
        implicit none

        REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: x
        REAL (KIND=wp), DIMENSION(:,:), ALLOCATABLE :: xmat
        !...

        ALLOCATE ( x (300) )
        ALLOCATE ( xmat (200,300) )

        x (i) = 5.0_wp ! i being an integer with value in the proper range
        xmat(i,j) = 3.2_wp

        DEALLOCATE (x, xmat)

        END PROGRAM arraysundefined

