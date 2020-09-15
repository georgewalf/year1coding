!----------------------------------------
!PHY1038 Assignment 5 - Linear Algebra
!URN 6457454
!May 12th 2017
!----------------------------------------

FUNCTION Determinant(Matrix) !this program finds the determinate of a 3x3 matrix
			IMPLICIT none
			REAL, DIMENSION (1:3,1:3) :: Matrix
			REAL :: Determinant

			Determinant =   (Matrix(1,1)*Matrix(2,2)*Matrix(3,3))&
					+(Matrix(1,2)*Matrix(2,3)*Matrix(3,1))&
					+(Matrix(1,3)*Matrix(2,1)*Matrix(3,2))&
					-(Matrix(1,3)*Matrix(2,2)*Matrix(3,1))&
					-(Matrix(1,2)*Matrix(2,1)*Matrix(3,3))&
					-(Matrix(1,1)*Matrix(2,3)*Matrix(3,2))
			!the value of this function is now the determinate of the matrix
		END FUNCTION

		FUNCTION Cofactor(Matrix) !this function returns a 3x3 cofactor matrix of a 3x3 matrix
			IMPLICIT none
			REAL, DIMENSION (1:3,1:3) :: Matrix, Cofactor

			Cofactor(1,1) = (Matrix(2,2)*Matrix(3,3))-(Matrix(2,3)*Matrix(3,2))
			Cofactor(1,2) = -((Matrix(2,1)*Matrix(3,3))-(Matrix(2,3)*Matrix(3,1)))
			Cofactor(1,3) = (Matrix(2,1)*Matrix(3,2))-(Matrix(2,2)*Matrix(3,1))
	
			Cofactor(2,1) = -((Matrix(1,2)*Matrix(3,3))-(Matrix(1,3)*Matrix(3,2)))
			Cofactor(2,2) = (Matrix(1,1)*Matrix(3,3))-(Matrix(1,3)*Matrix(3,1))
			Cofactor(2,3) = -((Matrix(1,1)*Matrix(3,2))-(Matrix(1,2)*Matrix(3,1)))

			Cofactor(3,1) = (Matrix(1,2)*Matrix(2,3))-(Matrix(1,3)*Matrix(2,2))
			Cofactor(3,2) = -((Matrix(1,1)*Matrix(2,3))-(Matrix(1,3)*Matrix(2,1)))
			Cofactor(3,3) =	(Matrix(1,1)*Matrix(2,2))-(Matrix(1,2)*Matrix(2,1))	

			Cofactor = TRANSPOSE(Cofactor)!we transpose so that the cofactor matrix is printed to the screen correctly

		END FUNCTION

		FUNCTION Inverse(Cofactor, Determinant)
			IMPLICIT none 
			REAL, DIMENSION (1:3,1:3) :: Inverse, Cofactor
			REAL :: Determinant

			Inverse = (1.0/Determinant)*(Cofactor) !formula given in assignment sheet

		END FUNCTION
		

!----------------------------------------
PROGRAM MatAlg

	IMPLICIT none
	REAL, DIMENSION (1:3,1:3) :: Matrix !this is the left side of the simultaneous equations
	REAL, DIMENSION (1:3) :: Solution, RightSide 
	
	INTERFACE 
		FUNCTION Determinant(Matrix)
			REAL, DIMENSION(1:3, 1:3) :: Matrix
			REAL :: Determinant
		END FUNCTION Determinant
		
		FUNCTION Cofactor(Matrix) 
			REAL, DIMENSION (1:3,1:3) :: Matrix, Cofactor	
		END FUNCTION Cofactor
		
		FUNCTION Inverse(Cofactor, Determinant)		
			REAL, DIMENSION (1:3,1:3) :: Inverse, Cofactor
			REAL :: Determinant
		END FUNCTION Inverse

	END INTERFACE !declares the functions to be used in the program
	
	RightSide = (/0,3,1/)
	
	OPEN(unit=10, file='matrix.dat') !opens the matrix from a datafile
	READ(10,*) Matrix
	Matrix = TRANSPOSE(Matrix)
	CLOSE(10)

	WRITE(6,*)"This program will solve the simultaneous equation:"
	WRITE(6,*)"2x+y−z=0"
	WRITE(6,*)"x+2y+z=3"
	WRITE(6,*)"−x+3y+2z=1"
	WRITE(6,*)""

	WRITE(6,*)"The Matrix for this is:"
	WRITE (6,'(3f8.4)') TRANSPOSE(Matrix)
	WRITE(6,*) ""

	WRITE(6,'(a36,f8.4)') "The determinant of this matrix is: ", Determinant(Matrix)
 	WRITE(6,*) ""

	WRITE(6,*) "The cofactor of this matrix is: "
 	WRITE(6,'(3f8.4)') Cofactor(Matrix)
 	WRITE(6,*) ""
	
	WRITE(6,*)"The inverse matrix is: "
	WRITE(6,'(3f8.4)') TRANSPOSE(Inverse(Cofactor(Matrix), Determinant(Matrix)))
	WRITE(6,*) ""	
	
	Solution = MATMUL(Inverse(Cofactor(Matrix), Determinant(Matrix)), RightSide)
	WRITE(6,'(a3,f8.4)') "x=", Solution(1)
	WRITE(6,'(a3,f8.4)') "y=", Solution(2)
	WRITE(6,'(a3,f8.4)') "z=", Solution(3)

END PROGRAM MatAlg

