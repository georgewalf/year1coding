!----------------------------------------
!PHY1038 Assignment 2 - Series Computation
!URN 6457454
!February 27th 2017
!----------------------------------------

PROGRAM Maclaurin

IMPLICIT none

REAL :: function_of_x
DOUBLE PRECISION :: taylor_approx, nthderivative, x
INTEGER :: n, i, j, k

REAL, PARAMETER :: PI = 4*atan(1.0)

k=100                                                   !Number of Steps for DO loop
                                                      

WRITE(6,*) "This program will output a Taylor series"
WRITE(6,*) "up to a nth order defined by the user to"
WRITE(6,*) "a external data file."
WRITE(6,*) ""
WRITE(6,*) "Please enter the Taylor series order approximation:"
READ(5,*) n             
WRITE(6,*) ""
WRITE(6,*) "This program will evaluate for the function"
WRITE(6,*) "f(x) = sin(x)*(exp(-x/2)) between the range"
WRITE(6,*) "-ve 2pi to +ve 2pi and output to the file"
WRITE(6,*) "named: taylor.dat"            

OPEN (unit=20, file='taylor.dat')
DO i = -k , k                                            !defines number of datapoint on graph	
	x = (2.0*PI)*i/DBLE(k)                           !sets the range from -ve 2pi to +ve 2pi
	function_of_x = sin(x)*(exp(-x/2))	
	taylor_approx = sin(0.0)                         !f(x) at x==0.0, prevents 0**0 runtime error
                                 
	DO j = 1, n
	nthderivative = ((-sqrt(5.0)*0.5)**j)*sin(-j*atan(2.0))  !nth derivative of the function sin(x)*(exp(-x/2))
        taylor_approx = taylor_approx + (nthderivative*(x**j)/gamma(1.0+j*1.0) )!maclaurin series
	END DO                                                                     
	
	WRITE(20,*) x,function_of_x,taylor_approx

END DO

END PROGRAM Maclaurin
