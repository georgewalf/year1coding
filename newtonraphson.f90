!-------------------------------------------------
!PHY1038 Assignment 1 - Newton-Raphson root finder
!URN 6457454
!February 2017
!-------------------------------------------------

program newtonraphson

Implicit none

real :: x0, x1, a, b, c, d, f, f1, tol, diff
integer :: n

tol=1E-6
n=0

write(6,*) 'This program will find the Newton-Raphson roots'
write(6,*) 'of a cubic function. Enter the coefficients a,b,c,d'
read(5,*) a,b,c,d
write(6,*) 'Enter your initial guess for x'
read(5,*) x0

do
	f1=((3*a*x0+2*b)*x0+c)      !Gradient of cubic function.
	f=(((a*x0+b)*x0+c)*x0+d)    !Cubic function.
	x1=x0-(f/f1)                !X-intercept function.
	diff=abs(x1-x0)
	n=n+1 
	if (diff<=tol) then         !If difference between x0 and x1 is less than 
		exit                !tolerance then the root has been found to 
		else                !sufficient accuracy so we can exit the do-loop.
		x0=x1               !Use x1 as new approximation to the root.
		cycle
	end if
end do

write(6,*) 'The Newton-Raphson root is'
write(6,'(f4.1)')  x1
write(6,*) 'Number of iterations:'	
write(6,*) n
write(6,*) 'Tolerance used:'
write(6,*) '10^6'
write(6,*) 'Run the program again with a differen'
write(6,*) 'intial guess to find other roots.'
end program newtonraphson






