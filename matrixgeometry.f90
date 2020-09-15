FUNCTION comrot(vertex, angle, xdisp, ydisp) !this function will rotate a triangle about it's centre of mass
IMPLICIT NONE

REAL, DIMENSION(1:3) :: vertex, translated, rotated, comrot
REAL, DIMENSION(1:3, 1:3) :: transmtrx, rotmtrx

REAL :: xdisp, ydisp, angle


transmtrx(1,1) = 1.0 ;  transmtrx(1,2) = 0.0 ;  transmtrx(1,3) = xdisp
transmtrx(2,1) = 0.0 ;  transmtrx(2,2) = 1.0 ;  transmtrx(2,3) = ydisp
transmtrx(3,1) = 0.0 ;  transmtrx(3,2) = 0.0 ;  transmtrx(3,3) = 1.0
					     !this matrix translate a vector with x and y displacment xdisp and ydisp
rotmtrx(1,1) = COS(angle) ;  rotmtrx(1,2) = -SIN(angle) ;  rotmtrx(1,3) = 0.0
rotmtrx(2,1) = SIN(angle) ;  rotmtrx(2,2) = COS(angle)  ;  rotmtrx(2,3) = 0.0
rotmtrx(3,1) = 0.0        ;  rotmtrx(3,2) = 0.0         ;  rotmtrx(3,3) = 1.0
					     !this inputs the angle given by the user into the 3x3 rotation matrix

translated = MATMUL(transmtrx, vertex)	     !translates the vector such that COM is at (0,0) in coordiante system

rotated = MATMUL(rotmtrx, translated)	     !multiplies the matrices together to apply the rotation to the point vector

transmtrx(1,3) = -xdisp 
transmtrx(2,3) = -ydisp			    !this reverses the translation so the COM is back to where it was originally

comrot = MATMUL(transmtrx, rotated)  	    !as stated above

END FUNCTION comrot

PROGRAM matrixgeo
IMPLICIT NONE

REAL, DIMENSION(1:3) :: va, vb, vc, vx, vy, vz  !v(a,b,c) are the 3 vertices of the triangle, v(x,y,z) are the rotated vertices
REAL :: angle, xdisp, ydisp, pi

INTERFACE 
	FUNCTION comrot(vertex, angle, xdisp, ydisp)
	REAL, DIMENSION(1:3) :: vertex, comrot	
  	REAL :: xdisp, ydisp, angle
	END FUNCTION comrot
END INTERFACE				       !this tells the program that a function with these arguments will be used

pi = 4*ATAN(1.0)			       !gives accurate value for pi

WRITE(6,*) "This program will rotate a triangle	about it's"
WRITE(6,*) "centre of mass by an angle defined by the user."
WRITE(6,*) " "
WRITE(6,*) " "
WRITE(6,*) "Enter the coordinates of the a vertex of the"
WRITE(6,*) "triangle in x y format:"
READ(5,*) va(1), va(2)
WRITE(6,*) "another one:"
READ(5,*) vb(1), vb(2)
WRITE(6,*) "another one:"
READ(5,*) vc(1), vc(2)

va(3)=1
vb(3)=1
vc(3)=1                              !this gives a fixed z value for the vertices
WRITE(6,*) "Enter the angle the triangle is to be rotated by:"
READ(5,*) angle

angle = (angle*pi)/180               !degrees to radians conversion 
xdisp = -((va(1)+vb(1)+vc(1))/3)         
ydisp = -((va(2)+vb(2)+vc(2))/3)
!the triangle must then be translated so that the COM is at the centre of the coordinate 
!system ie translating each point by the negative of the COM vector

OPEN(10,file='6457454.dat')	    !opens file from which gnuplot will plot from

vx = comrot(va, angle, xdisp, ydisp)
vy = comrot(vb, angle, xdisp, ydisp)
vz = comrot(vc, angle, xdisp, ydisp)
!applies to COM rotation to each point of the triangle
WRITE(10,*) va(1), va(2), vx(1), vx(2)
WRITE(10,*) vb(1), vb(2), vy(1), vy(2)
WRITE(10,*) vc(1), vc(2), vz(1), vz(2)
WRITE(10,*) va(1), va(2), vx(1), vx(2)

WRITE(6,*) "The triangle's vertice's new positions are:"
WRITE(6,*) "(", vx(1), ",", vx(2), ")", "(", vy(1), ",", vy(2), ")", "(", vz(1), ",", vz(2), ")"

END PROGRAM matrixgeo








