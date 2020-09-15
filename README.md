# year1coding
This repository contains coursework completed in my first year at university. This was my first exposure to programming. 

Each program here is written in Fortran 90. This was the language taught by the Physics department at University of Surrey in 2017. Professors gave various justifications for using this ancient language over something more useful to students, I will not go into them.

Please note that these programs are presented as submitted in 2017. They all compile but they are certainly not perfect, by any means. The purpose of these repository is to track my journey through the coding world, and these were my first steps.



The program titled Newton-Raphson uses the Newton-Raphson method to find roots in the case of a cubic function. 

Assignment brief:
This assignment is to write a program to find the roots in the case of a cubic function, f(x) = ax3 + bx2 + cx + d, using the Newton-Raphson method. Your programshould ask for, and read in, values for the coefficients a, b, c and d. It should also read in an initial guess for the root x0, and then repeatedly iterate through the steps 1-3 above until a value for f(x) sufficiently close to zero (say better than 10-6) has been reached. A cubic equation has 3 roots, 1 or 3 of which are real numbers. It is not necessary to write your program to deliver all 3 roots at once. It can be run several times with different initial guesses to find the three roots.

You should format your output in such a way that it is concise and informative. It should include at least the initial guess, the value of the root, the tolerance used, and the number of iterations it took to reach the final answer.

As a check, you should find that when a=1, b=-5, c=-1, d=5, then the roots are -1, 1 and 5, as is easily checked.

