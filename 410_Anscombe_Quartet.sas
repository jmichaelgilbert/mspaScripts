**********************************************************************;
*	410-DL | Examining Anscombe's Quartet;
*	Last updated: 2015-09-15 by MJG;
**********************************************************************;

*	Create Anscombe's Quartet;
data Anscombe_Quartet;
	input Y1 X1 Y2 X2 Y3 X3 Y4 X4;
datalines;
8.04 10 9.14 10 7.46 10 6.58 8
6.95 8 8.14 8 6.77 8 5.76 8
7.58 13 8.74 13 12.74 13 7.71 8
8.81 9 8.77 9 7.11 9 8.84 8
8.33 11 9.26 11 7.81 11 8.47 8
9.96 14 8.1 14 8.84 14 7.04 8
7.24 6 6.13 6 6.08 6 5.25 8
4.26 4 3.1 4 5.39 4 12.5 19
10.84 12 9.13 12 8.15 12 5.56 8
4.82 7 7.26 7 6.42 7 7.91 8
5.68 5 4.74 5 5.73 5 6.89 8
;
run;

*	Visually check the results;
proc print data = Anscombe_Quartet;
run; quit;

*	Use PROC MEANS to examine similarities between fields;
proc means data = Anscombe_Quartet
	N mean std var stderr;
run; quit;

*	Now regress the pairings for comparison;
proc reg data = Anscombe_Quartet;
	model Y1 = X1;
	model Y2 = X2;
	model Y3 = X3;
	model Y4 = X4;
run; quit;
