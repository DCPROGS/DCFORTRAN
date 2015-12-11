	subroutine MODEC50(imod,QT,npar,kdim,pmax,ec50)
	use menu_f90
c To return Pmax and ec50 (real*8) for specific models, in HJCFIT given QT
c with parameters in it, called in QSET_HJC -NB must NOT have concentration
c in QT.
c Fixed for CH82 and Milone 97 models only
c with or without block
c NB VALUES RETURNED ARE ALWAYS THOSE WITHOUT BLOCK
C BUT IN CASE EXTRA AUERBACH STATE THE Pmax AND EC50 INCLUDE THIS STATE
C (WHICH REDUCES Pmax, BUT D/R STILL MONOTONIC)  CALCULATION FOR MOD 36, 37
c AND 40 CORRECTED 06/19/02 03:05pm)
c
c Modif again 06/20/02 07:04pm NB alpha(a) and beta(a) for extra state
c were defined opposite way round from Auerbach so EAB should be defined
c as alpha(a)/beta(a) -fixed 06/20/02 07:05pm
c
c imod=1,11 (for CH82 without, with block)
c imod=29,33 (for Milone 97 without, with block)
c
c  For use in HJCFIT, need to solve for rate constant that gives specified
c ec50, so need for bisection, a function routine that calculates ec50 as a
c function of the required rate constant.
c
c ERROR in calc for mod 29 etc corrected (see 'WRONG') 
c
c For now specify ec50 WITHOUT block (with block, have trouble
c with defining maximum!).  However, when the value EC50 without block is
c specified, it can still be used for constraint in HJCFIT even when model
c being fitted has got block -but numbering of the parameters will be different
c when fitted model has got block in it
c
c Models included, at present, are
c (1) CH82  (model 1 in DC's qmodel.dat)
c== (2) CH82 with block but no singly-liganded opening (model 10 in DC's qmodel.dat)
c== (3) CH82 with block of singly and doubly-liganded opening (model 11 in DC's qmodel.dat)
c (4) Milone et al 1997 (model 29 in DC's qmodel.dat)
c (5) Milone et al 1997 with block of all 3 open states (model 33 in DC's qmodel.dat)
c 06/14/01 11:28am added models 9, 10, 34-40 in old DC/CJH qmodel.dat
c
c Need to
c (a) Verify that Q matrix and theta are appropriate for the model
c	number specified
c (b) Extract each parameter (E, KA etc) from theta (or QD)
c (c) Define Popen(xA) and Pmax(xA->infinity) for the model
c (d) Get EC50 either from explicit expression if reasonably simple, or
c	by bisection otherwise
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
c
c	real*8 k22b,k12b,k22a,k12a,k21a,k11a,k21b
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim)
	real*8 theta(200)
	real*8 pmax
	character*11 cstring
	common/ir/irate(200),jrate(200)
c
	one=1.0d0
	two=2.0d0
	four=4.0d0
	half=one/two
c Get parameters in theta0() (all parameters, not just those to be estimated)
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   theta(m)=QT(i,j)
	enddo
c
	SELECT CASE(imod)

	CASE(1)
c CH82 without block (model 1 in DC qmodel.dat)
c 1     q( 1, 4)=   alpha(1)      10000.0
c 2     q( 4, 1)=   beta(1)       15.0000
c 3  *  q( 1, 2)=   k(+2)'       0.100000E+09
c 4   ô q( 2, 1)=   2k(-2)'      0.840000
c 5     q( 2, 3)=   alpha(2)      1400.00
c 6     q( 3, 2)=   beta(2)       50000.0
c 7     q( 3, 4)=   2k(-2)        20000.0
c 8  *  q( 4, 3)=   k(+2)        0.100000E+09
c 9     q( 4, 5)=   k(-1)         10000.0
c10  *  q( 5, 4)=   2k(+1)       0.200000E+09
	alpha1=theta(1)
	beta1=theta(2)
c	akass2op=theta(3)		!k(+2)'
c	a2kdis1=theta(4)		!2k(-2)'
	alpha2=theta(5)
	beta2=theta(6)
	a2kdis2=theta(7)		!2k(-2)
	akass2=theta(8)		!k(+2)
	akdis1=theta(9)		!k(-1)
	a2kass1=theta(10)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	E1=beta1/alpha1
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
c	   call BELL(1)
c	   print 1,disc
		call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(9)
c   C--C--C--O
c   4  3  2  1
	alpha2=theta(1)
	beta2=theta(2)
	a2kdis2=theta(3)		!2k(-2)
	akass2=theta(4)		!k(+2)
	akdis1=theta(5)		!k(-1)
	a2kass1=theta(6)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
c
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
	endif

	CASE(10)
c   C--C--C--O--B
c   For ec50 without block this is same as 9 except for numbering of parameters
	alpha2=theta(1)
	beta2=theta(2)
	a2kdis2=theta(5)		!2k(-2)
	akass2=theta(6)		!k(+2)
	akdis1=theta(7)		!k(-1)
	a2kass1=theta(8)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
c
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(11)
c CH82 with block (model 11 in DC qmodel.dat)
c 1     q( 1, 6)=   alpha1...     10000.0
c 2     q( 6, 1)=   beta1....     15.0000
c 3  *  q( 1, 2)=   k(+2)'...    0.100000E+09
c 4   ô q( 2, 1)=   2k(-2)'..    0.000000
c 5  *  q( 1, 4)=   k(+b)'...    0.300000E+08
c 6     q( 4, 1)=   k(-b)'...     50000.0
c 7     q( 2, 5)=   alpha2...     1500.00
c 8     q( 5, 2)=   beta2 ...     50000.0
c 9  *  q( 2, 3)=   k(+b)....    0.300000E+08
c 10     q( 3, 2)=   k(-b)....     50000.0
c 11     q( 5, 6)=   2k(-2)...     15000.0
c 12  *  q( 6, 5)=   k(+2)....    0.100000E+09
c 13     q( 6, 7)=   k(-1)....     7500.00
c 14  *  q( 7, 6)=   2k(+1)...    0.200000E+09
	alpha1=theta(1)
	beta1=theta(2)
c	akass2op=theta(3)		!k(+2)'
c	a2kdis1=theta(4)		!2k(-2)'
	alpha2=theta(7)
	beta2=theta(8)
	a2kdis2=theta(11)		!2k(-2)
	akass2=theta(12)		!k(+2)
	akdis1=theta(13)		!k(-1)
	a2kass1=theta(14)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	E1=beta1/alpha1
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif


	CASE(29)
c Milone 97 without block (model 29 in DC qmodel.dat)
c 1     q( 1, 4)=   alpha2...     1500.00
c 2     q( 4, 1)=   beta2....     50000.0
c 3     q( 2, 5)=   alpha1a..     13000.0
c 4     q( 5, 2)=   beta1a...     9.00000
c 5     q( 3, 6)=   alpha1b..     15000.0
c 6     q( 6, 3)=   beta1b...     7.00000
c 7     q( 4, 6)=   k(-2)b ..     6000.00
c 8  *  q( 6, 4)=   k(+2)b...    0.100000E+09
c 9     q( 4, 5)=   k(-2)a...     5000.00
c10  *  q( 5, 4)=   k(+2)a...    0.100000E+09
c11     q( 5, 7)=   k(-1)a...     6000.00
c12  *ô q( 7, 5)=   k(+1)a...    0.100000E+09
c13     q( 6, 7)=   k(-1)b...     5000.00
c14  *  q( 7, 6)=   k(+1)b...    0.100000E+09
	alpha2=theta(1)
	beta2=theta(2)
	alpha1a=theta(3)
	beta1a=theta(4)
	alpha1b=theta(5)
	beta1b=theta(6)
	ak22b=theta(7)	!k(-2)b
	ak12b=theta(8)	!k(+2)b
	ak22a=theta(9)	!k(-2)a
	ak12a=theta(10)	!k(+2)a
	ak21a=theta(11)	!k(-1)a
	ak11a=theta(12)	!k(+1)a
	ak21b=theta(13)	!k(-1)b
	ak11b=theta(14)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
c=	b1=two*E1a/aK1a + two*E1b/aK1b   !WRONG
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)		!NK this is K1a*K2b in notation used in paper!
c=	b2=two*(one+E1a)/aK1a + two*(one+E1b)/aK1b	!WRONG!
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif
c======== Debug
c	x=16.81*1.d-6
c	popen=(b1*x+a1*x*x)/(1+b2*x+a2*x*x)
c	prel=popen/pmax
c	x=ec50
c	popen=(b1*x+a1*x*x)/(1+b2*x+a2*x*x)
c	prel=popen/pmax

	CASE(33)
c Milone 97 with block (model 33 in DC qmodel.dat)
c 1     q( 1, 7)=   alpha2...     1500.00
c 2     q( 7, 1)=   beta2....     50000.0
c 3  *  q( 1, 4)=   k(+B)  ..    0.200000E+08
c 4     q( 4, 1)=   k(-B)  ..     80000.0
c 5     q( 2, 8)=   alpha1a..     7000.00
c 6     q( 8, 2)=   beta1a...     60.0000
c 7  *  q( 2, 5)=   k(+B)a ..    0.200000E+08
c 8     q( 5, 2)=   k(-B)a...     70000.0
c 9  *  q( 3, 6)=   k(+B)b...    0.200000E+08
c10     q( 6, 3)=   k(-B)b...     90000.0
c11     q( 3, 9)=   alpha1b..     30000.0
c12     q( 9, 3)=   beta1b...     1.00000
c13     q( 7, 9)=   k(-2)b...     12000.0
c14  *  q( 9, 7)=   k(+2)b...    0.100000E+09
c15     q( 7, 8)=   k(-2)a...     2400.00
c16  *  q( 8, 7)=   k(+2)a...    0.100000E+09
c17     q( 8,10)=   k(-1)a...     12000.0
c18  *ô q(10, 8)=   k(+1)a...    0.100000E+09
c19     q( 9,10)=   k(-1)b...     2400.00
c20  *  q(10, 9)=   k(+1)b ..    0.100000E+09

	alpha2=theta(1)
	beta2=theta(2)
c	ak1blck=theta(3)	!1=assoc
c	ak2blck=theta(4)	!2=dissoc
	alpha1a=theta(5)
	beta1a=theta(6)
c	ak1blcka=theta(7)	!1=assoc
c	ak2blcka=theta(8)	!2=dissoc
c	ak1blckb=theta(9)	!1=assoc
c	ak2blckb=theta(10)	!2=dissoc
	alpha1b=theta(11)
	beta1b=theta(12)
c	k1blckb=theta(13)	!1=assoc
	ak22b=theta(13)	!k(-2)b
	ak12b=theta(14)	!k(+2)b
	ak22a=theta(15)	!k(-2)a
	ak12a=theta(16)	!k(+2)a
	ak21a=theta(17)	!k(-1)a
	ak11a=theta(18)	!k(+1)a
	ak21b=theta(19)	!k(-1)b
	ak11b=theta(20)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c?	aK2b=ak22b/ak12b
c	aKblck=k2blck/k1blck
c	aKblcka=k2blcka/k1blcka
c	aKblckb=k2blckb/k1blckb
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)

	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
c	   call BELL(1)
c	   print 1,disc
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(34)
c As model 1 but with block of doubly-liganded only
c Same as 1 except for numbering of parameters
c   C6ÄÄC5ÄÄC4
c        ³   ³
c       O1ÄÄO2ÄÄC3
c 5  4  (calc by micro rev)
c 1      q( 1, 5)=   .alpha(1).
c 2      q( 5, 1)=   .beta(1)..
c 3   *1 q( 1, 2)=   .k(+2b)...
c 4      q( 2, 1)=   .2k(-2b)..
c 5      q( 2, 4)=   .alpha(2).
c 6      q( 4, 2)=   .beta(2)..
c 7   *1 q( 2, 3)=   .k(+B)....
c 8      q( 3, 2)=   .k(-B)....
c 9      q( 4, 5)=   .2k(-2a)..
c10  ô*1 q( 5, 4)=   .k(+2a)...
c11      q( 5, 6)=   .k(-1)....
c12   *1 q( 6, 5)=   .2k(+1)...
	alpha1=theta(1)
	beta1=theta(2)
c	akass2op=theta(3)		!k(+2)'
c	a2kdis1=theta(4)		!2k(-2)'
	alpha2=theta(5)
	beta2=theta(6)
	a2kdis2=theta(9)		!2k(-2)
	akass2=theta(10)		!k(+2)
	akdis1=theta(11)		!k(-1)
	a2kass1=theta(12)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	E1=beta1/alpha1
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(35)
c Milone 97 with block of doubly-liganded open only
c Same as 29 (Milone, no block) apart from numbering of params
c       O3
c        ³
c    ÚÄÄC7ÄÄÄ¿
c    ³       ³
c   C8      C5ÄÄO1ÄÄC4
c    ³       ³
c    ÀÄÄC6ÄÄÄÙ
c        ³
c       O2
c 1      q( 1, 5)=   .alpha(2).
c 2      q( 5, 1)=   .beta(2)..
c 3   *1 q( 1, 4)=   .k(+B)....
c 4      q( 4, 1)=   .k(-B)....
c 5      q( 2, 6)=   .alpha(1a)
c 6      q( 6, 2)=   .beta(1a).
c 7      q( 3, 7)=   .alpha(1b)
c 8      q( 7, 3)=   .beta(1b).
c 9      q( 5, 7)=   .k(-2b)...
c10  ô*1 q( 7, 5)=   .k(+2b)...
c11      q( 5, 6)=   .k(-2a)...
c12   *1 q( 6, 5)=   .k(+2a)...
c13      q( 6, 8)=   .k(-1a)...
c14   *1 q( 8, 6)=   .k(+1a)...
c15      q( 7, 8)=   .k(-1b)...
c16   *1 q( 8, 7)=   .k(+1b)...
	alpha2=theta(1)
	beta2=theta(2)
	alpha1a=theta(5)
	beta1a=theta(6)
	alpha1b=theta(7)
	beta1b=theta(8)
	ak22b=theta(9)	!k(-2)b
	ak12b=theta(10)	!k(+2)b
	ak22a=theta(11)	!k(-2)a
	ak12a=theta(12)	!k(+2)a
	ak21a=theta(13)	!k(-1)a
	ak11a=theta(14)	!k(+1)a
	ak21b=theta(15)	!k(-1)b
	ak11b=theta(16)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(36)
c Milone 97 + Auerbach '1 ms' fudge state (no block)
c Extra state is 1-4 as for block (case 35) but in this case the extra
c state should be included as part of the Popen (not excluded as in the
c case of block)
c       O3
c        ³
c    ÚÄÄC7ÄÄÄ¿
c    ³       ³
c   C8      C5ÄÄO1ÄÄC4
c    ³       ³
c    ÀÄÄC6ÄÄÄÙ
c        ³
c       O2
c 1      q( 1, 5)=   .alpha(2).
c 2      q( 5, 1)=   .beta(2)..
c 3      q( 1, 4)=   .alpha(A).
c 4      q( 4, 1)=   .beta(A) .
c 5      q( 2, 6)=   .alpha(1a)
c 6      q( 6, 2)=   .beta(1a).
c 7      q( 3, 7)=   .alpha(1b)
c 8      q( 7, 3)=   .beta(1b).
c 9      q( 5, 7)=   .k(-2b)...
c10  ô*1 q( 7, 5)=   .k(+2b)...
c11      q( 5, 6)=   .k(-2a)...
c12   *1 q( 6, 5)=   .k(+2a)...
c13      q( 6, 8)=   .k(-1a)...
c14   *1 q( 8, 6)=   .k(+1a)...
c15      q( 7, 8)=   .k(-1b)...
c16   *1 q( 8, 7)=   .k(+1b)...
	alpha2=theta(1)
	beta2=theta(2)
	alphaAB=theta(3)	!for extra state
	betaAB=theta(4)	!ditto
	alpha1a=theta(5)
	beta1a=theta(6)
	alpha1b=theta(7)
	beta1b=theta(8)
	ak22b=theta(9)	!k(-2)b
	ak12b=theta(10)	!k(+2)b
	ak22a=theta(11)	!k(-2)a
	ak12a=theta(12)	!k(+2)a
	ak21a=theta(13)	!k(-1)a
	ak11a=theta(14)	!k(+1)a
	ak21b=theta(15)	!k(-1)b
	ak11b=theta(16)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
c***	EAB=betaAB/alphaAB	!extra state
	EAB=alphaAB/betaAB	!extra state NB must define as q(1,4)/q(4,1)
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
c	a2=(one+E2)/(aK1a*aK2a)
c	a2=(one+E2+EAB)/(aK1a*aK2a)	!extra state appears in coeff of A^2
	a2=(one+E2+E2*EAB)/(aK1a*aK2a)	!extra state appears in coeff of A^2
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
c	pmax=E2/(one+E2)
	pmax=E2/(one+E2+E2*EAB)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	call BELL(1)
c	print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(37)
c Milone 97 + Auerbach '1 ms' fudge state + block of doubly-liganded only
c Calc same as fo (36) except for numbering of parameters
c       O3
c        ³
c    ÚÄÄC8ÄÄÄ¿   ÚÄÄC4
c    ³       ³   ³
c   C9      C6ÄÄO1
c    ³       ³   ³
c    ÀÄÄC7ÄÄÄÙ   ÀÄÄC5
c        ³
c       O2
c   1   *1 q( 1, 4)=   .k(+B)   .
c   2      q( 4, 1)=   .k(-B)  ..
c   3      q( 1, 6)=   .alpha(2).
c   4      q( 6, 1)=   .beta(2)..
c   5      q( 1, 5)=   .alpha(A).
c   6      q( 5, 1)=   .beta(A) .
c   7      q( 2, 7)=   .alpha(1a)
c   8      q( 7, 2)=   .beta(1a).
c   9      q( 3, 8)=   .alpha(1b)
c  10      q( 8, 3)=   .beta(1b).
c  11      q( 6, 8)=   .k(-2b)...
c  12  ô*1 q( 8, 6)=   .k(+2b)...
c  13      q( 6, 7)=   .k(-2a)...
c  14   *1 q( 7, 6)=   .k(+2a)...
c  15      q( 7, 9)=   .k(-1a)...
c  16   *1 q( 9, 7)=   .k(+1a)...
c  17      q( 8, 9)=   .k(-1b)...
c  18   *1 q( 9, 8)=   .k(+1b)...
	alpha2=theta(3)
	beta2=theta(4)
	alphaAB=theta(5)	!for extra state
	betaAB=theta(6)	!ditto
	alpha1a=theta(7)
	beta1a=theta(8)
	alpha1b=theta(9)
	beta1b=theta(10)
	ak22b=theta(11)	!k(-2)b
	ak12b=theta(12)	!k(+2)b
	ak22a=theta(13)	!k(-2)a
	ak12a=theta(14)	!k(+2)a
	ak21a=theta(15)	!k(-1)a
	ak11a=theta(16)	!k(+1)a
	ak21b=theta(17)	!k(-1)b
	ak11b=theta(18)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
c***	EAB=betaAB/alphaAB	!extra state
	EAB=alphaAB/betaAB	!extra state NB must define as q(1,4)/q(4,1)
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
c	a2=(one+E2)/(aK1a*aK2a)
	a2=(one+E2+E2*EAB)/(aK1a*aK2a)	!extra state appears in coeff of A^2
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
c	pmax=E2/(one+E2)
	pmax=E2/(one+E2+E2*EAB)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(38)
c  Milone 97 with dissoc from A2R -as 29 (does not affect equilib) but
c parameter numbering different
c       O3ÄÄÄÄÄÄÄ¿
c        ³       ³
c    ÚÄÄC6ÄÄÄ¿   ³
c    ³       ³   ³
c   C7      C4ÄÄO1
c    ³       ³   ³
c    ÀÄÄC5ÄÄÄÙ   ³
c        ³       ³
c       O2ÄÄÄÄÄÄÄÙ
c 1      q( 1, 3)=   .k'(-2b)..
c 2   *1 q( 3, 1)=   .k'(+2b)..
c 3      q( 1, 4)=   .alpha(2).
c 4      q( 4, 1)=   .beta(2)..
c 5      q( 1, 2)=   .k'(-2a)..
c 6  ô*1 q( 2, 1)=   .k'(+2a)..
c 7      q( 2, 5)=   .alpha(1a)
c 8      q( 5, 2)=   .beta(1a).
c 9      q( 3, 6)=   .alpha(1b)
c10      q( 6, 3)=   .beta(1b).
c11      q( 4, 6)=   .k(-2b)...
c12  ô*1 q( 6, 4)=   .k(+2b)...
c13      q( 4, 5)=   .k(-2a)...
c14   *1 q( 5, 4)=   .k(+2a)...
c15      q( 5, 7)=   .k(-1a)...
c16   *1 q( 7, 5)=   .k(+1a)...
c17      q( 6, 7)=   .k(-1b)...
c18   *1 q( 7, 6)=   .k(+1b)...
	alpha2=theta(3)
	beta2=theta(4)
	alpha1a=theta(7)
	beta1a=theta(8)
	alpha1b=theta(9)
	beta1b=theta(10)
	ak22b=theta(11)	!k(-2)b
	ak12b=theta(12)	!k(+2)b
	ak22a=theta(13)	!k(-2)a
	ak12a=theta(14)	!k(+2)a
	ak21a=theta(15)	!k(-1)a
	ak11a=theta(16)	!k(+1)a
	ak21b=theta(17)	!k(-1)b
	ak11b=theta(18)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif
c
	CASE(39)
c  Milone 97 with dissoc from A2R, plus block of A2R* only
c       O3ÄÄÄÄÄÄÄ¿
c        ³       ³
c    ÚÄÄC7ÄÄÄ¿   ³
c    ³       ³   ³
c   C8      C5ÄÄO1ÄÄC4
c    ³       ³   ³
c    ÀÄÄC6ÄÄÄÙ   ³
c        ³       ³
c       O2ÄÄÄÄÄÄÄÙ
c 1      q( 1, 3)=   .k'(-2b)..
c 2   *1 q( 3, 1)=   .k'(+2b)..
c 3      q( 1, 5)=   .alpha(2).
c 4      q( 5, 1)=   .beta(2)..
c 5   *1 q( 1, 4)=   .k(+B)....
c 6      q( 4, 1)=   .k(-B)....
c 7      q( 1, 2)=   .k'(-2a)..
c 8  ô*1 q( 2, 1)=   .k'(+2a)..
c 9      q( 2, 6)=   .alpha(1a)
c10      q( 6, 2)=   .beta(1a).
c11      q( 3, 7)=   .alpha(1b)
c12      q( 7, 3)=   .beta(1b).
c13      q( 5, 7)=   .k(-2b)...
c14  ô*1 q( 7, 5)=   .k(+2b)...
c15      q( 5, 6)=   .k(-2a)...
c16   *1 q( 6, 5)=   .k(+2a)...
c17      q( 6, 8)=   .k(-1a)...
c18   *1 q( 8, 6)=   .k(+1a)...
c19      q( 7, 8)=   .k(-1b)...
c20   *1 q( 8, 7)=   .k(+1b)...
	alpha2=theta(3)
	beta2=theta(4)
	alpha1a=theta(9)
	beta1a=theta(10)
	alpha1b=theta(11)
	beta1b=theta(12)
	ak22b=theta(13)	!k(-2)b
	ak12b=theta(14)	!k(+2)b
	ak22a=theta(15)	!k(-2)a
	ak12a=theta(16)	!k(+2)a
	ak21a=theta(17)	!k(-1)a
	ak11a=theta(18)	!k(+1)a
	ak21b=theta(19)	!k(-1)b
	ak11b=theta(20)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c	aK2b=ak22b/ak12b
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
	a2=(one+E2)/(aK1a*aK2a)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(40)
c  Milone 97 with dissoc from A2R, plus "1 ms" fudge state
c       O3ÄÄÄÄÄÄÄ¿
c        ³       ³
c    ÚÄÄC7ÄÄÄ¿   ³
c    ³       ³   ³
c   C8      C5ÄÄO1ÄÄC4
c    ³       ³   ³
c    ÀÄÄC6ÄÄÄÙ   ³
c        ³       ³
c       O2ÄÄÄÄÄÄÄÙ
c 1      q( 1, 3)=   .k'(-2b)..
c 2   *1 q( 3, 1)=   .k'(+2b)..
c 3      q( 1, 5)=   .alpha(2).
c 4      q( 5, 1)=   .beta(2)..
c 5      q( 1, 4)=   .alpha(A).
c 6      q( 4, 1)=   .beta(A)..
c 7      q( 1, 2)=   .k'(-2a)..
c 8  ô*1 q( 2, 1)=   .k'(+2a)..
c 9      q( 2, 6)=   .alpha(1a)
c10      q( 6, 2)=   .beta(1a).
c11      q( 3, 7)=   .alpha(1b)
c12      q( 7, 3)=   .beta(1b).
c13      q( 5, 7)=   .k(-2b)...
c14  ô*1 q( 7, 5)=   .k(+2b)...
c15      q( 5, 6)=   .k(-2a)...
c16   *1 q( 6, 5)=   .k(+2a)...
c17      q( 6, 8)=   .k(-1a)...
c18   *1 q( 8, 6)=   .k(+1a)...
c19      q( 7, 8)=   .k(-1b)...
c20   *1 q( 8, 7)=   .k(+1b)...
	alpha2=theta(3)
	beta2=theta(4)
	alphaAB=theta(5)	!for extra state
	betaAB=theta(6)	!ditto
	alpha1a=theta(9)
	beta1a=theta(10)
	alpha1b=theta(11)
	beta1b=theta(12)
	ak22b=theta(13)	!k(-2)b
	ak12b=theta(14)	!k(+2)b
	ak22a=theta(15)	!k(-2)a
	ak12a=theta(16)	!k(+2)a
	ak21a=theta(17)	!k(-1)a
	ak11a=theta(18)	!k(+1)a
	ak21b=theta(19)	!k(-1)b
	ak11b=theta(20)	!k(+1)b
c    Define eq constants
	E2=beta2/alpha2
c***	EAB=betaAB/alphaAB	!extra state
	EAB=alphaAB/betaAB	!extra state NB must define as q(1,4)/q(4,1)
	E1a=beta1a/alpha1a
	E1b=beta1b/alpha1b
	aK1a=ak21a/ak11a
	aK1b=ak21b/ak11b
	aK2a=ak22a/ak12a
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=E1a/aK1a + E1b/aK1b
	a1=E2/(aK1a*aK2a)
	b2=(one+E1a)/aK1a + (one+E1b)/aK1b
c	a2=(one+E2)/(aK1a*aK2a)
c	a2=(one+E2+EAB)/(aK1a*aK2a)	!extra state appears in coeff of A^2
	a2=(one+E2+E2*EAB)/(aK1a*aK2a)	!extra state appears in coeff of A^2
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
c	pmax=E2/(one+E2)
	pmax=E2/(one+E2+E2*EAB)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
c	call BELL(1)
c	print 1,disc
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	CASE(41)
c CH82 without block (model 41 in DC qmodel.dat, like mod 1 but open states not connected)
c 1     q( 1, 3)=   alpha2
c 2     q( 3, 1)=   beta2
c 3     q( 2, 4)=   alpha1
c 4     q( 4, 2)=   beta1
c 5     q( 3, 4)=   2k(-2)
c 6  *  q( 4, 3)=   k(+2)
c 7     q( 4, 5)=   k(-1)
c 8  *  q( 5, 4)=   2k(+1)
	alpha1=theta(3)
	beta1=theta(4)
	alpha2=theta(1)
	beta2=theta(2)
	a2kdis2=theta(5)		!2k(-2)
	akass2=theta(6)		!k(+2)
	akdis1=theta(7)		!k(-1)
	a2kass1=theta(8)		!2k(+1)
c    Define eq constants
	E2=beta2/alpha2
	E1=beta1/alpha1
	aK1=two*akdis1/a2kass1
	aK2=half*a2kdis2/akass2
c Get coefficients for quadratic as in ec50.mcd
c First define coeffs of the form Popen=(b1*A+a1*A^2)/(1+b2*A+a2*A^2)
	b1=two*E1/aK1
	a1=E2/(aK1*aK2)
	b2=two*(one+E1)/aK1
	a2=(one+E2)/(aK1*aK2)
c then define quadratic coefficients in a*x^2 +  b*c + c = 0
	pmax=E2/(one+E2)
	a=two*a1 - pmax*a2
	b=two*b1 - pmax*b2
	c=-pmax
	disc=b*b -four*a*c
	if(disc.ge.0) then
	   ec50=(-b+dsqrt(disc))/(two*a)
	else
	call realtoch(disc,cstring,11)
		imes=gmdisplaymessagebox('',' ERROR IN MODEC50: disc = '//
     &	cstring,gstop,gok)
c	   call BELL(1)
c	   print 1,disc
c1	   format(' ERROR IN MODEC50: disc = ',g13.6)
	endif

	END SELECT
c
	RETURN
	end

