	subroutine CHTOREAL(ch,real)
c	real function CHTOREAL(ch)
c To convert a character string (NCN characters in length) to a
c real number
c Modified for Lahey V5.01 11/24/92 06:05pm. With this compiler 'ch' is
c only nl characters in length, so get an error if ch(j:j) referred to
c with j=nl+1
	character*(*) CH
	logical before,expon,digchar
c	logical caplock,flag
c
	real=0.0
c
	before=.true.	!looking at dig BEFORE dec point
c	flag=.false.	!not at present accumulating a valid number
	expon=.false.
	expval=0.0		!for exponent
	nb=0
	na=0
	ne=0
	sign=1.0
	s1=1.0
	nl=nblank(ch)
c
c Note that there may be NO invalid characters in ch(1:nl) so go on
c to nl+1 which is interpreted as the end of the number -NOT for V5.01
c -cannot go beyond nl, but still need loop up to nl+1 so that number
c is set correctly, but must prevent reference to any byte above ch(nl:nl)
	do 11 j=1,nl+1
c	if(caplock()) print 70,ch(j:j),flag
c70	format(2x,a1,2x,l4)
c
c Current number has finished (or first one not yet started) if current
c char is not a valid character (0-9 + - . e or E), ie DIGCHAR=false OR
c if row has just ended (i.e. j=istcol and i>istrow) though in latter
c case the current character may be a valid one.
c
	if(j.le.nl) ival=ichar(ch(j:j))
	if(ival.eq.32.and.j.lt.nl) goto 11	!skip embedded spaces
	if((.not.digchar(ival)).or.j.eq.nl+1) then
c
c	   -when number just finished, fix its sign and exponent
c        (if BEFORE is still true then there was no decimal point
c		or exponent so must fix scaling here)
c		if(caplock()) print 71,real,before,nb,ne,expval,s1,sign
			if(before) real=real*10.**(nb-1)
			expval=expval*10.**(ne-1)
			real=(sign*real)*10.0**(s1*expval)
c		if(caplock()) print 71,real,before,nb,ne,expval,s1,sign
c71		format(' real,before,nb,ne,expval,s1,sign=',/,
c     &		2x,g13.6,2x,l4,2i8,g11.2,2f4.1)
		goto 99 	!FINISHED
	endif
c
	if(expon) goto 30
c
c -if current character is a sign (+,-):
	if(ival.eq.43) then
		sign=1.0
		goto 11		!skip +
	else if(ival.eq.45) then
		sign=-1.0
		goto 11			!skip to next char
	endif
c -if current character is a digit (0-9):
c (value before dec point is accum in form 2.3789 and later mult by 10**(nb-1)
	if(ival.ge.48.and.ival.le.57) then
		if (before) then
			real=real+float(ival-48)*10.**(-nb)
			nb=nb+1	!add 1 to NB=# of dig before dec point
		else
			na=na+1	!add 1 to NA=# of dig after dec point
			real=real+float(ival-48)*10.**(-na)
		endif
	endif
c -if current character is either (a) dec point(ival=46), (b)e or E then
c the digits before dec point are finished and can now be scaled correctly
c by multiplying digits before dec point by 10**(nb-1). Once scaled, BEFORE
c is set false, and this is used to prevent scaling again if e/E is
c encountered after a decimal point has been found and caused scaling
c of the pre-decimal point part of the number.
	if(ival.eq.46.or.ival.eq.69.or.ival.eq.101) then
c		if(caplock()) print 72,real,nb
		if(before) then
		  real=real*10.**(nb-1)
		  before=.false.
		endif
c		if(caplock()) print 72,real,nb
c72		format(' real= ',g13.6,' nb= ',i4)
		if(ival.eq.69.or.ival.eq.101) expon=.true.	!start of exponent
	endif
	goto 11
c
c Jump to here (30) for 1st character of exponent. Exponent can validly
c contain only +,- or digits
30	continue
	if(ival.eq.43) then
		s1=1.
		goto 11		!skip +
	else if(ival.eq.45) then
		s1=-1.0
		goto 11			!skip to next char
	endif
c -if current character is a digit (0-9):
c (value before dec point is accum in form 2.3789 and later mult by 10**(nb-1)
	if(ival.ge.48.and.ival.le.57) then
		expval=expval+float(ival-48)*10.**(-ne)
		ne=ne+1
c		if(caplock()) print 73,expval,ne
c73		format(' expval,ne= ',g13.6,i8)
	else
		goto 90		!error
	endif
	goto 11
c Errors
90	print 901
901	format(' Invalid character in exponent')
c
11	continue	!end of loop for each char
c
99	continue
c	CHTOREAL=real
	RETURN
	end

