	subroutine DBLETOCH(real8,ch,n)
c Same as realtoch.for but argument is double precision
c NEW VERSION BASED ON LAHEY INTERNAL WRITE (see also DCFORMAT, EFORMAT)
c n=declared length of CH in calling prog.
c Call with ROUND or ROUND1 of x to limit the number of sig figs
c Converts a real number,x, to a character string.
c Function version (REALTOCF) gives problems.
c (a) Numbers up to 99999.99.. in fixed format, then as +1.1234e+51
c (b) numbers down to +0.00100001 in fixed format, then as +1.1234e-51
c
c Modif 02/16/95 11:05am so fixed format does allocates # of figs
c  to right of decimal point that keeps result within single precision
c Modif 12/18/91 08:10pm so that input value of x not altered by call.
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	character ch*(*)
	character cfield*11,cright*11
	character fmt*8
	logical expform
	real*8 real8
c
	x=real8
c
	if((x.gt.-1.d-307).and.(x.lt.1.d-307)) then
	   ch='0.0'
	   RETURN
	endif
c Problem to discover declared length of CH in calling prog.  If CH is
c initialised to char(32) before calling the LEN(ch)=0
c	n=LEN(ch)
c	do i=1,n
c         ch(i:i)=char(32)
c	enddo
c Get no of digits before decimal point (nb)
	x1=dabs(x)
	nb=1 + dint(dlog10(x1))		!e.g.123.4 gives nb=3
	if(x1.lt.1.0) nb=1
c Check size to decide if E notation to be used
	expform=(x1.lt.0.001d0.or.x1.ge.100000.0d0)	!E format
c
	if(expform) then
	   if(n.gt.6) then	!round so number of sig figs fits into CH
		n1=n-6
	   else
		n1=1
	   endif
	   x=DROUND(x,n1)
	   ifield=n		!fill whole string
	   iright=n-7
	   if(iright.lt.0) iright=0
	   if(iright.gt.6) iright=6	!single precision!
	   call INTCONV(ifield,cfield)
	   call INTCONV(iright,cright)
	   nf=len_trim(cfield)
	   nr=len_trim(cright)
	   fmt='(D'//cfield(1:nf)//'.'//cright(1:nr)//')'
	else
	   if(n.gt.2) then	!round so number of sig figs fits into CH
		n1=n-2
	      if(n1.gt.6) n1=6	!single precision!
	   else
		n1=1
	   endif
	   x=DROUND(x,n1)
	   ifield=n		!fill whole string
	   ileft=nb
c For single prec in fixed format,iright+ileft should be not
c more than 7 (or 6?), so now calc thus:
c	   iright=n-ileft-2
	   iright=7-ileft
	   if(iright.lt.0) iright=0
	   if(iright.gt.6) iright=6	!single precision!
	   call INTCONV(ifield,cfield)
	   call INTCONV(iright,cright)
	   nf=len_trim(cfield)
	   nr=len_trim(cright)
	   fmt='(D'//cfield(1:nf)//'.'//cright(1:nr)//')'
	endif
c
	WRITE(ch,fmt) x
c
c If fixed format, remove trailing zeros from string -leave ONE trailing zero
	n2=len_trim(ch)
	if(n2.gt.1) then
	 do i=n2,1,-1
	  if(ch(i:i).eq.'0'.and.ch(i-1:i-1).eq.'0') then
	     ch(i:i)=char(32)
	  else
	     goto 1		!jump out when non-zero value found
	  endif
	 enddo
	endif
1	continue
c Also remove leading blanks (place for sign is blank for pos numbers,
c and get several blanks e.g. for x=3.4 and fmt=F11.5)
	call LBLANK(ch,n2)
c
	RETURN
	end


