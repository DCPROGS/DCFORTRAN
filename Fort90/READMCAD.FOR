	subroutine READMCAD(datfil,len,nsamp,Yval,ndv1,ndimd)
c
c To read MathCad data from MCDAT.DAT file.
c Lahey V5 version of READMC.  12/09/94 05:30pm.  Assumes that length, LEN,
c of data file (in DATFIL) has been determined before call, and that Yval
c has been allocated as Yval(ndv1,nimd) before calling.
c By inspection, seems that there are 18 characters per number if neg,
c and 17 if not. Could read as ascii arrays, but prob safer to read
c a byte at a time
	real*4 YVAL(ndv1,ndimd)
	character datfil*33
	ALLOCATABLE bytes
	character*1 bytes(:)
	character*18 ascnum
	logical discprt,digchar1
	common/dp/discprt
c
c Read in one byte at a time
	ALLOCATE(bytes(len))
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     &    access='TRANSPARENT')
	do i=1,len
	   read(20,40) bytes(i)
40	   format(a1)
	enddo
	CLOSE(unit=20)
c
c Go through each byte checking for end of a number string
	j=0		!counts # of characters (bytes) in each number
	n=0		!counts number of reals found
	ascnum='                  '
	do 6 i=1,len
	   ival=ichar(bytes(i))
	   if(DIGCHAR1(ival)) then
		j=j+1
		ascnum(j:j)=bytes(i)	!put character into number string
	   else
		if(j.ge.1) then		!get real from ascnum() just found
		   n=n+1
		   call CHTOREAL(ascnum,Yval(n,1))
		endif
	  	ascnum='                  '
		j=0			!ready for next number
	   endif
6	continue
	nsamp=n
c
      print 4,datfil,nsamp
c      if(pon()) write(7,4) datfil,nsamp
      if(discprt) write(8,4) datfil,nsamp
4	format(
     &' MathCad data from ',a33,': nsamp = ',i6)
c
	RETURN
	end
