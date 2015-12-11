	subroutine READPC(ndev,nsamp,irate,Yval1,ierr)
c To read MathCad data from MCDAT.DAT file. By inspection, seems that there
c are 18 characters per number if neg, 17 if not. Could read as ascii arrays,
c but prob safer to read a byte at a time
c	real*4 YVAL(2048,15)
	real*4 Yval1(30720)		!equivalenced
c	real*4 YVAL(2048,15),Yval1(30720)		!equivalenced
c	real*4 YVAL(200000)                !for Mark
	character ndev*2,datfil*15
	character*1 bytes(200000)
	character*18 ascnum
	logical discprt,present,digchar1
	common/dp/discprt
c
c	EQUIVALENCE (YVAL,Yval1)
c
	ierr=0		!O.K.
	print 1
1	format(' Sample rate (integer Hz) = ')
	read 2,irate
2	format(i8)
c	print 3
c3	format('&Number of points = ')
c	read 2,nsamp
	if(ndev.eq.'A:') pause ' Now mount data floppy:'
	datfil=ndev//'\MCDAT.DAT'
	INQUIRE(file=DATFIL,exist=present,flen=len)
	if(.not.present.or.len.le.0) then
	   call BELL(2)
	   ierr=1		!file not present
	   RETURN
	endif
	if(len.gt.200000) then
	   call BELL(2)
	   print*,' Too many bytes!'
	   stop
	endif
c	print*,'len is ',len
c	pause
c Read in one byte at a time
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     & access='TRANSPARENT')
	do 5 i=1,len
	  read(20,40) bytes(i)
40	  format(a1)
5	continue
	CLOSE(unit=20)
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
c		   call CHTOREAL(ascnum,Yval(n,1))
		   call CHTOREAL(ascnum,Yval1(n))      !for Mark
		endif
	  	ascnum='                  '
		j=0			!ready for next number
	   endif
6	continue
	nsamp=n
c
      print 4,ndev,nsamp,irate
c      if(pon()) write(7,4) ndev,nsamp,irate
      if(discprt) write(8,4) ndev,nsamp,irate
4	format(
     &' pClamp data from disc ',a2,': nsamp = ',i6,' rate = ',i8,' Hz')
c
	RETURN
	end
