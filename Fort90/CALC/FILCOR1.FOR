	program FILCOR1
c
c 24.9.93
c This is a program to calculate the time course of the conductance change
c at a synaptic site (eg) by correcting for the filtering that occurs during
c whole-cell recording.  The filter corrected conductance [g(t)] can be
c calculated in terms of the measured current [i(t)] and the first
c derivative of this function given assumptions about the uniformity of
c voltage clamp and a model for the cell consisting of a single compartment.
c To produce a smooth trace from which to calculate the derivative of the
c current, a polynomial is fitted to the original data using a least
c squares method.
c
c
	real*4 Xnum(20480)
	real*4 Xnum1(2,10240)
	equivalence (Xnum,Xnum1)
c
	real*4 Xpoly(2400,50)	!for polynomial fit
	real*8 bval(50)
	real*4 cval(2400)
      character*1 ans,UC
	logical read
c
4	format(i8)
101   format(a1)
c
c Declared dimensions -GENREG needs them!
	nd=2400
	ndp=50
c
	read=.true.
	ndim=20480
c
	ioff=0
c	print 50
c50	format(' Input byte number of first value to convert [')
c	read 55,ibg
c	if(ibg.gt.0) goto 58		!read data
	ilinhead=5	!5 lines of header before data
c58	continue
	call ASCIODC(read,ioff,ilinhead,ncols,nrows,ntot,Xnum,ndim)
c
c	do i=1,100
cc	 print*,'Xnum1(1,i)= ',Xnum1(1,i),' Xnum1(2,i)= ',Xnum1(2,i)
c	 print*,'Xnum(i)= ',Xnum(i)
c	 pause
c	enddo
c	print*,'Nrows = ',nrows
c	pause
c
c Now generate arrays for polynomial fit
34	continue
	print 36
36	format(' Enter number of terms in polynomial  ')
	read 4,n
	if((n.gt.0).and.(n.le.100)) then
	  np=n
	else
	  call BELL(1)
	  print 42
42	  format(' Number of terms must be between 1 and 100')
	  goto 34
	endif
c
c Generate Xpoly here
	m=0
	do j=1,np
	  do i=1,nrows
c	  do i=1,3 		! 3 for testing
	    if(m.eq.0) then
		Xpoly(i,j)=1.0
	    else
	      Xpoly(i,j)=(Xnum1(1,i))**m
	    endif
c	    print*,' Xpoly(i,j)= ',Xpoly(i,j)
c	    pause
	  enddo
	  m=m+1
	enddo
c
	do i=1,nrows
	  cval(i)=Xnum1(2,i)
c	  print*,'cval = ',cval(i)
c	  pause
	enddo
c
	call GENREG2(bval,Xpoly,cval,np,nrows,ndp,nd)
c	call GENREG3(bval,Xpoly,cval,np,nrows)    !no good!
c
	Do i=1,np
	  print*,'bval(i) = ',bval(i)
	  pause
	enddo
c
999	continue
	end





