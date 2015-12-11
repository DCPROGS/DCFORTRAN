	subroutine QSWAP(QD,IQ,k,inew,irate,jrate,npar,ndim)
C SWAPS ROWS AND COLS OF QD and IQ ACCORDING TO INEW
c This version is like MSWAP but definition of inew is reversed (as it is
c in SCJUMP and NEWMOD) compared with original definition used
c in MSWAP and HJCFIT.
c	do i=1,k
c	   print 106,i,inew(i)
c106	   format('&Old state #',i2,' is new state # [',i2,'] = ') !QSWAP VERSION
c	   call INPUTi(inew(i))
c	enddo
c
	REAL*8 QD(ndim,ndim),Q1(100,100)
	INTEGER INEW(ndim),IQ(ndim,ndim),IQ1(100,100)
	integer irate(200),jrate(200)
C
	do i=1,k
	   do j=1,k
		i1=inew(i)
		j1=inew(j)
		Q1(i1,j1)=QD(i,j)
		IQ1(i1,j1)=IQ(i,j)
	   enddo
	enddo
C COPY RESULT BACK TO QD
	do i=1,k
	   do j=1,k
		QD(i,j)=Q1(i,j)
		IQ(i,j)=IQ1(i,j)
	   enddo
	enddo
c Redefine irate, jrate from new IQ()
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)
	   irate(m)=i
	   jrate(m)=j
	enddo
c
	RETURN
	END


