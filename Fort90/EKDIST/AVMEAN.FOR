	program AVMEAN
c To read condtional means from ASCII files output from
c EKDIST (cdist1,...) and average over experiments
c for input to CVFIT.  Need two forms of output file, one with
c x=lag, and one with x=mean of values in each range (as for 2 sorts
c of graph in CDIST1).
	character ascfil*33,text*70,cnum*11
	allocatable::Xnum,sx,sy,sw
	real*4 Xnum(:,:),sx(:,:),sy(:,:),sw(:,:)
	ALLOCATABLE colhead
	character*20 colhead(:)		!for ASCREAD1
	logical dcfile
c
	logical discprt
	logical pon,slock,debug,caplock
	character*11 cdate,ctime
	common/dp/discprt
	character mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='AVMEAN.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1       FORMAT(' AVMEAN: To average conditional means',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')

c
	n1=500	!max number of lags
	n2=20		!max number of ranges
	ALLOCATE(sx(n1,n2),sy(n1,n2),sw(n1,n2))
	do i=1,n1
	   do j=1,n2
		sx(i,j)=0.0
		sy(i,j)=0.0
		sw(i,j)=0.0
	   enddo
	enddo
	ncols=100		!for dimension of colhead in first call to ascread1
	ALLOCATE(colhead(ncols))
c
11	print 12
12	format(
     &' Number of conditional mean files to be pooled = ')
	call INPUTi(nfile)
	if(nfile.lt.1) goto 11
	do ifile=1,nfile
	   ioff=-1
	   ilinhead=-1
c       Get filename, number of rows and cols
	   call ASCREAD1(ioff,ilinhead,ncols,nrows,len,
     & 	ascfil,colhead,text,lentext,dcfile)
c==	   ncols=2		!===temp: reports ncols=1 after 1st file!
	   if(mod(ncols,3).ne.0) then
		call BELL(2)
		print 77
77		format(' NUMBER OF COLUMNS IS NOT A MULTIPLE OF 3!')
	   endif
	   print 13,ifile,ascfil
	   if(discprt) write(8,13) ifile,ascfil
13	   format(' INPUT ',i4,': read data from ',a33)
	   nd1=nrows
	   nd2=ncols
	   ALLOCATE(xnum(nd1,nd2))
	   call ASCREAD2(ioff,ncols,nrows,len,
     &	 ascfil,xnum,nd1,nd2)
	   print 108
108	   format(/)
	   if(dcfile) then
		if(lentext.gt.0) then
		   print 211,text
		   if(discprt) write(8,211) text
211		   format(' Title: ',a70)
		endif
c		do j=1,ncols
c		   call GBLANK(colhead(j),20,n1,n2)
c		   print 23,colhead(j)(n1:n2)
c		   if(discprt) write(8,211) colhead(j)(n1:n2)
c23		   format('&',a13)
c		enddo
	   endif
c	   print 21,nrows,ncols
c21	   format(1x,i3,' rows of data in ',i3,
c     &	' columns: First 3 rows are:')
c	   do i=1,3		!print first 3 lines
c		print 22,(xnum(i,j),j=1,ncols)
c22		format(6g12.5)
c	   enddo
c
c Pool the means for the current file -supposes columns are in sets
c of 3=x, y, sd.  The row number=lag value, column 1-3 =x,y,sd(y) for
c range 1; col 4-6=x,y,sd(y) for range 2 etc.  Want plain average of
c x values over expts, and weighted average of y values.
	   nval=ncols/3		!number of ranges
	   nlag=nrows		!number of lags
	   j1=0
	   do j=1,nval
		j1=j1+3
		do i=1,nlag
		   x=xnum(i,j1-2)
		   y=xnum(i,j1-1)
		   sd=xnum(i,j1)
		   w=1.0/(sd*sd)
		   sx(i,j)=sx(i,j) + x
		   sy(i,j)=sy(i,j) + w*y
		   sw(i,j)=sw(i,j) + w
		enddo
	   enddo
	   DEALLOCATE(xnum)
	enddo		!next file
c Now get the mean for all files, and write back to xnum to write
c to new ascii file
c
	j1=0
	do j=1,nval
	   j1=j1+3
	   do i=1,nlag
c		xnum(i,j1-2)=sx(i,j)/float(nfile)
c		xnum(i,j1-1)=sy(i,j)/sw(i,j)	!weighted mean
c		xnum(i,j1)=sqrt(1.0/sw(i,j))		!sd for ditto
		sx(i,j)=sx(i,j)/float(nfile)
		sy(i,j)=sy(i,j)/sw(i,j)	!weighted mean
		sw(i,j)=sqrt(1.0/sw(i,j))		!sd for ditto
	   enddo
	enddo
c Write to new ascii files
c (1) X=lag, Y=conditional mean, range #= set # in CVFIT so can
c	 plot Y vs lag for various ranges.
c (2) X=mean value in range, Y=conditional mean, set # in cvfit=lag, so
c 	can plot Y vs X for various lags.
c
	nd1=nrows
	nd2=ncols
	ALLOCATE(xnum(nrows,ncols))		!same as input
	j1=0
	do j=1,nval
	   j1=j1+3
	   do i=1,nlag
		xnum(i,j1-2)=float(i)
		xnum(i,j1-1)=sy(i,j)	!weighted mean
		xnum(i,j1)=sw(i,j)		!sd for ditto
	   enddo
	enddo
	icolwid=13
	print 14
	if(discprt) write(8,14)
14	format(/,' OUTPUT AS FUNCTION OF LAG (FOR DIFFERENT RANGES)')
	call ASCWRITE(xnum,nrows,ncols,text,nd1,nd2,lentext,
     &		icolwid,colhead,1)   !call with ic=1 so no check on colhead
	if(dcfile.and.lentext.gt.0) then
	   print 211,text
	   if(discprt) write(8,211) text
c211	   format(/,' OUTPUT title: ',/,a70)
	endif
	DEALLOCATE(xnum)
	DEALLOCATE(colhead)
c
c Now SECOND form of output file
	nrows=nval		!number of ranges
	ncols=nlag*3	!x,y,sd at each lag
	nd1=nrows
	nd2=ncols
	ALLOCATE(colhead(ncols))
	do i=1,ncols,3
	   call INTCONV(1+i/3,cnum)
	   colhead(i)='X (lag '//charnb(cnum)//')'
	   colhead(i+1)='Y (lag '//charnb(cnum)//')'
	   colhead(i+2)='SD(mean)('//charnb(cnum)//')'
	enddo
	ALLOCATE(xnum(nrows,ncols))		!same as input
	j1=0
	do j=1,nlag
	   j1=j1+3
	   do i=1,nval
		xnum(i,j1-2)=sx(j,i)
		xnum(i,j1-1)=sy(j,i)	!weighted mean
		xnum(i,j1)=sw(j,i)		!sd for ditto
	   enddo
	enddo
	print 15
	if(discprt) write(8,15)
15	format(/,' OUTPUT AS FUNCTION OF RANGE (FOR DIFFERENT LAGS)')
	call ASCWRITE(xnum,nrows,ncols,text,nd1,nd2,lentext,
     &		icolwid,colhead,1)
	if(dcfile.and.lentext.gt.0) then
	   print 211,text
	   if(discprt) write(8,211) text
c211	   format(/,' OUTPUT Title: ',/,a70)
	endif
c
	DEALLOCATE(xnum,colhead)
	call ENDPRINT
c
	END

