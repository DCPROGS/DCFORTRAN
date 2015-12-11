	program AVCOR
c To read correlation coefficients from ASCII files output from
c EKDIST (corcal, cocals, corcalj) and average over experiments
c for input to cvfit
c Input files (from CORCAL etc) have 2 cols 1=r, 2=df
c Output files have 3 cols, 1=lag, 2=rbar, 3=df
c
	character ascfil*33,text*70
	allocatable::Xnum,rbar,enbar
	real*4 Xnum(:,:),rbar(:),enbar(:)
	ALLOCATABLE colhead
	character*20 colhead(:)		!for ASCREAD1
	logical discprt
	logical pon,slock,debug,caplock
	character*11 cdate,ctime
	common/dp/discprt
	character mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	logical dcfile
c
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='AVCOR.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1     FORMAT(' AVCOR: To average autocorrelations',/)
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
	ALLOCATE(rbar(1000),enbar(1000))
	do i=1,1000
	   rbar(i)=0.0
	   enbar(i)=0.0
	enddo
c
	ncols=60		!for dimension of colhead in first call to ascread1
	ALLOCATE(colhead(ncols))
c
11	print 10
10	format(
     &' Number of correlation coefficient files to be pooled = ')
	call INPUTi(nfile)
	if(nfile.lt.1) goto 11
	do ifile=1,nfile
	   ioff=-1
	   ilinhead=-1
c       Get filename, number of rows and cols
	   call ASCREAD1(ioff,ilinhead,ncols,nrows,len,
     & 	ascfil,colhead,text,lentext,dcfile)
	   if(ncols.ne.2) then	!expect 2 cols for input
		call BELL(2)
		print 77
77		format(' NUMBER OF COLUMNS IS NOT 2!')
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
		do j=1,ncols
		   call GBLANK(colhead(j),20,n1,n2)
		   print 23,colhead(j)(n1:n2)
23		   format('&',a13)
		enddo
	   endif
	   print 21,nrows,ncols
21	   format(1x,i3,' rows of data in ',i3,
     &	' columns: First 3 rows are:')
	   do i=1,3		!print first 3 lines
		print 22,(xnum(i,j),j=1,ncols)
22		format(6g12.5)
	   enddo
c
c Pool the correlation coeffs for the current file
	   do i=1,nd1
		r=xnum(i,1)
	      z=0.5*alog((1.0+r)/(1.0-r))
		en=xnum(i,2)
		rbar(i)=rbar(i) + z
		enbar(i)=enbar(i) + en
	   enddo
	   DEALLOCATE(xnum)
	enddo		!next file
c Now get the mean for all files, and write back to xnum to write
c to new ascii file
c
	nd2=3		!3 cols for output
	ALLOCATE(xnum(nd1,nd2))
	do i=1,nd1
	   rbar(i)=rbar(i)/float(nfile)
	   xnum(i,1)=float(i)	!=lag
	   s=exp(2.0*rbar(i))
	   xnum(i,2)=(s-1.0)/(s+1.0)
	   xnum(i,3)=enbar(i)
	enddo
c Write to new ascii file
	ndata=nd1	!number of lines of data
	ncol=3	!3 entries in each line
	icolwid=13		!the minimum col width
	colhead(1)='lag     '
	colhead(2)='pooled r'
	colhead(3)='d.f.'
	call ASCWRITE(xnum,ndata,ncol,text,nd1,nd2,lentext,
     &		icolwid,colhead,1)
	if(dcfile.and.lentext.gt.0) then
	   print 211,text
	   if(discprt) write(8,211) text
c211	   format(' Title for avearged output file: ',/,a70)
	endif
c
	DEALLOCATE(xnum)
	DEALLOCATE(colhead)
c
	call ENDPRINT
c
	END

