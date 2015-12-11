	subroutine CVREAD(xval,yval,weight,icurvw,ndat,
     &		ndv,ndimd,kwi,kwj)
c to read P(open) values from a cvdat.dat file, in HJCDISP
c
	real*4 XVAL(ndv,ndimd),YVAL(ndv,ndimd)
	real*4 weight(kwi,kwj)
	integer ndat(5),icurvw(ndimd)
c other declarations as in CVDISK
	ALLOCATABLE::datcop,xobs,yobs,wobs
	real*4 datcop(:),xobs(:,:),yobs(:,:),wobs(:,:)
	integer jstrec(100),lstrec(100)
	character*60 titled(200)
c	character*60 titled(njset)
	integer nj(200)
	real setx(200)
	character*40 titleds(10)	!shorter set titles for iver=>1002
	character*60 titlef		!file names (for iver=1002)
	character*10 TITLEP(20)		!names of params for fit
	character*40 titlex,titley
 	character*1 ans
	character infil*40
	logical debug,caplock,setvar
	logical discprt,hdisp,present,readini
	common/dp/discprt
c
	debug()=caplock()
c
	njset=200
	readini=.false.		!until values out into hjcfit.ini
c
	infil='CVDAT.DAT'		!default for ndisc=1
31	continue
	call TITENT0(
     &'Name and path of CVFIT data file:',infil,40,.false.)
	call CHKFILE(infil,nerr,40)
	if(nerr.ne.0) then
	   call BELL(1)
	   print 311
311	   format(' INVALID file name -try again')
	   goto 31
	endif
c
	INQUIRE(file=infil,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   call BELL(3)
	   print 24,infil
24	   format(1x,a40,' does not exist')
	   present=.false.		!so asks for ndev
	   goto 31
	else
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024,err=31)
c NB recl is in bytes (4*value for PDP)
	endif
5	continue	!jump here if already open
c
c
c Section to read data  for file #ifile
1	continue
	read(12,rec=1) nfile,jstrec,lstrec,iver
	if(iver.le.1000.or.iver.gt.1100) iver=1001	!original version
c iver=version # to get format of data files. Original version
c now has iver=1001, If iver=1002 then an extra overall title (titlef(60))
c is stored for each file, as well as the data set titles (titled)
c iver=1003 has new organisation of CVDAT with transparent read/write!
	if(iver.ge.1003) then
	   CLOSE(unit=12)		!then reopen as transparent
         OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT')
	endif
c
	print 335,nfile
335	format(1x,i5,' files: list file titles [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'N') goto 338
c In Lahey fortran file is not of pre-fixed size but has as many records
c as have been written
	do 518 ifile=1,nfile
	irec=jstrec(ifile)
c modif to cope with old data files that have different iver in same
c data file -check if nset=1-10 when read as if old, and set iver from this
c (overwrites the iver read from record #1)
	if(iver.lt.1003) then
	   read(12,rec=irec) (titled(k),k=1,10),nset
	   if(nset.ge.1.and.nset.le.10) then
		iver=1001		!old
	   else
		iver=1002		!new
	   endif
	else		!iver=>1003
	   read(12,rec=irec) nset
	   if(nset.gt.njset-5) then
		call BELL(2)
		print 19,nset,njset
19	     format(' Number of sets = ',i4,' too large for array size',
     &	i4,/,' Allocate a larger size before proceeding')
		CLOSE(unit=12)
		RETURN
	   endif
	endif
	if(iver.eq.1001) then
	   read(12,rec=irec) (titled(k),k=1,10),nset
         print 520,ifile,nset
         if(discprt) write(8,520) ifile,nset
520	   format(' File number ',i3,':  contains ',i4,' data sets')
	else if(iver.eq.1002) then
	   read(12,rec=irec) titlef,titleds,nset
	   call SWAPTIT(titled,titleds,nset,njset,1)  !copy titleds into titled
         print 5201,ifile,titlef,nset
         if(discprt) write(8,5201) ifile,titlef,nset
5201	   format(' File no ',i3,': ',a60,/,' contains ',i4,' data sets')
	else if(iver.eq.1003) then
c      N.B. nj() etc already allocated in calling prog to size=njset
	   read(12,rec=irec) nset,titlef,(titled(j),j=1,nset)
         print 5201,ifile,titlef,nset
         if(discprt) write(8,5201) ifile,titlef,nset
	endif
	do 519 j=1,nset
      print 10,j,titled(j)(1:60)
      if(discprt) write(8,10) j,titled(j)(1:60)
10	format('  data set #',i3,2x,a60)
519	continue
	pause
c      ans=getch(ktype)
518	continue	!end of loop to print titles
c
	print 138
138	format(
     & ' (1) Use this disk file',/,
     & ' (2) Try another disk',/,
     & ' Option number [1] = ')
	i=1
	call INPUTi(i)
	if(i.eq.2) then
	   close(unit=12)
	   present=.false.		!so asks for ndev
	   goto 31
	endif
338	continue
c Print file name/path
      if(discprt) write(8,20) infil
20	format(' Data from: ',a33)
c
	if(nfile.eq.1) then
	   ifile=1
	else if(readini) then
	   if(ifile1.gt.nfile) ifile1=1
	   print 141,nfile,ifile1
141	   FORMAT(' Read file number (1 -',i3,') [',i3,'] = ')
	   ifile=ifile1
	   call INPUTi(ifile)
	else
	   print 140,nfile
140	   FORMAT(' Read file number (1 -',i3,') = ')
	   call INPUTi(ifile)
	endif
c
	ifile1=ifile	!now redefine ifile1 as file actually read
c
	if(ifile.lt.1.or.ifile.gt.nfile) goto 338
      if(discprt) write(8,1401)ifile
1401	format(' Data from disk file ',i5)
	nrect=lstrec(ifile)-jstrec(ifile)+1		!total number of records
	nrecd=nrect-1				!number of records for Xval etc
	irec=jstrec(ifile)
	if(iver.ge.1003) then	!check array size
	   read(12,rec=irec) nset
	   if(nset.gt.njset-5) then
		call BELL(2)
		print 19,nset,njset
c19	     format(' Number of sets = ',i4,' too large for array size',
c     &	i4,/,' Allocate a larger size before proceeding')
		CLOSE(unit=12)
		RETURN
	   endif
	endif
	if(iver.eq.1001) then
	   read(12,rec=irec) titled,nset,(nj(k),k=1,10),
     &	(titlep(k),k=1,20),setvar,
     &	(setx(k),k=1,10),iw,titlex,titley,ilabel,hdisp
	else if(iver.eq.1002) then
	   read(12,rec=irec) titlef,titleds,nset,(nj(k),k=1,10),
     &	(titlep(k),k=1,20),setvar,
     &	(setx(k),k=1,10),iw,titlex,titley,ilabel,hdisp
	    call SWAPTIT(titled,titleds,nset,njset,1) !copy titleds into titled
	else if(iver.eq.1003) then
	   read(12,rec=irec) nset,titlef,(titled(j),j=1,nset),
     & 	(nj(j),j=1,nset),(setx(j),j=1,nset),setvar,iw,
     &       titlex,titley,ilabel,hdisp,ioffset,k
c NB add ioffset=byte offset to where data starts, and
c k=number of values in datcop
	   ntot=0
	   do j=1,nset
		ntot=ntot+nj(j)
	   enddo
	endif
	do 521 j=1,nset
      print 10,j,titled(j)
      if(discprt) write(8,10) j,titled(j)
521	continue
c Now read 'compressed' data in DATCOP from disk
	if(iver.le.1002) then
	   ALLOCATE(datcop(3000))
	   n=1
	   do j=1,nrecd
		m=n+255
		irec=irec+1
		read(12,rec=irec) (DATCOP(i),i=n,m)
		n=n+256
	   enddo
	else
	   ALLOCATE(datcop(3*ntot))
	   read(12,rec=irec+ioffset) (DATCOP(i),i=1,k)
	endif
c
c NB data must be returned in col j=1, but the compression of data in
c datcop() does not allow reading of a single set when nset>1, so
c allocate arrays to read all sets
	njmax=0
	do j=1,nset
	   if(nj(j).gt.njmax) njmax=nj(j)
	enddo
	ALLOCATE(xobs(njmax,nset),yobs(njmax,nset),wobs(njmax,nset))
c now decode DATCOP into data arrays

	k=0		!index for datcop
	do j=1,nset
	   n=nj(j)
	   do i=1,n
	      k=k+1
	      Xobs(i,j)=datcop(k)
	   enddo
	   if(hdisp) goto 47	!X only for histograms
	   do i=1,n
		k=k+1
		Yobs(i,j)=datcop(k)
	   enddo
	   do i=1,n
		k=k+1
		wobs(i,j)=datcop(k)
	   enddo
47	   continue
	enddo
c
c Now decide which set to use from specified file, and print it
c NB data must be returned in col j=1, but the compression of data in
c datcop() does not allow reading of a single set when nset>1, so
c allocate arrays to read all sets
	jset=1
	if(nset.gt.1) then
51	   print 50,nset,jset
50	   format(/,
     &   ' Display set mumber (1 to ',i2,') [',i2,'] = ')
	   call INPUTi(jset)
	   if(jset.lt.1.or.jset.gt.nset) goto 51
	endif

c Copy required set to output
	icurvw(1)=-1		!sd not supplied
	ndat(1)=nj(jset)
	do i=1,ndat(1)
	   xval(i,1)=xobs(i,jset)
	   yval(i,1)=yobs(i,jset)
	   weight(i,1)=wobs(i,jset)
	   if(weight(i,1).gt.1.e-18) icurvw(1)=1	!at least on SD supplied
	enddo
	DEALLOCATE(xobs,yobs,wobs)
c
	j=1
	call PRINTJ(j,titled,setx,setvar,nj,weight,xval,yval,
     & niobs,njset,ndv,ndimd,kwi,kwj)
c
c
	DEALLOCATE(datcop)
	CLOSE(unit=12)		!then reopen as transparent
	RETURN
c
	end

	subroutine PRINTJ(j,titled,setx,setvar,nj,w,xobs,yobs,
     & niobs,njset,ndv,ndimd,kwi,kwj)
c Modified from PRINTJ in CVFIT
c To print data for set #j in CVFIT
c
	real*4 Xobs(ndv,ndimd),Yobs(ndv,ndimd)
	real*4 w(kwi,kwj)
c=	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	real setx(njset)
	character*60 titled(njset)
	integer nj(njset)
	logical discprt,setvar
	common/dp/discprt
c
	print 1092,j,titled(j)
      if(discprt) write(8,1092) j,titled(j)
1092	FORMAT('  Data set # ',i3,3x,a60)
	if(setvar) then
	   print 334,setx(j)
	   if(discprt) write(8,334) setx(j)
334	   format( ' Set variable= ',g13.6)
	endif
      print 39
	if(discprt) write(8,39)
39	format(
     & ' Obs #       X value      Y value           s(Y)       weight')
	do i=1,nj(j)
	   if(w(i,j).gt.1.e-37) then
		sd=sqrt(1.0/w(i,j))
	   else
		sd=1.e36
	   endif
	   print 1091,I,XOBS(I,J),YOBS(I,J),sd,w(i,j)
	   if(discprt)write(8,1091)I,XOBS(I,J),YOBS(I,J),sd,w(i,j)
1091	   FORMAT(I4,3X,4(2X,G13.6))
	enddo
	RETURN
	end

	subroutine SWAPTIT(titled,titleds,nset,njset,icop)
c To copy titled into titleds (icop=0) or vice versa (icop=1)
c Titled=60 character set titles used in iver=1001
c Titleds=40 character set titles used in iver=1002
	character*60 titled(njset)
	character*40 titleds(10)
c
	if(icop.eq.0) then
	   do 1 i=1,nset
1	   titleds(i)=titled(i)(1:40)
	else
	   do 2 i=1,nset
	   titled(i)(1:40)=titleds(i)
2	   titled(i)(41:60)='                   '
	endif
	RETURN
	end

