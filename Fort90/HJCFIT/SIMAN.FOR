	program SIMAN
c To analyse binary output from simulations in HJCFIT
c
	real*8 thtrue(200),thtrsav(200),thsav(200)
	real*8 stpfac,confac,errfac,ec50,tresdum
	real*4 conc(10,10)
	real*8 ec50out,penfunc
	logical fixec50
	logical abort,present,readini,allocated
	character*1 ans,ans1
	character*11 cdate,ctime
	character*10 titlep(200)
	character*40 simfile,simfile1,simfile2	!file names for output of simulation
	real*8 thetval,elmval,elmset
	allocatable thetval(:,:),elmval(:),elmset(:,:)	!for simulations
	allocatable::nintval,ixval,iyval,izval,nevals
	real*8 ec50val
	allocatable::ec50val(:)
	integer nintval(:,:),ixval(:),iyval(:),izval(:),nevals(:)
	allocatable::ytemp
	real*4 ytemp(:)
	allocatable::iomit
	integer iomit(:)
	logical dcmod
c For simval.dat from Windows version , iver=106
	character*74 mtitlesw
	character*40 qfilem
c
	real*8 p1,p2,pv,sy,syy,fi,one,ybar,var,sd,sdm,cv
	real*8 a2,E2,E1a,E1b,ak2
c
	character mtitle1*40,filnam*32,prtport*4		!for WINPRINT
	character*40 mtitles 		!for sim model (v 104 only)
	common/dpp/filnam,prtport,ndisc,icol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	character inifile*40
	logical discprt
	common/dp/discprt
c
	filnam='SIMAN.PRT'
	call WINPRINT		!print file control
c
	print 100
	if(discprt) write(8,100)
100	FORMAT(
     &' SIMAN: Analysis of repeated simulated fits from HJCFIT',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 6,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,6) cdate,ctime(1:8),mtitle1
6	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(' CAPS LOCK on for debugging')
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	call GINO
	call vga
	call mode(3)
c
c Read the .ini file
452	readini=.false.
	inifile='SIMAN.INI'
	call TITENT0(
     &    'Name for siman.ini file:',inifile,40,.false.)
	INQUIRE(file=inifile,exist=present,flen=nlen,err=452)
	if(.not.present.or.nlen.eq.0) then
	   call BELL(1)
	   print 451,inifile
451	   format(' Could not find ',a40)
c	   ans='N'
	   OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=512)
	   write(19,rec=1) inifile,simfile1,simfile2,ans1
	   CLOSE(unit=19)
	   goto 452
	else
	   readini=.true.
	   OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=512)
		read(19,rec=1) inifile,simfile1,simfile2,ans1
	   CLOSE(unit=19)
	endif
	if(ans1.ne.'N'.and.ans1.ne.'Y') ans1='N'
	
c
c Specify data file
c	simfile1='SIMVAL.dat'
	call TITENT0(
     & 'File name for binary output of simulations',simfile1,40,.false.)
	OPEN(unit=10,file=simfile1,status='UNKNOWN',
     &	access='TRANSPARENT')
	read(unit=10,rec=1) iver1
	if(iver1.le.101) then
	   read(unit=10,rec=1) iver1,nsims1,abort,npar,nset
	else
	   read(unit=10,rec=1) iver1,nsims1,npar,nset
	endif
	nsims=nsims1	!unless increased by 2nd file
	print 31, nsims1,simfile1
	if(discprt) write(8,31) nsims1,simfile1
31	format(/,' Runs 1 to ',i5,' from ',a40)
c Read the file
	nsims2=0
	ans=ans1
	call DCASK('Pool another file',ans,ans)
	ans1=ans
	if(ans.eq.'Y') then
	   call TITENT0(
     & 'File name for second file',simfile2,40,.false.)
	   OPEN(unit=11,file=simfile2,status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(unit=11,rec=1) iver2
	   if(iver2.le.101) then
		read(unit=11,rec=1) iver2,nsims2,abort,npar,nset
	   else
		read(unit=11,rec=1) iver2,nsims2,npar,nset
	   endif
	   nsims=nsims1 + nsims2		!total number
	   CLOSE(unit=11)

	   print 32, nsims2,nsims1+1,nsims,simfile2
	   if(discprt) write(8,32) nsims2,nsims1+1,nsims,simfile2
32	   format(' plus ',i5,' runs (',i5,' to ',i5,') from ',a40,/)
	endif
c
	ALLOCATE(thetval(npar,nsims),nintval(nset,nsims),
     &    ixval(nsims),iyval(nsims),izval(nsims),elmval(nsims),
     &	elmset(nset,nsims),nevals(nsims),ec50val(nsims))
	ALLOCATE(ytemp(nsims))
c
c In iver=101, tres was written as treso*1.d3 instead of treso*1.e3
c and this caused it to be written as 8 bytes not 4, so read bacl as
c real*8 tresdum
	if(iver1.le.101) then
	   read(unit=10,rec=1) iver,
     &    nsims1,abort,npar,nset,ix1sav,iy1sav,iz1sav,
     &    tresdum,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &    fixec50,ec50,stpfac,confac,errfac,nevm,
     &   (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &    ((thetval(i,j),i=1,npar),j=1,nsims1)
	   treso=sngl(tresdum)		!correct error
	else if(iver1.eq.102) then
	   read(unit=10,rec=1) iver,
     &    nsims1,npar,nset,
     &    treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &    fixec50,ec50,stpfac,confac,errfac,nevm,
     &   (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &    ((thetval(i,j),i=1,npar),j=1,nsims1),
     &    ((conc(i,j),i=1,nlig),j=1,nset),
     &    ((nintval(i,j),i=1,nset),j=1,nsims1),
     &    (ixval(i),i=1,nsims1),
     &    (iyval(i),i=1,nsims1),
     &    (izval(i),i=1,nsims1),
     &    (elmval(i),i=1,nsims1)
	else if(iver1.eq.103) then
	   read(unit=10,rec=1) iver,
     &    nsims1,npar,nset,
     &    treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &    fixec50,ec50,stpfac,confac,errfac,nevm,
     &   (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &    ((thetval(i,j),i=1,npar),j=1,nsims1),
     &    ((conc(i,j),i=1,nlig),j=1,nset),
     &    ((nintval(i,j),i=1,nset),j=1,nsims1),
     &    (ixval(i),i=1,nsims1),
     &    (iyval(i),i=1,nsims1),
     &    (izval(i),i=1,nsims1),
     &    (elmval(i),i=1,nsims1),
     &    ((elmset(i,j),i=1,nset),j=1,nsims1),
     &     ix1,iy1,iz1
	else if(iver1.eq.104) then
	   read(unit=10,rec=1) iver,
     &    nsims1,npar,nset,
     &    imods,mtitles,npars,
     &    treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &    fixec50,ec50,stpfac,confac,errfac,nevm,
     &  (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &    ((thetval(i,j),i=1,npar),j=1,nsims1),
     &    ((conc(i,j),i=1,nlig),j=1,nset),
     &    ((nintval(i,j),i=1,nset),j=1,nsims1),
     &    (ixval(i),i=1,nsims1),
     &    (iyval(i),i=1,nsims1),
     &    (izval(i),i=1,nsims1),
     &    (elmval(i),i=1,nsims1),
     &    ((elmset(i,j),i=1,nset),j=1,nsims1),
     &    ix1,iy1,iz1,(nevals(i),i=1,nsims1)
	else if(iver1.eq.105) then
	   read(unit=10,rec=1) iver,
     &    nsims1,npar,nset,
     &    imods,mtitles,npars,
     &    treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &    fixec50,ec50,stpfac,confac,errfac,nevm,
     &  (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &    ((thetval(i,j),i=1,npar),j=1,nsims1),
     &    ((conc(i,j),i=1,nlig),j=1,nset),
     &    ((nintval(i,j),i=1,nset),j=1,nsims1),
     &    (ixval(i),i=1,nsims1),
     &    (iyval(i),i=1,nsims1),
     &    (izval(i),i=1,nsims1),
     &    (elmval(i),i=1,nsims1),
     &    ((elmset(i,j),i=1,nset),j=1,nsims1),
     &    ix1,iy1,iz1,(nevals(i),i=1,nsims1)

c     &    ec50out,penfunc
	else if(iver1.eq.106) then
	    read(unit=10,rec=1) iver,
     &	nsims1,npar,nset,
     & 	imods,mtitlesw,npars,
     & treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     & fixec50,ec50,stpfac,confac,errfac,nevm,qfilem,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     & 	((thetval(i,j),i=1,npar),j=1,nsims1),
     & 	((conc(i,j),i=1,nlig),j=1,nset),
     &	((nintval(i,j),i=1,nset),j=1,nsims1),
     & 	(ixval(i),i=1,nsims1),
     &	(iyval(i),i=1,nsims1),
     & 	(izval(i),i=1,nsims1),
     & 	(elmval(i),i=1,nsims1),
     & 	((elmset(i,j),i=1,nset),j=1,nsims1),
     &   	ix1,iy1,iz1,(nevals(i),i=1,nsims1),
     &   	(ec50val(i),i=1,nsims1)
c     &  	(penval(i),i=1,nsims1)
c     & 	 ec50out,penfunc
c
		mtitles=mtitlesw(1:40)
	endif
c
	CLOSE(unit=10)
c
	if(nsims2.gt.0) then
	   n1=nsims1+1
	   OPEN(unit=11,file=simfile2,status='UNKNOWN',
     &	access='TRANSPARENT')
	   if(iver2.le.101) then
		   read(unit=11,rec=1) iver,
     &         nsims2,abort,npar,nset,ix1sav,iy1sav,iz1sav,
     &         tresdum,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &         fixec50,ec50,stpfac,confac,errfac,nevm,
     &    (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &        ((thetval(i,j),i=1,npar),j=1,n1,nsims)
	   	   treso=sngl(tresdum)		!correct error
	   else if(iver2.eq.102) then
		   read(unit=11,rec=1) iver,
     &         nsims2,npar,nset,
     &         treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &         fixec50,ec50,stpfac,confac,errfac,nevm,
     &    (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &         ((thetval(i,j),i=1,npar),j=n1,nsims),
     &         ((conc(i,j),i=1,nlig),j=1,nset),
     &         ((nintval(i,j),i=1,nset),j=n1,nsims),
     &         (ixval(i),i=n1,nsims),
     &         (iyval(i),i=n1,nsims),
     &         (izval(i),i=n1,nsims),
     &         (elmval(i),i=n1,nsims)
	   else if(iver2.eq.103) then
		   read(unit=11,rec=1) iver,
     &         nsims2,npar,nset,
     &         treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &         fixec50,ec50,stpfac,confac,errfac,nevm,
     &    (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &         ((thetval(i,j),i=1,npar),j=n1,nsims),
     &         ((conc(i,j),i=1,nlig),j=1,nset),
     &         ((nintval(i,j),i=1,nset),j=n1,nsims),
     &         (ixval(i),i=n1,nsims),
     &         (iyval(i),i=n1,nsims),
     &         (izval(i),i=n1,nsims),
     &         (elmval(i),i=n1,nsims),
     & 	   ((elmset(i,j),i=1,nset),j=n1,nsims),
     & 	   ix1,iy1,iz1
	   else if(iver2.eq.104) then
		   read(unit=11,rec=1) iver,
     &         nsims2,npar,nset,
     &         imods,mtitles,npars,
     &         treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &         fixec50,ec50,stpfac,confac,errfac,nevm,
     &   (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &         ((thetval(i,j),i=1,npar),j=n1,nsims),
     &         ((conc(i,j),i=1,nlig),j=1,nset),
     &         ((nintval(i,j),i=1,nset),j=n1,nsims),
     &         (ixval(i),i=n1,nsims),
     &         (iyval(i),i=n1,nsims),
     &         (izval(i),i=n1,nsims),
     &         (elmval(i),i=n1,nsims),
     & 	   ((elmset(i,j),i=1,nset),j=n1,nsims),
     &         ix1,iy1,iz1,(nevals(i),i=n1,nsims)
	   else if(iver2.eq.105) then
		   read(unit=11,rec=1) iver,
     &         nsims2,npar,nset,
     &         imods,mtitles,npars,
     &         treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     &         fixec50,ec50,stpfac,confac,errfac,nevm,
     &   (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &         ((thetval(i,j),i=1,npar),j=n1,nsims),
     &         ((conc(i,j),i=1,nlig),j=1,nset),
     &         ((nintval(i,j),i=1,nset),j=n1,nsims),
     &         (ixval(i),i=n1,nsims),
     &         (iyval(i),i=n1,nsims),
     &         (izval(i),i=n1,nsims),
     &         (elmval(i),i=n1,nsims),
     & 	   ((elmset(i,j),i=1,nset),j=n1,nsims),
     &         ix1,iy1,iz1,(nevals(i),i=n1,nsims)
c     &    	   ec50out,penfunc
	else if(iver2.eq.106) then
	    read(unit=10,rec=1) iver,
     &	nsims2,npar,nset,
     & 	imods,mtitlesw,npars,
     & treso,cdate,ctime,mtitle1,simfile,k,kfit,imod0,nlig,
     & fixec50,ec50,stpfac,confac,errfac,nevm,qfilem,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     & 	((thetval(i,j),i=1,npar),j=n1,nsims),
     & 	((conc(i,j),i=1,nlig),j=n1,nset),
     &	((nintval(i,j),i=1,nset),j=n1,nsims),
     & 	(ixval(i),i=n1,nsims),
     &	(iyval(i),i=n1,nsims),
     & 	(izval(i),i=n1,nsims),
     & 	(elmval(i),i=n1,nsims),
     & 	((elmset(i,j),i=1,nset),j=n1,nsims),
     &   	ix1,iy1,iz1,(nevals(i),i=n1,nsims),
     &   	(ec50val(i),i=n1,nsims)
c     &  	(penval(i),i=n1,nsims)
c     & 	 ec50out,penfunc
c
		mtitles=mtitlesw(1:40)
	   endif
	   CLOSE(unit=11)
	endif
c
c Define array to omit specified runs (eg if seed duplicated)
	allocate(iomit(nsims))
	nomit=0
	do i=1,nsims
	   iomit(i)=0
	enddo
c If iver.ge.102 then check for consistency
c (1) make sure no simulations have the same seeds (possible before
c     error in simwrt.for fixed)
c (2) Check that all simulations have the same true rates and initial guesses
c     (can't do this -only last thtrue() is recorded)
c
	if(iver1.ge.102.and.iver2.ge.102) then
c check seeds
	   do i=2,nsims
		ix=ixval(i)
		iy=iyval(i)
		iz=izval(i)
		do j=1,i-1	!check all earlier seeds
		   if(ix.eq.ixval(j)) then
			if(iy.eq.iyval(j).and.iz.eq.izval(j)) then
			   print 25,i,j,ix,iy,iz
25			   format(
     &	          ' Runs ',i5,' and ',i5,' have same seeds:',3i7)
			   i1=min(i,j)
			   print 26,i1
26			   format(' Omit run # [',i5,'] = ')
			   call INPUTi(i1)
			   iomit(i1)=1
			   nomit=nomit+1
			   print 27,i1
			   if(discprt) write(8,27) i1
27			   format(' Run #',i5,' omitted (duplicate seeds)')
			endif
		   endif
		enddo
	   enddo
cc check true values
c	   do i=2,nsims
c		do j=1,i-1	!check all earlier values
c		   do n=1,npars
c
c		   enddo
c		enddo
c	   enddo

	endif
c
c
c Confirm the mechanism (in case numbering not same as in DC's qmechs.dat
	iopt=0
	if(imod0.eq.29) iopt=1
	if(imod0.eq.39) iopt=2
	if(imod0.eq.36) iopt=3
	print 24, iopt
24	format(/,
     & ' Confirm which mechanism was used for FITTING:',/,
     & ' (0) None of those below',/,
     & ' (1) Milone-type ACh scheme (3 open, 4 shut states)',/,
     & ' (2) Ditto plus one desens state  (3 open, 5 shut states)',/,
     & ' (3) Ditto plus extra brief shut state (mod 36)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.0) imod0=0
	if(iopt.eq.1) imod0=29
	if(iopt.eq.2) imod0=39
	if(iopt.eq.3) imod0=36
	if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
	   dcmod=.true.
	else
	   dcmod=.false.
	endif
	if(iver.le.103.and.imod0.eq.29) then 	!fudge! (imods not kept)
	   ans='N'
	   if(thtrue(3).ge.4.9.and.thtrue(3).le.5.1) ans='Y'
	   call DCASK(
     & ' Did mechanism used for simulation have desensitised state',
     &   ans,ans)
	   if(ans.eq.'Y') then	!omit desens rates =3,4 for printing
		print 80, thtrue(3),thtrue(4)
		if(discprt) write(8,80) thtrue(3),thtrue(4)
80		format(
     &	' Model used for simulation had desensitised state',/,
     &	' k(+d) = ',g13.6,' k(-d) = ',g13.6)
		n=0
		do i=1,16		!npars=16 for mod 39
		   thtrsav(i)=thtrue(i)		!keep original for print
		   if(i.ne.3.and.i.ne.4) then
			n=n+1
			thtrue(n)=thtrue(i)
		   endif
		enddo
		thtrue(13)=10000.		!not stored in earlier versions
		thtrue(14)=4.0e8		!not stored in earlier versions
		print 77,thtrue(13),thtrue(14)
77		format(' Check unsaved values (see hjcfit.prt):',/,
     &        ' true value of parameter 15, k(-1b) = ',g13.6,/,
     &        ' true value of parameter 16, k(+1b) = ',g13.6,/,
     &        ' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') then
		   print 78
78		   format(
     &        ' true value of parameter 15, k(-1b) = ')
		   call INPUTr(thtrue(13))
		   print 79
79		   format(
     &        ' true value of parameter 16, k(+1b) = ')
		   call INPUTr(thtrue(14))
		endif
	   endif
	endif
	if(iver.ge.104) then
	   if(imod0.eq.29.and.imods.eq.39) then
		n=0
		do i=1,npars		!npars=16 for mod 39
		   thtrsav(i)=thtrue(i)		!keep original for print
		   if(i.ne.3.and.i.ne.4) then
			n=n+1
			thtrue(n)=thtrue(i)
		   endif
		enddo
	   else
		do i=1,npars		!npars=16 for mod 39 and mod 36
		   thtrsav(i)=thtrue(i)		!keep original for print
		enddo
	   endif
	endif
c
	if(dcmod) then
	   titlep(npar+1)=' E2=b2/a2 '
	   titlep(npar+2)=' E1a      '
	   titlep(npar+3)=' E1b      '
	   titlep(npar+4)='Tot k(-2) '
	   titlep(npar+5)=' K2b (->a)'
	   titlep(npar+6)=' K2a (->b)'
	   titlep(npar+7)=' K1a      '
	   titlep(npar+8)=' K1b      '
	   titlep(npar+9)=' Lmax     '
	   titlep(npar+10)=' N eval   '
	else
	   titlep(npar+1)=' Lmax     '
	   titlep(npar+2)=' N eval   '
	endif

c
c NB mod 39 refers to QMECHSDC (not CH) -it is block of A2R only
c Mod 36 refers to QMECHSCH -it is Auerbach extra state which is formally
c the same apart from conc dep
	if(imod0.eq.39.or.imod0.eq.36) then
	   id=2	!all rates apart from 1, 2 have index increased by 2
	else if(imod0.eq.29) then
	   id=0
	endif
	iopt=1
c
200	continue
	print 10,iopt
10	format(/,' MENU OPTIONS',/,
     & ' (1) List header',/,
     & ' (2) List true values and initial guesses and param names',/,
     & ' (3) List Lmax, alpha2, E2, E1a, E1b, total diss',/,
     & ' (4) For each run, list parameter estimates',/
     & ' (5) Mean and SD of parameter estimates in a specified range',/,
     & ' (6) List all param estimates with param #j in a spec range',/,
     & ' (7) Plot distributions',/,
     & ' (8) Finish ',/,
     & ' Option number [',i2,'] = ')
c     & '',/,
c     & ' (7) Reorder parameter estimates',/,
	nopt=8
	call INPUTi(iopt)
	if(iopt.eq.nopt) goto 99
	if(iopt.eq.3.or.iopt.eq.4) then
	   n1=1
	   n2=nsims
	   print 11,nsims,n1,n2
11	   format(
     & ' List for runs n1 to n2 (up to ',i6,'): n1, n2 [',
     &	i3,',',i6,'] = ')
	   call INPUT2i(n1,n2)
	endif
c
	if(iopt.eq.1) then
	   print 12, nsims,iver,treso,cdate,ctime,mtitle1,
     &     simfile,k,npar,kfit,imod0,nlig,fixec50,ec50,
     &     stpfac,confac,errfac,nevm,nset
	   if(discprt) write(8,12) nsims,iver,treso,cdate,ctime,
     &     mtitle1,
     &     simfile,k,npar,kfit,imod0,nlig,fixec50,ec50,
     &     stpfac,confac,errfac,nevm,nset
12	   format(/,/,' Number of simulations recorded = ',i8,/,
     &     ' Version = ',i4,' resolution = ',f8.3,2x,a11,2x,a11,/,
     &     ' machine: ',a40,'   output file: ',a40,/,
     &     ' k = ',i3,
     &     ' # of rates = ',i4,' # fitted = ',i4,' mech = ',i3,/,
     &     ' # ligands = ',i3, ' ec50 fixed ?',L4,' ec50 = ',g13.6,/,
     &    ' For simplex: step = ',g13.6,' confac = ',g13.6,' errfac = ',
     &	g13.6, ' max eval = ',i10,/,
     &     ' number of sets = ',1x,i8)
	   do j=1,nset
		print 13,j,(1.e6*conc(i,j),i=1,nlig)
		if(discprt) write(8,13) j,(1.e6*conc(i,j),i=1,nlig)
13		format(' SET ',i2,': Conc = ',10g13.6)
		if(iver.ge.102) then		!not available in v101
c==		   ALLOCATE(ytemp(nsims))
		   do i=1,nsims
			ytemp(i)=float(nintval(j,i))
		   enddo
		   call VARV1(ytemp,nsims,xbar,varx,xmin,xmax)
		   if(varx.gt.0.) sd1=sqrt(varx)
		   print 131,xbar,sd1,xmin,xmax
		   if(discprt) write(8,131) xbar,sd1,xmin,xmax
131		   format(
     &   '  No. of resolved intervals: mean = ',g13.6,' SD = ',g13.6,/,
     &	'   (range from ',g13.6,' to ',g13.6,')')
c==		   DEALLOCATE(ytemp)
		endif
		if(iver.ge.105) then
		   if(fixec50) then
			n=0
			do i=1,nsims1
c=======doesn't work! ec50out etc must be arrays in hjcfit!
			enddo
		   endif
		endif
	   enddo
	else if(iopt.eq.2) then
c	   print 14,(thsav(i),i=1,npar)
c	   if(discprt) write(8,14) (thsav(i),i=1,npar)
c14	   format(
c     &   ' Guesses: ',100g13.6)
c	   print 15,(thtrue(i),i=1,npar)
c	   if(discprt) write(8,15) (thtrue(i),i=1,npar)
c15	   format(
c     &   ' True:    ',100g13.6)
c	   print 16,(titlep(i),i=1,npar)
c	   if(discprt) write(8,16) (titlep(i),i=1,npar)
c16	   format(
c     &   ' Names:   ',100g13.6)
c Print in columns!
	   print 14
	   if(discprt) write(8,14)
14	   format(' #    Name            true        guess')
	   do i=1,npar
		print 15,i,titlep(i),thtrue(i),thsav(i)
		if(discprt) write(8,15) i,titlep(i),thtrue(i),thsav(i)
15	   	format(i3,1x,a10,2x,g13.6,2x,3g13.6)
	   enddo
	   if(iver.ge.104) then
		print 121,imods,mtitles,npars,(thtrsav(i),i=1,npars)
		if(discprt) write(8,121)imods,mtitles,npars,
     &		(thtrsav(i),i=1,npars)
121		format(' For simulation, model # = ',i3,' title: ',a40,/,
     &    ' with ',i3,' rates, viz.',/,30(6g13.6,/))
	   endif
	else if(iopt.eq.3) then
c        NB ixval(n) set negative in simwrt as code for aborted run
c	   if(imod0.ne.29) then
c		call BELL(1)
c		goto 200
c	   endif
	   if(.not.dcmod) then
		call BELL(1)
		print 70
70		format(' Option not available for this mechanism')
		if(iopt.lt.nopt) iopt=iopt+1
		goto 200	!another option
	   endif
	   print 171
	   if(discprt) write(8,171)
171	   format(
     &'    Lmax         alpha2        E2           E1(a)         E1(b)',
     &'          k-2a+k-2b    Lmax for each set')
	   if(iver.ge.102) then		!not available in v101
		do n=n1,n2
c		   print 17,n,iabs(ixval(n)),iyval(n),izval(n),
c     &	elmval(n),thetval(1,n),(nintval(i,n),i=1,nset)
c		  if(discprt) write(8,17)n,iabs(ixval(n)),iyval(n),izval(n),
c     &	elmval(n),thetval(1,n),(nintval(i,n),i=1,nset)
c17		   format(1x,i5,3i8,3x,g13.6,3x,g13.6,
c     &		' # of resolved intervals = ',10i6)
		   a2=thetval(1,n)
		   E2=thetval(2,n)/thetval(1,n)
		   E1a=thetval(4+id,n)/thetval(3+id,n)
		   E1b=thetval(6+id,n)/thetval(5+id,n)
		   ak2=thetval(7+id,n) + thetval(9+id,n)
		   print 17,elmval(n),a2,E2,E1a,E1b,ak2,
     &		(elmset(i,n),i=1,nset)
		  if(discprt) write(8,17)elmval(n),a2,E2,E1a,E1b,ak2,
     &		(elmset(i,n),i=1,nset)
17		   format(6(1x,g13.6),10(1x,g13.6))
		enddo
	   endif
	else if(iopt.eq.4) then
	   print 161,(titlep(i),i=1,npar)
	   if(discprt) write(8,161) (titlep(i),i=1,npar)
161	   format(/,30(6(2x,a10,1x),/))
	   if(.not.dcmod) then
		call BELL(1)
		print 70
c70		format(' Option not available for this mechanism')
		if(iopt.lt.nopt) iopt=iopt+1
		goto 200	!another option
	   endif
c161	   format(/,8x,
c     &    '    E2          E1a           E1b            k-2a+k-2b    ',
c     &	100(a10,3x))
	   if(dcmod) then
		print 1611
		if(discprt) write(8,1611)
1611	      format(8x,
     &    '    E2          E1a           E1b            k-2a+k-2b    ')
	   endif
	   do j=n1,n2
		abort=ixval(j).lt.0	!code used to indicate aborted run in simwrt
		E2=thetval(2,j)/thetval(1,j)
		E1a=thetval(4+id,j)/thetval(3+id,j)
		E1b=thetval(6+id,j)/thetval(5+id,j)
		ak2=thetval(7+id,j) + thetval(9+id,j)
		print 184,j
		if(discprt) write(8,184) j
184		format(' Run #',i5)
		print 18,(thetval(i,j),i=1,npar)
		if(discprt) write(8,18) (thetval(i,j),i=1,npar)
18		format(30(6g13.6,/))
		if(dcmod) then
		   print 181,E2,E1a,E1b,ak2
		   if(discprt) write(8,181) E2,E1a,E1b,ak2
181		   format(4(1x,g13.6))
		endif
		if(abort) then
		   print 19
		   if(discprt) write(8,19)
19		   format('&    (aborted)')
		endif
	   enddo
	else if(iopt.eq.5) then
	   do i=1,npar
		print 20,i,titlep(i),thtrue(i)
		if(discprt) write(8,20) i,titlep(i),thtrue(i)
20		format(1x,i3,2x,a10,'  true = ',g13.6)
	   enddo
	   if(dcmod) then
		print 30,npar+1,npar+2,npar+3,npar+4,
     &	npar+5,npar+6,npar+7,npar+8
		if(discprt) write(8,30) npar+1,npar+2,npar+3,npar+4,
     &	npar+5,npar+6,npar+7,npar+8
30		format(/,
     &   ' (',i3,') E2 = beta2/alpha2',/,
     &   ' (',i3,') E1a = beta1a/alpha1a',/,
     &   ' (',i3,') E1b = beta1b/alpha1b',/,
     &   ' (',i3,') Total dissoc rate k(-2)a+k(-2)b',/,
     &   ' (',i3,') K2b (old) =K2a (new)',/,
     &   ' (',i3,') K2a (old) =K2b (new) ',/,
     &   ' (',i3,') K1a ',/,
     &   ' (',i3,') K1b ',/)
	   endif
	   print 21,ip
21	   format (
     &	' Parameter number for which range specified [',i3,'] = ')
	   call INPUTi(ip)
	   p1s=sngl(p1)
	   p2s=sngl(p2)
	   print 22,p1s,p2s
22	   format(' Range of values to be used [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(p1s,p2s)
	   p1=dble(p1s)
	   p2=dble(p2s)
	   if(discprt) write(8,33) ip,p1,p2
33	   format(
     &    ' Values for parameter ',i3,' in range ',g13.6,' to ',g13.6)
c
	   n=0
	   one=1.0d0
	   do j=1,nsims
	    if(imod0.eq.0) then
		if(ip.le.npar) then
		   pv=thetval(ip,j)
		endif
		if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
		 if(ip.eq.npar+1) then
		   pv=thetval(2,j)/thetval(1,j)
		 else if(ip.eq.npar+2) then
		   pv=thetval(4+id,j)/thetval(3+id,j)
		 else if(ip.eq.npar+3) then
		   pv=thetval(6+id,j)/thetval(5+id,j)
		 else if(ip.eq.npar+4) then
		   pv=thetval(7+id,j) + thetval(9+id,j)
	       else if(ip.eq.npar+5) then
		   pv=thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      	 else if(ip.eq.npar+6) then
		   pv=thetval(8+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
	       else if(ip.eq.npar+7) then
		   pv=thetval(11+id,j)/thetval(12+id,j)	!K1a
      	 else if(ip.eq.npar+8) then
		   pv=thetval(13+id,j)/thetval(14+id,j)	!K1b
		 endif
		endif
		if(pv.ge.p1.and.pv.lt.p2) then
		   n=n+1
		   if(n.eq.1) then
			sy=pv
			syy=0.d0
		   else
			fi=dfloat(n)
			sy=sy + pv
			syy=syy + (fi*pv-sy)**2/(fi*(fi-one))
		   endif
		endif
	    endif
	   enddo
	   if(n.ge.1) then
		ybar=sy/dfloat(n)
		var=syy/dfloat(n-1)
		if(var.ge.0.d0) then
		   sd=dsqrt(var)
		   cv=100.d0*sd/ybar
		   sdm=sd/dsqrt(dfloat(n))
		endif
	   endif
	   titlep(npar+1)=' E2=b2/a2 '
	   print 23,ip,titlep(ip),n,nsims,
     &	100.*float(n)/float(nsims),p1,p2,ybar,sd,cv,sdm
	   if(discprt) write(8,23) ip,titlep(ip),n,nsims,
     &	100.*float(n)/float(nsims),p1,p2,ybar,sd,cv,sdm
23	   format(/,' For parameter # ',i3,' = ',a10,/,
     &    1x,i4,' values out of ',i4,' (',f6.2,' %) are between ',
     &    g13.6,' and ',g13.6,/,
     &   ' mean = ',g13.6,' SD = ',g13.6,' CV = ',g13.6,' SDM = ',g13.6)
	else if(iopt.eq.6) then
	   do i=1,npar
		print 20,i,titlep(i),thtrue(i)
		if(discprt) write(8,20) i,titlep(i),thtrue(i)
c20		format(1x,i3,2x,a10,'  true = ',g13.6)
	   enddo
	   if(dcmod) then
		print 30,npar+1,npar+2,npar+3,npar+4,
     &	npar+5,npar+6,npar+7,npar+8
		if(discprt) write(8,30) npar+1,npar+2,npar+3,npar+4,
     &	npar+5,npar+6,npar+7,npar+8
c30		format(/,
c     &   ' (',i3,') E2 = beta2/alpha2',/,
c     &   ' (',i3,') E1a = beta1a/alpha1a',/,
c     &   ' (',i3,') E1b = beta1b/alpha1b',/,
c     &   ' (',i3,') Total dissoc rate k(-2)a+k(-2)b',/,
c     &   ' (',i3,') K2b (old) =K2a (new)',/,
c     &   ' (',i3,') K2a (old) =K2b (new) ',/,
c     &   ' (',i3,') K1a ',/,
c     &   ' (',i3,') K1b ',/)
	   endif
	   print 21,ip
c21	   format (
c     &	' Parameter number for which range specfied [',i3,'] = ')
	   call INPUTi(ip)
	   p1s=sngl(p1)
	   p2s=sngl(p2)
	   print 22,p1s,p2s
c22	   format(' Range of values to be used [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(p1s,p2s)
	   p1=dble(p1s)
	   p2=dble(p2s)
	   if(discprt) write(8,33) ip,p1,p2
c33	   format(
c     &    ' Values for parameter ',i3,' in range ',g13.6,' to ',g13.6)
c
	   print 161,(titlep(i),i=1,npar)
	   if(discprt) write(8,161) (titlep(i),i=1,npar)
c161	   format(/,30(6(2x,a10,1x),/)
	   if(dcmod) then
		print 1611
		if(discprt) write(8,1611)
c1611	      format(8x,
c     &    '    E2          E1a           E1b            k-2a+k-2b    ')
	   endif
	   n=0
	   one=1.0d0
	   do j=1,nsims
		if(ip.le.npar) then
		   pv=thetval(ip,j)
		endif
		if(dcmod) then
		 if(ip.eq.npar+1) then
		   pv=thetval(2,j)/thetval(1,j)
		 else if(ip.eq.npar+2) then
		   pv=thetval(4+id,j)/thetval(3+id,j)
		 else if(ip.eq.npar+3) then
		   pv=thetval(6+id,j)/thetval(5+id,j)
		 else if(ip.eq.npar+4) then
		   pv=thetval(7+id,j) + thetval(9+id,j)
      	 else if(ip.eq.npar+5) then
		   pv=thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      	 else if(ip.eq.npar+6) then
		   pv=thetval(8+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
      	 else if(ip.eq.npar+7) then
		   pv=thetval(11+id,j)/thetval(12+id,j)	!K1a
      	 else if(ip.eq.npar+8) then
		   pv=thetval(13+id,j)/thetval(14+id,j)	!K1b
		 endif
		endif
		if(pv.ge.p1.and.pv.lt.p2) then
		   n=n+1
		   abort=ixval(n).lt.0	!code used to indicate aborted run in simwrt
		   E2=thetval(2,j)/thetval(1,j)
		   E1a=thetval(4+id,j)/thetval(3+id,j)
		   E1b=thetval(6+id,j)/thetval(5+id,j)
		   ak2=thetval(7+id,j) + thetval(9+id,j)
		   print 182,n,j
		   if(discprt) write(8,182) n,j
182		   format(1x,i5,': Run #',i5)
		   print 18,(thetval(i,j),i=1,npar)
		   if(discprt) write(8,18) (thetval(i,j),i=1,npar)
c18		   format(30(6g13.6,/))
		   if(dcmod) then
			print 181,E2,E1a,E1b,ak2
			if(discprt) write(8,181) E2,E1a,E1b,ak2
c181			format(4(1x,g13.6))
		   endif
c		   print 18,n,E2,E1a,E1b,ak2,(thetval(i,j),i=1,npar)
c		   if(discprt) write(8,18) n,E2,E1a,E1b,ak2,
c     &		(thetval(i,j),i=1,npar)
cc18		   format(i8,4(1x,g13.6),100g13.6)
		   if(abort) then
			print 19
			if(discprt) write(8,19)
c19			format('&    (aborted)')
		   endif
		endif
	   enddo
c
	else if(iopt.eq.7) then
c Display of distributions
	   ndimd=1
	   ndimc=1
	   call DISPSIM(thetval,npar,nsims,imod0,ndimd,ndimc,titlep,
     &   simfile1,thtrue,elmval,elmset,ixval,nevals,nset,id,iomit,nomit)
c
c	   do i=1,npar
c		print 25,i,titlep(i)
c25		format(1x,i3,2x,a10)
c	   enddo
c	   print 26,ip
c26	   format (' Parameter number [',i3,'] = ')
c	   call INPUTi(ip)
ccc Reordering of mono-liganded rates
cc Sites (a) and (b) are arbitrary, but if (a) and (b) swapped for
cc one rate constant, they must be swapped for all
cc Biggest difference in true rates is alpha1(a)=6000 vs alpha1(b)=50000
cc ALSO  k(-1)(a)=1500 vs k(-1)(b)=10000
cc In contrast beta1(a)=50 and beta1(b)=150
cc If alpha1a and alpha1b are swapped then must also swap
cc (a) beta1a and beta1b
cc (b)
c		if(imod0.eq.29) then
c		   print 24
c24		   format(
c     & ' (1) Swap (a) and (b) sites so (a) has the smaller alpha1',/,
c     & ' (2) Swap (a) and (b) sites so (a) has the smaller k(-1)',/,
c     & ' Option number [1] = ')
c=======
c	      endif
	endif		!end of iopt options
c
	if(iopt.lt.nopt) iopt=iopt+1
	goto 200	!another option
c
c
c
c Write the ini file
99	continue
	call TITENT0(
     &    'Name for siman.ini file:',inifile,40,.false.)
	OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=512)
	   write(19,rec=1) inifile,simfile1,simfile2,ans1
	CLOSE(unit=19)
c
	end

