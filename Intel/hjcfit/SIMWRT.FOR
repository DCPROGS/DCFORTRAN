	subroutine SIMWRT(
     & nsims,simfile,simfile1,imod0,k,npar,nset,nlig,titlep,
     & thtrue,thsav,conc,kfit,fixec50,ec50,stpfac,confac,errfac,nevm,
     & cDATEW,ctimew,apfile,treso,mtitle1,nsim,
     & ix1sav,iy1sav,iz1sav,nint,nev,elmax,theta0,
     & thetval,nintval,ixval,iyval,izval,elmval,elmset,ABORTW,
     &imods,npars,mtitles,nevals,ec50out,penfunc,ec50val,nsims0,qfilem,
     & nofit)
c
c To keep on disk the output from series of fits to simulated data
c in HJCFIT, so distribution of estimates can be investigated
c
c Close and re-open file each time, so data not lost when HJCFIT crashes!
c
c Get problem with restarting -if 10 done on previous run
c then thetval() was declared as thetval(npar,10), but if we now want to
c add one more then here declare thetval(npar,1)
c
c Modif 05/17/04 02:58pm so that ix1,iy1,iz1 not read back from file (these
c will be old values and will overwrite the seeds for the next run)
c
c Modif 04/05/02 11:41am iver=105 has real*8 penfunc, ec50out (so can
c check for each fit that ec50 was attained correctly)
c
c Modif 02/04/02 08:36am iver=104 has separate details for model used
c for simulation which may now be defferent from that used for fit
c -number of params for former =npars and thtrue(1:npars), model #=imods
c model title = mtitles and also nevals() (previously only in text output)
c
c Modif 01/27/02 05:25am iver=103 has also elmset(nset,nsim) to keep separately the
c likelihood for each set when nset>1 (for nset=1 it duplicates elmval).
c Also has ix1,iy1,iz1= current values of seeds at the moment of writing
c (can start or restart from these seeds in event of crash)
c
c Version 102 has Lmax(i),nint(i,j) and ix(i),iy(i),iz(j)=seeds at START of the
c fit being written now. All these are differnt
c for each fit, and so need to be written to binary file, simval.dat, as arrays
c like thetval(i,j) (forgotten in ver 100, 101!)
c
c Things to keep
c (a) Header (things that are same for all fits)
c	Version number
c	number of records (fits) that have been written
c	model number (and charmod?)
c	npar
c	names of rate constants (titlep)
c	initial guesses (npar) -same for all fits
c	true values of rates used for simulation
c	nset
c	conc
c	kfit
c	constraints etc
c
c (b) For each fit keep
c	(1) Fit number
c	(2) Random seeds,ix,iy,iz
c	(2) Number of 'observations '
c	(2) fitted parameters (theta0(j),j=1,npar)
c	(4) errors for parameters (not at first!!)
c	(3) elmax for this fit
c	(6) Number of evaluations (did it reach nevmax?)
c
c
      USE IFCORE
      USE IFPORT
	real*8 theta0(200),thtrue(200),thsav(200)
	real*8 thtrue2(200),thsav2(200)
	real*8 elmax,stpfac,confac,errfac,ec50
	real*8 ec50out,penfunc
	real*4 conc(10,10)
	integer nint(10)
	logical fixec50,nofit
	logical apfile,ABORTW
	character*11 cDATEW,ctimew
		character*10 titlep(200)
	character*10 titlep2(200)

	character*40 mtitle1,qfilem
	character*74 mtitles,cdum3
	character*40 simfile,simfile1	!file names for output of simulation
	character*11 cnum1,cnum2
c To accumulate results over runs for simval.dat
	real*8 thetval(npar,nsim+nsims0)
	integer nintval(nset,nsim+nsims0)
	real*8 thetval2(npar,nsim+nsims0)
	integer nintval2(nset,nsim+nsims0)
	integer ixval(nsim+nsims0)
	integer iyval(nsim+nsims0)
	integer izval(nsim+nsims0)
	integer nevals(nsim+nsims0)
	real*8 elmval(nsim+nsims0)
	real*8 elmset(nset,nsim+nsims0)
	real*8 ec50val(nsim+nsims0)
c For read of existing binary data
	real*8 rdum
	real*4 rdum4
	integer*4 idum
	logical ldum,present
	character*11 cdum1
	character*40 cdum2
      TYPE (FILE$INFO) info
	common/rand/ix1,iy1,iz1
	common/ranlast/ixlast,iylast,izlast
c
c   Open in transparent mode ONLY to write the current value of nsims
c  for rec=1
c	iver=101
c	iver=102
c	iver=103		!has elmset too
c	iver=104		!and imods,npars etc for simulation model
c	iver=105		!and ec50out, penfunc
      iver=106         !windows
	if(.not.apfile) then
         OPEN(unit=9,file=SIMFILE,status='UNKNOWN',
     &	 access='DIRECT', form='BINARY',RECL=1)
c	   write(cnum1,'i11') iver
c	   write(cnum2,'i11') nsims
	   call INTCONV(iver,cnum1)
	   call INTCONV(nsims,cnum2)
	   write(unit=9,rec=1) cnum1,cnum2
	   CLOSE(unit=9)
	endif
c
c Use same type of file as for dscprt
c      OPEN(unit=9,file=SIMFILE,status='UNKNOWN',
c     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
c
c     Write header part
	if(nsims.eq.1) then
         OPEN(unit=9,file=SIMFILE,status='UNKNOWN',
     &    access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	    if(.not.apfile) REWIND(unit=9)
	   write(9,1) iver,treso*1.e3,cDATEW,ctimew,mtitle1,simfile,
     &     ix1sav,iy1sav,iz1sav,
     &     k,npar,kfit,imod0,nlig,fixec50,ec50*1.d6,
     &     stpfac,confac,errfac,nevm,nset,
     &     imods,mtitles,npars
1	   format(/,/,16x,
     &     ' Version = ',i4,' resolution = ',f8.3,2x,a11,2x,a11,/,
     &     ' machine: ',a40,'   output file: ',a40,/,
     &     ' seeds = ',3(1x,i8),/,' k = ',i3,
     &     ' # of rates = ',i4,' # fitted = ',i4,' mech = ',i3,/,
     &     ' # ligands = ',i3, ' ec50 fixed ?',L4,' ec50 = ',g13.6,/,
     &    ' For simplex: step = ',g13.6,' confac = ',g13.6,' errfac = ',
     &	g13.6, ' max eval = ',i10,/,
     &     ' number of sets = ',1x,i8,/,
     &     ' For simulation: model #',i3,2x,a40,' # of params = ',i3)
	   do i=1,nlig
		do j=1,nset
		   write(9,6) i,j,conc(i,j)*1.e6	!micromolar
6		   format(' conc(',i2,',',i2,') = ',g13.6)
		enddo
	   enddo
	   write(9,3) (thsav(i),i=1,npar)
3	   format(' Initial guesses:     ', 200(1x,g13.6))
	   write(9,31) (thtrue(i),i=1,npar)
31	   format(' True rate constants: ', 200(1x,g13.6))
c Titles for output
	   write(9,2) (titlep(i),i=1,npar)
2	   format(/,'      i    Nobs    eval    Lmax      ',200(4x,a10))
	   CLOSE(unit=9)
	endif
c
c now write the records written after every fit
      OPEN(unit=9,file=SIMFILE,status='UNKNOWN',
     &    access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	write(9,5) nsims,nint(1),nev,elmax,(theta0(i),i=1,npar)
5	format(1x,i6,2x,i6,2x,i6,2x,g13.6,200(1x,g13.6))
	if(nset.gt.1) then	!record rest of nint()
	   do j=2,nset
		write(9,4) nint(j)
4		format(9x,i6)
	   enddo
	endif
	if(.not.ABORTW) then
	   write(9,7) ix1sav,iy1sav,iz1sav,ix1,iy1,iz1
7	   format(' seeds at start= ',3(1x,i8),': at end = ',3(1x,i8))
	else
	   write(9,71) ix1sav,iy1sav,iz1sav,ix1,iy1,iz1
71	   format(' seeds at start= ',3(1x,i8),': at end = ',3(1x,i8),
     &	' RUN ABORTWED')
	endif
	write(9,72) (elmset(j,nsims),j=1,nset)
72	format(' Likelihood for each set = ',10g13.6)
	write(9,73) penfunc,1.d6*ec50out
73	format(' penfunc, ec50 = ',3g13.6)
	CLOSE(unit=9)
	if(nofit) goto 99
c
c And write same data in binary form
c Simplest way is to keep all theta values in 2D array, allocated in main
c prog, thetval(i,j), and write the whole thing every time at record 1
c (this way need no index, or counting of # 0f bytes written)
c
c If this is a restart, then must first read the existing file
c  (it was checked in hjcsim1 that simfile1 is present)
c NB want only thetval -do NOT overwrite nsims, ABORTW,ix,iy,iz, ctimew etc!
	if(apfile) then
	  INQUIRE (FILE=simFILE1,EXIST=PRESENT)
	  if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(simfile1, info, ihandle)
			nLEN=info%length
        if(nlen.gt.0) then
	   OPEN(unit=10,file=simfile1,status='UNKNOWN',
     &	 access='DIRECT', form='BINARY',RECL=1)
	   read(unit=10,rec=1) iver1,nsims1
	   if(iver1.le.101) then
	      read(unit=10,rec=1) idum,
     &	 nsims1,ldum,idum,idum,idum,idum,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &	 ((thetval(i,j),i=1,npar),j=1,nsims1)

	   else if(iver1.eq.102) then
	      read(unit=10,rec=1) idum,
     & 	 nsims1,idum,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,
     &(thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &	 ((thetval(i,j),i=1,npar),j=1,nsims1),
     &	 ((conc(i,j),i=1,nlig),j=1,nset),
     &	 ((nintval(i,j),i=1,nset),j=1,nsims1),
     &	 (ixval(i),i=1,nsims1),
     &	 (iyval(i),i=1,nsims1),
     &	 (izval(i),i=1,nsims1),
     &	 (elmval(i),i=1,nsims1)
	   else if(iver1.eq.103) then
	      read(unit=10,rec=1) idum,
     & 	 nsims1,idum,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),
     &	 ((thetval(i,j),i=1,npar),j=1,nsims1),
     &	 ((conc(i,j),i=1,nlig),j=1,nset),
     &	 ((nintval(i,j),i=1,nset),j=1,nsims1),
     &	 (ixval(i),i=1,nsims1),
     &	 (iyval(i),i=1,nsims1),
     &	 (izval(i),i=1,nsims1),
     &	 (elmval(i),i=1,nsims1),
     &	 ((elmset(i,j),i=1,nset),j=1,nsims1),
     &	  ix2,iy2,iz2
c     &	  ix1,iy1,iz1
	   else if(iver1.eq.104) then
	      read(unit=10,rec=1) idum,
     & 	 nsims1,idum,idum,
     &	 idum,cdum2,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,
     &(thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &	 ((thetval(i,j),i=1,npar),j=1,nsims1),
     &	 ((conc(i,j),i=1,nlig),j=1,nset),
     &	 ((nintval(i,j),i=1,nset),j=1,nsims1),
     &	 (ixval(i),i=1,nsims1),
     &	 (iyval(i),i=1,nsims1),
     &	 (izval(i),i=1,nsims1),
     &	 (elmval(i),i=1,nsims1),
     &	 ((elmset(i,j),i=1,nset),j=1,nsims1),
     &  	 ix2,iy2,iz2,(nevals(i),i=1,nsims1)
c     &  	 ix1,iy1,iz1,(nevals(i),i=1,nsims1)
	   else if(iver1.eq.105) then
	      read(unit=10,rec=1) idum,
     & 	 nsims1,idum,idum,
     &	 idum,cdum2,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,
     &(thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     &	 ((thetval(i,j),i=1,npar),j=1,nsims1),
     &	 ((conc(i,j),i=1,nlig),j=1,nset),
     &	 ((nintval2(i,j),i=1,nset),j=1,nsims1),
     &	 (ixval(i),i=1,nsims1),
     &	 (iyval(i),i=1,nsims1),
     &	 (izval(i),i=1,nsims1),
     &	 (elmval(i),i=1,nsims1),
     &	 ((elmset(i,j),i=1,nset),j=1,nsims1),
     &   ix2,iy2,iz2,(nevals(i),i=1,nsims1)
   
	 else if(iver1.eq.106) then
	      read(unit=10,rec=1) idum,
     & 	 nsims1,idum,idum,
     &	 idum,cdum3,idum,
     &	 rdum4,cdum1,cdum1,cdum2,cdum2,idum,idum,idum,idum,
     &	 ldum,rdum,rdum,rdum,rdum,idum,cdum2,
     &(thsav2(i),i=1,npar),(thtrue2(i),i=1,npars),(titlep2(i),i=1,npar),
     &	 ((thetval2(i,j),i=1,npar),j=1,nsims1),
     &	 ((conc(i,j),i=1,nlig),j=1,nset),
     &	 ((nintval2(i,j),i=1,nset),j=1,nsims1),
     &	 (ixval(i),i=1,nsims1),
     &	 (iyval(i),i=1,nsims1),
     &	 (izval(i),i=1,nsims1),
     &	 (elmval(i),i=1,nsims1),
     &	 ((elmset(i,j),i=1,nset),j=1,nsims1),
     &  	 ix2,iy2,iz2,(nevals(i),i=1,nsims1),
     &	 (ec50val(i),i=1,nsims1)

	   endif
	   CLOSE(unit=10)
	 endif
	 else
	  apfile=.false.
	 endif
	endif
c Add the current theta0 to thetval
c (written only as far as thetval(i,j) in iver=101
c  In ver 101, nsims was followed by 'ABORTW', but this was always overwritten
c    by current value of ABORTW -to avoid making array of ABORTW values, encode
c    info by setting current ixval() negative if run is ABORTWed
c Similarly ix1sav etc not written in iver=102 -values are in ixval()
	do i=1,npar
	   thetval(i,nsims)=theta0(i)
	enddo
	do i=1,nset
	   nintval(i,nsims)=nint(i)
	enddo
	ixval(nsims)=ix1sav
	if(ABORTW) ixval(nsims)=-ixval(nsims)	!code for ABORTWed run
	iyval(nsims)=iy1sav
	izval(nsims)=iz1sav
	elmval(nsims)=elmax
	nevals(nsims)=nev
	ec50val(nsims)=ec50out
c==	penval(nsims)=penfunc
c      iver=106
       iver=105
       OPEN(unit=10,file=simfile1,status='UNKNOWN',
     &	 access='DIRECT', form='BINARY',RECL=1)
      if(iver.eq.106) then
	
	write(unit=10,rec=1) iver,
     & nsims,npar,nset,
     & imods,mtitles,npars,
     & treso*1.e3,cDATEW,ctimew,mtitle1,simfile,k,kfit,imod0,nlig,
     & fixec50,ec50*1.d6,stpfac,confac,errfac,nevm,qfilem,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     & ((thetval(i,j),i=1,npar),j=1,nsims),
     & ((conc(i,j),i=1,nlig),j=1,nset),
     & ((nintval(i,j),i=1,nset),j=1,nsims),
     & (ixval(i),i=1,nsims),
     & (iyval(i),i=1,nsims),
     & (izval(i),i=1,nsims),
     & (elmval(i),i=1,nsims),
     & ((elmset(i,j),i=1,nset),j=1,nsims),
     &  ix1,iy1,iz1,(nevals(i),i=1,nsims),
     &  (ec50val(i),i=1,nsims)
      else
      write (unit=10,rec=1) iver,nsims,npar,nset,imods,mtitles(1:40),
     & npars,treso*1.e3,cDATEW,ctimew,mtitle1,simfile,
     & k,kfit,imod0,nlig,fixec50,ec50*1.d6,stpfac,confac,errfac,nevm,
     & (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),
     & ((thetval(i,j),i=1,npar),j=1,nsims),
     & ((conc(i,j),i=1,nlig),j=1,nset),
     & ((nintval(i,j),i=1,nset),j=1,nsims),(ixval(i),i=1,nsims),
     & (iyval(i),i=1,nsims),(izval(i),i=1,nsims),
     & (elmval(i),i=1,nsims),((elmset(i,j),i=1,nset),j=1,nsims),
     & ix1,iy1,iz1,(nevals(i),i=1,nsims)
 
      endif
  
	CLOSE(unit=10)
c
99	apfile=.false.	!after first time
c
	RETURN
	end

