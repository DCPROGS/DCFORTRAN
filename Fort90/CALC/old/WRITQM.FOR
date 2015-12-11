	subroutine WRITQM(imod0,qfilem,qfilout,altmod,jmod,
     & mtitle,mtits,k,kA,kB,kC,kD,rtitle,rtitles,
     & iver,nrecs,nextrec,ireclast,jstart,irecsav,irecqsav,
     & ilast,jlast,nrateq,ncon,ncdep,nlig,chardef,
     & boundef,ncyc,vref,nvdep,kmfast,indmod,npar,nsetq,kstat,
     & charmod,irate,jrate,QT,titlep,ligname,nbound,
     & IX,JX,IL,dgamma,nsc,IM,JM,IV,JV,hpar,pstar,kmcon,
     & ieq,jeq,ifq,jfq,efacq,statname,
     & nsub,kstat0,npar0,kcon,npar1,ncyc0,idest)
c

c To store a record in qmechs.dat in theory progs and hjcfit.
c needs all of the following for iver=102
c No commoms here -all parameter of subroutine
c Called with altmod=true if the model is a newly defined one -in this case
c a new model number is needed.
c On entry
c	irecsav=jstart(irecq)	!start byte for data for last run
c	irecqsav=irecq		!record # for data for last run
c	mtitle=title for the model (but title for rates read in here)
c Return with idest=101 to try again
c
c Modified 01/02/03 03:51pm so that independent models store also
c  nsub=number of subunits
c  kstat0=number of states per subunit (kstat=nsub*kstat0)
c  npar0= Number of rate constants within each subunit (npar=nsub*npar0)
c  kcon = Number of concerted states
c  npar1 =  Number of rate constants to/from/within concerted states
c  ncyc0 = Number of cycles within each subunit
c   (total number of cycles=ncyc0*nsub + ncyc1)
c NB kstat is replaced by kstat1=kstat+kcon in case of independent
c models with concerted states added, because this is the number of
c statname(i) to be read/written
c
c Record 1 iver, nrecs,nextrec,ireclast,jstart
c Record number i, which starts at byte # = jstart(i) contains the following
c     In general put the scalars at start, arrays later
c											bytes
c	iver =version number again						4
c 	imod = model number for this record					4
c	***mtitle (in iver=102 . . . ) 					74
c	k, kA, kB, kC, kD								20
c	rtitle*74=title for rates (need to read all into array)	74
c	ilast, jlast=row and col number for charmod (=1 if charmod
c		does not exist, e.g. for 3D models)				8
c	nrateq=number of rate constants=2*ncon				4
c	ncon                                                        4
c	ncdep										4
c	nlig										4
c     chardef (T if charmod define)						4
c	boundef (T if nbound defined)                               4
c	ncyc										4
c	vref=potential at which rate constants are specfied		4
c	nvdep                                                       4
c	kmfast									4
c	indmod									4
c	npar                                                        4
c	nsetq                                                       4
c    now arrays                                                   4
c	((charmod(i,j),i=1,ilast),j=1,jlast)				4*ilast*jlast
c	(irate(i),i=1,nrateq)							4*nrateq
c	(jrate(i),i=1,nrateq)							4*nrateq
c	(QT(irate(i),jrate(i)),i=1,nrateq)					8*nrateq
c	(titlep(i),i=1,npar)							10*npar
c     (ligname(i),i=1,nlig)							20*nlig
c	((nbound(i,j),i=1,k),j=1,nlig)					4*k*nlig
c	(IX(i),i=1,ncdep)								4*ncdep
c	(JX(i),i=1,ncdep)								4*ncdep
c	(IL(i),i=1,ncdep)								4*ncdep
c	(dgamma(i),i=1,kA)							8*kA
c	(nsc(i),i=1,nyc)								4*ncyc
c	((im(i,j),j=1,nsc(i)),i=1,ncyc)					4*ncyc*SUM[nsc(i)]
c	((jm(i,j),j=1,nsc(i)),i=1,ncyc)					4*ncyc*SUM[nsc(i)]
c	(iv(i),i=1,nvdep)								4*nvdep
c	(jv(i),i=1,nvdep)                                           4*nvdep
c	(hpar(i),i=1,nvdep)                                         4*nvdep
c	(pstar(i),i=1,4)                                            4*4=16
c	(kmcon(i),i=1,9)								4*9=36
c	(ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),				2*4*nsetq
c	(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),                      2*4*nsetq
c	(efacq(i),i=1,nsetq)                                        4*nsetq
c     (statname(i),i=1,kstat)							10*kstat
c
	real*8 QT(100,100),dgamma(100)
	character*2 charmod(25,40)	!to print model
	real*4 pstar(4)
	character qfilem*40,qfilout*40
	logical samefil,present,altmod
	integer jmod(500)
	integer jstart(500)
	character*10 titlep(200)
	integer kmcon(9),IV(100),JV(100)
	real*4 HPAR(100)
	integer IX(100),JX(100)
	integer  NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
c	integer IE(200),JE(200),IF(200),JF(200)
c	real*4 EFAC(200)
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	real*4 efacq(200)
	integer IL(100)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle,rtitle1,rtitles(500)	!title to describe rate constants
	character*74 mtits(100)	!to read all ditto from disc
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod
c
	logical kmfast
	character*1 ans
	character*74 mtitle,mtitle1		!title for model
c	logical debug,caplock
	logical discprt
	common/dp/discprt
	integer nbound(100,10)
c
	idest=0
	iver=102
	imod=imod0
c   Calculate number of bytes needed
c	Calc isum=SUM[nsc(i)]
	isum=0
	do i=1,ncyc
	   isum=isum + nsc(i)
	enddo
	if(.not.indmod) then
	   kstat1=kstat
	else if(indmod) then
	   kstat1=kstat+kcon		!number of names to be read/written
	endif
	nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &    8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat1
     &    +6*4
c
c     &	qfilem,40,.false.)
	qfilout=qfilem
	call TITENT0('Name/path of file to save to:',
     &	qfilout,40,.false.)
	samefil=qfilout.eq.qfilem
      call TITENT0(' Enter title for rates',rtitle,74,.false.)
	if(.not.samefil) then
         INQUIRE(file=qfilout,exist=present,flen=nlenm)
	   if(.not.present.or.nlenm.eq.0) then
		print 101,qfilout
101		format(' Could not find ',a40,/,' Create it [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') then
c		   goto 100
		   idest=101
		   goto 99
		else
		   nrecs=0
		   do i=1,500
			jstart(i)=0
		   enddo
		   jstart(1)=20000
		   irec=20000
		   nextrec=20000
		   ireclast=irec
		   rtitles(1)=rtitle
		endif
 	      OPEN(unit=15,file=qfilout,status='UNKNOWN',
     &	access='TRANSPARENT')
		write(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
		CLOSE(unit=15)
	   endif
	endif
c
      OPEN(unit=15,file=qfilout,status='UNKNOWN',
     &	access='TRANSPARENT')
	read(unit=15,rec=1) iver1
	if(iver1.eq.101) then
	   call CONVQM(qfilout)		!converts v101 to v102
         OPEN(unit=15,file=qfilout,status='UNKNOWN',
     &	access='TRANSPARENT')	!re-open after convqm
	endif
	read(unit=15,rec=1) iver1,
     &	nrecs,nextrec,ireclast,jstart
c If output qmechs is not the same as the input, need to check
c that the current model #, imod=imod0, does not already exist
c in the output file to denote a different model
c If this is a new model (altmod=T), then need to give it a unique number,
c whether qmechs file is same or different
	if(altmod.or.(.not.samefil)) then
	   read(unit=15,rec=1) iver1,
     &	nrecs,nextrec,ireclast,jstart
c	Read part of data record
c	Redefine jmod for the output file
c     Do not overwrite the mtitle and rtitle already defined for the
c      data to be outputted
	   do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=15,rec=irec) iver1,
     &	imod1,mtitle1,k1,kA1,kB1,kC1,kD1,rtitle1
	      jmod(ir)=imod1
		mtits(imod1)=mtitle1	!so mtits(i)=title of model #i
		rtitles(ir)=rtitle1
	   enddo
c   Look for lowest unused model number in qfilout
	   im2=1
81	   do i=1,nrecs
		if(im2.eq.jmod(i)) then
		   im2=im2+1
		   goto 81	!repeat
		endif
	   enddo
	   if(altmod) then
	     imod=im2
		print 83,im2,imod
83		format(
     &  ' The lowest unused number is ',i3,/,
     &  ' Give a model number for the new mechanism [',i3,'] = ')
	     call INPUTi(imod)
	   endif

84	   do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=15,rec=irec) iver1,
     &	  imod1,mtitle1,k1,kA1,kB1,kC1
		kF=kB+kC
		kF1=kB1+kC1
		if(imod.eq.imod1) then 		!current number occurs
		   if(mtitle1.ne.mtitle.or.kA1.ne.kA.or.kF1.ne.kF) then
			print 80,qfilout,imod,mtitle1,kA1,kF1,imod,im2
80			format(
     &  ' In qmechs.dat file, ',a40,/,
     &  ' mechanism # ',i4,' already exists but appears to denote',/,
     &  ' a different mechanism with title',/,2x,a74,/,
     &  ' and ',i2,' open states, + ',i3,' shut states.',/,
     &  ' To write to this file the present mechanism must be given',/,
     &  ' a different number.',/, /,
     &  ' Change the number from ',i3,' to the ',/,
     &  ' lowest unused number, i.e. ',i3,' O.K. [Y] ? ')
			ans='Y'
			if(ans.eq.'Y') then
			   imod=im2
			else
			   print 85,imod,im2
85			   format(
     & ' Present mechanism # = ',i3,': change it to # [',i3,'] = ')
			   imod=im2
			   call INPUTi(imod)
			   goto 84		!check if new imod is OK
			endif
		   endif
		endif
	   enddo
	endif		!end of if(.not.samefil)
	CLOSE(unit=15)
c RE-SAVE model/rates
c In qmechs.dat, all details of every model are kept in each record that follows
c the header bit (iver,nrecs,nextrec,ireclast,nmods,mtits,jstart), so can't
c easily overwrite a model - can remove it from mtits(), but its details will
c still be in the record that holds imod, charmod, rtitle etc.  Since each
c record no longer occupies the same amount of disc space, it would not be easy
c to remove a record and replace it with another.  Could just set imod=-1
c for records in which model has been overwritten by a different one, but
c probably not worth the trouble
c
c Now much simpler -model and rates just written to byte #nextrec
c (jstart and nextrec updated), or not written at all. Only other option
c is to overwrite the last record.
c BUT COULD OVERWRITE ANY RECORD FOR THE SAME MODEL (+SAME NUMBER OF LIGAND,
C SAME nvdep etc) SO SPACE OCCUPIED IN DISC WAS SAME -ONLY RATES DIFFER
c (CAN CHECK via JSTART WHETHER THERE IS ROOM FOR IT)
	iopt=1
82	print 7,iopt
7	format(/,
     & ' (1) Write data starting at next vacant record in qmechs.dat',/,
     & ' (2) Overwrite the last record in qmechs.dat',/,
     & ' (3) Replace existing record',/,
     & ' (4) Overwrite an earlier record in qmechs.dat',/,
     & ' (5) Abandon re-saving of data',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.5) goto 82
	if(iopt.eq.5) goto 99
c
	if(iopt.eq.1) then
	   nrecs=nrecs+1
	   irec=nextrec		!where to write the record
	   ireclast=irec
	   jstart(nrecs)=irec
	   rtitles(nrecs)=rtitle
	   nextrec=irec + nbytes + 1024 	!leave gap of 256 bytes
	else if(iopt.eq.2) then 		!overwrite last record
	   irec=jstart(nrecs)	!nrecs not changed
	   ireclast=irec
	   rtitles(nrecs)=rtitle
	   nextrec=irec + nbytes + 1024	!re-calculate nextrec
	else if(iopt.eq.3) then 		!replace existing record
c NB    can only replace existing record if length not increased by enough
c	  to overwrite next record: irec,ireclast,nextrec all unchanged
	   irec=irecsav
	   irecq=irecqsav
	   if(irecq.lt.nrecs) then
	    if(irec+nbytes.ge.jstart(irecq+1)) then
		call BELL(2)
		print 136,irec+nbytes,irecq+1,jstart(irecq+1)
136		format(
     &   ' Data would extend to byte # ',i11,' which would overwrite',/,
     &   ' start of record ',i4,' which starts at byte # ',i11,/)
		goto 82
	    endif
	   endif
	else if(iopt.eq.4) then 		!overwrite earlier record
c NB    can only replace existing record if length not increased by enough
c	  to overwrite next record
1371	   print 137,nrecs
137	   format(' Overwrite record number (1-',i3,') = ')
	   call INPUTi(irq)
	   if(irq.lt.1.or.irq.gt.nrecs) goto 1371
	   print 138,rtitles(irq),jmod(irq),mtits(jmod(irq))
138	   format(
     &   ' This will overwrite record',/,
     &    1x,a74,/,
     &   ' for mechanism number ',i3,':',/,
     &    1x,a74,/,
     &   ' Is this OK [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') goto 82
	   irec1=jstart(irq)
	   if(irq.lt.nrecs) then
		print 140,nrecs,irq
140		format(
     & ' Make this the last record, so total number of records ',/,
     & ' reduced from ',i4,' to ',i4,' [Y]  ? ')
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   nrecs=irq
		else if(irq.lt.nrecs) then
		   if(irec1+nbytes.ge.jstart(irq+1)) then
			call BELL(2)
			print 136,irec1+nbytes,irq+1,jstart(irq+1)
c136			format(
c     &   ' Data would extend to byte # ',i11,' which would overwrite',/,
c     &   ' start of record ',i4,' which starts at byte # ',i11,/)
			goto 82
		   endif
		endif
	   endif
c       OK to overwrite so set values
	   irecq=irq
	   irec=irec1
	   ireclast=irec
	   rtitles(irq)=rtitle
	   if(irecq.eq.nrecs) then	!otherwise nextrec unchanged
	      nextrec=irec + nbytes + 1024 	!leave gap of 1024 bytes
	   endif
	endif
c
c802	continue
c For new model, print model # and charmod here
	if(discprt) write(8,670)mtitle,imod
670	format(/,1x,a74,/,' Model number = ',i3,/)
	if(chardef) then
	  do i=1,ilast
	   if(discprt) write(8,671) (charmod(i,j),j=1,jlast)
671	     format(4x,35a2)
	  enddo
	else
c=======
	endif
c
      OPEN(unit=15,file=qfilout,status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)
	iver=102
c	write(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart	!iver=101
	write(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart		!iver=102
c	  Write data record

	write(unit=15,rec=irec) iver,
     &	imod,mtitle,k,kA,kB,kC,kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat1,
     &	((charmod(i,j),i=1,ilast),j=1,jlast),
     &	(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),
     &	(QT(irate(i),jrate(i)),i=1,nrateq),
     &	(titlep(i),i=1,npar),
     &      (ligname(i),i=1,nlig),
     &	((nbound(i,j),i=1,k),j=1,nlig),
     &	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
     &	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
     &	(hpar(i),i=1,nvdep),
     &	(pstar(i),i=1,4),(kmcon(i),i=1,9),
     &	(ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),
     &	(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),
     &	(efacq(i),i=1,nsetq),
     &	(statname(i),i=1,kstat1),
     & 	nsub,kstat0,npar0,kcon,npar1,ncyc0
c
	CLOSE(unit=15)
      print 9,nrecs,qfilout,imod,mtitle,rtitles(nrecs),
     &    nbytes,jstart(nrecs),nextrec
	if(discprt) write(8,9) nrecs,qfilout,imod,mtitle,rtitles(nrecs),
     &    nbytes,jstart(nrecs),nextrec
9	format(/,
     & ' Rates and mechanism written to record # ',i3,' in file',/,
     &  2x,a40,/,
     & '  for model # ',i3,/,2x,a74,/,
     & ' Title for this record is',/,2x,a74,/,
     & 3x,i10,' bytes written, starting at byte # ',i12,/,
     & ' Next vacant byte number = ',i12,/)
99	continue
	imod0=imod
	RETURN
	end

