	subroutine QCONVERT(qfilem)
c To find and read the old rate and mechanism files, qmodel.dat and qdat.dat,
c and convert them to a single QMECHS.DAT file that can be read with GETQD
C THIS IS SIMPLIFIED VERSION THAT WRITES ALL RECORDS IN SAME ORDER AS
C IN QDAT.DAT, EVEN IF QD IS INCOMPLETE, AND KEEPS MODEL NUMBERING
C UNCHANGED (SO PROG.INI SHOULD WORK) [VERSION OF THIS THAT COULD
C SKIP RECORDS NOW IN QCONVER0.FOR]
c
c Modif 02/01/02 05:52am to write qmechs as iver=102 from old files
c
c______________________________________________________________
c New QMECHS.DAT format for storing models and rate constants
c_______________________________________________________________
c Note how 2D array must be written/read if one endex depends on the other
c (see t2Dwrite.for in \ekdist-new) write(19,1) ((jmat(i,j),1=1,n(j)),j=1,n)
c
c (1) Model and rate constants are kept in the same file

c (2) for number of states> 11 x 18 the character model is not kept, but
c	when it is kept, it is repeated for each set of rates for that
c	model, rather than being in a separate file

c (3) QMECHS.dat is transparent file, which starts with index. This cannot
c	be expanded so make it big enough for say 100 models/500 rate constant
c	data sets

c (4) Need addition to cope with independent subunit type models, in which
c	the same basic rate constant appears several times in Q.
c
c	indmod=F for all old models (npar=nrateq=2*ncon)
c	indmod=T when npar < nrateq=2*ncon. In this case must have nsetq>0
c	   so ie(), je(), if(), jf(), efac() can be used to get rest of Q(i,j)
c	Say npar=number of basic rates constants -names in titlep(i), i=1,npar
c	    nrateq=number of non-zero rates in Q(i,j)=2*ncon (always)
c	Normally npar=nnrateq, and nsetq=0, but when indmod=T then npar<nrateq
c	and must have
c	   nsetq=ntrateq-npar
c 	constraints set to define the rest of the Q(i,j)
c	NB constraints are not normally part of the model, and are present
c	in GETQD only for use in HJCFIT where the are used in fitting
c	In contrast when INDMOD=true then the constraints are used
c	to define the model itself
c	In this case need a separate array, IQ say (npar x npar) to
c	indicate which q(i,j) are defined in the basic npar rate constants
c	(whereas IQQ() now define position of ALL q(i,j))
c
c (5) QMECHS.DAT starts with 'header', written starting at record 1
c	iver = version number =101 initially
c	nrecs = number of records of model/rates stored in the file
c	nextrec=next vacant record (byte) # = jstart() value for next record
c		(need to count everything written to calculate this each time)
c	ireclast=number of record used in last run of any theory prog
c	  (as previously in qgen.ini) -may be overwritten by value from prog.ini)
c	nmods=number of different models (.le. nrecs)
c	character*74 mtits(100)	!mtits(i)=title for model #1 (whole array written
c	   =14800 bytes)
c	jstart(500)=start byte # for storage of the ith record (2000 bytes)
c	   whole array written)
c	   NB in old qdat.dat, jmod() indicated the model used in each record
c	   which must mow be found by reading each record.  There was no need
c	   for jstart() in old qdat.dat because record lengths were fixed
c  Total of these = 4+4+4+4+14800+2000 = 16816 bytes
c  Start writing the records at byte # = 20000 (leaves gap for expamsion of header)
c  so jstart(1)=20000
c
c (6) Record number i, which starts at byte # = jstart(i) contains the following
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
c	nsetq                                                         4
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

c Other things (imod0, conc, nchan etc) all go in .ini for each program
c
	logical present
	integer inew(10),inew1(100)
	character qfile*40,qfile1*40,qfilem*40,rfile*40
	character*40 qfile2
	character qpath*30,qname*8,suffix*3,ndev*2

c Declarations for old qmodel/qdat (add 0 to name if not same as for new)
c No need for new arrays, since the only bits that are written to qmechs.dat
c are those actually defined. Need new arrays only for those for which
c whole array is written in header bit, mtits(100) and jstart(500)
	REAL*8 QD(10,10),Pinf(10),P0(10),dgamma(10),dgamma1(10)
	character*2 charmod(20,30)	!to print model
	real*4 pstar(4)
	character*1 ans
	character*8 titles(10)
	character*10 titlep(50)
	real*8 QT(10,10),r
	real*8 pzero(10)		!not used
	integer kmcon(9),IV(10),JV(10),IX(10),JX(10)
	integer NSC(8),IM(8,15),JM(8,15),IL(10),IC(2,20)
	real*4 HPAR(10)
	character*74 rtitles(500)	!title to describe rate constants
c
	logical vjump,cjump,kmfast
	character*74 mtitle	!title for model
c	character*74 mtits(41)	!to read all ditto from disc
	character*74 mtits(100)	!to read all ditto from disc
c	integer jmod(40)
	integer jmod(100)
c	LOGICAL slock
c	logical debug,caplock
	logical discprt
c Addition for getbound
c	integer nbound(10,2)
	integer nbound(100,10)
	common/nbnd/nbound
c Declarations for new qmechs.dat
	allocatable::irate,jrate,jstart
	integer irate(:),jrate(:),jstart(:)
	character*74 rtitle		!title to describe rate constants
	logical boundef,chardef,indmod
	integer ieq(10),jeq(10),ifq(10),jfq(10)
	real*4 efacq(10)
	character*20 ligname(10)
	character*20 lignams(10,100)
	character*10 statname(100)
	character*11 cnum
	real*4 conc(10)
c
	common/dp/discprt
c
	do i=1,10
	   do j=1,100
		lignams(i,j)='                   '
	   enddo
	enddo
c Allocate arrays for writing the new qmechs.dat
	ALLOCATE(irate(200),jrate(200),jstart(500))
c
c Locate files
	qfile='QMODEL.DAT'
22	INQUIRE(file=qfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   qfile='\QMODEL.DAT'
	   INQUIRE(file=qfile,exist=present,flen=nlen)
	   if(.not.present.or.nlen.eq.0) then
		print 21
21	   	format(
     & ' File QMODEL.DAT not found in local directory or root')
	      call TITENT0('Path for QMODEL.DAT:',qfile,40,.false.)
		goto 22
	   endif
	endif
	call PARSNAME(qfile,qpath,ndev,qname,suffix,nopath,40)
	qfile1=charnb(qpath)//'QGEN.INI'
23	INQUIRE(file=qfile1,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   print 210,qfile1
210	   format(' QGEN.INI not found in: ',a40)
	   call TITENT0('Path for QGEN.INI:',qfile1,40,.false.)
	   goto 23
	endif
	qfile2=charnb(qpath)//'QDAT.DAT'
24	INQUIRE(file=qfile2,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   print 211,qfile2
211	   format(' QDAT.DAT not found in: ',a40)
	   call TITENT0('Path for QDAT.DAT:',qfile2,40,.false.)
	   goto 24
	else
	   nlendat=nlen
	endif
	print 13
13	format(
     & ' Old style data files are present.',/,
     & ' Convert them to new qmechs.mec format [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(ans.eq.'N') RETURN
c Open qgen.ini
      OPEN(unit=17,file=qfile1,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(17,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=17)
	ireclast=jreclast		!new notation 01/17/01 09:20am
c If the wrong qgen.ini is present, the number of records in qgen.dat, which
c is recorded ONLY in qgen.ini (!!), may be wrong, and if it is too big
c will cause 'read beyond end of record' error below, so check here
	nr1=nlendat/4096		!should be exact number
	if(nrecq.gt.nr1) then
	   call BELL(1)
	   print 7,nrecq,nr1
7	   format(
     & ' According to QGEN.INI the number of files in QDAT.DAT',/,
     & ' should be ',i5,' but only ',i5,' found')
	   nrecq=nr1
	   pause
	endif
c Read all titles for sets of rate constants initially here (otherwise
c reading rtitle() involves reading also QT, nchan etc so any values of these
c set here may be overwritten
c Array nbound(100) added to QDAT, and ibflag as indicator of whether
c valid values for nbound have been put into qdat.dat yet -set ibflag=1001
c if nbound() has been defined
c Now change ibflag to boundef=true if nbound is defined
      OPEN(unit=18,file=qfile2,status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	do i=1,nrecq
	   jrecq=2*i	!read SECOND record of data
	   read(18,rec=jrecq) QT,Pinf,nmod,imod1,vref,Vkin,PZERO,PSTAR,
     &      KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,dgamma,rtitles(i),
     &	ibflag,nbound
	enddo
	CLOSE(unit=18)
c   but reset ibflag to 0 here (interested in value ONLY for the rates actually used)
	ibflag=0
c In F90/Gino get problems with crazy undefined values in kmcon, pstar,
c when kmfast=false (it reads as false under debugger, but program does
c not work as though it was false!!).  Put in catch here
	if((pstar(1).gt.1.).or.(pstar(1).lt.0.)) then
	   kmfast=.false.
	   do i=1,4
		pstar(i)=1.
	   enddo
	   do i=1,9
		kmcon(i)=2
	   enddo
	endif
c
c If get to here then old files all found, so now read them and convert
c to the new format qmechs.dat.  Do loop for each of the rate records in
c qdat.dat, and for each read relevant qmodel.dat record to get what is
c needed to write qmechs.dat
c
c Open qgen.ini
c      OPEN(unit=17,file=qfile1,status='UNKNOWN',
c Open qdat.dat
      OPEN(unit=18,file=qfile2,status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
c Open qmodel.dat
      OPEN(unit=14,file=qfile,status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=3072)
c
c Start loop for each record in qdat.dat
c
	nextrec=20000	!byte # to start writing first data rec in qmechs.dat
c
	IRQ=0	!count separately from loop variable in case a record is skipped
c
	do irq1=1,nrecq
c
c    Read QDAT
	   jrq=2*irq1-1
	   read(18,rec=jrq) QD,Pinf,K,ka,kb,kc,kd,nmod,imod0,mtitle,
     &    xa,xa,xb,xb,nlig,P0,c0,dgamma,kmfast,pstar,vhold,vkin,
     &    nchan,vjump,cjump,titles,ncdep,IL,IX,JX
	   read(18,rec=jrq+1) QT,Pinf,nmod,imod00,vref,Vkin,PZERO,PSTAR,
     &    KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,dgamma1,rtitle,
     &    ibflag,nbound
c In some qdat.dat files, nvdep and dgamma seem to be corrupted so
c check former and take dgamma from 1st record. Also vref, vkin may
c be corrupted
c Also in some qdat.dat seem to have imod0=0
	if(imod0.eq.0) then
10	   call BELL(1)
	   print 1,irq1
1	   format(/,' ERROR: record #',i3,' has model number 0',/,
     &   ' (1) Specify correct model number',/,
     &   ' (2) Skip this record',/,
     &   ' Option number [2] = ')
	   iopt=2
	   call INPUTi(iopt)
	   if(iopt.eq.1) then
		print 11,mtitle
11		format(' Model title:',/,
     &	2x,a74,/,
     &	' Give correct number for this model: number = ')
		call INPUTi(imod0)
	   else if(iopt.eq.2) then
		print 12,irq1
		if(discprt) write(8,12) irq1
12		format(/,
     &  ' RECORD #',i3,' from QDAT.DAT had model number = 0: SKIPPED',/)
		goto 99	!go to end of irq1 loop without updating irq
	   else
		goto 10
	   endif
	endif
c
c Update IRQ only if NOT skipped
	IRQ=IRQ+1
c     Clean up
	   nchan=1
	   if(nvdep.lt.0.or.nvdep.gt.k) nvdep=0
	   call CHECKV(vref,-80.)
	   call CHECKV(vkin,-80.)
	   call CHECKV(vhold,-80.)
	   kmfast=.false.
c	   if((pstar(1).gt.1.).or.(pstar(1).lt.0.)) then
		kmfast=.false.
		do i=1,4
		   pstar(i)=1.
		enddo
		do i=1,9
		   kmcon(i)=2
		enddo
c	   endif
	   do i=kA+1,10
		dgamma(i)=0.d0
	   enddo
c Set values for new file
	   conc(1)=xA
	   conc(2)=xB
c only QT needed (?). Remove conc from QD
	   if(ncdep.gt.0) then
		do i=1,k
		   do j=1,k
			r=QD(i,j)
			if(r.gt.1.d-25.and.i.ne.j) then
			   do n=1,ncdep
				if(ix(n).eq.i.and.jx(n).eq.j) then
				   x=conc(il(n))
				   r=r/x
				endif
			   enddo
			endif
		   enddo
		enddo
	   endif
c        Set diagonals to zero (not defined until conc specified)
	   do i=1,k
		QD(i,i)=0.0d0
	   enddo
c Now read model stuff for model imod0 from qmodel.dat
         OPEN(unit=14,file=qfile,status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=3072)
	   read(14,rec=1) nmodc
	   if(nmodc.gt.41) then	!look for qtitles.dat
      	OPEN(unit=16,file='QTITLE.DAT',status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=10240)
		read(16,rec=1) nmodc,(mtits(i),i=1,nmodc)
		CLOSE(unit=16)
	   else
		read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
	   endif
	   read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     &    ncyc,nsc,im,jm,kA1,kB1,kC1,kD1,titlep,ncon
	   close(unit=14)
c       Check values here against those from qdat.dat
	   k1=kA1+kB1+kC1+kD1
c
c All in now so construct everything for qmechs.dat
c	   iver=101
	   iver=102			!with mtitle inj each record
	   nrecs=nrecq		!# of sets of rate constants
c	   nextrec=nextrec
	   nmods=nmodc		!# of different models
	   imod=imod0		!for write
c      end of 'header' section -now data
c	   rtitle=rtitles
	   chardef=.true.		!all old models were defined in charmod form
	   boundef=ibflag.eq.1001	!true if nbound has been defined
	   print 26,irq,jrq,jrq+1,imod0,mtits(imod0)
	   if(discprt) write(8,26)
     &	irq,jrq,jrq+1,imod0,mtits(imod0)
26	   format(/,/,
     * ' Record # ',i3,' (=records ',i3,' and ',i3,' in qdat.dat)',/,
     & ' Model # ',i4,/,1x,a74)
c     give state names
	   print 672,mtits(imod)
c672	   format(1x,a74)
	   do i=1,ilast
		print 671, (charmod(i,j),j=1,jlast)
c671	      format(4x,30a2)
	   enddo
	   kstat=k		!number of basic states (=k except for indep models)
	   do i=1,kstat
		call INTCONV(i,cnum)
	      call TITENT0('Give name (eg A2R) for state # '//
     &	 charnb(cnum)//':',statname(i),10,.false.)
	   enddo
	   if(nlig.gt.0) then
		print 672,mtits(imod)
672		format(1x,a74)
		do i=1,ilast
		   print 671, (charmod(i,j),j=1,jlast)
671	         format(4x,30a2)
		enddo
		do i=1,nlig
		  if(irq.gt.1) then
			ligname(i)=lignams(i,imod0)	!default
		  endif
		  call INTCONV(i,cnum)
	        call TITENT0('Give name for ligand # '//charnb(cnum)//':',
     &	  ligname(i),20,.false.)
		  lignams(i,imod0)=ligname(i)	!default for model #=imod0
		enddo
	   endif
c Now make irate(), jrate() from QD -better done via IC() and IQ() as in getqd,
c and IC() comes from charQ
	   call CHARQOLD(charmod,ilast,jlast,kA,kF,ncon,ic)	!get IC,kA,kF
c	   do i=1,10		!ditto
c		do j=1,10
c		   IQ(i,j)=0
c		enddo
c	   enddo
	   i1=0
	   do m=1,ncon
		i=IC(1,m)
		j=IC(2,m)
		i1=i1+1
		irate(i1)=i
		jrate(i1)=j
c		IQ(i,j)=i1
		i1=i1+1
c		IQ(j,i)=i1
		irate(i1)=j
		jrate(i1)=i
	   enddo
	   jmod(irq)=imod
c
31	   continue
	   jstart(irq)=nextrec	!start byte # for this record
	   indmod=.false.		!no independent models is old qmodel.dat
	   nsetq=0			!no independent models is old qmodel.dat
	   nsub=0
	   kstat0=0
	   npar0=0
	   kcon=0
	   npar1=0
	   ncyc0=0
	   nrateq=2*ncon	!number of rate constants in model=2*ncon
	   npar=nrateq 	!since indmod=F
c Add up number of bytes written and update nextrec so value in qmechs.dat
c is where write should start next time a new model/rates is defined
c 	   Calc isum=SUM[nsc(i)]
	   isum=0
	   do i=1,ncyc
		isum=isum + nsc(i)
	   enddo
c==check by writing single record
	   nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &    8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat
     &    +6*4
c Update start record for next write
c
	   nrecs=irq
	   qfilem='QMECHS.mec'
         OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)
c	   write(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
	   write(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart			!iver=102
c	  Write data record
	   nextrec=nextrec + nbytes + 256 	!leave gap of 256 bytes
	   irec=jstart(irq)	!previous nextrec
	   write(unit=15,rec=irec) iver,
     &	imod,mtitle,k,kA,kB,kC,kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat,
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
     &	(statname(i),i=1,kstat),
     & 	nsub,kstat0,npar0,kcon,npar1,ncyc0
c
	   CLOSE(unit=15)
	   print 34,irq,jstart(irq),jstart(irq)+nbytes-1
	   if(discprt) write(8,34) irq,jstart(irq),jstart(irq)+nbytes-1
34	   format(' Record # ',i3,' written to qmechs.mec, bytes ',
     &	i11,' to ',i11)
c
c
99	   continue		!jump here if record skipped
	enddo		!end of loop for all sets of rate constants
c
c Print what was done
	print 30,nrecs,nmods,nextrec
	if(discprt) write(8,30)nrecs,nmods,nextrec
30	format(/,
     & ' The old files that defined models and rates (qgen.ini,',/,
     & ' qmodel.dat and qdat.dat) have been converted into a single',/,
     & ' new file QMECHS.MEC, which contains definitions of',i4,/,
     & ' different models and ',i4,' sets of rate constants.',/,
     & ' Next vacant record for write to QMECHS.MEC = byte ',i11)
	pause
c
c
c If renum.dat is also present, convert it to new bigger file
   	rfile=charnb(qpath)//'RENUM.DAT'
25	INQUIRE(file=rfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   print 212,rfile
212	   format(' RENUM.DAT not found in: ',a40)
	   goto 999		!abandon
c	   call TITENT0('Path for RENUM.DAT:',rfile,40,.false.)
c	   goto 25
	endif
      OPEN(unit=14,file=rfile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=128)
	read(14,rec=1) imodnew,kA,kB,kC,kD,inew
      CLOSE(unit=14)
	do i=1,100
	   inew1(i)=0
	enddo
	do i=1,10
	   inew1(i)=inew(i)	!copy original
	enddo
c Open with larger record size
	call RENAME('renum.dat','renum0.dat')
      OPEN(unit=14,file=rfile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
	write(14,rec=1) imodnew,kA,kB,kC,kD,inew1
      CLOSE(unit=14)
	print 2121
	if(discprt) write(8,2121)
2121	format(/,
     &' File RENUM.DAT, for renumbering states, converted from',/,
     &' 128 bytes (renamed renum0.dat) to 512 bytes')
c
999	continue
	DEALLOCATE(irate,jrate,jstart)
	RETURN
	end

	subroutine CHECKV(v,vdef)
c To test for corrupted values. If values looks bad it is set to vdef(ault)
c Values like 0.2e-44 give problems!
	if((v.lt.-250.).or.(v.gt.250.)) v=vdef
	if((v.ne.0.0).and.(abs(v).lt.1.e-35)) v=vdef
	RETURN
	end

