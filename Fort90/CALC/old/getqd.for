	subroutine GETQD(QT,conc,nchan,dgamma,vkin,vhold,vref,
     &  npar,titlep,ligname,iflag,iprint,readini,irecq,kflag,idest)
c
c Subroutine for ALL theory progs to define Q matrix defined at concentrations
c in conc() for use in rest of program
c Many arrays in commons so those needed can be transmitted to calling prog
c
c Modified 01/17/01 10:26am for new large arrays, and data storage in qmechs.dat
c Note: xA, and xB in call replaced by conc(10)
c Note: GETQD now returns QT without conc (or diagonals), so rather than using
c QNEWC to change conc (only), use QSETD or QSETC to set whole Q matrix (does 
c not need the 'standard' conc, cA1, cB1, used in old version)
c
c==================
c Changes to be made in progs that use the new GETQD
c==================
c (1) Change all array sizes
c (2) Declare conc() and remove all xA,xb,cA,cb
c	For jumps will need conc0() etc
c (3) Replace all QNEWC with  QSETD?
c (4) NB GETQD now returns QT, i.e. Q matrix without conc (and without diagonals)
c 	and calling prog must use QSETD to set QD.
c 	IQ() is probably not needed at all, and not GETIJ() either -can use
c 	irate(m),jrate(m), m=1,npar, to identify the i,j for mth paramater
c 	(in case of independent models, this identifies the npar basic rates
c	and QSETD puts in the rest via constraints (nsetq,ieq,jeq,ifq,jfq)
c______________________________________________________________
c New QMECHS.DAT format for storing models and rate constants
c_______________________________________________________________
c Note how 2D array must be written/read if one index depends on the other
c (see t2Dwrite.for in \ekdist-new) write(19,1) ((jmat(i,j),1=1,n(j)),j=1,n)
c
c (1) Model and rate constants are kept in the same file

c (2) for number of states> 11 x 18 the character model is not kept, but
c	when it is kept, it is repeated for each set of rates for that
c	model, rather than being in a separate file
c
c (3) QMECHS.dat is transparent file, which starts with index. This cannot
c	be expanded so make it big enough for say 100 models/500 rate constant
c	data sets
c (4) Need addition to cope with independent subunit type models, in which
c	the same basic rate constant appears several times in Q.
c
c	indmod=F for all old models (npar=nrateq=2*ncon)
c	indmod=T when npar < nrateq=2*ncon. In this case must have nsetq>0
c	  so ieq(), jeq(), ifq(), jfq(), efacq() can be used to get rest of Q(i,j)
c     NB these work identically to neq, ie(), je(), if(), jf(), efac() but
c	 better keep the names separate
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
c=should different name be used for this sort of constraint, not nsetq, ie()?
c=does ncdep refer to numbering in theta(npar), or to whole nrateq() values
c=likewise for nvdep etc
c=normally nyc=0, unless their is cycle within an indep subunit?
c= (lots of cycles, but setting values via nsetq ensures that all obey mr)
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
c
c
c Other things (imod0, conc, nchan etc) all go in .ini for each program
c
c Modif 03/28/00 12:58pm so obeymr is set to be false if ncyc=0
c
c 01/11/99 08:44pm
c  May have problem with constraints, when GETQD is called in HJCFIT
c If model has been changed then constraints read form hjcfit.ini will
c be nonsense so set neq=0, temporarily, before calling qsetd, so rates
c that are type in are used to set QD.
c There can be a general problem when model is changed here, in that
c values of things like constraints may not be appropriate to the new model.
c  Therefore imodold added to common/model
c where imodold may be input e.g. from the .ini file for the program
c that calls getqd (=value of imod0 for last run) then define
c=== NB theory progs other than HJCFIT have not got imodold in the .ini
c file at present -may have to put it in or get problems, or else
c add imode parameter so getqd knows when it is being called from HJCFIT
c
c Definition of kflag parameter:
c kflag=0 to omit question about reallocation of kA,kB,kC,kD (e.g
c   these not needed in SCALCS) or
c kflag=1 to ask if realloc required.
c kflag=2 to use input values of kA etc (e.g. from .ini) and ignore
c   values from disk
c
c Call with idest=105 to go straight to bit that changes current params;
c  if idest=105 then value(s) of changed params are printed
c  (need to SAVE some values, or re-read them, in this case?)
c
c Modif 12/14/98 09:52am by adding re-ordering of titlep (to be tested!)
c Modif 12/14/98 08:55am by adding common/model/
c Modif 12/12/97 07:50am so looks for qmodel.dat etc in local directory if
c   not found in root
c Modif 09/20/97 05:06pm -nmod added to parameters -now removed again
c Modif 05/28/95 07:11pm -added common/qblk/IQ
c Modif 11/13/99 03:07pm
c Getbound returns nbound(i,j)=number of ligands if type j (j=1 or 2 at present)
c that are bound to state i (in common) -added common/LIG/
c
	REAL*8 QD(100,100),Pinf(100),P0(100),dgamma(100),peq(100)
c=	character*2 charmod(20,30)	!to print model
c=	character*2 cmodnew(20,30)	!for NEWMOD
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
	real*4 pstar(4)
	character*8 titles(100)
c  For parsname
c	character filnam*(*),path*30,pname*8,suffix*3,ndev*2,UC*1
c	logical nopath,nosuf
c Example: for filnam=c:\fortran\compress.for this routine returns
c pname=compress, suffix=for, path=c:\fortran\, ndev=C:
	character qfile*40,qpath*30,qname*8,suffix*3,ndev*2
	character qfile1*40,qfile2*40,rfile*40,qfilem*40
c Declarations for modif 03/05/95 06:16pm
	character*10 titlep(200),titlep1(200)
	integer SCRNBUF0(1024)
	integer IQ(100,100)
	integer inew(100)
	real*8 QT(100,100),r
	real*8 psav(200)
	real*8 pzero(100)		!not used
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
c=	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
c=	integer im1(8,15),jm1(8,15)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer im1(50,100),jm1(50,100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/LIG/nlig,IL(100)
	common/nmodel/cmodnew
	COMMON/qblk/IQ
c New arrays for qmech.dat
	integer jstart(500)
	character*20 ligname(10)
	character*74 rtitle,rtitles(500)	!title to describe rate constants
	integer IQQ(100,100)
	integer irate(200),jrate(200)
	integer irateq(200),jrateq(200)
	real*4 conc(10)
	logical boundef,chardef,indmod
c For qdefine
	logical readp
c
	logical vjump,cjump,kmfast,readini,present,renum,alter,newrate
	logical defrate
	character*1 ans,UC,ans1
	character*74 mtitle	!title for model
c	character*74 mtits(41)	!to read all ditto from disc
	character*74 mtits(100)	!to read all ditto from disc
	character string*74,pre*3
c===	integer jmod(40)
	integer jmod(500)
	LOGICAL pon,slock
c	logical debug,caplock
	logical discprt
	common/dp/discprt
c Addition for getbound
c	integer nbound(10,2)
	integer nbound(100,10)
	common/nbnd/nbound
c For inwindc
	character*79 heading,title
	character*11 cdata(50,3),cnum
c
c From eqoccd
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c
c Add common in case charmod etc needed in calling prog
c	SAVE QT,pinf,imod0,charmod,jlast,ilast,mtitle
	SAVE pinf
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	common/mod/imod
c
c
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	common/db/ideb		!debug level
	common/ren/renum		!so can be kept in .ini if req
c
	pon()=slock()
c	debug()=caplock()
c
	km=100				!array dimensions
	kmax=100
	if(kflag.eq.2) then   !save input values
	   kA1=kA
	   kB1=kB
	   kC1=kC
	   kD1=kD
	endif
c
	if(.not.readini) imodold=-1
c
	if(idest.eq.105) goto 105	!another run, same model
c
	imodsav=-1		!until defined
	newrate=.false.
	defrate=.false.
c First check whether the new model+rates file, QMECHS.DAT is present.
c If not call QCONVERT to look for old files and create qmwch.dat.
	qfilem='QMECHS.DAT'
22	INQUIRE(file=qfilem,exist=present,flen=nlenm)
	if(.not.present.or.nlenm.eq.0) then
	   qfilem='\QMECHS.DAT'
	   INQUIRE(file=qfilem,exist=present,flen=nlenm)
	   if(.not.present.or.nlen.eq.0) then
		print 21
21	   	format(
     & ' File QMECHS.DAT not found in local directory or root')
	      call QCONVERT(qfilem)
		chardef=.true.	!all old models defined in charmod() form
		goto 22
	   endif
	endif
	call PARSNAME(qfilem,qpath,ndev,qname,suffix,nopath,40)
c No need for qgen.ini either, ireclast is kept in qmechs.dat, and
c probably in prog.ini too
c
c	qfilem='QMECHS.DAT'
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c     Read header part
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c	Read part of data record, up to rtitle, to define rtitles() and jmod()
	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
	   read(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle
	   jmod(irq)=imod
	   rtitles(irq)=rtitle
	enddo
	nrecq=nrecs		!nrecq is old name -should be phased out
	nmodc=nmods		! old name -should be phased out
	CLOSE(unit=15)
	irec=ireclast		!until PROG.INI is read
	if(readini) then
	   irec=irecq	!use value from prog.ini, rather than qmechs.dat?
	   print 261,imodold,mtits(imodold),irecq,rtitles(irecq)(1:74)
261	   format(
     & ' Last run used mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' with rates from file #',i3,':',/,
     &  1x,a74,/)
	endif
c
	print 26
26	format(
     &  ' (1) Read mechanism and rate constants from <prog>.ini',/,
     &  ' (2) Choose a mechanism from those already defined',/,
     &  ' (3) Define a completely new mechanism',/,
     &  ' (4) Define a new mechanism by modifying an existing one',/,
     &  ' Option number [1] = ')
	iread=1
	call INPUTi(iread)
c
	if(iread.eq.3.or.iread.eq.4) then
	   call QDEFINE(iread,QD,PI,pinf,QT,IQ,titlep,titles,
     &    npar,nrateq,nlig,IL,conc,dgamma,chardef,qfilem,
     &    indmod,irate,jrate,IQQ,ligname,readp)
	   goto 99
	endif
	if(iread.eq.2) then	!read mtits() -already read above!
c         OPEN(unit=14,file=qfile,status='UNKNOWN',
c     &     access='DIRECT',form='UNFORMATTED',recl=3072)
cc	   read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)		!read above now
c	   read(14,rec=1) nmodc
c	   if(nmodc.gt.41) then	!look for qtitles.dat
c      	OPEN(unit=15,file='QTITLE.DAT',status='UNKNOWN',
c     &       access='DIRECT',form='UNFORMATTED',recl=10240)
c		read(15,rec=1) nmodc,(mtits(i),i=1,nmodc)
c		CLOSE(unit=15)
c	   else
c		read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
c	   endif
c	   read(14,rec=imod0+1) charmod,ilast,jlast	!needed?
c         CLOSE(unit=14)
	   defrate=.true.
	   ans='N'		!so models listed at label 59
	   goto 59		!get another model
	endif
c
c Now section for iread=1
c Read everything for model/rates specified in .ini, from record #irec,
c defined above
182	continue
	irec=jstart(irecq)	!start byte for data in record #irecq
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,
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
     &	(efacq(i),i=1,nsetq)
	nchan=1 	!always (not kept in qmechs.dat)
c
	iflag=3		!so numerical values shown in QWIND2
c no need to check model # since model and rates both in same record now
c	if(imodsav.ge.1.and.imod0.ne.imodsav) then
c	   call BELL(1)
c	   print 8,imod0
c8	   format(' Model number for rates (= ',i3,') is wrong',/,
c     &	' Enter new values for rates')
c	   pause
c	   imod0=imodsav
c	   newrate=.true.
cc      iflag=1 rate constant values initially blank, names shown
c	   iflag=1
c	endif
c
	CLOSE(unit=15)
	imod0=imod
	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
	if(.not.newrate) then
	   print 669,imod0,mtitle,irecq,rtitles(irecq)(1:74)
669	   format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Rates from file #',i3,':',/,
     &  1x,a74,/,
     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	else
	   ans='Y'
	endif
c List available models

59	if(UC(ans).eq.'N') then
58	   do i=1,nmods	!list the model titles
	      print 56,i,mtits(i)
56          format(' (',i2,')',a74)
		if(mod(i,20).eq.0) then
		   call BELL(1)
		   print 11
11		   format(' Hit space bar for more')
		   call ANYKEY
		endif
	   enddo
	   call BELL(2)
	   print 561,imod0
561	   format(' Fit model number [',i3,'] = ')
	   call INPUTi(imod0)
	   if(imod0.lt.1.or.imod0.gt.nmods) goto 58
	   if(pon()) write(7,670)mtitle,imod0
	   if(discprt) write(8,670)mtitle,imod0
670	   format(/,1x,a74,/,' Model number = ',i3,/)
c
c Next bit involves use of QWIND to input rate constants (done in HJCFIT,
c but not in theory progs at present)
c List files containing rate data from QDAT.DAT
c Now read title for rate constants for every set stored in QDAT
c column headings
c Modif 01/13/98 04:54pm so prints only the sets of rates that correspond
c to the specified model (imod0)
	   n=0
	   do i=1,nrecs
		if(jmod(i).eq.imod0) n=n+1
	   enddo
	   if(n.eq.0) then
		print 191
191		format(' No rate constants have been stored for this model')
	      newrate=.true.
c      iflag=1 rate constant values initially blank, names shown
	      iflag=1
	   else
		print 190,imod0,mtits(imod0)(1:74)
190		format(' For model #',i3,': ',a74,/,
     &   	' the following rate constants have been stored:',/,
     &   	'   file #:                      Name of rate file')
		do i=1,nrecs
		   if(jmod(i).eq.imod0) then
			print 192,i,rtitles(i)
192			format(1x,i3,': ',a74)
			irecq=i	!use as default
		   endif
		enddo
		print 15,irecq
15		format(' File # for rates [',i2,'] = ')
		call INPUTi(irecq)
		imodsav=imod0
		goto 182		!read rates
	   endif
	endif
c Whole section for nmod=0 removed here: nmod now superfluous?
c Print the rate constants here
c Change any rate constants?
c Show parameters in QWIND automatically, for approval
c Now show model + QWIND
	mtitle=mtits(imod0)
105	continue		!return here to modify rate constants for another run
c	  Define IQ here when same Q used
	if(chardef) then
	   call CQLAST(charmod,ilast,jlast)		!get ilast,jlast from charmod
	   call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)	!get IC,kA,kF
	   do i=1,100		!ditto
		do j=1,100
		   IQ(i,j)=0
		enddo
	   enddo
	else
c kA, kF already defined, but need to define IC(1,m)=i. IC(2,m)=j when
c state i is connected to state j (done in CHARQ if model defined in
c character for, chardef=true). Check non-zero rates (could also be
c done from irate() and jrate())
	   m=0
	   do i=2,k
		do j=1,i	!look at lower trangular part of QT only
		   if(QT(i,j).gt.1.d-30) then
			m=m+1
			IC(1,m)=i
		      IC(2,m)=j
		   endif
		enddo
	   enddo
	   if(m.ne.ncon) then	!check
		call BELL(2)
		print 77,m,ncon
77		format(/,' ERROR in GETQD: m = ',i6,' but ncon = ',i6)
		pause
	   endif
	endif
	i1=0
	do m=1,ncon
	   i=IC(1,m)
	   j=IC(2,m)
	   i1=i1+1
	   IQ(i,j)=i1
	   i1=i1+1
	   IQ(j,i)=i1
	enddo
c
	if(kflag.eq.2) then   !restore input values
	   kA=kA1
	   kB=kB1
	   kC=kC1
	   kD=kD1
	endif
	if(kflag.eq.1.or.kflag.eq.2) then
323	   print 2011,kA,kF
2011	   FORMAT('  number of open and shut states, kA, kF = ',2i5,/)
	   print 2012,kA,kB,kC,kD
2012	   format(' kA,kB,kC,kD = ',4i5,'  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		print 206,kA1
206		format('&  number of open states, kA [',i2,'] = ')
	      call INPUTi(kA)
		print 202,kB1
202		format(
     &	'&  number of short-lived shut states, kB [',i2,'] = ')
	      call INPUTi(kB)
	      print 203,kC1
203	      format(
     &	'&  number of long-lived shut states C, kC [',i2,'] = ')
	      call INPUTi(kC)
	      print 204,kD1
204	      format(
     &	'&  number very long (between cluster) shut'
     &	' states, kD [',i2,'] = ')
	      call INPUTi(kD)
	      if((kA+kB+kC+kD).ne.k) then
		   call BELL(2)
		   goto 323
		endif
	   endif
	endif
c
c JUMP TO HERE (?) IF NEW MODEL DEFINED IN QDEFINE()
c99	continue
c
c Check number of channels and their conductance (also print values)
c (do for all kflag?)
c	print 73,nchan,kA,vkin,(dgamma(i)*1.d12,i=1,kA)
	print 73,nchan,kA,(dgamma(i)*1.d12,i=1,kA)
73	format(' Number of channels = ',i8,/,
     & ' Number of open states = ',i3,/,
c==  & ' membrane potential (mV) = ',g13.6,/,
     & ' conductances (pS) = ',10g13.6)
	print 74
74	format('  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') then
	   print 75,nchan
75	   format(' Number of channels [',i3,'] = ')
	   call INPUTi(nchan)
c	   v=-100.		!tgis is now in getvdep, called below
c	   print 77,v
c77	   format(' Membrane potential (mV) [',f8.3,'] = ')
c	   call INPUTR(v)
c	   vkin=v
	   do i=1,kA
		gam=sngl(dgamma(i)*1.d12)
		print 76,i,gam
76		format(
     &	' Open state #',i2,': conductance (pS) = [',f8.2,'] = ')
		call INPUTr(gam)
		dgamma(i)=dble(gam)*1.0d-12
	   enddo
	endif
c Print to disc
	if(discprt) write(8,670)mtitle,imod0
c670	format(/,1x,a74,/,' Model number = ',i3,/)
	if(chardef) then
	  do i=1,ilast
	   if(discprt) write(8,671) (charmod(i,j),j=1,jlast)
c671	     format(4x,30a2)
	  enddo
	endif
	if(discprt) write(8,73) nchan,kA,vkin,(dgamma(i)*1.d12,i=1,kA)
c
c Check ligands
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
	iopt=0
	call GETLIG(iopt,il,nlig,ligname)	!subroutine is part of qdefine.for
	if(ncdep.eq.0) then
	   if(discprt) write(8,132)
132	   format(' No concentration-dependent rates')
	else
	   if(discprt) write(8,133) nlig
133	   format(/,' Number of ligands = ',i3,/,
     &   ' Concentration-dependent elements:',/,
     &   '   i   j     ligand #   Ligand name')
	   do L=1,ncdep
		if(discprt) write(8,4) IX(L),JX(L),IL(L),ligname(IL(L))
4		format(1x,2i3,5x,i3,2x,a20)
	   enddo
	endif
c
c Check voltage dependence
c	call GETVDEP(nmod,k,titlep,npar,IQ,readp,sameq,idest)
	call GETVDEP(k,titlep,npar,IQ,.true.,.false.,
     & vkin,vhold,vref,idest)
c
c
c Renumbering -won't work unless IC,IX,JX,IM,JM (at least) also renumbered
c and realloc kB,kC etc? (if kflag=1).  Allow renumbering only if neq=0, nsetq=0
c because RENUMB.for will not renumber constraints at present
	if(neq.gt.0.or.nsetq.gt.0) then
	   do i=1,ilast
		do j=1,jlast
		   cmodnew(i,j)=charmod(i,j)
		enddo
	   enddo
	   goto 70		!skip renumbering
	endif
	if(.not.readini) renum=.false.	!otherwise value from .ini
	ans1='N'
	if(renum) ans1='Y'
	call DCASK('Renumber the states',ans1,ans)
	if(ans.eq.'Y') then
	   renum=.true.   !so renumbered rates can't be saved on disk
	   do i=1,k		!initialise INEW
		inew(i)=i
	   enddo
	   call DCASK('Read numbering from \RENUM.DAT','Y',ans)
	   if(ans.eq.'Y') then
   		rfile=charnb(qpath)//'RENUM.DAT'
25		INQUIRE(file=rfile,exist=present,flen=nlen)
		if(.not.present.or.nlen.eq.0) then
		   print 212,rfile
212		   format(' RENUM.DAT not found in: ',a40)
		   call TITENT0('Path for RENUM.DAT:',rfile,40,.false.)
		   goto 25
		endif
c      	OPEN(unit=14,file='\RENUM.DAT',status='UNKNOWN',
c      	OPEN(unit=14,file=rfile,status='UNKNOWN',
c     & 	access='DIRECT',form='UNFORMATTED',recl=128)
c        renum.dat converted to larger size in qconvert.for
      	OPEN(unit=14,file=rfile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
		read(14,rec=1) imodnew,kA,kB,kC,kD,inew
      	CLOSE(unit=14)
	   endif
	   print 108
108	   format(/)
	   alter=.false.
	   do i=1,k
		isav=inew(i)
		print 106,i,inew(i)
c=106		format('&New state #',i2,' is old state # [',i2,'] = ')
106		format('&Old state #',i2,' is new state # [',i2,'] = ')
		call INPUTi(inew(i))
		if(isav.ne.inew(i)) alter=.true.
	   enddo
	   print 206,kA
	   call INPUTi(kA)
	   print 202,kB
	   call INPUTi(kB)
	   print 203,kC
	   call INPUTi(kC)
	   print 204,kD
	   call INPUTi(kD)
c
	   ans1='N'
	   if(alter) ans1='Y'		!default
	   call DCASK('Save numbering in \RENUM.DAT',ans1,ans)
	   if(ans.eq.'Y') then
   		rfile=charnb(qpath)//'RENUM.DAT'
      	OPEN(unit=14,file=rfile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
		write(14,rec=1) imodnew,kA,kB,kC,kD,inew
      	CLOSE(unit=14)
	   endif
	   call QSWAP(QT,IQ,k,inew,km)	!swap rows and cols of QT and IQ
c renumber conductances, c-dep rates, v-dep rates, connections and cycles
	   call RENUMB(inew,dgamma,k)
	   call NEWMOD(charmod,inew,ilast,jlast,pon())	!cmodnew in common
c rearrange parameter names accordingly
	   do m=1,npar
		 titlep1(m)=titlep(m)	!copy names of param
	   enddo
	   do m=1,npar
		 titlep(m)=titlep1(IQ(i,j))	!reordered names of param
	   enddo
	else		!define cmodnew anyway for use below
	   do i=1,ilast
		do j=1,jlast
		   cmodnew(i,j)=charmod(i,j)
		enddo
	   enddo
	endif
c
70	continue
c End of renumbering
c
      if(pon()) write(7,2431)ka,kb,kc,kd
      if(discprt) write(8,2431)ka,kb,kc,kd
2431	format(
     & ' No of states in each subset: kA, kB, kC, kD = ',4i3)
c
c Check which states to be calculated by micro rev (zero values from .ini
c if model has been changed)
c This should not be needed now, as m.r. parameters are set above
c either from .ini if model same, or read from qmodel.dat
c	if(.not.(imod0.eq.imodold)) then
c	   do i=1,8
c		nsc(i)=0
c		do j=1,15
c		   im(i,j)=0
c		   jm(i,j)=0
c		enddo
c	   enddo
c	endif
c
	if(obeymr) then
	   call GETREV()
	endif
c
c Record parameter values before QWIND so any changes can be noted
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   psav(m)=QD(i,j)
	enddo
	if(chardef) then
	   call CAPTSCN(scrnbuf0)	!before putting up model
c	   call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
	   call MODWIND(cmodnew,ilast+2,jlast+2,2,ilast,jlast)	!display model
	   if(.not.indmod) then
c===		ni=2*ncon	!number of rows
		ni=npar		!number of rows (OK for independent models too)
		nidisp=ni
		nr1=1		!posn of QWIND on screen
		nc1=2*jlast+7
		call QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     &       nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,irate,jrate,itry)
	   else if(indmod) then
		do m=1,npar
		   cdata(m,1)=titlep(m)
		   xs=sngl(QT(irate(m),jrate(m)))
		   call REALTOCH(xs,CDATA(m,2),11)
		   if(ncdep.gt.0) then
			do n=1,ncdep
			   i=ix(n)
			   j=jx(n)
			   if(i.eq.irate(m).and.j.eq.jrate(m)) then
				il1=IL(n)		!ligand number
				cdata(m,3)=ligname(il1)(1:11)
			   endif
			enddo
		   endif
		enddo
		ni0=npar		!rows
		nj0=3		!columns
		nidisp=npar
		if(npar.gt.20) nidisp=20
		nr1=-1		!so window initially in default position
		iconst=0	!no constraint
		iflagc=3		!show all cols
		nd1=50		!declared dimensions of cdata
		nd2=3
		call INTCONV(npar,cnum)
		title='     '//charnb(cnum)//' rate constants'
		heading='    name         value        ligand '
		call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	 nr1,nc1,iflagc,iconst,nd1,nd2)
		do i=1,npar
		   titlep(i)=cdata(i,1)(1:10)
		   call CHTOREAL(cdata(i,2),xs)
		   QT(irate(i),jrate(i))=dble(xs)
		enddo
         endif
	   call DISPSCN(scrnbuf0)  !restore orig screen
	   if(itry.eq.1) then
		ans='N'
		goto 59	!reread rates (from another disc?)
	   endif
	else		!when model too big/complex for chessboard
c==	   call MODRATES()	TO BE DONE
	endif
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c May have problem with constraints, when GETQD is called in HJCFIT
c If model has been changed then constraints read form hjcfit.ini will
c be nonsense so set neq=0
	if(.not.(imod0.eq.imodold)) then
	   neq=0	!old constraints not relevant for new model
c==	   nsetq=0	!for indep models -NO!
	endif
c===	call QSETD(xA,xB,IL,V1,QT,QD,k,.false.)
	call QSETD(conc,IL,V1,QT,QD,k,.false.)
	string=char(244)//' = calculated by microscopic rev'
	print 13,string
      if(pon()) write(7,13) string
      if(discprt) write(8,13) string
13	format(/,' Values of rate constants from disk',/,
     &'   (1/s, or 1/(Ms) for association rate constants)',/,
     &'    *n = association rate constant for ligand #n ',/,
     &  4x,a74)
	if(nvdep.gt.0) then
	   print 131,vhold,vref
         if(discprt) write(8,131) vhold,vref
131	   format(' Rate constants at holding potential = ',f8.3,' mV',/,
     &    '  (input values at ',f8.3,' mV)')
	endif
c
c NB now work with QT throughout so no need to remove conc
	do m=1,npar
	   pre='   '
c===	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   i=irate(m)
	   j=jrate(m)
	   r=QT(i,j)
	   if(ncdep.gt.0) then
		do n=1,ncdep
		   if(ix(n).eq.i.and.jx(n).eq.j) then
			pre(2:2)='*'		!asterisk indicates rate to be mult by conc
			pre(3:3)=CHAR(il(n)+48)	!ligand number
		   endif
		enddo
	   endif
c check if either if i,j or j,i is a micro rev route
	   if(ncyc.gt.0) then
		do n=1,ncyc
		   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
		   endif
		enddo
	   endif
	   if(r.eq.psav(m).or.idest.ne.105) then
      	print 12,m,pre,i,j,titlep(m),r
      	if(pon()) write(7,12)m,pre,i,j,titlep(m),r
	      if(discprt) write(8,12)m,pre,i,j,titlep(m),r
12		format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	   else
      	print 121,m,pre,i,j,titlep(m),r,psav(m)
      	if(pon()) write(7,121)m,pre,i,j,titlep(m),r,psav(m)
	      if(discprt) write(8,121)m,pre,i,j,titlep(m),r,psav(m)
121		format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6,
     &	' (last run was ',g13.6,')')
	   endif
	enddo
c===not much point in printing occupancies at 'standard conc' -remove
c	call EQOCCd(QD,k,k-1,kmax,Peq)		!CALC EQUILIB OCCS
c	if(iprint.ge.3) call SLIFED2(QD,Peq,pon(),discprt,kmax,kmax)
c
c Get the number of ligands bound to each state (returns nbound(i) in common)
c===	if(ibflag.ne.1001) then 	!nbound not read from qdat.dat
	if(.not.boundef) then 	!nbound not read defined yet in qmechs.dat
	   call GETBOUND(k,npar)
	endif
	print 401,(ligname(i),i=1,nlig)
	if(discprt) write(8,401) (ligname(i)(1:12),i=1,nlig)
401	format(/,
     & ' Number of ligands bound',/,
     & ' State   ',9a12)
	do i=1,k
	   print 40,i,(nbound(i,n),n=1,nlig)
	   if(discprt) write(8,40) i,(nbound(i,n),n=1,nlig)
40	   format('  ',i3,9(5x,i5))
	enddo
	ans='Y'
	call DCASK(' Are these correct',ans,ans)
	if(ans.eq.'N') then
	   print 41
41	   format(' Please tell DC that GETBOUND failed!')
	   do n=1,nlig
		do i=1,k
		   print 42,n,i,ligname(il(n))
42		   format(' Number of ligand #',i2,3x,a20,
     &		': molecules bound to state #',i3,' = ')
		   call INPUTi(nbound(i,n))
		enddo
	   enddo
	endif
c
c RE-SAVE model (e.g. if parameter names changed)
	if(ncyc.eq.0) obeymr=.false.
	if(.not.renum) then
	   call DCASK('Re-save the model and rates on disc','n',ans)
	else
	   goto 99		!if renumbered
	endif
	if(ans.eq.'N') goto 99
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
	call TITENT0(' Enter title for rates',rtitle,74,.false.)
	iopt=1
82	print 7,iopt
7	format(/,
     & ' (1) Write data starting at next vacant record in qmechs.dat',/,
     & ' (2) Overwrite the last record in qmechs.dat',/,
     & ' (3) Overwrite an earlier record in qmechs.dat',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.3) then
	   call BELL(2)
	   print 81
81	   format(' Option 3 not yet fixed')
	   goto 82
	endif
c
	imod=imod0		!for write
c    Calc isum=SUM[nsc(i)]
	isum=0
	do i=1,ncyc
	   isum=isum + nsc(i)
	enddo
	nbytes=4+4+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &    8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq
	if(iopt.eq.1) then
	   nrecs=nrecs+1
	   irec=nextrec		!where to write the record
	   ireclast=irec
	   jstart(nrecs)=irec
	   rtitles(irec)=rtitle
	   nextrec=nextrec + nbytes + 256 	!leave gap of 256 bytes
	else 		!overwrite last record
	   irec=jstart(nrecs)	!nrecs not changed
	   ireclast=irec
	   rtitles(irec)=rtitle
	   nextrec=nextrec + nbytes + 256 	!re-calculate nextrec
	endif
c
802	continue
c For new model, print model # and charmod here
	if(pon()) write(7,670)mtitle,imod0
	if(discprt) write(8,670)mtitle,imod0
c670	format(/,1x,a74,/,' Model number = ',i3,/)
	if(chardef) then
	  do i=1,ilast
	   if(pon()) write(7,671) (charmod(i,j),j=1,jlast)
	   if(discprt) write(8,671) (charmod(i,j),j=1,jlast)
671	     format(4x,30a2)
	  enddo
	else
c=======
	endif
c
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)
	write(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c	  Write data record
	write(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,
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
     &	(efacq(i),i=1,nsetq)
	CLOSE(unit=15)
c
	print 9,nrecs,imod,mtits(imod),rtitles(nrecs),
     & nbytes,jstart(nrecs),nextrec
	if(discprt) write(8,9) nrecs,imod,mtits(imod),rtitles(nrecs),
     & nbytes,jstart(nrecs),nextrec
9	format(/,
     & ' Data written to record # ',i3,' in qmechs.dat, for model # ',
     & i3,/,1x,a74,/,
     & ' Title for this record is',/,1x,a74,/,
     & 1x,i10,' bytes written, starting at byte # ',i12,/,
     & ' Next vacant byte number = ',i12)
c
c JUMP TO HERE IF NEW MODEL DEFINED IN QDEFINE()
99	continue
	idest=0
	imod=imod0		!copy for common
	RETURN
	end


