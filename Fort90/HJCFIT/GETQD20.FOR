	subroutine GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &  nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &  autosim,thtrue,irect,jcon,kflag,idest)
c
c Subroutine for ALL theory progs to define Q matrix defined at concentrations
c in conc() for use in rest of program
c Many arrays in commons so those needed can be transmitted to calling prog
c
c 12/04/01 06:26am GETQD split into GETQD1 which defines model, and GETQD2
c which defines rates (mainly so that, in HJCFIT, all constraints can be
c defined after model defined, but before rates defined).
c  GETQD2 carries on with original getqd from point where rates displayed
c New common/model2/ defined in getqd1 (for getqd2 only) to transmit values
c defined in getqd1 and needed here.
c GETQD2 is now specific for HJCFIT, and in autosim case checks rates both
c for initial guess (as in normal case) and for true rates for simulation.
c
c Modif 11/26/01 09:43am to add qfilem to arguments (for simulations in hjcfit)
c
c Modif 11/23/01 08:36am to change use of obeymr.  Now logical array obeymr(50)
c so can be specified separately for each cycle whether or not MR constraint is
c to be applied
c On entry im() etc are values from .ini (if read)
c
c Modif 10/29/01 08:35pm so kflag=-1 causes a temporary QMECHS.DAT to be
c written -called at END of HJCFIT to keep the fitted parameters (undesirable
c to use the regular qmechs.dat which would get too big)
c
c Modif 06/28/01 09:46am withoption 5 to list models.  This also writes
c blanks (char(0)) to sections of qmechs that are not used because these
c contain rubbish that seems to cause problems on web
c
c Modif 04/10/01 05:44pm for problem with setting rates via micro rev
c   Also conc() removed from arguments and QDEFINE fixed to return QT only
c
c   Problem arises when, as here, Q is set starting from QT which has neither
c   diagonals nor m.r. rates in it.  If (as in nic model with singly liganded
c   openings) we determine q(2,1) (A2R* -> AR*) by m.r., this can be done
c   only when agonist conc is non-zero.  AT zero conc, code below detects
c   that cycle is broken, and does not attempt to set q(2,1) which remains
c   zero, as in QT.  However this is a dissociation rate constant and should
c   be the same regardless of agonist conc.  In old version, Q was set
c   usually by removing conc from an existing Q matrix and putting in new
c   conc (in QNEWC routine), so as long as the Q matrix was originally set
c   with a finite conc (cA1, cB1, in old version) the m.r.parameters
c   like q(2,1) would be set then, and not changed here if QSETD called
c   with zero conc that breaks the cycle
c   Thus must put the m.r. values into QT (via call at finite conc) -if
c   QSETD is called with again with finite conc this value will be
c   ovewritten, but if QSETD is called with zero conc (and this breaks
c   the cycle) then existing values will be used.
c
c   This problem is solved here by calling QSETD with all conc=1,0 so rates
c   in QT are not changed, BUT m.r. parameters are set, and copying
c   this back into the final QT
c
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
c     (statname(i),i=1,kstat)							10*kstat
c
c
c Other things (imod0, conc, nchan etc) all go in .ini for each program
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
c kflag=-1 for write of QTEMP.DAT at end of hjcfit
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
c Getbound returns nbound(i,j)=number of ligands if type j
c that are bound to state i (in common) -added common/LIG/
c
	REAL*8 QT(100,100),dgamma(100)
	character*2 charmod(25,40)	!to print model
	real*4 pstar(4)
	character qfilem*40
	integer jcon(200)
c Declarations for modif 03/05/95 06:16pm
	character*10 titlep(200)
	COMMON/qblk/IQ(100,100)
c New arrays for qmechs.dat
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle,rtitles(500)	!title to describe rate constants
	integer irate(200),jrate(200)
	real*4 conc(10,10)
	logical boundef,chardef,indmod
c For print of eq constants
	logical iopen,jopen,bind	!to print equilib constants
	character*7 kname 	!to print equilib constants
c For autosim
	logical autosim
	allocatable::QTtrue
	real*8 QTtrue(:,:)
	real*8 thtrue(200)
	real*8 theta0(200)
c
	logical kmfast,readini,renum
	character*1 ans
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	character string*74,pre*3
	integer jmod(500)
	logical discprt
	common/dp/discprt
c Addition for getbound
c	integer nbound(10,2)
	integer nbound(100,10)
	common/nbnd/nbound
c
c For eqec50
	real*4 EC50,curmax,curinfs,concmax
	real*8 cfacd,vknd
	real*4 conc1(10)
	logical monot
c
c Commons as in getqd1
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
c
	character*79 wtitle
c
	logical obeymr(50)
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c
c Add common in case charmod etc needed in calling prog
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	real*8 psav(200)
	common/model2/chardef,indmod,nrateq,kstat,irecsav,irecqsav,
     & iver,nmods,boundef,jmod,mtits,statname,ksav
	common/mod/imod
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
c
c
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	common/db/ideb		!debug level
	common/ren/renum		!so can be kept in .ini if req
c For statmod
	common/sm/nsmax		!for getqd/charq
c
c	debug()=caplock()
c
c  Carry on with original getqd from point where rates displayed
	k=ksav	!from common
	kdim=100
	allocate(QTtrue(100,100))
	if(kflag.eq.-1) then
	   goto 100
	endif
c May have problem with constraints, when GETQD is called in HJCFIT
c If model has been changed then constraints read form hjcfit.ini will
c be nonsense so set neq=0
	if(.not.(imod0.eq.imodold)) then
	   neq=0	!old constraints not relevant for new model
c==	   nsetq=0	!for indep models -NO!
	endif
c
c Open qmechs.dat again
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
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
c List the rates that have been stored for this model
	n=0
	do i=1,nrecs
	   if(jmod(i).eq.imod0) n=n+1
	enddo
c
	print 190,imod0,mtits(imod0)(1:74)
190	format(' For model #',i3,': ',a74,/,
     &  ' the following rate constants have been stored:',/,
     &  '   file #:                      Name of rate file')
	do i=1,nrecs
	   if(jmod(i).eq.imod0) then
		print 192,i,rtitles(i)
192		format(1x,i3,': ',a74)
		ir=i	!use as default if not readini
	   endif
	enddo
	if(readini) then
	   ir=irecq		!file # for TRUE rates
	endif
	print 15,ir
15	format(/,
     & ' File # for INITIAL GUESSES for rate constants [',i2,'] = ')
	call INPUTi(ir)
	irecq=ir
	if(autosim) then
	   if(readini) then
		ir=irect		!file # for TRUE rates
	   endif
	   print 151,ir
151	   format(
     &' File # for TRUE RATE CONSTANTS used for simulation [',i2,'] = ')
	   call INPUTi(ir)
	   irect=ir
	endif
c
c NOW READ FILE FOR INITIAL GUESSES (# irecq)
c
	irec=jstart(irecq)	!start byte for data in record #irecq
	irecsav=irec
	irecqsav=irecq
c    Erase any previously-read charmod()
	do i=1,25
	   do j=1,40
		charmod(i,j)='  '
	   enddo
	enddo
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c
c First read the constants to guard against undefined values of indmod
c parameters
c Need only rates in QT() really -rest has already been read in call
c to getqd1
	read(unit=15,rec=irec) iver,
     &  imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &  ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &  indmod,npar,nsetq1,kstat1,
     &  ((charmod(i,j),i=1,ilast),j=1,jlast),
     &  (irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),
     &  (QT(irate(i),jrate(i)),i=1,nrateq)
c     &  (QT(irate(i),jrate(i)),i=1,nrateq),
c     &  (titlep(i),i=1,npar),
c     &      (ligname(i),i=1,nlig),
c     &	((nbound(i,j),i=1,k),j=1,nlig),
c     &	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
c     &	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
c     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
c     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
c     &	(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
c     &	(hpar(i),i=1,nvdep),
c     &	(pstar(i),i=1,4),(kmcon(i),i=1,9),
c     &	(ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),
c     &	(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),
c     &	(efacq(i),i=1,nsetq),
c     &	(statname(i),i=1,kstat)
c
	CLOSE(unit=15)
	nchan=1 	!always (not kept in qmechs.dat)
c Define theta0 to contain parameters -done in hjcfit
c	do m=1,npar
c	   i=irate(m)
c	   j=jrate(m)
c	   theta0=QT(i,j)
c	enddo
c
c Set all constraints (but no conc) in QT using QSET_TRU (and, with imode=1,
c also generate theta0())
	jset=1
c	call SETMR(QT,jset,conc,kdim)
	imode=1
	do m=1,npar
	   psav(m)=theta0(m)	!to test for changes in qset
	enddo
	call QSET_TRU(QT,theta0,jset,conc,npar,irate,jrate,IQ,
     & imode,k)
c
	imod0=imod
	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
c	kF=k-kA
c	if(.not.newrate) then
	   print 669,imod0,mtitle,irect,rtitles(irect)(1:74)
669	   format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Initial guesses for rates from file #',i3,':',/,
     &  1x,a74,/,
     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
c	else
c	   ans='Y'
c	endif
c===what is  done with 'ans'?
	icolour=0	!normal blue
	wtitle='INITIAL GUESSES [1/(Ms) for assoc rates]'
	call DISPRATE(QT,irate,jrate,chardef,indmod,npar,titlep,
     & jcon,wtitle,ligname,icolour)
c
	mtitle=mtits(imod0)
c IQ etc already defined
c display rates, alter, print (and re-store?) for true mechanism
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
===NOT TO DISK -only final values for discprt
	string=char(244)//' = calculated by microscopic rev'
	print 13,string
      if(discprt) write(8,13) string
13	format(/,' INITIAL GUESSES for rate constants',/,
     &'   (1/s, or 1/(Ms) for association rate constants)',/,
     &'  *n = association rate constant for ligand #n ',/,
     &  2x,a74,/,
     &'   & = constrained value')
	if(nvdep.gt.0) then
	   print 131,vhold,vref
         if(discprt) write(8,131) vhold,vref
131	   format(' Rate constants at holding potential = ',f8.3,' mV',/,
     &    '  (input values at ',f8.3,' mV)')
	else
	   vhold=-80.	!so defined to calculate current and noise
	   vref=-80.
	   vkin=-80.
	endif
c
C IF AUTOSIM, NEXT READ FILE FOR TRUE RATES (# irect)
	if(autosim) then
c Now read chosen file
	   irec=jstart(irect)	!start byte for data in record #irecq
	   irecsav=irec
	   irectsav=irect
c    Erase any previously-read charmod()
	   do i=1,25
		do j=1,40
		   charmod(i,j)='  '
		enddo
	   enddo
         OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c
c First read the constants to guard against undefined values of indmod
c parameters
c Need only rates in QT() really -rest has already been read in call
c to getqd
c   read QTtrue
	   read(unit=15,rec=irec) iver,
     &  imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &  ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &  indmod,npar,nsetq1,kstat1,
     &  ((charmod(i,j),i=1,ilast),j=1,jlast),
     &  (irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),
     &  (QTtrue(irate(i),jrate(i)),i=1,nrateq)
c     &  (QT(irate(i),jrate(i)),i=1,nrateq),
c     &  (titlep(i),i=1,npar),
c     &      (ligname(i),i=1,nlig),
c     &	((nbound(i,j),i=1,k),j=1,nlig),
c     &	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
c     &	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
c     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
c     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
c     &	(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
c     &	(hpar(i),i=1,nvdep),
c     &	(pstar(i),i=1,4),(kmcon(i),i=1,9),
c     &	(ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),
c     &	(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),
c     &	(efacq(i),i=1,nsetq),
c     &	(statname(i),i=1,kstat)
c
	   CLOSE(unit=15)
	   nchan=1 	!always (not kept in qmechs.dat)
c Define thtrue() to contain parameters
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		thtrue(m)=QTtrue(i,j)
	   enddo
c
c Need set all constraints in QT (temp local file) for QWIND, but only thtrue is returned from here)
	   do m=1,npar
		psav(m)=theta0(m)	!to test for changes in qset
	   enddo
	   jset=1
	   imode=1
	   call QSET_TRU(QTtrue,thtrue,jset,conc,npar,irate,jrate,IQ,
     &	 imode,k)
c
	   imod0=imod
	   mtitle=mtits(imod0)
	   k=kA+kB+kC+kD
	   print 669,imod0,mtitle,irect,rtitles(irect)(1:74)
c669	   format(
c     & ' Mechanism #',i3,':',/,
c     &  1x,a74,/,
c     & ' ''True'' rates for simulation from file #',i3,':',/,
c     &  1x,a74,/,
c     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
c===what is done with 'ans'!!
c
	   mtitle=mtits(imod0)
c IQ etc already defined
c display rates, alter, print (and re-store?) for true mechanism
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
         string=char(244)//' = calculated by microscopic rev'
===NOT TO DISK -only final values for discprt
	   print 1311,string
         if(discprt) write(8,1311) string
1311	   format(/,' TRUE RATE CONSTANTS: values for simulation',/,
     &'   (1/s, or 1/(Ms) for association rate constants)',/,
     &'  *n = association rate constant for ligand #n ',/,
     &  2x,a74,/,
     &'   & = constrained value')
	   if(nvdep.gt.0) then
		print 132,vhold,vref
        	if(discprt) write(8,132) vhold,vref
132	      format(' Rate constants at holding potential = ',f8.3,' mV',/,
     &    '  (input values at ',f8.3,' mV)')
	   else
		vhold=-80.	!so defined to calculate current and noise
		vref=-80.
		vkin=-80.
	   endif
	   icolour=1	!green
	   wtitle='TRUE RATES [1/(Ms) for assoc rates]'
	   call DISPRATE(QTtrue,irate,jrate,chardef,indmod,npar,titlep,
     &    jcon,wtitle,ligname,icolour)
	endif		!of autosim
c
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c===print/discprt the final values (need to do also for TRUE values in
c===autosim case -put in subroutine with QWIND etc?

==========start of prtrate
	string=char(244)//' = calculated by microscopic rev'
	print 1312,string
      if(discprt) write(8,1312) string
1312	format(/,' Values of rate constants',/,
     &'   (1/s, or 1/(Ms) for association rate constants)',/,
     &'    *n = association rate constant for ligand #n ',/,
     &  4x,a74,
     &'   & = constrained value')
	if(nvdep.gt.0) then
	   print 131,vhold,vref
         if(discprt) write(8,131) vhold,vref
c131	   format(' Rate constants at holding potential = ',f8.3,' mV',/,
c     &    '  (input values at ',f8.3,' mV)')
	else
	   vhold=-80.	!so defined to calculate current and noise
	   vref=-80.
	   vkin=-80.
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
		   if(obeymr(n).and.im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
		   endif
		enddo
	   endif
	   if(r.eq.psav(m).or.idest.ne.105) then
      	print 12,m,pre,i,j,titlep(m),r
	      if(discprt) write(8,12)m,pre,i,j,titlep(m),r
12		format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	   else
      	print 121,m,pre,i,j,titlep(m),r,psav(m)
	      if(discprt) write(8,121)m,pre,i,j,titlep(m),r,psav(m)
121		format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6,
     &	' (last run was ',g13.6,')')
	   endif
	enddo
c
c Now print also equilibrium constants
	print 91
	if(discprt) write(8,91)
91	format(/,
     &' Equilibrium constants calculated from these rate constants')
	do m=1,ncon
	   isfac1=0		!statistical factors for assoc/dissoc
	   isfac2=0		!statistical factors for assoc/dissoc
	   i=IC(1,m)
	   j=IC(2,m)
	   iopen=i.le.kA
	   jopen=j.le.kA
	   if(iopen.and.(.not.jopen)) then
		eqK=sngl(QT(j,i)/QT(i,j))
		i1=j
		j1=i
		m2=IQ(i,j)
		m1=IQ(j,i)
		kname='  E   '
	   else if(jopen.and.(.not.iopen)) then
		eqK=sngl(QT(i,j)/QT(j,i))
		i1=i
		j1=j
		m1=IQ(i,j)
		m2=IQ(j,i)
		kname='  E   '
	   else	!open-open or shut-shut trans (bindin/unbinding or isomerisation
c is it assoc/dissoc?
		bind=.false.	!current i,j is open-open or shut-shut isomerisation
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j).or.
     &	    (ix(n).eq.j.and.jx(n).eq.i)) then
			bind=.true.
			jlig=IL(n)	!ligand bound/unbound for current step
		   endif
		enddo
		if(bind) then	!current i,j is assoc-dissoc reaction
		   nbmax=0 !max number bound for ligand bound in current step
		   do n=1,k
			if(nbound(n,jlig).gt.nbmax) nbmax=nbound(n,jlig)
		   enddo
		   if((ix(n).eq.i.and.jx(n).eq.j)) then	!i->j is assoc
		      eqK=1.e6*sngl(QT(j,i)/QT(i,j))
			kname=' K (uM)'
			i1=j
			j1=i
			m2=IQ(i,j)
			m1=IQ(j,i)
			isfac1=nbmax-nbound(i,jlig)	!assoc
			isfac2=nbound(j,jlig)		!dissoc
		   else		!j->i is assoc
		      eqK=1.e6*sngl(QT(i,j)/QT(j,i))
			i1=i
			j1=j
			m1=IQ(i,j)
			m2=IQ(j,i)
			isfac1=nbmax-nbound(j,jlig)	!assoc
			isfac2=nbound(i,jlig)		!dissoc
			kname= ' K (uM)'
		   endif
		else			!current i,j is open-open or shut-shut isomerisation
		   eqK=QT(i,j)/QT(j,i)
		   i1=j  		!no way to decide which way up is best
		   j1=i
		   m2=IQ(i,j)
		   m1=IQ(j,i)
		   kname=' Kisom '
		endif
	   endif
	   print 71,kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
	   if(discprt) write(8,71)
     &		kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
71	   format(
     &      1x,a7,' = q(',i2,',',i2,')/q(',i2,',',i2,') = ',
     &	a10,'/',a10,' = ',g13.6)
	   if(isfac1.gt.0) then
		sfac=float(isfac1)/float(isfac2)
		print 5,isfac1,isfac2,sfac,sfac,kname,sfac*eqK
		if(discprt) write(8,5)isfac1,isfac2,sfac,sfac,kname,sfac*eqK
5		format(
     &	'    Statistical factor = ',i2,'/',i2,' = ',f7.3,
     &	': ',f7.3,'*',a7,' = ',g13.6)
	      isfac1=0		!reset statistical factors for assoc/dissoc
	      isfac2=0
	   endif
	enddo 	!end of equilibrium constants
	pause
c
	if(idest.eq.105) goto 99
c
c insert ec50 here
	kdim=100
	vknd=dble(vkin*float(nchan))
	vknd=DROUND(vknd,0)	!exact double precision as long as vkin is integer
	cfacd=vknd*1.d-3*1.d12
	nlvar=1
	if(nlig.gt.1) then
	   print 590,nlvar
590	   format(' Agonist (to calc EC50) is ligand number [',i2,'] = ')
	   call INPUTi(nlvar)
	endif
	call EQEC50(EC50,curinfs,monot,curmax,concmax,QT,
     & 	conc1,nlvar,vhold,vref,cfacd,dgamma,k,kdim)
	if(monot) then
	   print 591,curinfs,ec50*1.e6
         if(discprt) write(8,591) curinfs,ec50*1.e6
591	   format(/,
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g11.4,/,
     & '    Conc for 50% of this maximum (EC50) (muM) = ',g11.4,/)
c		ec50eq=ec50*1.e6	!micromolar
	   else	!not monotonic (never true for binding curve)
      	print 592,curmax,concmax*1.e6,ec50*1.e6,curinfs
      	if(discprt)write(8,592)curmax,concmax*1.e6,ec50*1.e6,curinfs
592		format(/,
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (muM) (left of max) = ',
     &     g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
	   endif
c	   yinfeq=abs(curinfs)	!maximum resonse/binding (for Hill slopes)
c
c RE-SAVE model (e.g. if parameter names changed)
c=	if(ncyc.eq.0) obeymr=.false.
	if(.not.renum) then
	   call DCASK('Re-save the model and rates on disc','n',ans)
	else
	   goto 99		!if renumbered
	endif
	if(ans.eq.'N') goto 99
c
c
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
     & ' (3) Replace existing record',/,
     & ' (4) Overwrite an earlier record in qmechs.dat',/,
     & ' (5) Abandon re-saving of data',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.5) goto 99
c	if(iopt.eq.4) then
c	   call BELL(2)
c	   print 81
c81	   format(' Option 4 not yet fixed')
c	   goto 82
c	endif
c
	imod=imod0		!for write
100	continue	!jump here to write if kflag=-1
	if(kflag.eq.-1) then
	   qfilem='QTEMP.DAT'
	   call TITENT0('Name/path for temporary models file:',
     &	qfilem,40,.false.)
	   call TITENT0(' Enter title for rates',rtitle,74,.false.)
	   iopt=6   !only for kflag=-1
	endif
c
c    Calc isum=SUM[nsc(i)]
	isum=0
	do i=1,ncyc
	   isum=isum + nsc(i)
	enddo
	nbytes=4+4+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &    8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat
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
c	   irec=jstart(irecq)	!start byte for data in record #irecq
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
	   irec1=jstart(irq)
	   if(irq.lt.nrecs) then
	    if(irec1+nbytes.ge.jstart(irq+1)) then
		call BELL(2)
		print 136,irec1+nbytes,irq+1,jstart(irq+1)
c136		format(
c     &   ' Data would extend to byte # ',i11,' which would overwrite',/,
c     &   ' start of record ',i4,' which starts at byte # ',i11,/)
		goto 82
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
	else if(iopt.eq.6) then 		!kflag=-1 case -only one record
	   nrecs=1
	   jstart(1)=20000
	   irec=jstart(1)
	   ireclast=irec
	   rtitles(1)=rtitle
	   nextrec=irec + nbytes + 1024	!re-calculate nextrec -not used for qtemp
	endif
c
c802	continue
c For new model, print model # and charmod here
	if(discprt) write(8,670)mtitle,imod0
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
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)
	write(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c	  Write data record
	write(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
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
     &	(statname(i),i=1,kstat)

	CLOSE(unit=15)
c
	if(iopt.ne.6) then
         print 9,nrecs,imod,mtits(imod),rtitles(nrecs),
     &    nbytes,jstart(nrecs),nextrec
	   if(discprt) write(8,9) nrecs,imod,mtits(imod),rtitles(nrecs),
     &    nbytes,jstart(nrecs),nextrec
9	   format(/,
     & ' Data written to record # ',i3,' in qmechs.dat, for model # ',
     & i3,/,1x,a74,/,
     & ' Title for this record is',/,1x,a74,/,
     & 1x,i10,' bytes written, starting at byte # ',i12,/,
     & ' Next vacant byte number = ',i12,/)
	else if(iopt.eq.6) then
         print 90,qfilem,rtitles(nrecs)
	   if(discprt) write(8,90) qfilem,rtitles(nrecs)
90	   format(/,
     & ' Model and rate constants written to record 1 of temporary',/,
     & '  qmechs file named ',a40,/,
     & ' Title for this record is',/,1x,a74,/)
	endif
7c
=========end of prtrate
c JUMP TO HERE IF NEW MODEL DEFINED IN QDEFINE()
99	continue
c
	idest=0
999	continue		!return without resetting idest
	imod=imod0		!copy for common
	DEallocate(QTtrue)
	RETURN
	end


