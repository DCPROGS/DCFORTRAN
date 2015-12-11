	subroutine GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &  nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &  theta0,jcon,autosim,thtrue,irect,irates,jrates,
     &  npars,dgammas,kflag,icon,useprim,nfix,jfix,idest)
c
c Subroutine for ALL theory progs to define Q matrix defined at concentrations
c in conc() for use in rest of program
c Many arrays in commons so those needed can be transmitted to calling prog
c
c Modif 09/05/04 05:35am Criterion for Q(i,j) being 'changed' is now
c  a difference of 1.d-7 (1/sec), rather than 1.d-20
c
c Modif 07/07/04 09:09am
c If spanning tree method is used (useprim=true), fixed values have already
c been set by call to SETFIX from GETQD1, so these values should not be changed
c in QT when it is read from QMECHS.MEC -the fixed values must be saved and
c after QT has been read, the fixed values re-inserted into it.  Therefore
c useprim,nfix, jfix added to parameters.
c
c Modif 01/29/02 05:32am to allow different mechs to be used for simulation
c and fit

c 12/04/01 06:26am GETQD split into GETQD1 which defines model, and GETQD2
c which defines rates (mainly so that, in HJCFIT, all constraints can be
c defined after model defined, but before rates defined).
c  GETQD2 carries on with original getqd from point where rates displayed
c New common/model2/ defined in getqd1 (for getqd2 only) to transmit values
c defined in getqd1 and needed here.
c GETQD2 is now specific for HJCFIT, and in autosim case checks rates both
c for initial guess (as in normal case) and for true rates for simulation.
c idest=105 option removed
c
c  Modif 01/10/03 02:28pm so that if things altered in GETQD1 are not
c   overwritten when model is re-read here,  viz. (a) conductances and
c   (b)  states to be defined by micro rev
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
c Dummy arrays to prevent overwriting dgamma(), im(), jm() which may be
c altered in GETQD1
	allocatable::dgamdum,nscdum,imdum,jmdum
	real*8 dgamdum(:)
	integer*4 nscdum(:),imdum(:,:),jmdum(:,:)
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
c For autosim
	logical autosim,recheck
	allocatable::QTtrue
	real*8 QTtrue(:,:)
	real*8 thtrue(200)
	real*8 theta0(200)
	allocatable::QTsav
	real*8 QTsav(:,:)
	logical allocated
c
	logical kmfast,readini,renum
	character*1 ans
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
c	character string*74
	integer jmod(500)
	logical discprt
	common/dp/discprt
c Addition for getbound
c	integer nbound(10,2)
	integer nbound(100,10)
	common/nbnd/nbound
	logical nodata
	common/ndata/nodata
c Added 07/07/04 09:50am
	integer*4 jfix(200)
	logical useprim
	allocatable::qsav
	real*8 qsav(:)
c added 07/07/05 12:44pm
	logical null
c
c Commons as in getqd1
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	common/indblk1/nsub,kstat0,npar0,kcon,npar1,ncyc0
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
c
	character*79 wtitle
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
c
c Add common in case charmod etc needed in calling prog
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	common/model2/chardef,indmod,nrateq,kstat,irecsav,irecqsav,
     & iver,nmods,boundef,jmod,mtits,statname,ksav
c similar common for the model used for simulation
	character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print model
	common/models/imods,charmods,jlasts,ilasts,mtitles,imodolds
c=	common/mod/imod
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
c
	common/deb2/idebug2	!for printing in checkmr
c
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	common/db/ideb		!debug level
	common/ren/renum		!so can be kept in .ini if req
c For statmod
	common/sm/nsmax		!for getqd/charq
c For separate simulation mechanism (from getqd1)
	integer irates(200),jrates(200)
	real*8 dgammas(100)
	common/KBLKs/kAs,kBs,kCs,kDs
	common/cpars/ncdeps,IXs(100),JXs(100),xs
	common/LIGs/nligs,ILs(100)
	COMMON/qblks/IQs(100,100)
	common/QPARs/ncons,ICs(2,200)
c
c	debug()=caplock()
c
c  Carry on with original getqd from point where rates displayed
	k=ksav	!from common
	ks=kAs+kBs+kCs+kDs
	allocate(QTtrue(100,100))
	allocate(QTsav(100,100))	!to check for changes
	do i=1,k
	   do j=1,k
		QTtrue(i,j)=0.d0
		QTsav(i,j)=0.d0
	   enddo
	enddo
	if(kflag.eq.-1) then	!write final fit to QTEMP.DAT
	   idest=100
	   imd=0
	   call PRTRATE(QT,npar,irate,jrate,titlep,jcon,nchan,
     &    ligname,dgamma,qfilem,k,kflag,idest,kA,kB,kC,kD,
     &    imod0,charmod,jlast,ilast,mtitle,
     &    ncon,IC,ncdep,IX,JX,IQ,nlig,IL,nlvar,vhold,vref,imd)
	   goto 99
	endif
c May have problem with constraints, when GETQD is called in HJCFIT
c If model has been changed then constraints read form hjcfit.ini will
c be nonsense so set neq=0
	if(.not.(imod0.eq.imodold)) then
	   neq=0	!old constraints not relevant for new model
c==	   nsetq=0	!for indep models -NO!
	endif
c
c Open qmechs.mec again
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c	read(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c All iver=102 now
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
c	Read part of data record, up to rtitle, to define rtitles() and jmod()
	imodmax=0
	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
	   read(unit=15,rec=irec) iver,
     &	imod,mtitle,k,kA,kB,kC,kD,rtitle
	   if(imod.gt.imodmax) imodmax=imod
	   mtits(imod)=mtitle	!so mtits(i)=title of model #i
	   jmod(irq)=imod
	   rtitles(irq)=rtitle
	enddo
c List the rates that have been stored for this model
	n=0
	do i=1,nrecs
	   if(jmod(i).eq.imod0) n=n+1
	enddo
c
	if(autosim) then
 	   print 190,imods,mtits(imods)(1:74)
c190	   format(' For model #',i3,': ',a74,/,
c     &  ' the following rate constants have been stored:',/,
c     &  '   file #:                      Name of rate file')
	   n=0
	   do i=1,nrecs
		if(jmod(i).eq.imods) then
		   print 192,i,rtitles(i)
c192		   format(1x,i3,': ',a74)
		   ir=i	!use as default if not readini
		   n=n+1
		   if(mod(n,20).eq.0) then
			call BELL(1)
			print 11
11			format(' Hit space bar for more')
			call ANYKEY
		   endif
		endif
	   enddo
	   if(readini) then
		ir=irect		!file # for TRUE rates
	   endif
	   print 151,ir
151	   format(
     &' File # for TRUE RATE CONSTANTS used for simulation [',i4,'] = ')
	   call INPUTi(ir)
	   irect=ir
	endif
c MODEL FOR FIT
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
22	if(readini) then
	   ir=irecq		!file # for TRUE rates
	endif
	if(.not.nodata) then
	   print 15,ir
15	   format(/,
     & ' File # for INITIAL GUESSES for fit of rate constants ['
     & ,i4,'] = ')
	else if(nodata) then
	   print 152,ir
152	   format(/,
     & ' File # for values of rate constants ['
     & ,i4,'] = ')
	endif
	call INPUTi(ir)
	irecq=ir
c
	icon=1
c
C IF AUTOSIM, FIRST READ FILE FOR TRUE RATES (# irect)
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
c to getqd1
c Allocate dummy arrays to prevent overwriting dgamma(), im(), jm() which
c may be altered in GETQD1 -likewise for nsc() and ncyc
	   ALLOCATE(dgamdum(100),nscdum(50),imdum(50,100),jmdum(50,100))
c   read QTtrue
	   read(unit=15,rec=irec) iver,
     &  imod,mtitles,ks, kAs, kBs, kCs, kDs,rtitle,ilasts,jlasts,nrateq,
     &  ncons,ncdeps,nligs,chardef,boundef,ncycsd,vref,nvdep,kmfast,
     &  indmod,npars,nsetq1,kstat2,
     &  ((charmods(i,j),i=1,ilasts),j=1,jlasts),
     &  (irates(i),i=1,nrateq),(jrates(i),i=1,nrateq),
     &  (QTtrue(irates(i),jrates(i)),i=1,nrateq),
     &  (titlep(i),i=1,npars),
     &  (ligname(i),i=1,nligs),
     &  ((nbound(i,j),i=1,ks),j=1,nligs),
     &  (IX(i),i=1,ncdeps),(JX(i),i=1,ncdeps),(IL(i),i=1,ncdeps),
c     &  (dgamma(i),i=1,kAs),(nsc(i),i=1,ncycs),
c     &  ((im(i,j),j=1,nsc(i)),i=1,ncycs),
c     &  ((jm(i,j),j=1,nsc(i)),i=1,ncycs),
     &  (dgamdum(i),i=1,kAs),(nscdum(i),i=1,ncycsd),
     &  ((imdum(i,j),j=1,nscdum(i)),i=1,ncycsd),
     &  ((jmdum(i,j),j=1,nscdum(i)),i=1,ncycsd),
     &  (iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
     &  (hpar(i),i=1,nvdep),
     &  (pstar(i),i=1,4),(kmcon(i),i=1,9),
     &  (ieq(i),i=1,nsetq1),(jeq(i),i=1,nsetq1),
     &  (ifq(i),i=1,nsetq1),(jfq(i),i=1,nsetq1),
     &  (efacq(i),i=1,nsetq1),
     &  (statname(i),i=1,kstat2),
     &   nsub,kstat0,npar0,kcon,npar1,ncyc0
c ===? Is kstat needed here (in earlier version it was read as kstst1 which was not used)
c	   if(.not.indmod) then
c		kstat=kstat2
c	   else if(indmod) then
c		kstat=kstat2 - kcon
c	   endif
c   Make sure IQ is defined
	   do i=1,100		!ditto
		do j=1,100
		   IQs(i,j)=0
		enddo
	   enddo
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		IQs(i,j)=m
	   enddo

	   CLOSE(unit=15)
	   DEALLOCATE(dgamdum,nscdum,imdum,jmdum)
	   nchan=1 	!always (not kept in qmechs.dat)
c Define thtrue() to contain parameters
c Parameter numbering is problem for defining thtrue() in case where
c Simulated mech differs from fitted mech.  For simulation it is thtrue() that
c is used in HJCSIM2 so it must contain all the parameters, but for
c printout want to omit, e.g. the desens rates in mod 39 (qmechsdc.dat)
	   do m=1,npars
		i=irates(m)
		j=jrates(m)
		thtrue(m)=QTtrue(i,j)
	   enddo
c
c Need set all constraints in QT (temp local file) for QWIND, but only
c thtrue is returned from here). But if mechs  are  different, it would be
c tedious to put in a separate set of constraints, with different parameter
c numbering, for both mechs, so in this case just use input values (is IQs
c needed in this case?)
	   if(imods.eq.imod0.and.neq.gt.0) then
		print 81,neq,icon
81		format(
     &	' Constraints have been specified for ',i3,' rates:',/,
     &	' (1) Apply constraints to both true rates and fitted',/,
     &	' (2) Apply constraints only to fitted rates',/,
     &	' (3) Apply constraints only to true rates',/,
     &	' Option number [',i2,'] = ')
		call INPUTi(icon)
		if(discprt) then
		   if(icon.eq.1) then
			write(8,82)
82			format(/,
     &	    ' Constraints applied to both true rates and fitted',/)
		   else if(icon.eq.2) then
			write(8,83)
83			format(/,
     &	    ' Constraints applied only to fitted rates',/)
		   else if(icon.eq.3) then
			write(8,84)
84			format(/,
     &	    ' Constraints applied only to true rates',/)
		   endif
		endif
	   endif
c
	   if(imods.ne.imod0) then
		call BELL(1)
		print 80
80		format(
     &' Different mechanisms have been specified for simulation',

     & ' and fit.',/,
     & ' Constraints are not specified separately for the true',
     & ' mechanism,',/,
     & ' so it is the responsibility of the user to check that the',/,
     & ' rate constants to be specified next obey the constraints ',/,
     & ' and microscopic reversibility that are required')
		pause
	   endif
c
	   jset=1
	   imode=1
	   if(imods.eq.imod0) then
		icons=0				!apply constraints (if any)
		if(icon.eq.2) icons=1		!don't apply constraints
	      call QSET_TRU(QTtrue,thtrue,jset,conc,npars,
     &	 irates,jrates,IQs,imode,icons,ks)
	   endif
c
	   imods=imod
	   mtitles=mtits(imods)
	   ks=kAs+kBs+kCs+kDs
	   print 669,imods,mtitle,irect,rtitles(irect)(1:74)
c669	   format(
c     & ' Mechanism #',i3,':',/,
c     &  1x,a74,/,
c     & ' ''True'' rates for simulation from file #',i3,':',/,
c     &  1x,a74,/,
c     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') goto 22
c
	   mtitles=mtits(imods)
c IQ etc already defined
c display rates, alter, print (and re-store?) for true mechanism
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
	   wtitle='TRUE RATES [1/(Ms) for assoc]'
21	   do i=1,ks
		do j=1,ks
		   QTsav(i,j)=QTtrue(i,j)
		enddo
	   enddo
c	   call DISPRATE(QTtrue,irate,jrate,chardef,indmod,npar,titlep,
c     &    jcon,wtitle,ligname,icolour)
	   imd=1		!mode to prevent display of constraints not def for Qtrue
	   call DISPRATE(QTtrue,
     &	irates,jrates,chardef,indmod,npars,titlep,
     &	jcon,wtitle,ligname,icolour,
     &  	imods,charmods,jlasts,ilasts,mtitles,
     &	ncons,ICs,ncdeps,IXs,JXs,IQs,nligs,ILs,imd)
c check for manual changes in qwindhjc, and re-apply constraints in case
c manual changes made in qwind (but not if mechanisms are different for
c simulation and fitting)
	   recheck=.false.
	   if(imods.eq.imod0) then
	      call QSET_TRU(QTtrue,thtrue,jset,conc,npars,
     &	 irates,jrates,IQ,imode,icons,ks)
		do i=1,k
		   do j=1,k
c=			if(QTsav(i,j).ne.QTtrue(i,j)) then
			if(dabs(QTsav(i,j)-QTtrue(i,j)).gt.1.d-7) then
			  print 3,i,j,QTsav(i,j),QTtrue(i,j)
			  if(discprt) write(8,3) i,j,QTsav(i,j),QTtrue(i,j)
3			   format(' True QT(',i2,',',i2,')',
     & 		' changed from ',g13.6,' to ',g13.6)
			  recheck=.true.
			endif
		   enddo
		enddo
		if(recheck) then
		   i=idebug2	!save
		   idebug2=2	!so prints to screen and disk
c		   idebug2=1	!so prints to screen only
		   call CHECKMR(QTtrue)
		   idebug2=i	!restore
		   pause 'Recheck rate constants'
		   do i=1,k
			do j=1,k
			   QTsav(i,j)=QTtrue(i,j)	!save current values
			enddo
		   enddo
		   recheck=.false.
		   goto 21
      	endif
	   endif
c	   print 17,irect,rtitles(irect)(1:74)
	   if(discprt) write(8,17) irect,rtitles(irect)(1:74)
17	   format(/,' -----------------------------------',/,
     & ' TRUE RATE CONSTANTS, used for simulation ',/,
     & ' Values for rates from file #',i3,':',/,
     &  1x,a74)
	   call PRTRATE(QTtrue,npars,irates,jrates,titlep,jcon,nchan,
     &    ligname,dgammas,qfilem,ks,kflag,idest,kAs,kBs,kCs,kDs,
     &    imods,charmods,jlasts,ilasts,mtitles,
     &    ncons,ICs,ncdeps,IXs,JXs,IQs,nligs,ILs,nlvar,vhold,vref,imd)
c   Copy qttrue(i,j) the thtrue(m) again in case Q altered
	   do m=1,npars
		i=irates(m)
		j=jrates(m)
		thtrue(m)=QTtrue(i,j)
	   enddo
	endif		!of autosim
c7
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
c NB if spanning tree method is used (useprim=true), fixed values have already
c been set by call to SETFIX from GETQD1, so these values should not be changed
c in QT when it is read from QMECHS.MEC -the fixed values must be saved and
c after QT has been read, the fixed values re-inserted into it.
c Save the fixed values in QT, for re-insertion
	if(useprim.and.nfix.gt.0) then
	   if(allocated(qsav)) deallocate(qsav)
	   allocate(qsav(nfix))
	   n=0
	   do m=1,npar
		if(jfix(m).eq.1) then
	 	   i=irate(m)
		   j=jrate(m)
		   n=n+1
		   qsav(n)=QT(i,j)
		endif
	   enddo
	endif		!end of nfix > 0
c
c Allocate dummy arrays to prevent overwriting dgamma(), im(), jm() which
c may be altered in GETQD1 -likewise for nsc(),ncyc
	ALLOCATE(dgamdum(100),nscdum(50),imdum(50,100),jmdum(50,100))
	read(unit=15,rec=irec) iver,
     &  imod,mtitle,k,kA,kB,kC,kD,rtitle,ilast,jlast,nrateq,
     &  ncon,ncdep,nlig,chardef,boundef,ncycd,vref,nvdep,kmfast,
     &  indmod,npar,nsetq1,kstat2,
     &  ((charmod(i,j),i=1,ilast),j=1,jlast),
     &  (irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),
     &  (QT(irate(i),jrate(i)),i=1,nrateq),
     &  (titlep(i),i=1,npar),
     &  (ligname(i),i=1,nlig),
     &  ((nbound(i,j),i=1,k),j=1,nlig),
     &  (IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
c     &  (dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
c     &  ((im(i,j),j=1,nsc(i)),i=1,ncyc),
c     &  ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &  (dgamdum(i),i=1,kA),(nscdum(i),i=1,ncycd),
     &  ((imdum(i,j),j=1,nscdum(i)),i=1,ncycd),
     &  ((jmdum(i,j),j=1,nscdum(i)),i=1,ncycd),
     &  (iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
     &  (hpar(i),i=1,nvdep),
     &  (pstar(i),i=1,4),(kmcon(i),i=1,9),
     &  (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),
     &  (ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),
     &  (efacq(i),i=1,nsetq),
     &  (statname(i),i=1,kstat2),
     &  nsub,kstat0,npar0,kcon,npar1,ncyc0
c
c ===? Is kstat needed here (in earlier version it was read as kstst1 which was not used)
c	if(.not.indmod) then
c	   kstat=kstat1
c	else if(indmod) then
c	   kstat=kstat1 - kcon
c	endif
c   Make sure IQ is defined
	do i=1,100		!ditto
	   do j=1,100
		IQ(i,j)=0
	   enddo
	enddo
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   IQ(i,j)=m
	enddo
c
	CLOSE(unit=15)
	DEALLOCATE(dgamdum,nscdum,imdum,jmdum)
	nchan=1 	!always (not kept in qmechs.dat)
c
c NB if spanning tree method is used, now restore any fixed values into QT
	if(useprim.and.nfix.gt.0) then
	   n=0
	   do m=1,npar
		if(jfix(m).eq.1) then
	 	   i=irate(m)
		   j=jrate(m)
		   n=n+1
		   QT(i,j)=qsav(n)
		endif
	   enddo
	   if(allocated(qsav)) deallocate(qsav)
	endif		!end of nfix > 0
c
c Set all constraints (but no conc) in QT using QSET_TRU (and, with imode=1,
c also generate theta0())
c 07/07/05 12:38pm insert catch here in case mechanism has been read from
c Windows version without defining rates, in which case all rates will be 
c zero giving zero divide in setmr.
	null=.true.		!to check if QD all zero
	do i=1,k
	   do j=1,k
		if(dabs(QT(i,j)).gt.1.d-20) null=.false.
	   enddo
	enddo
	if(null) then
	   do i=1,k
		do j=1,k
		   if(i.ne.j) then
			QT(i,j)=1.d0
		   endif
		enddo
	    enddo
	endif
c
	jset=1
	imode=1
	icons=0				!apply constraints (if any)
	if(icon.eq.3) icons=1		!don't apply constraints to fitted rates
	call QSET_TRU(QT,theta0,jset,conc,npar,irate,jrate,IQ,
     &    imode,icons,k)
c
	imod0=imod
	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
c	kF=k-kA
c	if(.not.newrate) then
	   print 669,imod0,mtitle,irecq,rtitles(irecq)(1:74)
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
	if(ans.eq.'N') goto 22
	icolour=0	!normal blue
	if(.not.nodata) then
	   wtitle='INITIAL GUESSES FOR FIT [1/(Ms) for assoc rates]'
	else
	   wtitle='VALUES FOR RATE CONSTANTS [1/(Ms) for assoc rates]'
	endif
20	do i=1,k
	   do j=1,k
		QTsav(i,j)=QT(i,j)
	   enddo
	enddo
	imd=0		!to display all constraints etc
	call DISPRATE(QT,irate,jrate,chardef,indmod,npar,titlep,
     &    jcon,wtitle,ligname,icolour,
     &    imod0,charmod,jlast,ilast,mtitle,
     &    ncon,IC,ncdep,IX,JX,IQ,nligs,IL,imd)
c check for manual changes in qwindhjc
c	re-apply constraints in case manual changes made in qwind
	call QSET_TRU(QT,theta0,jset,conc,npar,irate,jrate,IQ,
     &	 imode,icons,k)
	recheck=.false.
c
	do i=1,k
	   do j=1,k
c		if(QTsav(i,j).ne.QT(i,j)) then
		if(dabs(QTsav(i,j)-QT(i,j)).gt.1.d-7) then
		   print 30,i,j,QTsav(i,j),QT(i,j)
		   if(discprt) write(8,30) i,j,QTsav(i,j),QT(i,j)
30		   format(' Guess for QT(',i2,',',i2,')',
     & 		' changed from ',g13.6,' to ',g13.6)
		   recheck=.true.
		endif
	   enddo
	enddo
	if(recheck) then		!reset theta0 too
	   i=idebug2	!save
	   idebug2=2	!so prints to screen and disk
c	   idebug2=1	!so prints to screen only
	   call CHECKMR(QT)
	   idebug2=i	!restore
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
c		thsav(m)=theta0(m)
	   enddo
	   pause 'Recheck rate constants'
	   do i=1,k
		do j=1,k
		   QTsav(i,j)=QT(i,j)	!save current values
		enddo
	   enddo
	   recheck=.false.
	   goto 20
	endif
c	print 16,imod0,mtitle,irecq,rtitles(irecq)(1:74)
	if(.not.nodata) then
	   if(discprt) write(8,16) imod0,mtitle,irecq,rtitles(irecq)(1:74)
16	   format(/,
     & ' INITIAL GUESSES for fit of rate constants ',/,
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Initial guesses for rates from file #',i3,':',/,
     &  1x,a74)
	else
	   if(discprt) write(8,161)imod0,mtitle,irecq,rtitles(irecq)(1:74)
161	   format(/,
     & ' Values for rate constants ',/,
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Values for rates from file #',i3,':',/,
     &  1x,a74)
	endif
c
c Now insert PRNTEC50 here too, so EC50 for guesses can be seen
c before guesses accepted

	print 1
     	if(discprt) write(8,1)
1       format(/,' EC50 FOR INITIAL GUESSES:')
	call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
	ans='N'
	call DCASK(' Revise guesses to get different EC50',ans,ans)
	if(ans.eq.'Y') goto 20
c
	call PRTRATE(QT,npar,irate,jrate,titlep,jcon,nchan,
     &    ligname,dgamma,qfilem,k,kflag,idest,kA,kB,kC,kD,
     &    imod0,charmod,jlast,ilast,mtitle,
     &    ncon,IC,ncdep,IX,JX,IQ,nlig,IL,nlvar,vhold,vref,imd)
c
	mtitle=mtits(imod0)
c IQ etc already defined
c display rates, alter, print (and re-store?) for true mechanism
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c===NOT TO DISK -only final values for discprt
c	string=char(244)//' = calculated by microscopic rev'
c	print 13,string
c      if(discprt) write(8,13) string
c13	format(/,' INITIAL GUESSES for rate constants',/,
c     &'   (1/s, or 1/(Ms) for association rate constants)',/,
c     &'  *n = association rate constant for ligand #n ',/,
c     &  2x,a74,/,
c     &'   & = constrained value')
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
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c JUMP TO HERE IF NEW MODEL DEFINED IN QDEFINE()
99	continue
c
	idest=0
999	continue		!return without resetting idest
	imod=imod0		!copy for common
	if(allocated(QTtrue)) DEallocate(QTtrue)
	deallocate(QTsav)
	RETURN
	end


