	subroutine GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  useprim,kflag,idest)
c	subroutine GETQD(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
c     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c
c Subroutine for ALL theory progs to define Q matrix defined at concentrations
c in conc() for use in rest of program
c
c Many arrays in commons so those needed can be transmitted to calling prog
c
c Modif 11/16/03 08:12pm to set useini=false if last run used spanning tree
c (useprim=true) and this run does not, or vice versa (can tell that last run
c used tree if maximum nsc() is greater than 4)
c
c Modif 11/05/03 10:32am to use spanning tree method
c
c Modif 08/20/03 05:35pm to add more things that must be read and resaved
c for Windows version (read them if iver.ge.200, and resave them -no nead to
c read or write if iver=102.  These save position, colour etc of model diagram
c (like charmod in DOS version) -keep them for rewriting
c in common/winmod/ (in hjcfit)
c	character*3 snumw(100)		! state number
c	integer*4 icolwin(100)		! colour for open/schut
c	real*4 wposx(100),wposy(100)	! position
c	integer*4 nwidwin,nhghtwin	!window dimensions
c	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
c If iver.ge.200 than chardef=.false. and must add to read and write
c     &	((ic(i,j),i=1,2),j=1,ncon)
c     &	(snumw(i),i=1,k),(icolwin(i),i=1,k),(wposx(i),i=1,k),
c     &	(wposy(i),i=1,k),nwidwin,nhghtwin
c extra bytes k*(3+4+4+4) + 8
c
c Modif 06/25/03 09:06am to put npar and irate, jrate also in commons
c
c Modif 01/27/03 03:46pm to remove ec50 calcs -these should be done after
c leaving getqd so actual conc of other ligands know (when nlig>1)
c still use conc=1muM internally so MR rates can be set, but this should
c be invisible to user
c
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
c Modif 02/21/03 04:03pm to read models designed in the new windows program
c	iver = version number =101 initially, 102 now, 200 for models designed
c      in windows version
c  For these models chardef=false (even if they would fit on 2D chessboard),
c and the picture of the model must be taken from windows version

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
c	iver = version number =101 initially, 102 now, 200 for models designed
c      in windows version
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
	REAL*8 QD(100,100),Pinf(100),dgamma(100)
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
	character qpath*30,qname*8,suffix*3,ndev*2
	character rfile*40,qfilem*40,qfilout*40,qfilmsav*40
c Declarations for modif 03/05/95 06:16pm
	character*10 titlep(200),titlep1(200)
	integer SCRNBUF0(1024)
	integer IQ(100,100)
	integer inew(100)
	real*8 QT(100,100),r
	real*8 psav(200)
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
c=	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/LIG/nlig,IL(100)
	common/nmodel/cmodnew
	COMMON/qblk/IQ
c New arrays for qmechs.dat
	allocatable::nsc1(:),im1(:,:),jm1(:,:)	!for values from .ini
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle,rtitles(500)	!title to describe rate constants
	integer IQQ(100,100)
c=	integer irateq(200),jrateq(200)
c=	real*4 conc(10)
	real*4 conc1(10)
	logical boundef,chardef,indmod,repeat,samefil,altmod
c For qdefine
	logical readp
	logical iopen,jopen,bind	!to print equilib constants
	character*7 kname 	!to print equilib constants
c
	logical kmfast,readini,useini,present,renum,alter,newrate
c=	logical vjump,cjump
c	logical defrate
	character*1 ans,UC,ans1
	character*74 mtitle,mtitle1	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	character string*74,pre*3
c===	integer jmod(40)
	integer jmod(500)
c	logical debug,caplock
	logical discprt
	common/dp/discprt
c Addition for getbound
c	integer nbound(10,2)
	integer nbound(100,10)
	common/nbnd/nbound
c For inwindc
	character*79 heading,title
	character*11 cdata(50,3),cnum,cnum0
c
cc For eqec50
c	real*4 EC50,curmax,curinfs,concmax
c	real*8 cfacd,vknd
c	logical monot
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	logical useprim,useprim0		!use spanning tree method
c  For windows version
	character*3 snumw(100)		! state number
	integer*4 icolwin(100)		! colour for open/schut
	real*4 wposx(100),wposy(100)	! position
	integer*4 nwidwin,nhghtwin	!window dimensions
	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
c
c Add common in case charmod etc needed in calling prog
c	SAVE QT,pinf,imod0,charmod,jlast,ilast,mtitle
	SAVE pinf
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	common/mod/imod
c
c
	character*40 mtitle2*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle2 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	common/db/ideb		!debug level
	common/ren/renum		!so can be kept in .ini if req
c For statmod
	ALLOCATABLE::statmod(:,:)
	character*1 statmod
	common/sm/nsmax		!for getqd/charq
c  For debugging qmechs
	logical allocated
	allocatable::chardum
	character*1 chardum(:)
	logical statdef	!state names defined
c  Common for calcmr/dfpmin/getrev
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
	integer isetmr(50)
	common/mr1/isetmr
c
c for find_cyc
c	allocatable::link
c	integer link(:,:)
c for definitions of cubes
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
c
c	debug()=caplock()
c
	km=100				!array dimensions
	if(kflag.eq.2) then   !save input values
	   kA1=kA
	   kB1=kB
	   kC1=kC
	   kD1=kD
	else if(kflag.eq.-1) then
	   goto 100
	endif
c
	if(.not.readini) imodold=-1
c
	if(idest.eq.105) goto 105	!another run, same model
c
c	imodsav=-1		!until defined
c
	if(allocated(nsc1)) deallocate(nsc1,im1,jm1)
	allocate(nsc1(50),im1(50,100),jm1(50,100))
	if(readini) then	!save im() etc from .ini
	   ncyc1=ncyc
	   do i=1,50
		nsc1(i)=nsc(i)
		do j=1,100
		   im1(i,j)=im(i,j)
		   jm1(i,j)=jm(i,j)
		enddo
	   enddo
	endif
c
	newrate=.false.
c	defrate=.false.
c First check whether the new model+rates file, QMECHS.DAT is present.
c If not call QCONVERT to look for old files and create qmwch.dat.
	if(.not.readini) qfilem='QMECHS.mec'
	qfilmsav=qfilem
22	call TITENT0('Name/path for models file:',qfilem,40,.false.)
	samefil=qfilmsav.eq.qfilem
	INQUIRE(file=qfilem,exist=present,flen=nlenm)
	call DOSFILE(qfilem,40,qfilem,nerr)
	if(nerr.ne.0) then
	   call BELL(1)
	   print 221
221	   format(' ERROR: not DOS file format')
	   goto 22
	endif
	if(.not.present.or.nlenm.eq.0) then
	   print 21,qfilem
21	   format(
     & ' File not found: ',a40)
	   qfilem='QMECHS.mec'
	   INQUIRE(file=qfilem,exist=present,flen=nlenm)
	   if(.not.present.or.nlen.eq.0) then
		print 216
216	   	format(
     & ' Can''t find file ',a40)
	      call QCONVERT(qfilem)
		chardef=.true.	!all old models defined in charmod() form
		goto 22
	   endif
	endif
	call PARSNAME(qfilem,qpath,ndev,qname,suffix,nopath,40)
c No need for qgen.ini either, ireclast is kept in qmechs.dat, and
c probably in prog.ini too
c
c	qfilem='QMECHS.mec'
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c     Read header part
	read(unit=15,rec=1) iver
	if(iver.eq.101) then
c	   read(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart		!v101 format
	      call CONVQM(qfilem)		!converts v101 to v102
      	OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')	!re-open after convqm
	endif
c from now on, qmechs is version 102
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
c	Read part of data record, up to rtitle, to define rtitles() and jmod()
	imodmax=0
	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
	   read(unit=15,rec=irec) iver,
     &	imod,mtitle,k,kA,kB,kC,kD,rtitle
	   jmod(irq)=imod
	   if(imod.gt.imodmax) imodmax=imod
	   mtits(imod)=mtitle	!so mtits(i)=title of model #i
	   rtitles(irq)=rtitle
	enddo
c Check number of different models that are in qmechs.dat (in retrospect,
c it was big mistake not to put mtitle in each record! (Make conversion prog
c to do this?). Without this it hard to transfer a record from one qmechs
c to another
	nmods=0
	do irq=1,nrecs
	   imod=jmod(irq)	!just defined
	   repeat=.false.
	   if(irq.gt.1) then	!has imod occurred already?
		do j=1,irq-1
		   if(jmod(j).eq.imod) repeat=.true.	!imod already occurred
		enddo
	   endif
	   if(.not.repeat) nmods=nmods+1
	enddo
c
	CLOSE(unit=15)
	irec=ireclast		!until PROG.INI is read
262	if(readini.and.imodold.ne.0.and.irecq.ne.0) then
	   print 261,imodold,mtits(imodold),irecq,rtitles(irecq)(1:74)
261	   format(/,
     & ' Last run used mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' with rates from file #',i4,':',/,
     &  1x,a74,/)
	endif
c
	iread=1
263	print 26,iread
26	format(
     &  ' (1) Read mechanism and rate constants from <prog>.ini',/,
     &  ' (2) Choose a mechanism from those already defined',/,
     &  ' (3) Define a completely new mechanism',/,
     &  ' (4) Define a new mechanism by modifying an existing one',/,
     &  ' (5) List all the models in QMECHS.MEC (new version)',/,
     &  ' (6) Exit',/,
     &  ' Option number [',i2,'] = ')
	call INPUTi(iread)
c
	if(iread.eq.6) STOP
	if(iread.eq.1) then
	   if(.not.samefil) then
		call BELL(1)
		print 264
264		format(
     &  ' Cannot use values from .ini because different qmechs.mec',
     &  ' file specified',/)
		iread=2
		imodold=0	!so useini set false below
		goto 263
	   else
		goto 182
	   endif
	endif
c
	if(iread.eq.2) then	!read mtits() -already read above!
	   imodold=0	!so useini set false below
	   ans='N'		!so models listed at label 59
	   imod0=imodold	!use last run as default anyway? (if present)
	   goto 59		!get another model
	endif
c
	if(iread.eq.3.or.iread.eq.4) then
c       NB current version of qdefine does not ask for conc, or return QD
	   call QDEFINE(iread,pinf,QT,IQ,titlep,titles,
     &    npar,nrateq,nlig,IL,dgamma,chardef,qfilem,
     &    indmod,irate,jrate,IQQ,ligname,statname,readp,
     &    nmods,mtits,imodmax,altmod,charmod,ilast,jlast,kstat,
     &    nchan,vkin,vhold,vref,
     &    nsub,kstat0,npar0,kcon,npar1,ncyc0)
	   if(iread.eq.-1) then		!error in modifying model -try again
		iread=1
		goto 263
	   endif
c
	   if(chardef.and.(.not.indmod)) then
		nmax=0		!longest statname
		do m=1,kstat
		   n=NBLANK1(statname(m))
		   if(n.gt.nmax) nmax=n
		enddo
		if(mod(nmax,2).eq.1) nmax=nmax+1		!make nmax even
	      jlasts=1+jlast*nmax
	      ALLOCATE(statmod(ilast,jlasts))
	      call STATEMOD(ilast,jlast,jlasts,nmax,charmod,
     &	statname,statmod)
c Print statmod to disc and screen
	      print 108
	      if(discprt) write(8,108)
		do i=1,ilast
	         print 67,(statmod(i,j),j=1,jlasts)
	         if(discprt) write(8,67) (statmod(i,j),j=1,jlasts)
c67	         format(4x,70a1)
		enddo
		pause
		DEALLOCATE(statmod)
	   endif
	   goto 99		!saved, if req, within qdefine
	endif
c
c For iread=5 list (some) details of all models
c Nodified for iver=102, and some old debugging stuff removed
	if(iread.eq.5) then
	   print 210,qfilem,nrecs
	   if(discprt) write(8,210) qfilem,nrecs
210	   format(' Mechanisms file: ',a40,/,
     &   ' contains ',i4,' records of rate constants +model')
c  Clear the bit between end of header (record 1) and start of first data
c record
	   OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
cc	   nhead=4+4+4+4+4+74*100+500*4	!bytes in header v101
c	   nhead=4+4+4+4+500*4			!bytes in header v102
c	   irecn=jstart(1)			!first byte of data record
c	   iblank=irecn-nhead-1
c	   if(allocated(chardum)) deallocate(chardum)
c	   allocate(chardum(iblank))
c	   irs=nhead+1
c	   read(unit=15,rec=irs) (chardum(i),i=1,iblank),iver1
c	   read(unit=15,rec=irecn) iver11
c	   if(iver1.ne.iver11) then
c		pause 'record boundaries wrong'
c		STOP
c	   endif
c	   do i=1,iblank
c		chardum(i)=char(0)
c	   enddo
cc  Unaccountably the next line seems to overwrite iver, nrecs so rewrite
cc the whole header
c  next is old (iver=101) version
c	   write(unit=15,rec=1) iver,nrecs,
c     &    nextrec,ireclast,nmods,mtits,jstart,(chardum(i),i=1,iblank)
c	   deallocate(chardum)
cc==check header still OK
c	   read(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c	   read(unit=15,rec=irecn) iver

c
c    Calc isum=SUM[nsc(i)]
	   isum=0
	   do i=1,ncyc
		isum=isum + nsc(i)
	   enddo
	   do irecq=1,nrecs
		irec=jstart(irecq)	!start byte for data in record #irecq
	      print 215
	      if(discprt) write(8,215)
215		format(/,/,
     &     '______________________________________________________')
	      print 211,irecq,irec
	      if(discprt) write(8,211) irecq,irec
211		format(/,/,
     &     ' Record number ',i4,' (starts at byte #',i6,')')
c
		nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &  	  8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &  	  4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &  	  4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat
     &    	  +6*4
c    Erase any previously-read charmod()
		do i=1,25
		   do j=1,40
			charmod(i,j)='  '
		   enddo
		enddo
c Check on rubbish in qmechs file -is this between one record and next?
c    Calc isum=SUM[nsc(i)]
		isum=0
		do i=1,ncyc
		   isum=isum + nsc(i)
		enddo
c now iver=102 version
		if(irecq.lt.nrecs) then		!get number of bytes in record
		   read(unit=15,rec=irec) iver,
     &	   imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	   ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	   indmod,npar,nsetq,kstat
c
		   nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+
     &	   4*nrateq+
     &  	  8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &  	  4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &  	  4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat1
     &    	  +6*4
c  08/21/03 08:42am new things added to Windows version that must be read
c if present (iver.ge.200) so that they can be written back (in WRITQM) and
c the file can still be read in Windows version
		   if(iver.ge.200) then
			nbytes=nbytes+4*2*ncon + 30*kstat1	!addition for IC
			nbytes=nbytes + k*(3+4+4+4) + 8 	!addition for snumw() etc
		   endif
c NB the 4*2*ncon is for the IC(2,ncon) array which is kept as part
c of model for windows version -the 30*kstat1 is for parameters
c relating to Windows display that are not read or written in DOS version
		   irecend=irec+nbytes
		   iblank=jstart(irecq+1)-irecend-1   !# of empty bytes before next rec
		   irecb=irec+nbytes+1		!byte# where empty bytes start
		endif

		irecsav=irec
		irecqsav=irecq
		read(unit=15,rec=irec) iver,
     &	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
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
		if(.not.indmod) then
		   kstat=kstat1
		else if(indmod) then
		   kstat=kstat1 - kcon
		endif
c        Make sure IQ is defined
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
		print 214,mtitle,imod
c=		if(discprt) write(8,214)mtitle(1:74),imod
214   	format(1x,a74,/,' Model number = ',i3,/)
		if(nsetq.gt.0) then
		   if(discprt) write(8,228)
228  		   format(' Fixed q(i,j), set for independent models:')
		   do L=1,nsetq
			if(discprt) write(8,229) ieq(L),jeq(L),efacq(L),
     &			ifq(L),jfq(L)
229			format(1x,2i3,'= ',f9.3,' times ',2i3)
		   enddo
		endif
c
	      if((kA+kB+kC+kD).ne.k) then
		   call BELL(2)
		   print 324,k,kA,kB,kC,kD
		   if(discprt) write(8,324) k,kA,kB,kC,kD
324		   format(/,
     &	' Error in mechanism: number of states = ',i3,/,
     &	'  but kA, kB, kC, kD = ',4(3x,i4))
		   pause
		endif
		if(irecq.lt.nrecs) then		!get number of bytes in record
		   if(allocated(chardum)) deallocate(chardum)
		   allocate(chardum(iblank))
		   read(unit=15,rec=irecb) (chardum(i),i=1,iblank),iver1
		   irecn=jstart(irecq+1)
		   read(unit=15,rec=irecn) iver11
		   if(iver1.ne.iver11) then
			pause 'record boundaries wrong'
			STOP
		   endif
		   do i=1,iblank
		      chardum(i)=char(0)
		   enddo
		   if(irecb.eq.1) then
			pause 'irecb=1'
			STOP
		   endif
		   write(unit=15,rec=irecb) (chardum(i),i=1,iblank)
		   read(unit=15,rec=1) iver2,nrecs2
		   if(nrecs2.eq.0) then
			call BELL(3)
			pause ' nrecs overwritten!'
		   endif
		endif

    		n=0
		do i=1,nrecs
		   if(jmod(i).eq.imod) n=n+1
		enddo
		if(n.eq.0) then
		   call BELL(1)
		   print 191
		   if(discprt) write(8,191)
c191		   format(
c     &   ' No rate constants have been stored for this model.',/,
		else
		   print 213
		   if(discprt) write(8,213)
213		   format(
     &   	' The following rate constants have been stored:',/,
     &   	'   file #:                      Name of rate file')
		   do i=1,nrecs
			if(jmod(i).eq.imod) then
			   print 192,i,rtitles(i)
			   if(discprt) write(8,192) i,rtitles(i)
c192			   format(1x,i3,': ',a74)
			endif
		   enddo
		endif
c Draw model with charmod()
		if(chardef) then
		   do i=1,ilast
 			print 671, (charmod(i,j),j=1,jlast)
			if(discprt)write(8,671) (charmod(i,j),j=1,jlast)
671			format(4x,35a2)
		   enddo
		endif
c if defined, draw model with state names (not defined for independent
c  subunit mechanisms)
		if(chardef.and.(.not.indmod)) then
		   nmax=0		!longest statname
		   do m=1,kstat
			n=NBLANK1(statname(m))
			if(n.gt.nmax) nmax=n
		   enddo
		   if(mod(nmax,2).eq.1) nmax=nmax+1		!make nmax even
	         jlasts=1+jlast*nmax
	         ALLOCATE(statmod(ilast,jlasts))
	         call STATEMOD(ilast,jlast,jlasts,nmax,charmod,
     &	   statname,statmod)
c Print statmod to disc and screen if defined, otherwise print charmod
	         print 108
		   if(discprt) write(8,108)
		   statdef=.false.
		   do i=1,10
			is=ichar(UC(statname(i:i)))
			if(is.ge.65.and.is.le.90) statdef=.true.	!state names defined
		   enddo
		   if(statdef) then
			do i=1,ilast
	      	   print 67,(statmod(i,j),j=1,jlasts)
	      	   if(discprt) write(8,67) (statmod(i,j),j=1,jlasts)
c67	      	   format(4x,70a1)
			enddo
		   endif
		   DEALLOCATE(statmod)
		endif		!end of if(.not.indod)
	   enddo	!end of irecq=1,nrecs
c
	   read(unit=15,rec=1) iver2,nrecs2
	   if(nrecs2.eq.0) then
		call BELL(3)
		pause ' nrecs overwritten!'
	   endif
	   goto 262
	endif		!end of iread=5
c
c Now section for iread=1
c Read everything for model/rates specified in .ini, from record #irec,
c defined above
182	continue
	irec=jstart(irecq)	!start byte for data in record #irecq
	irecsav=irec
	irecqsav=irecq
c    Erase any previosly-read charmod()
	do i=1,25
	   do j=1,40
		charmod(i,j)='  '
	   enddo
	enddo
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c First read the constants to guard against undefined values of indmod
c parameters
	read(unit=15,rec=irec) iver,
     &	imod,mtitle,k,kA,kB,kC,kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat1
	if(.not.indmod) then
	    nsetq=0
	    kstat=k
	else if(indmod)then
	    kstat=kstat1 - kcon
	endif
	if(iver.ne.102.and.iver.ne.200) then
	   call BELL(1)
	   print 78,iver
78	   format(' ERROR in GETQD: version number = ',i4)
	endif
	if(iver.eq.200) then
	   call BELL(1)
	   print 781
781	   format(
     & ' This mechanism was designed in Windows version, and can',/,
     & ' be displayed graphically only in Windows',/)
	   pause
	endif
	irecsav=irec
	irecqsav=irecq
	if(iver.ge.200) then
	   read(unit=15,rec=irec) iver,
     &	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq1,kstat2,
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
     & 	nsub,kstat0,npar0,kcon,npar1,ncyc0,
     &	((ic(i,j),i=1,2),j=1,ncon),
     &	(snumw(i),i=1,k),(icolwin(i),i=1,k),(wposx(i),i=1,k),
     &	(wposy(i),i=1,k),nwidwin,nhghtwin
	else
	   read(unit=15,rec=irec) iver,
     &	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq1,kstat2,
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
	endif
c
	if(.not.indmod) then
	   kstat=kstat1
	else if(indmod) then
	   kstat=kstat1 - kcon
	endif
c        Make sure IQ is defined
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
      if((kA+kB+kC+kD).ne.k) then
	   call BELL(2)
	   print 324,k,kA,kB,kC,kD
c324	   format(/,
c     &  ' Error in mechanism: number of states = ',i3,/,
c     &  '  but kA, kB, kC, kD = ',4(3x,i4))
	endif
	if(nsetq.gt.0) then
	   if(discprt) write(8,228)
c228	   format(' Fixed q(i,j), set for independent models:')
	   do L=1,nsetq
		if(discprt) write(8,229) ieq(L),jeq(L),efacq(L),
     &		ifq(L),jfq(L)
c229		format(1x,2i3,'= ',f9.3,' times ',2i3)
	   enddo
	endif
	nchan=1 	!always (not kept in qmechs.dat)
c
	iflag=3		!so numerical values shown in QWIND2
c
	CLOSE(unit=15)

	imod0=imod

c in iver=102, mtitle is in each record
c	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
	kF=k-kA
	if(.not.newrate) then
	   print 669,imod0,mtitle,irecq,rtitles(irecq)(1:74)
669	   format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Rates from file #',i4,':',/,
     &  1x,a74,/,
     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	else
	   ans='Y'
	endif
c List available models (jump here for iread=2)
c Modified 10/30/01 10:12am do only those models that are actually
c in qmechs are listed. Go through each record -is model 1 present? -if
c so print title -then is model 2 present? if so print title, and so on
c up to the highest number model present =imodmax
c If model # imodold is present then use it as default, otherwise use last
c model as default
59	if(UC(ans).eq.'N') then
58	   continue
	   n=0
	   print 560
560	   format(' Model #              title')
	   do imd=1,imodmax  !list all the model titles (and details when iread=5)
		present=.false.
		do j=1,nrecs
		   if(imd.eq.jmod(j)) then
			present=.true.     	!model #im is present
		   endif
		enddo
		if(present) then
		   n=n+1
	         print 56,imd,mtits(imd)
56             format(' (',i2,') ',a74)
		   if(readini.and.imd.eq.imodold) then
			imod0=imodold		!default
		   else
			imod0=imd		!default
		   endif
		   mtitle=mtits(imd)
		   if(mod(n,20).eq.0) then
			call BELL(1)
			print 11
11			format(' Hit space bar for more')
			call ANYKEY
		   endif
		endif
	   enddo

	   call BELL(2)
	   if(n.gt.1) imod0=0		!if more than one model, force choice
	   print 561,imod0
561	   format(' Use model number [',i3,'] = ')
	   call INPUTi(imod0)
	   if(imod0.lt.1.or.imod0.gt.imodmax) goto 58
	   mtitle=mtits(imod0)
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
		call BELL(1)
		print 191
191		format(
     &	' No rate constants have been stored for this model.',/,
     &	' You have three options:',/,
     &	'  (1) Enter the mechanism and rate again from scratch',/,
     &	'    using option 3 or 4 that follow, OR',/,
     &	'  (2) Choose a different mechanism, OR',/,
     &	'  (3) Leave this program',/,
     &	' Option number [1] = ')
		in=1
		call INPUTi(in)
		if(in.eq.3) then
		   STOP
		else
		   goto 262
		endif
c	      newrate=.true.
cc      iflag=1 rate constant values initially blank, names shown
c	      iflag=1
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
15		format(' File # for rates [',i4,'] = ')
		call INPUTi(irecq)
c		imodsav=imod0
		goto 182		!read rates
	   endif
	endif
c Whole section for nmod=0 removed here: nmod now superfluous?
c Print the rate constants here
c Change any rate constants?
c Show parameters in QWIND automatically, for approval
c Now show model + QWIND
	mtitle=mtits(imod0)
c105	continue		!return here to modify rate constants for another run
c	  Define IQ here when same Q used
	if(chardef) then
	   call CQLAST(charmod,ilast,jlast)		!get ilast,jlast from charmod
	   call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)	!get IC,kA,kF
c	   do i=1,100		!ditto
c		do j=1,100
c		   IQ(i,j)=0
c		enddo
c	   enddo
	else
c kA, kF already defined, but need to define IC(1,m)=i. IC(2,m)=j when
c state i is connected to state j (done in CHARQ if model defined in
c character for, chardef=true). Check non-zero rates (could also be
c done from irate() and jrate())
	  if(iver.lt.200) then 	!IC read from record for Windows version
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
	endif
	i1=0
c IQ should already be defined (after 'read(unit=') for indmod too
	if(.not.indmod) then
	   i1=0
	   do m=1,ncon
		i=IC(1,m)
		j=IC(2,m)
		i1=i1+1
		IQ(i,j)=i1
		i1=i1+1
		IQ(j,i)=i1
	   enddo
	endif
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
	   if((kA+kF).ne.k) then
		call BELL(2)
	   print 325,kA,kF
325	   FORMAT(
     & '  Give number of open and shut states, kA, kF [',i3,
     &	', ',i3,'] = ')
		call INPUT2i(kA,kF)
		k=kA+kF
		goto 323
	   endif
	   print 2012,kB
2012	   format(
     &  ' For calculation of theoretical bursts, take number ',/,
     &   '   of short-lived (within burst) shut states [',i3,'] = ')
	   call INPUTi(kB)
	   kC=kF-kB
	   do i=1,kB
		print 2013,i+kA
2013		format(' State ',i3,' is assumed brief (shut within burst)')
	   enddo
	   do i=1,kC
		print 2014,i+kA+kB
2014	     format(' State ',i3,' is assumed long (shut between bursts)')
	   enddo
	   print 2015
2015	   format(' Are these  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		print 206,kA1
206		format('&  number of open states, kA [',i2,'] = ')
		kA=kA1
	      call INPUTi(kA)
		print 202,kB1
202		format(
     &	'&  number of short-lived shut states, kB [',i2,'] = ')
		kB=kB1
	      call INPUTi(kB)
		kC=k-kA-kB
	      print 203,kC1
203	      format(
     &	'&  number of long-lived shut states C, kC [',i2,'] = ')
	      call INPUTi(kC)
c Remove kD for now?
		kD=0
	      print 204,kD
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
	print 73,nchan,kA
73	format(/,' Number of channels = ',i8,/,
     & ' Number of open states = ',i3)
c==     & ' Number of open states = ',i3,/,
c==  & ' membrane potential (mV) = ',g13.6)
c     & ' conductances (pS) = ',10g13.6)
	do i=1,kA
	   print 321,i,statname(i),1.0d12*dgamma(i)
321	   format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
	enddo
	print 74
74	format('  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') then
	   print 75,nchan
75	   format(' Number of channels [',i3,'] = ')
	   call INPUTi(nchan)
c	   v=-100.		!this is now in getvdep, called below
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
c
	if(discprt) write(8,73) nchan,kA
	if(discprt) write(8,321) i,statname(i),1.0d12*dgamma(i)
c
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
c	if(.not.readini) renum=.false.	!otherwise value from .ini
c	ans1='N'
c	if(renum) ans1='Y'
c	call DCASK('Renumber the states',ans1,ans)
	if(renum) then
	   ans='Y'
	else
	   ans1='N'
	   call DCASK('Renumber the states',ans1,ans)
	endif
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
c      swap irate,jrate in qswap
	   call QSWAP(QT,IQ,k,inew,irate,jrate,npar,km)
c=	   call QSWAP(QT,IQ,k,inew,km)	!swap rows and cols of QT and IQ
c renumber conductances, c-dep rates, v-dep rates, connections and cycles
	   call RENUMB(inew,dgamma,statname,nbound,nlig,k)
	   call NEWMOD(charmod,inew,ilast,jlast,.false.)	!cmodnew in common
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
	if(discprt) then
	   do i=1,kA
		write(8,2010) i
2010		format(' State ',i3,' is open')
	   enddo
	   do i=1,kB
		write(8,2013) i+kA
c2013		format(' State ',i3,' is assumed brief (shut within burst)')
	   enddo
	   do i=1,kC
		write(8,2014) i+kA+kB
c2014	     format(' State ',i3,' is assumed long (shut between bursts)')
	   enddo
	endif
c     if(discprt) write(8,2431)ka,kb,kc,kd
c2431	format(
c     & ' No of states in each subset: kA, kB, kC, kD = ',4i3)
c
c Check which states to be calculated by micro rev (zero values from .ini
c if model has been changed)
c
c Modif 11/05/03 10:32am to use spanning tree method
c Number of rates to be found my MR found directly from
	nmr=ncon-k+1
	print 80,nmr
	if(discprt) write(8,80) nmr
80	format(/,
     & ' Number of rate constants to be set by microscopic rev = ',i3)
	if(nmr.gt.0) then
	   i=1
	   useprim0=useprim	!default from .ini
	   if(useprim) i=2	!default from .ini
	   print 81,i
81	   format(
     & ' (1) Use ordered 4-state cycles',/,
     & ' (2) Use spanning tree to locate cycles',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(i)
	   useprim=i.eq.2
	endif
c
c Want to use MR parameters from .ini as defaults in GETREV if thay are valid
c (for same model etc), but if some are set =0 (so MR not obeyed for that cycle)
c want to write the zero(s) only to .ini, not to qmechs.dat
c Also some progs did not store jm() in .ini so if not valid from .ini
c copy it from jm=values from qmechs
	if(readini.and.(imod.eq.imodold).and.
     &    nsc1(1).ge.1.and.nsc1(1).le.k) then	!restore im() etc from .ini
	   useini=.true.			!use mr params from .ini as defaults
	else
	   useini=.false.
	endif
c
c Set useini=false if last run used spanning tree (useprim=true) and this
c  run does not, or vice versa (can tell that last run used tree if
c  maximum nsc() is greater than 4)
c
	if(useprim.and.(.not.useprim0)) useini=.false.
	if((.not.useprim).and.useprim0) then
	   useini=.false.
	   do i=1,50
		nsc(i)=4
	   enddo
	endif
c
	if(useini) then
	   if(jm1(1,1).lt.1.or.jm1(1,1).gt.k) then	!jm invalid
		do i=1,ncyc
		   nsc(i)=nsc1(i)
		   do j=1,nsc(i)
			jm1(i,j)=jm(i,j)
		   enddo
		enddo
	   endif
	endif
c Next bit should not be needed now, as m.r. parameters are set above
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
	if(useprim) then
	   ncyc=nmr
	   ncmax=50		!max no of cycles
	   do i=1,ncmax
		automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
		isetmr(i)=i		!order irrelevant here
	   enddo
c  do not overwrite obeymr() from .ini
	   if(.not.useini) then
	      do i=1,ncmax
		   obeymr(i)=.true.
		enddo
	   endif
c
1	   continue
	   if(chardef) then
		do i=1,ilast
 		   print 671, (charmod(i,j),j=1,jlast)
		   if(discprt)write(8,671) (charmod(i,j),j=1,jlast)
c671		   format(4x,35a2)
		enddo
	   endif
c
c 	   getrevT is version for theory progs of getrevH used in hjcfit
	   call GETREVT(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,k,
     &     titlep,npar,irate,jrate,useini)
c
	else if(.not.useprim) then
c	   Best also check cycles (cycles with > 4 states could have been
c	   written to .ini, or to .mec if qdefine used spanning tree
c	   allocate(link(100,100)
c	   do j=1,ncon
c		k1=ic(1,j)
c		l1=ic(2,j)
c		link(k1,l1)=1
c		link(l1,k1)=1
c	   enddo
c	new find cycles subroutine
c	for the moment finds cycles with up to 4 states
c	   call find_cyc(k,link,ncyc,im,jm,nsc)
c	   deallocate(link)
	   ncmax=4
	   call CYCQ1(k,ncon,ic,ncyc,nsc,im,jm,ncmax)
c 	   Check cubes (done in qdefine, but at present not stored with model so
c        FINDCUBE must be done each time)
	   call FINDCUBE()		!all args in commons
c
c Now call GETREV with both nsc1, im1() etc from .ini, AND nsc, im() etc from
c QMECHS.
c GETREV now sets obeymr(i)=true if cycle #1 obeys MR
	   call GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,titlep,
     &      npar,irate,jrate,useini)
	endif

c im(),jm() and obeymr() now defined
c
	if(allocated(nsc1)) deallocate(nsc1,im1,jm1)
105	continue		!return here to modify rate constants for another run
c Record parameter values before QWIND so any changes can be noted
	do m=1,npar
c=	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   i=irate(m)
	   j=jrate(m)
	   psav(m)=QT(i,j)
	enddo
c===	if(chardef) then
	   call CAPTSCN(scrnbuf0)	!before putting up model
	   if(idest.eq.105)  goto 1051
c check the state names
	   if(.not.indmod) then
		kstat=k
	   else
		if(kstat.le.1.or.kstat.ge.k) then
		   print 112
112		   format(
     &	    ' Number of basic states in the mechanism = ')
		   call INPUTi(kstat)
		endif
	   endif
	   print 831,kstat
831	   format(/,
     &   ' Check the state names (eg A2R) of the ',i2,
     &	' basic states (up to 10 characters)')
	   pause
	   if(chardef) then
		call MODWIND(cmodnew,ilast+2,jlast+2,2,ilast,jlast,0)	!display model
	   endif
	   ni0=kstat	!rows
	   nj0=1		!columns
	   nidisp=kstat
	   if(kstat.gt.20) nidisp=20
	   nr1=-1		!so window initially in default position
	   iconst=0		!no constraint
	   iflagc=nj0	!show data
	   call INTCONV(kstat,cnum0)
c	   title=charnb(cnum0)//' States'
	   heading='   state name '
	   if(.not.indmod) then
		title=charnb(cnum0)//' States'
		do i=1,kstat
		   cdata(i,1)(1:10)=statname(i)
		enddo
	   else if(indmod) then	  !the kcon concerted states are 1st in statname
		title=charnb(cnum0)//'Subunit states'
		do i=1,kstat
		   cdata(i,1)(1:10)=statname(i+kcon)
		enddo
	   endif
	   nd1=50	!declared dimensions of cdata
	   nd2=3
	   call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2,0)
         if(discprt) write(8,134)
134	   format(/,' state #    state name')
	   nmax=0		!longest statname
	   kc0=0
	   if(indmod) kc0=kcon
	   do m=1,kstat
		statname(m+kc0)=cdata(m,1)(1:10)
		n=NBLANK1(statname(m))
		if(n.gt.nmax) nmax=n
            if(discprt) write(8,135) m,statname(m)
135		format(4x,i3,5x,a10)
	   enddo
	   if(mod(nmax,2).eq.1) nmax=nmax+1		!make nmax even
c
c   In case of indmod, display names of concerted states (if any) separately
         if(indmod.and.kcon.gt.0) then
		print 833,kcon,1,kcon
833		format(/,
     &   ' Check names of the ',i2,
     &	' concerted states (states #',i2,' - ',i2,')',/,
     &	' (names of open states first)')
		pause
		iflagc=3		!show data
c      concerted states are now the first kcon names in statname()
		do i=1,kcon
		   cdata(i,1)=statname(i)	!use old names for basic states
		enddo
		ni0=kcon	!rows
		nj0=1		!columns
		nidisp=kcon
		nr1=-1		!so window initially in default position
		call INTCONV(kcon,cnum0)
		title=charnb(cnum0)//'Concerted states'
		call CLS() 	!clear screen
		call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2,0)
		do m=1,kcon
		   statname(m)=cdata(m,1)(1:10)
		enddo
	   endif
c
1051	   continue		!return here to modify rate constants for another run
c Now make state diagram with AR, A2R etc rather than o1,c3 etc
c In statmod, jlast 2-byte characters per row, replaced by nmax-byte
c names
c	   jlasts=jlast+nsmax*(nmax-2)/2
c	   jlasts=2+jlast+nsmax*nmax
	   if(chardef.and.(.not.indmod)) then
c===	   if(.not.indmod) then
	      jlasts=1+jlast*nmax
	      ALLOCATE(statmod(ilast,jlasts))
	      call STATEMOD(ilast,jlast,jlasts,nmax,charmod,
     &	statname,statmod)
c Print charmod to disc
		mtitle=mtits(imod0)
	      if(discprt) write(8,670)mtitle,imod0
c670	      format(/,1x,a74,/,' Model number = ',i3,/)
	      if(chardef) then
		   do i=1,ilast
			if(discprt) write(8,671) (charmod(i,j),j=1,jlast)
c671			format(4x,35a2)
		   enddo
		endif
c Print statmod to disc and screen
	      print 108
	      if(discprt) write(8,108)
		do i=1,ilast
	         print 67,(statmod(i,j),j=1,jlasts)
	         if(discprt) write(8,67) (statmod(i,j),j=1,jlasts)
67	         format(4x,70a1)
		enddo
		pause
		DEALLOCATE(statmod)
	   endif	!end of 'if(chardef)'
c
c
c check the rate constants
	   if(chardef) then
		call MODWIND(cmodnew,ilast+2,jlast+2,2,ilast,jlast,0)	!display model
	   endif
	   if(.not.indmod) then
c===		ni=2*ncon	!number of rows
		ni=npar		!number of rows (OK for independent models too)
		nidisp=ni
		if(nidisp.gt.20) nidisp=20
		nr1=0		!posn of QWIND on screen
		if(chardef) then
		   nc1=2*jlast+11
		else
		   nc1=6
		endif
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
		nr1=0
		nc1=2*jlast+11
		iconst=0	!no constraint
		iflagc=nj0		!show all cols
		nd1=50		!declared dimensions of cdata
		nd2=3
		call INTCONV(npar,cnum)
		title='     '//charnb(cnum)//' rate constants'
		heading='    name         value        ligand '
		call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	 nr1,nc1,iflagc,iconst,nd1,nd2,0)
		do i=1,npar
		   titlep(i)=cdata(i,1)(1:10)
		   call CHTOREAL(cdata(i,2),xs)
		   QT(irate(i),jrate(i))=dble(xs)
		enddo
         endif
	   call DISPSCN(scrnbuf0)  !restore orig screen
	   if(discprt) write(8,6691)imod0,
     &	mtitle,irecq,rtitles(irecq)(1:74)
6691	   format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Rates from file #',i4,':',/,
     &  1x,a74,/)
	   if(itry.eq.1) then
		ans='N'
		goto 59	!reread rates (from another disc?)
	   endif
c===	else		!when model too big/complex for chessboard
c===	   call MODRATES()	TO BE DONE
c===	endif
c
c Get the number of ligands bound to each state (returns nbound(i) in common)
c (moved before print of rate and equilib constants because nbound() needed
c for latter)
	if(idest.ne.105) then
	 if(.not.boundef) then 	!nbound not read defined yet in qmechs.dat
	   call GETBOUND(k,npar)
	 endif
	 if(indmod) then	!don't print state names!
	   print 4011,(ligname(i),i=1,nlig)
	   if(discprt) write(8,4011) (ligname(i)(1:10),i=1,nlig)
4011	   format(/,
     & ' Number of ligands bound',/,10x,9(2x,a10))
	   do i=1,k
		print 4012,i,(nbound(i,n),n=1,nlig)
		if(discprt) write(8,4012) i,(nbound(i,n),n=1,nlig)
4012		format(' ',i3,': ',9(5x,i5))
	   enddo
	 else
	   print 401,(ligname(i),i=1,nlig)
	   if(discprt) write(8,401) (ligname(i)(1:10),i=1,nlig)
401	   format(/,
     & ' Number of ligands bound',/,
     & ' State   ',10x,9(2x,a10))
	   do i=1,k
		print 40,i,statname(i),(nbound(i,n),n=1,nlig)
		if(discprt) write(8,40) i,statname(i),(nbound(i,n),n=1,nlig)
40		format(' ',i3,': ',a10,9(5x,i5))
	   enddo
	 endif
	 ans='Y'
	 call DCASK(' Are these correct',ans,ans)
	 if(ans.eq.'N') then
	   print 41
41	   format(' Please tell DC that GETBOUND failed!')
	   do n=1,nlig
		do i=1,k
		   print 42,n,ligname(n),i
42		   format(' Number of ligand #',i2,3x,a20,
     &		': molecules bound to state #',i3,' = ')
		   call INPUTi(nbound(i,n))
		enddo
	   enddo
	 endif
	endif
c
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
c May have problem with constraints, when GETQD is called in HJCFIT
c If model has been changed then constraints read form hjcfit.ini will
c be nonsense so set neq=0
	if(.not.(imod0.eq.imodold)) then
	   neq=0	!old constraints not relevant for new model
c==	   nsetq=0	!for indep models -NO!
	endif
c   QD not used at present, but need to call QSETD here to set
c   the m.r. parameters at non-zero conc
c   can same thing be achieved by call to QSETD with epsim=true (but
c   still need to declare conc() for call)
	do i=1,10
	   conc1(i)=1.0
	enddo
	call QSETD(conc1,IL,V1,QT,QD,k,.false.)
	do i=1,k
	   do j=1,k
		QT(i,j)=QD(i,j)
	   enddo
	enddo
	string=char(244)//' = calculated by microscopic rev'
	print 13,string
      if(discprt) write(8,13) string
13	format(/,' Values of rate constants',/,
     &'   (1/s, or 1/(Ms) for association rate constants)',/,
     &'    *n = association rate constant for ligand #n ',/,
     &  4x,a74)
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
12		format(i4,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	   else
      	print 121,m,pre,i,j,titlep(m),r,psav(m)
	      if(discprt) write(8,121)m,pre,i,j,titlep(m),r,psav(m)
121		format(i4,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6,
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
		if(QT(i,j).gt.1.d-6) then
		   eqK=sngl(QT(j,i)/QT(i,j))
		else
		   eqK=0.0	!not defined
		endif
		i1=j
		j1=i
		m2=IQ(i,j)
		m1=IQ(j,i)
		if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
		kname='  E   '
	   else if(jopen.and.(.not.iopen)) then
		if(QT(j,i).gt.1.d-6) then
		   eqK=sngl(QT(i,j)/QT(j,i))
		else
		   eqK=0.0	!not defined
		endif
		i1=i
		j1=j
		m1=IQ(i,j)
		m2=IQ(j,i)
		if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
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
			if(QT(i,j).gt.1.d-6) then
			   eqK=1.e6*sngl(QT(j,i)/QT(i,j))
			else
			   eqK=0.0	!not defined
			endif
			kname=' K (uM)'
			i1=j
			j1=i
			m2=IQ(i,j)
			m1=IQ(j,i)
			if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
			isfac1=nbmax-nbound(i,jlig)	!assoc
			isfac2=nbound(j,jlig)		!dissoc
		   else		!j->i is assoc
			if(QT(j,i).gt.1.d-6) then
		         eqK=1.e6*sngl(QT(i,j)/QT(j,i))
			else
			   eqK=0.0	!not defined
			endif
			i1=i
			j1=j
			m1=IQ(i,j)
			m2=IQ(j,i)
			if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
			isfac1=nbmax-nbound(j,jlig)	!assoc
			isfac2=nbound(i,jlig)		!dissoc
			kname= ' K (uM)'
		   endif
		else			!current i,j is open-open or shut-shut isomerisation
		   if(QT(j,i).gt.1.d-6) then
			eqK=sngl(QT(i,j)/QT(j,i))
		   else
			eqK=0.0	!not defined
		   endif
		   i1=j  		!no way to decide which way up is best
		   j1=i
		   m2=IQ(i,j)
		   m1=IQ(j,i)
		   if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
		   kname=' Kisom '
		endif
	   endif
	   print 71,kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
	   if(discprt) write(8,71)
     &		kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
71	   format(
     &      1x,a7,' = q(',i2,',',i2,')/q(',i2,',',i2,') = ',
     &	a10,'/',a10,' = ',g13.6)
	   if(isfac2.gt.0) then
		sfac=float(isfac1)/float(isfac2)
		print 5,isfac1,isfac2,sfac,sfac,kname,sfac*eqK
		if(discprt) write(8,5)isfac1,isfac2,sfac,sfac,kname,sfac*eqK
5		format(
     &	'    Statistical factor = ',i2,'/',i2,' = ',f7.3,
     &	': ',f7.3,'*',a7,' = ',g13.6)
	      isfac1=0		!reset statistical factors for assoc/dissoc
	      isfac2=0
	   endif
9	   continue		!skip to end of loop
	enddo 	!end of equilibrium constants
	pause
c
	if(idest.eq.105) goto 99
c
92	continue		!skip to here if ncdep=0
c	   yinfeq=abs(curinfs)	!maximum resonse/binding (for Hill slopes)
c
c RE-SAVE model (e.g. if parameter names changed)
c=	if(ncyc.eq.0) obeymr=.false.
101	if(.not.renum) then
	   call DCASK('Re-save the model and rates on disc','n',ans)
	else
	   goto 99		!if renumbered
	endif
c WRITQM now altered to re-save windows-designed models (iver.ge.200)
	if(ans.eq.'N') then
	   goto 99
c	else if(iver.ge.200) then
c	   call BELL(1)
c	   print 782
c782	   format(
c     & ' This mechanism was designed in Windows version, and changes',/,
c     & ' to the mechanism or stored rate constants should be made ',/,
c     & ' only in the Windows version.')
c	   pause
c	   goto 99
	endif
c
c=	call TITENT0(' Enter title for rates',rtitle,74,.false.)
c
100	continue	!jump here to write if kflag=-1, or to save new model/rates
c
	imod=imod0		!for write
	call WRITQM(imod0,qfilem,qfilout,altmod,jmod,
     & mtitle,mtits,k,kA,kB,kC,kD,rtitle,rtitles,
     & iver,nrecs,nextrec,ireclast,jstart,irecsav,irecqsav,
     & ilast,jlast,nrateq,ncon,ncdep,nlig,chardef,
     & boundef,ncyc,vref,nvdep,kmfast,indmod,npar,nsetq,kstat,
     & charmod,irate,jrate,QT,titlep,ligname,nbound,
     & IX,JX,IL,dgamma,nsc,IM,JM,IV,JV,hpar,pstar,kmcon,
     & ieq,jeq,ifq,jfq,efacq,statname,
     & nsub,kstat0,npar0,kcon,npar1,ncyc0,idest)
	if(idest.eq.101) goto 101
c
99	continue
c
	idest=0
	imod=imod0		!copy for common
	RETURN
	end


