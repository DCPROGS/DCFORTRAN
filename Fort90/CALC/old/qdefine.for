	subroutine QDEFINE(iread,pinf,QT,IQ,titlep,titles,
     & npar,nrateq,nlig,IL,dgamma,chardef,qfilem,
     & indmod,irate,jrate,IQQ,ligname,statname,readp,
     & nchan,vkin,vhold,vref)

c  Subroutine called by GETQD to allow new mechanism to be defined
c within any program that uses GETQD (based on qdef.for in old QGEN)
c Expects that obeymr will be set (via common) on entry
c iread=3: Define a completely new mechanism (newmod=true)
c iread=4: Define a new mechanism by modifying an existing one (altmod=T)
c
c NB change in qwind2 will be needed as IQ is now defined within it
c=== for indmod, do we need array, like theta(npar) to hold the basic
c===  rates? -this is essntially what data(i) is within qwind2 at the moment!
c==NB "call qwind2" is in qdefine, getqd, hjcfit [and CYCLE, a stand-alone prog]
c===in hjcfit, have constraints which are not part of model!
c===need to think again whether to use same names (nsetq etc)
c
c Modif 04/11/01 05:53am to remove request for conc here, and QD, PI and conc()
c removed from arguments, and statname added to params -now returns only QT
c
c Modified 01/13/01 09:52am for new qmechs.dat file
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
c OLD NOTES FROM QDEF.FOR
c 07/24/93 10:33am Lahey V5.n version of QDEF2
c  (two channel Q matrix = old nmod=21,31 not done yet; see old QMOD2)
c
c New version: replace QDISC with QGEN.INI, and replace multiple QDAT2.DAT
c files (holding rate constants) with a single multi-record file called
c QDAT.DAT (see QDFIX.FOR). Also CHARQ.DAT, which holds details of models,
c now renamed QMODEL.DAT. All three now kept in current root segment (they
c need to be available to several different progs)
c
c Modif 01/11/00 02:14pm so max number of mdels and rates that can be kept is
c        increased to 100
c
c Modif 06/17/99 11:12am so that micro rev need not be obeyed -obeymr=true
c should be same as before -in common/mr/obeymr which is in eqoccd, getqd

c QGEN modif 01/02/95 08:37am so that does only 'single channel' option
c and QDEF  altered so does only 'Use new (or stored) model entered
c via window diagram' option -all other things (entering c-jump, vjump, epsc
c parameters now done in client progs (SCALCS, SCJUMP, SCBST etc -not all
c fully-fixed yet!)
c
c In old version:
c	QDAT2 used two records, of recl=2048 (3rd record used only for 2-channel
c	case which is not in at present -make a sep file for it, when needed)
c	QDISC recorded which model number had data (QDAT2) on each disk
c	 -array IMOD(i) = model number (imod0) on the ith disc (i=1-6
c 	 for disc=A-F Tosh, or i=1-12 for A: to L: on the DELL) -also recorded
c	 idisc=ndev for disc used for QDAT2 in last run.
c New version: Rather than keeping disc, keep record number in QDAT where
c	data is: file #=irecq=1,2,3,...,(actual 1st record # is jrecq=2*irecq-1
c	=1,3,5,...since each data set uses 2 records=1 file say).
c	 Thus JMOD(irecq)=model # (imod0) that has data located at point (file)
c	 in QDAT.DAT defined by irecq
c	(irecq=1,2,3,...,nrecq; nrecq=number of data sets/files currently defined)
c      (nrecq now 'nrecs')
c
c
c 02/09/93 08:06am ndisc=number of disc partitions added as parameter
c	If ndisc=1 then QDISC.DAT,CHARQ.DAT,QDAT2.DAT are in same directory
c	as QGEN2, otherwise they are on D:\
c
c 07/04/90 08:46am order of values in 18,rec=1 altered so ligand conc can
c be read more easily in SCBST etc
c 06/05/90 07:22am QDEF2 is much-altered version of QDEF1 with different
c disk storage (except for output of QD to other progs)
c
c OLD NOTES ON DISC STORAGE for QGEN2:
c Note that QMOD2 writes only QDAT.DAT rec=4
c viz: write(18,rec=4) nmod,theta,nchan,dgamma,nvdep,iv,jv,hpar
c (also optionally reads rec=4 in QMOD, in RDPAR2), and QMOD also sets values
c for the the things written to rec=4 (in KCHECK). Can probably do away with
c this when all models (inc 2 chan?) are eventually kept in character form?.
c  (1) Details of models (only) kept on disc (rather than program as in QMOD)
c	in CHARQ.DAT eg read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
c     & ncyc,nsc,im,jm,kB,kC,kD,titlep,ncon
c (no need to keep ncon,IC,kA,kF as these is worked out from CHARMOD)
c At present
c QDAT2.DAT
c	READ(18,rec=2) QT,Pinf,nmod,vref,Vkin,PZERO,PSTAR,KMFAST,KMCON,
c     & HPAR,IV,JV,NVDEP,nchan,dgamma
c QDAT.DAT (recl=4096)
c rec=1 to pass stuff to other progs
c After 01/21/91 08:04am added ncdep,IL(),IX(),JX(10) to rec=1 to enable
c calcs to be done with conc that varies with time (in EPSCSIM)
c	   write(18,rec=1) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig,P0,cur0,dgamma,kmfast,pstar,vhold,vkin,
c     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
c Values in records #2,3 needed for rerun of prog
c	write(18,rec=2) QD,QT,Pinf,k,ka,kb,kc,kd,nmod,vref,Vkin,IR,
c     & PZERO,PSTAR,KMFAST,KMCON,aka1,aka2,BA
c	write(18,rec=3) NCON,IC,NCYC,NSC,ICYC,IM,JM,nspec,IS,JS,neq,
c     & IE,JE,IF,JF,EFAC,ncdep,IX,JX,nchan,HPAR,IV,JV,NVDEP,dgamma
c THUS NOW USE:
c	CHARQ.DAT (recl=3072)
c       read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
c     & ncyc,nsc,im,jm,kB,kC,kD,titlep,ncon
c	QDAT2.DAT (recl=2048 is big enough- 4096 not needed)
c	(1) rec=2 for rerun
c	   (18,rec=2) QT,Pinf,nmod,imod0,vref,Vkin,PZERO,PSTAR,KMFAST,KMCON,
c     & HPAR,IV,JV,NVDEP,nchan,dgamma
c	(2) rec=1 for other progs (same as orig QDAT.DAT rec=1 except that
c		imod0 added)
c	   write(18,rec=1) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig,P0,cur0,dgamma,kmfast,pstar,vhold,vkin,
c     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
c	(3) rec=3 used only for 2-channel Q matrix (for TCHAN2,BCHAN2)(prev rec=5)
c	   write(18,rec=3) QD,PINF,K,ka,kb,kc,kd,nmod	!fot tchan2,bchan2
c plus:
c QDISC.DAT
c	write(17,rec=1) idisc,ndev
c ALSO QMOD2 (only) uses a separate file (prev=rec 4 of QDAT.DAT) now called
c   rec=1 of QMOD.DAT
c
c
c 06/21/89 09:11am  Lahey version
c Modif May-90 so that NMOD=0 option will normally input the model
c in character form, via CHARWIND etc; connections determined automatically
c but no of cycles, which param are conc dep, etc (all things that are
c characteristic of model only)-must be typed in here (first time a model is
c defined) rather than being being defined in program (as for nmod>0).
c
c=	real*8 QT(100,100),QD(100,100),P0(100),Pinf(100),pzero(100)
c=	real*8 Peq(100),dgamma(100)	!use Peq as arg for EQOCC
	real*8 QT(100,100),Pinf(100)
	real*8 dgamma(100)	!use Peq as arg for EQOCC
	real*8 Qdum(100,100)
	character*10 titlep(200)
	character*8 titles(100)
	character*74 mtitle	!title for MODWIND models
	character*74 mtits(100)	!to read all ditto from disc
c	character*74 rtitle(40)	!title to describe rate constants
	character*74 rtitles(500)	!title to describe rate constants
	character*74 rtitle	!title of current rate constants
	character*2 charmod(25,40)
	character qfilem*40
c=	real*4 PI(100,100)
	integer SCRNBUF0(1024)
c	integer IQ(10,10),jmod(40)
	integer IQ(100,100),IQQ(100,100),jmod(500)
	integer irate(200),jrate(200)
c=	integer irateq(200),jrateq(200)
C QM IS TO HOLD MINORS OF Q IN EQOCC. SEEMS THAT IT MUST BE
C DECLARED IN MAIN PROG FOR CORRECT CALCS (SEE COMMENTS IN EQOCC)
C **Do not ned this when DETD or DETV rather than DETERM used to
c calc determinants in EQOCC
	real*4 conc(10)
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
c=	character*11 cnum0,cnum1,cnum2
	character*11 cnum0
	logical chardef,boundef
c
	REAL*4 PSTAR(4)		!KM2 OCCS
	integer IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100),IL(100)
c=	integer NSC1(50),IM1(50,100),JM1(50,100),IX1(100),
c=     &	JX1(100),IL1(100)
	integer kmcon(9)
	character*1 ans,UC
	LOGICAL READP,KMFAST,newmod,altmod,blank,sameq
c	logical ranrate
	logical pon,slock,debug,caplock
	logical discprt
	logical indmod
c
c For inwindc
	character*79 heading,title
	character*11 cdata(50,3)
c
	common/dp/discprt
	COMMON/KM2/AKA1,BA,PSTAR,KMCON,KMFAST,aka2,arat	!for km2occ
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
c	common/Vval/dgamma,vhold,vkin,vref,enchan		!V in mV
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
c===?	integer IS(100),JS(100)	!declare for disc-write-not used now
	COMMON/SPAR/NSPEC,IS,JS		!special parameters
	COMMON/dimblk/K,KMAX,ir,nmod
	COMMON/KBLK/kA,kB,kC,kD
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	common/equil/ak1,ak2,ba2,bab,akb,akd	!for km2par,main,qdef1
c
c Addition for getbound
	integer nbound(100,10)
c	integer nbound(10,2)
	common/nbnd/nbound
c
	pon()=slock()
	debug()=caplock()
c
10	if(iread.eq.3) then
	   newmod=.true.
	   altmod=.false.
	   print 46
46	   format(
     & ' Mechanisms with independent subunits are such that the same',/,
     & ' rate constant appears several times in the Q matrix.',/,
     & ' Define an independent subunit mechanism [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   indmod=ans.eq.'Y'
	   print 47
47	   format(
     & ' Can model be drawn on chess board (2D and at most 11 x 18)',
     &   ' [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   chardef=ans.eq.'Y'
	else if(iread.eq.4) then
	   newmod=.false.
	   altmod=.true.
	   indmod=.false.
	endif
c
c
	nlig=1		!one ligand only, unless redefined below
	do i=1,100
	  IL(i)=1		!ligand #1 unless redefined
	enddo
c
	vref=-80.
c
	kmax=100
	nmod=0		!hangover from old QMOD2
	nsetq=0		!no constraints -except when indmod=T
	do i=1,200
	   efacq(i)=1.0
	enddo
	nspec=0
	Vkin=Vref		!unless reset
	Vhold=Vref		!unless reset
	readp=.false.
c
	kmfast=.false.	!for all models so far
c
	iflag=0   !not read from disc- new one typed into window
c
c Read all titles for sets of rate constants initially here (otherwise
c reading rtitle() involves read also QT, nchan etc so any values of these
c set here may be overwritten
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c     Read header
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
c=	nrecq=nrecs		!nrecq is old name -should be phased out
c      OPEN(unit=15,file=qfilem,status='UNKNOWN',
c     &	access='TRANSPARENT')
cc     Read header part
c	   read(unit=15,rec=1) iver,
c     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c
c Bit done when new or stored model requested (not sameQ)
	if(newmod) then 	!new model
	   imod0=nmods		!so says next model #=nmods+1 below
	   iflag=0   !not read from disc- new one typed into window
c===	   nrow=20	!for MODWIND
c===	   ncol=30
	   nrow=22	!for MODWIND
	   ncol=37
	endif
c Modify existing model
581	if(.not.newmod) then
	   iflag=1		!so charmod displayed
58	   continue
	   print 561,0
561      format(' (',i2,')',' New model to be defined')
	   do i=1,nmods	!list the model titles
		print 56,i,mtits(i)
56    	format(' (',i2,')',a74)
		if(mod(i,20).eq.0) then
		   call BELL(1)
		   print 11
11		   format(' Hit space bar for more')
		   call ANYKEY
		endif
	   enddo
	   call BELL(2)
	   print 57
57	   format('  Modify model number = ')
	   call INPUTi(imod0)
	   if(imod0.gt.nmods) goto 58
	   if(imod0.le.0) then
		imod0=nmods		!so says next model #=nmods+1 below
		iflag=0   !not read from disc- new one typed into window
		newmod=.true.
      	CLOSE(unit=14)
		goto 3
	   endif
	   mtitle=mtits(imod0)
c old read
c=	   read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
c=     &    ncyc,nsc,im,jm,kA,kB,kC,kD,titlep,ncon
c new read
	   read(unit=15,rec=irec) iver,
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
	   nrow=ilast+6	!for MODWIND
	   ncol=jlast+6
	endif		!end of section to read in model to be modified
c
c Jump to 3 to define totally new model
3	continue
	if(chardef) then
c=======temp for debug
c	   CHARMOD(1,1)="O1"
c	   CHARMOD(2,1)=" �"
c	   CHARMOD(3,1)="C5"
c	   CHARMOD(4,1)=" �"
c	   CHARMOD(5,1)="C9"
c	   CHARMOD(6,1)=" �"
c	   CHARMOD(7,1)="13"
c	   CHARMOD(1,2)="컴"
c	   CHARMOD(2,2)="  "
c	   CHARMOD(3,2)="컴"
c	   CHARMOD(4,2)="  "
c	   CHARMOD(5,2)="컴"
c	   CHARMOD(6,2)="  "
c	   CHARMOD(7,2)="컴"
c	   CHARMOD(1,3)="C2"
c	   CHARMOD(2,3)=" �"
c	   CHARMOD(3,3)="C6"
c	   CHARMOD(4,3)=" �"
c	   CHARMOD(5,3)="10"
c	   CHARMOD(6,3)=" �"
c	   CHARMOD(7,3)="14"
c	   CHARMOD(1,4)="컴"
c	   CHARMOD(2,4)="  "
c	   CHARMOD(3,4)="컴"
c	   CHARMOD(4,4)="  "
c	   CHARMOD(5,4)="컴"
c	   CHARMOD(6,4)="  "
c	   CHARMOD(7,4)="컴"
c	   CHARMOD(1,5)="C3"
c	   CHARMOD(2,5)=" �"
c	   CHARMOD(3,5)="C7"
c	   CHARMOD(4,5)=" �"
c	   CHARMOD(5,5)="11"
c	   CHARMOD(6,5)=" �"
c	   CHARMOD(7,5)="15"
c	   CHARMOD(1,6)="컴"
c	   CHARMOD(2,6)="  "
c	   CHARMOD(3,6)="컴"
c	   CHARMOD(4,6)="  "
c	   CHARMOD(5,6)="컴"
c	   CHARMOD(6,6)="  "
c	   CHARMOD(7,6)="컴"
c	   CHARMOD(1,7)="C4"
c	   CHARMOD(2,7)=" �"
c	   CHARMOD(3,7)="C8"
c	   CHARMOD(4,7)=" �"
c	   CHARMOD(5,7)="12"
c	   CHARMOD(6,7)=" �"
c	   CHARMOD(7,7)="16"
c	   iflag=1
c	   ilast=7
c	   jlast=7
c	   nrow=ilast+6	!for MODWIND
c	   ncol=jlast+6
c=======end temp for debug
	   call MODWIND(charmod,nrow,ncol,iflag,ilast,jlast)
	   call CQLAST(charmod,ilast,jlast)
c Call CHARQ to get connections (in IC(2,20)) and kA,kF
	   call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
	   nrateq=2*ncon
	else		!define kA,kF,ncon,IC,IQ 'by hand'
	   ilast=1
	   jlast=1
73	   print 70
70	   format(' Total number of states, k = ')
	   call INPUTi(k)
	   print 71
71	   format(' Number of OPEN states, kA = ')
	   call INPUTi(kA)
	   kF=k - kA
	   print 72,kF
72	   format(' Number of SHUT states, kF = ',i3,' O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') goto 73
	   print 74
74	   format(' Number of connections (number of q(i,j)/2) = ')
	   call INPUTi(ncon)
c NB in independent case, this refers to all rates in the Q matrix, so
c define IQQ, rather than IQ which, like irate(), and jrate(), refers
c to the basic npar rate constants in the mechanism
	   nrateq=2*ncon
	   i1=0
	   do m=1,ncon
		print 75,m,i,j
75		format(
     &	' (',i4,') connect states i and j: i, j [',i3,',',i3,'] = ')
		call INPUT2i(i,j)
		IC(1,m)=i
	      IC(2,m)=j
	      i1=i1+1
	      IQQ(i,j)=i1
c		call INTCONV(i1,cnum0)
c		call INTCONV(i,cnum1)
c		call INTCONV(j,cnum2)
c	      call TITENT0(
c     & 	   charnb(cnum0)//': give name for rate q('//charnb(cnum1)//
c     &	   ','//charnb(cnum2)//')',
c     &	   titlep(i1),10,.true.)
c	      i1=i1+1
c	      IQ(j,i)=i1
c		call INTCONV(i1,cnum0)
c	      call TITENT0(
c     & 	   charnb(cnum0)//': give name for rate q('//charnb(cnum2)//
c     &	   ','//charnb(cnum1)//')',
c     &	   titlep(i1),10,.true.)
		iflag=0	!so other details checked below
	   enddo
	endif
c
	k=kA+kF		!total number of states
c
c In case of independent models, need names of the npar basic rate constants
c here so it can be specified which are concentration dependent!
c First get npar when indmod=true
	if(.not.indmod) then
	   npar=nrateq
	   kstat=k
	endif
	if(indmod) then
111	   continue
	   print 112
112	   format(
     &	' Number of basic states in the mechanism = ')
	   call INPUTi(kstat)
	   print 1
1	   format(
     &	' Number of underlying rate constants in the mechanism = ')
	   call INPUTi(npar)
	   if(npar.ge.nrateq.or.kstat.ge.k) then
		call BELL(2)
		print 2,k,nrateq
2		format(' ERROR:',/,
     & ' For independent models:',/,
     & ' (a) number of underlying states is less than number',/,
     & '    of states in Q (k = ',i3,'), and ',/,
     & ' (b) number of underlying rate constants is less than number',/,
     & '    of q(i,j) (= ',i3,')',/,/,
     & ' (1) define an ordinary mechanism',/,
     & ' (2) redefine number of underlying rate constants',/,
     & ' Option number [1] = ')
	      call INPUTi(iopt)
	      if(iopt.eq.1) then
		   chardef=.false.
		   goto 10
		else
		   goto 111
		endif
	   endif	!end of error bit
	endif
c
	print 86
86	format(' Number of different ligands [1] = ')
	nlig=1
	call INPUTi(nlig)
	do i=1,nlig
	   call INTCONV(i,cnum0)
	   call TITENT0(
     &      ' Give name for ligand #'//charnb(cnum0),
     &      ligname(i),20,.true.)
	enddo
c
	if(indmod) then
	   print 831,kstat
831	   format(/,
     &   ' Now define names (eg A2R) of the ',i2,
     &	' basic states (up to 10 characters)')
	   pause
	else
	   print 832,kstat
832	   format(/,
     &   ' Now define names (eg A2R) of the ',i2,
     &	' states (up to 10 characters)')
	   pause
	endif
c
	ni0=kstat	!rows
	nj0=1		!columns
	nidisp=kstat
	if(kstat.gt.20) nidisp=20
	nr1=-1		!so window initially in default position
	iconst=0	!no constraint
	iflagc=0	!initially blank
	call INTCONV(kstat,cnum0)
	title=charnb(cnum0)//' States'
	heading='   state name '
	do i=1,kstat
	   cdata(i,1)='          '
	enddo
	nd1=50	!declared dimensions of cdata
	nd2=3
	call CLS() 	!clear screen
	call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2)
	do m=1,kstat
	   statname(m)=cdata(m,1)(1:10)
	enddo
c
c Now define parameter names
c
	if(indmod) then
	   print 83,npar
83	   format(
     &   ' Now define names of the ',i2,
     &	' basic rate constants (up to 10 characters)')
	   pause
c Can't use qwind to input the names because have no QT yet (and INWIND
c does not take names, only numbers)
	   ni0=npar		!rows
	   nj0=1		!columns
	   nidisp=npar
	   if(npar.gt.20) nidisp=20
	   nr1=-1		!so window initially in default position
	   iconst=0	!no constraint
	   iflagc=0	!initially blank
	   call INTCONV(npar,cnum0)
	   title=charnb(cnum0)//' rate constants'
	   heading='   param name '
	   do i=1,npar
		cdata(i,1)='          '
	   enddo
c=======temp for debug
c	   cdata(1,1)='2q43'
c	   cdata(2,1)='q34'
c	   cdata(3,1)='q32'
c	   cdata(4,1)='2q23'
c	   cdata(5,1)='beta1'
c	   cdata(6,1)='alpha1'
c	   cdata(7,1)= '2k43'
c	   cdata(8,1)= 'k34'
c	   cdata(9,1)= 'k32'
c	   cdata(10,1)='2k23'
c	   cdata(11,1)='beta2'
c	   cdata(12,1)='alpha2'
c	   iflagc=1
c=======end temp for debug
	   nd1=50	!declared dimensions of cdata
	   nd2=3
	   call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2)
c
	   print 84
84	   format(
     & ' Now give i,j for one occurrence of these rate constants in Q')
	   ncdep=0
	   do m=1,npar
		if(mod(m,2).eq.0) then	!default
		   i=j0
		   j=i0
		else
		   i=0
		   j=0
		endif
		titlep(m)=cdata(m,1)(1:10)
851		print 85,m,titlep(m),i,j
85		format(1x,i2,
     &	'. Give i,j for one value of ',a10,' in Q: i,j ['
     &	 ,i2,',',i2,'] = ')
		call INPUT2i(i,j)
		if(i.eq.0.or.j.eq.0) then
		   call BELL(1)
		   goto 851
		endif
		i0=i
		j0=j
c
90		if(nlig.gt.0) then
		 print 87,i,j,titlep(m)
87		 format(' Q(',i2,',',i2,') = ',a20,' is: ',/,
     &	 ' (0) NOT concentration dependent')
		 do n=1,nlig
		   print 88,n,n,ligname(n)
88		   format(
     &	   ' (',i1,') multiplied by conc of ligand #',i2,', ',a20)
		 enddo
		 iopt=0
		 print 89,iopt
89		 format(' Option number [',i2,'] = ')
		 call INPUTi(iopt)
		 if(iopt.lt.0.or.iopt.gt.nlig) goto 90
		 if(iopt.gt.0) then
		    ncdep=ncdep+1
		    ix(ncdep)=i
		    jx(ncdep)=j
		    IL(ncdep)=iopt	!ligand number
		    cdata(m,3)=ligname(iopt)(1:11)	!for display in inwindc below
		 endif
		endif		!end of nlig.gt.0
c Define IQ() and irate(), jrate(1) now -these refer only to the npar
c basic rates in the mechanism (as opposed to IQQ which refers to whole Q)
c In the case of inependent models the rest of the irate(), jrate() values,
c above the first npar values, are added below when constraints are set
		IQ(i,j)=m
		irate(m)=i
		jrate(m)=j
	   enddo
c Now cycles -only cycles that matter are those WITHIN the indpendent subunits
c (others are satisfied automatically), so do not set them in normal way from
c charmod, etc
	   ncyc=0
	   call DCASK(
     &	'Are there any cycles WITHIN independent units','N',ans)
	   if(ans.eq.'Y') then		!NOT YET TESTED
		call GETCYC(2)	!get all the values
		if(ncyc.gt.0) then
		   do i=1,ncyc
			do j=1,npar
			   if(irate(j).eq.im(i,j).and.jrate(j).eq.jm(i,j))then
				cdata(m,3)='set by m.r.'  !for display in inwindc below
			   endif
			enddo
		   enddo
		endif
	   endif
	endif		!end of if(indmod)
c
c Can print CHARMOD on printer to keep record of model used?
c Even if existing model was read, it may have been altered in MODWIND so
c check that cycles etc are OK;
c iflag=0 = it was read but may have been altered, so kB,kC,nlig etc may need
c	 changing OR new model, in which case nlig etc must be set
c iflag=1 means read, and not changed- just print values
	if(iflag.eq.0.or.iflag.eq.1) then
323	   print 2011,kA,kF
2011	   FORMAT('  number of open and shut states, kA, kF = ',2i5,/)
	   k=kA+kF
	   if(newmod) goto 155
	   print 512,kB
512	   FORMAT('  number of short shut states, kB = ',i2)
	   print 513,kC
513	   FORMAT('  number of long-lived shut states C, kC = ',i2)
	   print 514,kD
514	   FORMAT(
     &  '  number of very long (between cluster) shut states, kD = ',i2)
	   print 2012
2012	   format('  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') goto 156
155	   print 202	!kB=
202	   FORMAT('&  number of short shut states, kB = ')
	   call INPUTi(kB)
	   print 203   !kC=
203	   FORMAT('&  number of long-lived shut states C, kC = ')
	   call INPUTi(kC)
	   print 204   !kD=
204	   FORMAT(
     &  '&  number very long (between cluster) shut states, kD [0] = ')
	   kD=0
	   call INPUTi(kD)
	   if((kA+kB+kC+kD).ne.k) goto 323
156	   continue
	   if(pon()) write(7,2431)ka,kb,kc,kd
         if(discprt) write(8,2431)ka,kb,kc,kd
2431	   format(
     & ' No of states in each subset: kA,kB,kC,kD= ',4i3)
	endif
c
c cycles and ligands done separately above for indmod case
	if(.not.indmod)then
	 if(iflag.eq.0) then	!read, but possibly changed/or not read
	   call CYCQ(k,ncon,ic,ncyc,nsc,im,jm)
c In GET*** subroutines:
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c in GETCYC iopt=3 asks for rates to be calc by micro rev only
	   call GETCYC(3)	!get micro rev rate only
	   call GETLIG(2,il,nlig,ligname)	!ditto for ligands
	 else if(iflag.eq.1) then 	!read from disk and not changed
	   call GETCYC(0)	!set micro rev states
	   call GETLIG(0,il,nlig,ligname)	!ditto for ligands
	 endif
	endif
c#####check connections with getcon?
c Print final details for model
c	print 670,mtitle,imod0
59	continue
c	if(.not.newmod) then
	   if(pon()) write(7,670)mtitle,imod0
	   if(discprt) write(8,670)mtitle,imod0
670	   format(/,1x,a74,/,' Model number = ',i3,/)
	   if(chardef) then
		do i=1,ilast
c		   print 67,(charmod(i,j),j=1,jlast)
		   if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
		   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
67		   format(4x,35a2)
		enddo
	   else
c just print param names
		do m=1,npar
		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	         if(discprt) write(8,671) m,i,j,titlep(m)
671		   format(i3,3x,' q(',i2,',',i2,')=',3x,a10)
		enddo
	   endif
c	endif
	call PRTMOD(il,ligname)
	call DCASK('This model O.K.','y',ans)

	if(ans.eq.'N') then
c        OPEN(unit=14,file='QMODEL.DAT',status='UNKNOWN',
c     &    access='DIRECT',form='UNFORMATTED',recl=3072)	!re-open file
         OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
	   goto 581
	endif
c
c Model finished; but can't store it until rates etc displayed, because
c names of rate constants are set in QWIND and may be changed
c
c NOW RATE CONSTANTS.  Check whether to read
c numerical values for rate constants from disc for display in QWIND2
c When iflag=3 input values of both rate constant names and values shown
c      iflag=2 rate constant names  initially blank, values shown
c      iflag=1 rate constant values initially blank, names shown
c      iflag=0 both cols initially blank.
c
	iflag=1
c	if(ranrate) then
cc	   Random allocation of rates
cC        ALLOCATE ALL Q(I,J) RANDOMLY WITH SLOW RATES FOR LEAVING C-STATES ETC
c	   ncdep=0
c	   nvdep=0
c	   call GETCYC(3)	!get micro rev rate only
c	   call GETRAN(QT,ic)		!allocate QT
c	   goto 330
c	endif
c
305	continue
	print 36,vref
36	format(' Reference potential (mV) = ',f9.2,' O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') then
	   print 38
38	   format(' Reference potential (mV) = ')
	   call INPUTr(vref)
	endif
	if(pon()) write(7,33)vref
      if(discprt) write(8,33)vref
33	format(' Reference potential (mV) = ',f9.2)
	ans='Y'
	if(newmod) ans='N'
	print 301,vref,ans
301	FORMAT(
     &' Read rate constants (at Vref=',f4.0,'mV) from disc [',a2,'] ? ')
	call INPUTa(ans)
	readp=UC(ans).ne.'N'
	call QZERO(QT,k)		!in case not read
	if(.not.readp) goto 330
C Read Q etc from disc	(default for read/write defined above as always
c write but may not always read)
304	continue
c column headings
c Modif 11/18/99 09:20am so prints only the sets of rates that correspond
c to the specified model (imod0)
	n=0
	do i=1,nrecs
	   if(jmod(i).eq.imod0) n=n+1
	enddo
	if(n.eq.0) then
	   print 191
191	   format(' No rate constants have been stored for this model')
c      iflag=1 rate constant values initially blank, names shown
	   iflag=1
	   readp=.false.
	   goto 330
	else
	   print 190,imod0,mtits(imod0)(1:74)
190	   format(' For model #',i3,': ',a74,/,
     &   ' the following rate constants have been stored:',/,
     &   '   file #:                      Name of rate file')
	   do i=1,nrecs
		if(jmod(i).eq.imod0) then
		   print 192,i,rtitles(i)
192		   format(1x,i3,': ',a74)
		   irec=i	!use as default
		endif
	   enddo
	   print 15,irq
15	   format(' File # for rates [',i2,'] = ')
	   call INPUTi(irq)
	   irec=jstart(irq)
c	   imodsav=imod0
c===	   jrecq=2*irecq-1
	endif
c
c Read data
c      OPEN(unit=18,file='QDAT.DAT',status='UNKNOWN',
c     &  access='DIRECT',form='UNFORMATTED',recl=2048)
c	jrecq=2*irecq	!read SECOND record of data
c	read(18,rec=jrecq) QT,Pinf,nmod,imod1,vref,Vkin,PZERO,PSTAR,
c     & KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,dgamma,rtitle(irecq),
c     & ibflag,nbound
c	CLOSE(unit=18)
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c  NB read model number as imod1
	read(unit=15,rec=irec) iver,
     &	imod1,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
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
98	if(imod1.ne.imod0) then
	   call BELL(2)
	   print 157,imod1,imod0
157	   format(
     &  ' Model on disc (#',i3,') is not that requested (#',i3,')',/,
     &  ' (1) Type in new rate constants',/,
     &  ' (2) Try again (e.g.different disc)',/,
     &  ' (3) Use anyway (may be OK, eg if model # have been changed',/,
     &  ' Option number [2] = ')
	   call INPUTi(i)
	   if(i.le.0) i=2
	   if(i.eq.1) then
	      readp=.false.
	      goto 330
	   else if(i.eq.2) then
		goto 305
	   endif
	endif
	iflag=3
330	continue	!jump to here if QT not read
c
c Now show model + QWIND (should be OK whether chardef=true or not, because
c qwind can scroll over several pages
c
	ni=2*ncon	!number of rows
c===	npar=ni	!defined above!
	nidisp=ni
	if(nidisp.gt.20) nidisp=20
	nr1=0		!posn of QWIND on screen
	nc1=2*jlast+7
	if(newmod) then
	   print 91
91	   format(' Now enter values for rate constants in col 2')
	   pause
	endif
	call CAPTSCN(scrnbuf0)	!before putting up model
	if(chardef) then
	   call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
	endif
c IQ(npar),irate(npar), jrate(npar), titlep(npar) are defined in QWIND2
c (also when indmod=true). NB the IQ calculated here refers to the whole
c Q matrix, as do irate(), jrate(), so call them IQQ(), irateq(), jrate(q)
c in case of independent model (in all other cases they are same) -for
c indep model can't use qwind as qwind is designed to show ALL of QT

	if(.not.indmod) then
	   call QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     &    nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,irate,jrate,itry)
	else if(indmod) then
	   ni0=npar		!rows
	   nj0=3		!columns
	   nidisp=npar
	   if(npar.gt.20) nidisp=20
	   nr1=-1		!so window initially in default position
	   iconst=0	!no constraint
	   call INTCONV(npar,cnum0)
	   title='     '//charnb(cnum0)//' rate constants'
	   heading='    name         value        ligand '
c	names already set in cdata(i,1) after first call to INWINDc, now
c     enter values
	   do i=1,npar
		cdata(i,2)='          '
	   enddo
c======temp for debug
c	   call REALTOCH(2.e8,CDATA(1,2),11)
c	   call REALTOCH(100.,CDATA(2,2),11)
c	   call REALTOCH(1.e8,CDATA(3,2),11)
c	   call REALTOCH(200.,CDATA(4,2),11)
c	   call REALTOCH(2000.,CDATA(5,2),11)
c	   call REALTOCH(1000.,CDATA(6,2),11)
c	   call REALTOCH(2.e8,CDATA(7,2),11)
c	   call REALTOCH(100.,CDATA(8,2),11)
c	   call REALTOCH(1.e8,CDATA(9,2),11)
c	   call REALTOCH(200.,CDATA(10,2),11)
c	   call REALTOCH(3000.,CDATA(11,2),11)
c	   call REALTOCH(500.,CDATA(12,2),11)
c======end temp for debug
	   iflagc=3		!show all cols
	   nd1=50		!declared dimensions of cdata
	   nd2=3
	   call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2)
	   do i=1,npar
		call CHTOREAL(cdata(i,2),xs)
		QT(irate(i),jrate(i))=dble(xs)
	   enddo
	endif
	call DISPSCN(scrnbuf0)  !restore orig screen
	if(itry.eq.1) goto 305	!reread rates (from another disc?)
c
	if(indmod) then
cc========temp for debug
c	   nsetq=36
cc
c	   IEQ(1)=14
c	   IEQ(2)=15
c	   IEQ(3)=16
c	   IEQ(4)=10
c	   IEQ(5)=11
c	   IEQ(6)=12
c	   IEQ(7)=10
c	   IEQ(8)=11
c	   IEQ(9)=12
c	   IEQ(10)=6
c	   IEQ(11)=7
c	   IEQ(12)=8
c	   IEQ(13)=6
c	   IEQ(14)=7
c	   IEQ(15)=8
c	   IEQ(16)=2
c	   IEQ(17)=3
c	   IEQ(18)=4
c	   IEQ(19)=8
c	   IEQ(20)=12
c	   IEQ(21)=16
c	   IEQ(22)=7
c	   IEQ(23)=11
c	   IEQ(24)=15
c	   IEQ(25)=7
c	   IEQ(26)=11
c	   IEQ(27)=15
c	   IEQ(28)=6
c	   IEQ(29)=10
c	   IEQ(30)=14
c	   IEQ(31)=6
c	   IEQ(32)=10
c	   IEQ(33)=14
c	   IEQ(34)=5
c	   IEQ(35)=9
c	   IEQ(36)=13
cc
c	   JEQ(1)=10
c	   JEQ(2)=11
c	   JEQ(3)=12
c	   JEQ(4)=14
c	   JEQ(5)=15
c	   JEQ(6)=16
c	   JEQ(7)=6
c	   JEQ(8)=7
c	   JEQ(9)=8
c	   JEQ(10)=10
c	   JEQ(11)=11
c	   JEQ(12)=12
c	   JEQ(13)=2
c	   JEQ(14)=3
c	   JEQ(15)=4
c	   JEQ(16)=6
c	   JEQ(17)=7
c	   JEQ(18)=8
c	   JEQ(19)=7
c	   JEQ(20)=11
c	   JEQ(21)=15
c	   JEQ(22)=8
c	   JEQ(23)=12
c	   JEQ(24)=16
c	   JEQ(25)=6
c	   JEQ(26)=10
c	   JEQ(27)=14
c	   JEQ(28)=7
c	   JEQ(29)=11
c	   JEQ(30)=15
c	   JEQ(31)=5
c	   JEQ(32)=9
c	   JEQ(33)=13
c	   JEQ(34)=6
c	   JEQ(35)=10
c	   JEQ(36)=14
cc
c	   IFQ(1)=13
c	   IFQ(2)=13
c	   IFQ(3)=13
c	   IFQ(4)=9
c	   IFQ(5)=9
c	   IFQ(6)=9
c	   IFQ(7)=9
c	   IFQ(8)=9
c	   IFQ(9)=9
c	   IFQ(10)=5
c	   IFQ(11)=5
c	   IFQ(12)=5
c	   IFQ(13)=5
c	   IFQ(14)=5
c	   IFQ(15)=5
c	   IFQ(16)=1
c	   IFQ(17)=1
c	   IFQ(18)=1
c	   IFQ(19)=4
c	   IFQ(20)=4
c	   IFQ(21)=4
c	   IFQ(22)=3
c	   IFQ(23)=3
c	   IFQ(24)=3
c	   IFQ(25)=3
c	   IFQ(26)=3
c	   IFQ(27)=3
c	   IFQ(28)=2
c	   IFQ(29)=2
c	   IFQ(30)=2
c	   IFQ(31)=2
c	   IFQ(32)=2
c	   IFQ(33)=2
c	   IFQ(34)=1
c	   IFQ(35)=1
c	   IFQ(36)=1
cc
c	   JFQ(1)=9
c	   JFQ(2)=9
c	   JFQ(3)=9
c	   JFQ(4)=13
c	   JFQ(5)=13
c	   JFQ(6)=13
c	   JFQ(7)=5
c	   JFQ(8)=5
c	   JFQ(9)=5
c	   JFQ(10)=9
c	   JFQ(11)=9
c	   JFQ(12)=9
c	   JFQ(13)=1
c	   JFQ(14)=1
c	   JFQ(15)=1
c	   JFQ(16)=5
c	   JFQ(17)=5
c	   JFQ(18)=5
c	   JFQ(19)=3
c	   JFQ(20)=3
c	   JFQ(21)=3
c	   JFQ(22)=4
c	   JFQ(23)=4
c	   JFQ(24)=4
c	   JFQ(25)=2
c	   JFQ(26)=2
c	   JFQ(27)=2
c	   JFQ(28)=3
c	   JFQ(29)=3
c	   JFQ(30)=3
c	   JFQ(31)=1
c	   JFQ(32)=1
c	   JFQ(33)=1
c	   JFQ(34)=2
c	   JFQ(35)=2
c	   JFQ(36)=2
c	   do n=1,36	!must also define irate, jrate, for debug
c		i=ieq(n)
c		j=jeq(n)
c		irate(n+npar)=i	 !needed to write all QT elements to qmechs.dat
c		jrate(n+npar)=j	 !needed to write all QT elements to qmechs.dat
c	   enddo
c	   goto 900
cc========end temp for debug

	   nsetq=nrateq-npar
	   print 77,nsetq,npar
77	   format(1x,i3,
     & ' q(i,j) to be set same as the basic ',i3,' rate constants')
c Define number of q(i,j) equal to each basic rate constant (is this always
c same for each of them -even if not, there is no need to store it once
c the constraints have been defined)
	   npareq=nsetq/npar		!e.g. 36/12
	   n=0
	   do m=1,npar
		i1=irate(m)		!index in Q of titlep(m)
		j1=jrate(m)
781		print 78,i1,j1,m,titlep(m),npareq
78		format(
     & ' Number of q(i,j) equal to q(',i2,',',i2,') (rate(',i2,')=',
     &	a10,') [',i2,'] = ')
		nsav=npareq
		call INPUTi(npareq)
		if(npareq.ne.nsav) then
		   call BELL(1)
		   ans='N'
		   call DCASK('Are you sure',ans,ans)
		   if(ans.eq.'N') goto 781
		endif
		if(m.eq.1) then
		   i=0	!defaults
		   j=0
		   i0=i
		   j0=j
		endif
		do n1=1,npareq
		   if(n1.gt.1) then	!for defaults
			i=i0+idelt
			j=j0+jdelt
			if(i.gt.k) i=k
			if(j.gt.k) j=k
		   endif
c
791		   print 79,n1,i,j
79		   format('&',1x,i2,': specify i,j [',i2,',',i2,'] = ')
		   call INPUT2i(i,j)
		   if(i.eq.0.or.j.eq.0) then
			call BELL(1)
			goto 791
		   endif
		   if(n1.eq.1) then
			idelt=i-i1	!for defaults =1, or no of states/indep unit
			jdelt=j-j1
		   endif
		   i0=i
		   j0=j
		   n=n+1
		   ieq(n)=i
		   jeq(n)=j
		   ifq(n)=i1
		   jfq(n)=j1
		   IQ(i,j)=m	!already defined??
c		   IQQ(i,j)=n	 !needed?
c to write all elements of QT to qmechs.dat, need to define all nrateq values
c to irate,jrate. This is always done for 'normal' models, but for indep
c models (indmod=true) only the first npar values are defined -add the rest now
		   irate(n+npar)=i	 !needed to write all QT elements to qmechs.dat
		   jrate(n+npar)=j	 !needed to write all QT elements to qmechs.dat
	      enddo
	   enddo
	endif		!end of if(indmod)
900	continue	!temp for debug
c
	call GETCHAN(nchan,dgamma,readp,sameq,statname,idest)
      if(discprt) write(8,320) NCHAN,kA
320	FORMAT(/,' Number of channels = ',i10,/,
     & 3X,I3,' open states',/)
	do i=1,kA
c	   print 321,i,statname(i),1.0d12*dgamma(i)
	   if(discprt) write(8,321) i,statname(i),1.0d12*dgamma(i)
321	   format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
	enddo
c=	CFkin=nchan*Vkin*1.0E-3*1.0E9		!FOR nA at potential of expt
c Now V-dependence
	call GETVDEP(k,titlep,npar,IQ,readp,sameq,
     & vkin,vhold,vref,idest)
140	continue
c
	if(nvdep.eq.0) goto 37		!use Vkin=Vref set above
	print 1001,vkin
1001	FORMAT(
     & ' Membrane potential (E-Erev) (real mV)= ',f6.1,':  O.K.[Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') goto 324
	print 1002
1002	FORMAT(
     & ' Membrane potential (E-Erev) (real mV) = ')
	call INPUTr(vkin)
324	continue
	if(pon()) write(7,32) vkin
      if(discprt) write(8,32) vkin
32	FORMAT(/,' Membrane potential (E-Erev) = ',g13.6,' mV')
  	vhold=vkin		!pot at which kinetics measured
	V1=VHOLD-Vref		!V1 DEC WITH HYPERPOL- define for QSETD
c=	CFkin=float(NCHAN)*Vkin*1.0E-3*1.0E9	!FOR nA at potential of expt
c
37	continue
c Get the number of ligands bound to each state (returns nbound(i,j) in common)
c=====temp for debug
c	nbound(1,1)=2	!gly
c	nbound(1,2)=2	!glu
c	nbound(2,1)=2
c	nbound(2,2)=2
c	nbound(3,1)=2
c	nbound(3,2)=1
c	nbound(4,1)=2
c	nbound(4,2)=0
c	nbound(5,1)=2
c	nbound(5,2)=2
c	nbound(6,1)=2
c	nbound(6,2)=2
c	nbound(7,1)=2
c	nbound(7,2)=1
c	nbound(8,1)=2
c	nbound(9,2)=0
c	nbound(9,1)=1
c	nbound(9,2)=2
c	nbound(10,1)=1
c	nbound(10,2)=2
c	nbound(11,1)=1
c	nbound(11,2)=1
c	nbound(12,1)=1
c	nbound(12,2)=0
c	nbound(13,1)=0
c	nbound(13,2)=2
c	nbound(14,1)=0
c	nbound(14,2)=2
c	nbound(15,1)=0
c	nbound(15,2)=1
c	nbound(16,1)=0
c	nbound(16,2)=0
c	goto 901
cc=====end temp for debug
	if(.not.boundef) then 	!nbound not read from qdat.dat
c===temp removal	   call GETBOUND(k,npar)
	endif
	do n=1,nlig
	   print 401,n,ligname(n)
	   if(discprt) write(8,401) n,ligname(n)
401	   format(/,' Ligand number ',i2,', ',a20)
	   do i=1,k
		print 40,i,nbound(i,n)
		if(discprt) write(8,40) i,nbound(i,n)
40	      format(' Number of ligands bound to state #',i3,' = ',i2)
	   enddo
	enddo
	ans='Y'
	call DCASK(' Are these correct',ans,ans)
	if(ans.eq.'N') then
	   print 41
41	   format(' Please tell DC that GETBOUND failed!')
	   do i=1,k
		print 108
108		format(/)
	      do n=1,nlig
		   print 42,n,ligname(n),i
42		   format('& Number of ligand #',i2,' (',a20,
     &		') molecules bound to state #',i3,' = ')
		   call INPUTi(nbound(i,n))
		enddo
	   enddo
	endif
901	continue	!temp for debug
	boundef=.true.
C This version does not ask for agonist conc, or return QD
cC NOW GET AGONIST CONC.
c	if((.not.kmfast).and.ncdep.eq.0) goto 504	!conc not needed
c24	continue
c311	continue
c	if(nlig.eq.1) then
c	   xA=conc(1)	!default
c	   print 16
c16	   FORMAT(' Concentration (micromolar) = ')
c	   call INPUTr(xA)
c	   XA=XA*1.0E-6		!=MOLAR
c	   conc(1)=xA
c         if(discprt) write(8,163) 1,xa,ligname(1)
c163	   FORMAT(' Ligand #',i2,': concentration (micromolar) = ',
c     &	g13.6,'(',a20,')')
c	else
c	   do n=1,nlig
c		x=conc(n)	!default
c		print 161,n,ligname(n)
c161		FORMAT(1x,i2,'. Concentration of ',a20,' (micromolar) = ')
c		call INPUTr(x)
c		X=X*1.0E-6		!=MOLAR
c		conc(n)=x
c      	if(discprt) write(8,163) n,x,ligname(n)
c	   enddo
c	endif
c	xB=conc(2)		!for consistency with earlier versions
c	if(nlig.lt.1.or.nlig.gt.10) print 162
c162	format(' ERROR: not yet written for more than 10 ligands')
cc
c241	continue
cc
cc	IF(KMFAST) CALL KM2OCC(XA)
c	IF(KMFAST) CALL KM2OCC(conc(1))
c504	CONTINUE
cc calc QD to get init occs
c	call QZERO(QD,k)
c	call QSETD(conc,IL,V1,QT,QD,K,.false.)
cc	call QSETD(XA,XB,IL,V1,QT,QD,K,.false.)
cc
cc Print out final values of rate constants used (inc those determined
cc by micro rev?)
c   -problem here if conc=0. (eg as initial conc for a conc jump) since
c qd/xA=r/xA=0/0. Solve this by doing special call to QSETD with epsim=T
c and call result QDUM, used only for printing rate constants (also removes
c need to check which rates are conc-dep). Just set epsim=true
c so that rates are not multiplied by conc, so conc() ignored)
	call QZERO(Qdum,k)
	call QSETD(conc,IL,V1,QT,Qdum,k,.true.)
      if(pon()) write(7,13)
      if(discprt) write(8,13)
13	format(/,' Final values of rate constants')
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   r=qdum(i,j)
         if(pon()) write(7,12)m,i,j,titlep(m),r
         if(discprt) write(8,12)m,i,j,titlep(m),r
12	   format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	enddo
c
c	CALL EQOCCd(QD,K,K-1,KMAX,Peq)		!CALC EQUILIB OCCS
c
C CALC CURRENT FOR THIS (or these) conc (now all in SLIFED1)
c	call SLIFED2(QD,Peq,.false.,.false.,kmax,kmax) !screen only
c	CUR=0.0
c	DO 319 I=1,kA
c319	CUR=CUR+sngl(Peq(I)*dgamma(I))
c	CUR=CUR*CFkin
c	IF(KMFAST) CUR=CUR*PSTAR(1)	!FRACT OF STATE 1 THAT IS OPEN
c	print 421,Vkin,1.e3*CUR
c421	FORMAT( ' Current (pA), at ',F6.1,' mV= ',g13.6)
c3161	continue
c
c	IF(ncdep.eq.0) GOTO 317
c	print 316
c316	FORMAT( '  O.K.[Y] ? ')
c	ans='Y'
c	call INPUTa(ans)
c	if(UC(ans).EQ.'N') GOTO 311   !GET NEW CONC
C
C Calc PI and TYPE RESULTS
c317	continue
c	DO 20 I=1,K
c	DO 20 J=1,K
c	pi(i,j)=-1.		!in case denom=0
c	if(dabs(qd(i,i)).lt.1.d-25) goto 20
c	PI(I,J)=SNGL(QD(I,J)/(-QD(I,I)))
c20	continue
c	print 400
c400	FORMAT( ' Type Q and PI matrices [Y]? ')
c	ans='Y'
c	call INPUTa(ans)
c	if(UC(ans).EQ.'N') GOTO 45
c	CALL ATYPD(QD,'    QD  ',K,K,KMAX,KMAX)		!TYPE Q
c	CALL ATYPS(PI,'    PI  ',K,K,KMAX,KMAX)
c45	continue
	call flush(7)
C
c Model finished; now store it?
641	continue
c
	if(newmod.or.altmod) then
	   print 4
4	   format(/' Save new model (and rates) on disc [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
         if(ans.eq.'N') goto 99
	   if(newmod) blank=.true.
	   if(altmod) blank=.false.
	   call TITENT0(' Enter title for new model',mtitle,74,blank)
	   call TITENT0(' Enter title for rates',rtitle,74,blank)
	   nmods=nmods+1		!increment # of models
	   imod0=nmods		!new model number
	   mtits(imod0)=mtitle		!new title

	   print 8,mtitle,imod0
	   if(discprt) write(8,8) mtitle,imod0
8	   format(
     &   ' New model, named',/,1x,a74,/,
     &   ' stored as model number ',i4)
c===	   goto 802		!write to disc -don't skip next bit!!
	else
	   print 411
411	   format(/' Save rates (and model) on disc [N] ? ')
	   ans='N'
	   call INPUTa(ans)
         if(ans.eq.'N') goto 99
	   call TITENT0(' Enter title for rates',rtitle,74,.false.)
	endif
c
c Can either write to new record, or overwrite the last one written (but not
c any earlier ones). In latter case nrecs and jstart() are not altered, but
c must recalculate nextrec, as the new one may be different length from the
c one that is overwritten.
	iopt=1
82	print 7,nrecs+1,iopt
7	format(/,' Next vacant record in qmechs.dat = ',i6,/,
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
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat
	if(iopt.eq.1) then
	   nrecs=nrecs+1
	   irec=nextrec		!where to write the record
	   ireclast=irec
	   jstart(nrecs)=irec
	   rtitles(nrecs)=rtitle
	   nextrec=nextrec + nbytes + 256 	!leave gap of 256 bytes
	else 		!overwrite last record
	   irec=jstart(nrecs)	!nrecs not changed
	   ireclast=irec
	   rtitles(nrecs)=rtitle
	   nextrec=nextrec + nbytes + 256 	!re-calculate nextrec
	endif
c
c
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
c	imod0=n
c	mtits(imod0)=mtitle		!new title
c	if(imod0.gt.nmods) goto 676
cc NB if moved up then model numbers for rate constants etc in QDAT2.DAT will
cc be wrong
c	print 674,imod0,mtits(imod0)
c674	format(' Present model #',i3,' has title =',/,1x,a74,')',/,
c     &   ' (1) Overwrite present model',/,
c     &   ' (2) Move present model, and all above it, up one place',/,
c     &   ' (3) Abandon ',/,
c     &   ' Option number [3] = ')
c	call INPUTi(iw)
c	if(iw.le.0.or.iw.ge.3) goto 641
c	if(iw.eq.2) nmods=nmods+1
c	if(iw.eq.1) mtits(imod0)=mtitle		!new title
cc
802	continue
c For new model, print model # and charmod here
	if(pon()) write(7,670)mtitle,imod0
	if(discprt) write(8,670)mtitle,imod0
	if(chardef) then
	  do i=1,ilast
	   if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
	   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
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
	print 9,nrecs,imod,mtits(imod),rtitles(nrecs),
     & nbytes,jstart(nrecs),nextrec
	if(discprt) write(8,9) nrecs,imod,mtits(imod),rtitles(nrecs),
     & nbytes,jstart(nrecs),nextrec
9	format(/,
     & ' Data written to record # ',i3,' in qmechs.dat, for model # ',
     & i3,/,1x,a74,/,
     & ' Title for this record is',/,1x,a74,/,
     & 1x,i10,' bytes written, starting at byte # ',i12,/,
     & ' Next vacant byte number = ',i12,/)
c
99	continue
c	if(ichan2.eq.2) ichan2=3   !finish
c	if(ichan2.eq.1) ichan2=2   !return to QMOD1 to get 2-channel Q
	RETURN
	END               !end of main prog


	subroutine GETCON(iopt)
c To check, and/or/ask for connections
c Most param are in commons
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
	INTEGER IC(2,200)
      character*1 ans,UC
	COMMON/QPAR/NCON,IC
c	COMMON/SPAR/NSPEC,IS,JS		!special parameters
c	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
c
	ans='N'
c
	if(iopt.eq.2) goto 20
	print 123,(IC(1,j),ic(2,j),j=1,ncon)
123	format(' Connections= ',/,4(5(2i3,4x),/))
	if(iopt.eq.1) RETURN
	print 2
2	format('& O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
20    if(UC(ans).ne.'Y') then
	   print 50
50	   FORMAT( ' No of connections (up to 20) (-1 for old values)= ')
	   call INPUTi(L)
	   if(L.lt.0) goto 503
	   ncon=L
	   DO 60 M=1,NCON
	   print 52,M
52	   FORMAT( ' #',I2,'   I,J= ')
	   call INPUT2i(IC(1,m),IC(2,m))	!=i,j
C         E.G. I,J=1,2
C		2,5
C		5,4  ETC
60	   CONTINUE
	endif
503	continue
c
c	if(nspec.eq.0) goto 128
c	print 130,(IS(L),JS(L),L=1,nspec)
c130	format(' Forward desens rates= ',/,2(5(2i3,4x),/))
c128	if(neq.eq.0) goto 121
c	print 129,(IE(L),JE(L),EFAC(L),IF(L),JF(L),L=1,NEQ)
c129	format(' Constrained elements= ',/,
c     & 10(2i3,'= ',f9.3,' times ',2i3,/))
c
c121	continue
	RETURN
	end


	subroutine GETLIG(iopt,il,nlig,ligname)
C Define index for values to be mult by conc
c***Have problem if more than one ligand present- conc of WHICH ligand?
c so define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate (defined only if NLIG>1)
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
	integer IX(100),JX(100),IL(100)		!for ligand type
	character*20 ligname(10)
      character*1 ans,UC
	COMMON/CPAR/NCDEP,IX,JX,X
c
	ans='N'
	if(iopt.eq.2) goto 3
	if(ncdep.eq.0) then
	   print 1
1	   format(' No concentration-dependent rates')
	   nlig=0
	else
	   print 133,nlig
133	   format(' Number of ligands = ',i3,/,
     &   ' Concentration-dependent elements:',/,
     &   '   i   j     ligand #   Ligand name')
	   do L=1,ncdep
		print 4,IX(L),JX(L),IL(L),ligname(IL(L))
4		format(1x,2i3,5x,i3,12x,a20)
	   enddo
c	   Define nlig from the input values of IL() -if all IL() are =1 then
c Check nlig (but should not be necessary in thei version)
c	   nlig=1, otherwise nlig=2
c	   if(ncdep.eq.1) then
c		nlig=1
c	   else
c		nlig1=0
c		do i=1,ncdep
c		   if(il(i).ne.0) nlig1=1	!at least one ligand
c		enddo
c		if(nlig1.eq.1) then
c		   do i=2,ncdep
c			neq=0
c			n=0
c			do i1=1,i-1
c			   if(il(i).ne.il(1)) neq=neq+1
c			enddo
c			if(neq.eq.n) nlig1=nlig1+1
c		   enddo
c		endif
c		if(nlig.ne.nlig1) then
c		   call BELL(2)
c		   print 5,nlig,nlig1
c5		   format(' ERROR in GETLIG(): nlig, nlig1 = '2i4)
c		   pause
c		endif
c	   endif
	endif
	if(iopt.eq.1) RETURN
	print 2
2	format('  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
3     if(UC(ans).eq.'N') then
	   print 232
232	   format(' Number of different ligands present = ')
	   call INPUTi(nlig)
	   print 230
230	   format(
     & ' No of rate constants to be multiplied by concentration = ')
	   call INPUTi(L)
c	   if(L.lt.0) goto 231
	   ncdep=L
	   if(ncdep.ne.0) then
		do 210 L=1,ncdep
		print 214,L
214		FORMAT('&#',i2,': i,j = ')
		call INPUT2i(i,j)
		IX(L)=I	!index for C-dep Qij values
		JX(L)=J
		if(nlig.gt.1) then
		   print 211
211		   format('&Ligand number = ')
	   	   call INPUTi(IL(L))
		endif
210		continue
	   endif
231	continue
	endif
	RETURN
	end

	subroutine GETCYC(iopt)
	integer ICYC(50)
	integer NSC(50),IM(50,100),JM(50,100)
      character*1 ans,UC
c for eqoccd, getqd
	logical allmr	!local
	logical obeymr(50)
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
	COMMON/MPAR/NCYC,NSC,IM,JM
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c iopt=3 asks for rates to be calc by micro rev only
c
	ans='N'
c
	if(iopt.eq.2) goto 5
	if(iopt.eq.3) goto 6
132	if(ncyc.eq.0) then
	   print 1
1	   format(' No cycles in the model',/)
	else
	   do L=1,ncyc
		print 126,L
126		format(' Cycle # ',i3)
		if(obeymr(L)) then
		   print 1311,im(L,1),jm(L,1)
1311		   format(2i3,'  (calc by micro rev)')
		else
		   print 1312,im(L,1),jm(L,1)
1311		   format(2i3,'  (no micro rev constraint)')
		endif
		print 127,(IM(L,M),JM(L,M),m=2,nsc(L))
127		format(2(5(2i3,4x),/))
	   enddo
	endif
	if(iopt.eq.1) RETURN
c
	print 2
2	format('& O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
5     if(UC(ans).eq.'N') then
	   print 21
21	   format(
     & ' Alter ONLY the rates to be calc by microscopic rev. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
         if(UC(ans).eq.'Y') goto 6
	   print 61
61	   FORMAT( ' No of cycles (up to 50) = ')
	   call INPUTi(L)
	   if(L.lt.0) goto 66
	   ncyc=L
	   IF(NCYC.EQ.0) GOTO 66
	   DO 62 L=1,NCYC
	   print 63,L
63	   FORMAT( ' Number of states in cycle #',I2,' = ')
	   call INPUTi(nsc(L))
	   print 67
	   call INPUT2i(icyc(1),icyc(2))
	   print 65,ICYC(1),ICYC(2)
65	   FORMAT(' List states in cycle in correct order:',/,
     &    ' state # 1= ',i2,/,
     &    ' state # 2= ',i2)	!start with micro-rev route
	   DO M=3,NSC(L)
		print 651,M
651		FORMAT( ' state #',I2,' = ')  !EG, 1,3,4,7 IN CORRECT ORDER
		call INPUTi(icyc(m))
	   enddo
C SET IM,JM. IM(L,1)=I,JM(L,1)=J for Q(i,j) to be calc by micro-rev
c in the Lth cycle. The rest of the q(i,j) in the cycle, going round
c in the same direction, have i,j in the other IM,JM
	   DO M=1,NSC(L)-1
		IM(L,M)=ICYC(M)
		JM(L,M)=ICYC(M+1)
	   enddo
C LAST IM,JM
	   IM(L,NSC(L))=ICYC(NSC(L))
	   JM(L,NSC(L))=ICYC(1)		!COMPLETE THE CYCLE
62	   CONTINUE	!REPEAT FOR NEXT CYCLE
	endif
c
66	continue
	RETURN
c
c Special section for iopt=3, to get only the route to be calc
c by micro rev, for each cycle (when cycles have already been defined
c in CHARQ- values already in im,jm
6	continue
	if(ncyc.eq.0) RETURN
	ans='Y'
	call DCASK('Should ALL cycles be constrained by microscopic rev',
     &	ans,ans)
	allmr=ans.eq.'Y'
	do i=1,ncyc
17	   print 123,i
123	   format(' Cycle #',i3)
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!states for current cycle
	   enddo
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',50i3)
	   if(allmr) then
		obeymr(i)=.true.
	   else
		print 66,i
66		format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		obeymr(i)=ans.eq.'Y'
	   endif
	   if(obeymr(i)) then
		print 67,i,i1,j1
67		FORMAT(
     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
     &'   Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 9	!go to next cycle
	   endif
c	   print 67
c67	   FORMAT( '  route, q(i,j), to be calc by micro rev: i,j= ')
c	   call INPUT2i(i1,j1)
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),20)
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
	   goto 17
15	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1-m2,20)
	   goto 18
151	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1,20)
c and redefine im,jm in correct order
18	   continue
	   do m=1,nsc(i)-1
		im(i,m)=icyc(m)
		jm(i,m)=icyc(m+1)
	   enddo
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
c
9	   continue
	enddo		!next cycle
c
	RETURN	!from GETCYC
	end

	subroutine GETCHAN(nchan,dgamma,readp,sameq,statname,idest)
	REAL*8 dgamma(100)
	character*10 statname(100)
	logical readp,sameq
	character*1 ans,UC
	COMMON/KBLK/kA,kB,kC,kD
c
	ir=kA
	k=kA+kB+kC+kD
	if(.not.readp) goto 407
      print 320, NCHAN,kA
320	FORMAT(/,' Number of channels = ',i10,/,
     & 3X,I3,' open states',/)
	do i=1,kA
	   print 321,i,statname(i),1.0d12*dgamma(i)
321	   format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
	enddo
	if(sameq) goto 402
	print 43
43	FORMAT( ' Use same no of channels, and gamma values [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') GOTO 402	!IF NOT OVERWRITE WITH NEW ONES
C
407	continue
	DO 100 I=1,K	!INITIALISE
100	dgamma(I)=0.0d0
	print 403
403	FORMAT('&Number of channels (integer) [1] = ')
	nchan=1
	call INPUTi(nchan)
c
	DO 406 I=1,IR
	print 405,I
405	FORMAT('&gamma(',i2,') (pS) = ')
	call INPUTr(g)
	dgamma(i)=dble(g)*1.0d-12		!IN SIEMENS
406	CONTINUE
402	continue
	RETURN		!return from GETCHAN
	END

c Random rates not implemented at present
c	subroutine GETRAN(QT,ic)
c	real*8 QT(10,10)
c	integer IC(2,20)
c	logical route
c	common/rand/ix,iy,iz
c	COMMON/KBLK/kA,kB,kC,kD
cc
c	k=kA+kB+kC+kD
c	DO 72 I=1,KA		!FIRST KA ROWS
c	DO 721 J=1,K		!ALL COLS
c	i1=i
c	j1=j
c	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 721		!NO ROUTE FROM I TO J
c	IF(J.LE.KA) QT(I,J)=DBLE(100.*(1.+9.*random()))  !QAA=100-1000
c	IF(J.GT.KA) QT(I,J)=DBLE(100.*(5.+45.*random()))
cC						!QAB,QAC=500-5000
c721	CONTINUE
c72	CONTINUE
cC
c	DO 73 I=KA+1,KA+KB		!NEXT KB ROWS
c	DO 731 J=1,K		!ALL COLS
c	i1=i
c	j1=j
c	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 731	!NO ROUTE FROM I TO J
c	IF(J.LE.KA.OR.J.GT.KA+KB) QT(I,J)=DBLE(1000.*(5.+95.*random()))
cC						!QBA,QBC=5000-100000.
c	IF(J.GT.KA.AND.J.LE.KA+KB) QT(I,J)=DBLE(10.*(1.+9.*random()))
cC						!QBB=10-100
c731	CONTINUE
c73	CONTINUE
cC
c	DO 74 I=KA+KB+1,K		!LAST KC ROWS
c	DO 741 J=1,K		!ALL COLS
c	i1=i
c	j1=j
c	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 741		!NO ROUTE FROM I TO J
c97	IF(J.LE.KA+KB) QT(I,J)=DBLE((1.+9.*random()))   !QCA,QCB=1-10
c	IF(J.GT.KA+KB) QT(I,J)=DBLE(10.*(1.+9.*random()))
cC							  !QCC=10-100
c741	CONTINUE
c74	CONTINUE
c	RETURN	!from GETRAN
c	end

