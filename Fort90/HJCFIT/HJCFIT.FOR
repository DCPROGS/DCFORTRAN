c TO DO!

c===print out 'advanced settings'
c=====(1) add model recognition so can use modec50 again in appropiate cases
c	Also fix for liksurf case?
c	 -make stuff to recognise
c====(2)calc dependency as sep plot option?
c=== Constrain both EC50 for two co-agonists (NMDA)

	program HJCFIT
c
c To fit model specified by q(i,j) to a set of open-shut intervals using
c HJC distributions (exact for 2 deadtimes, then asymptotic).
c Lahey V5.n version
c (converted from version called HJCFIT1 for V2.n compiler, and HJCLIK1
c  (& HJCDISP1 similarly renamed).  Modified to use new QGEN.INI, QDAT.DAT
c and QMODEL.DAT files (see QGEN and QDFIX)
c All three files (QGEN.INI, QDAT.DAT, QMODEL.DAT) now kept in current root
c segment (they need to be available to several different progs)
c
c DATA TYPE
c	idatyp=1	data from normal .SCN file
c	idatyp=2	data from old scandat.dat file
c	idatyp=3	internal simulated data (autosim=T)
c	idatyp=4	data from Dempster *.EDE file
c	idatyp=5	data from Axon *.EVL file
c
c Modif 06/14/06 10:24am for case of very fast open channel block, with known
c value of KB (could later make it estimate KB), Number of rate constants that
c are slowed by block=nfblock (all with same KB) ifb(m),jfb(m)=i,j such that all q(i,j) are divided
c by (1.0+B/KB) (m=1,nfblock). This is the factor by which block extends the
c apparent opening if NO blockages are detected
c NB all rates that lead out of an open state need to be multiplied by this
c factor, because the factor is the fraction of time that the open/blocked
c state is actually open, and therefore available for the transition.
c	logical*4 fastblk
c	real*4 aKB
c	integer*4 ifb(20),jfb(20),mfb(20),iflig(20)
c	common/fblck/fastblk,aKB,nfblock,ifb,jfb ,mfb.iflig
c NB the blocker correction appears ONLY in QD(), which has, eg,
c alpha2/(1.0+B/KB) as actual transition rate.  As with aother connc dep
c transition rates, the conc is not present in theta0() or QT()
c
c Modif 03/28/06 07:06am to read windows ini files
c	character*60 pathdata,pathmec
c	character*8 ftype

c Modif 11/14/01 03:41pm to insert ratemax=maximum allowable value for any
c  rate in 1/sec (as well as assmax=maximum allowable value
c  for ANY association rate constant).
c
c Modified 09/29/03 12:32pm for new MR setting that should work for
c cubic mechanisms (see SCBST.FOR for details)
c
c Modifed 08/04/03 to use Ball method in ROOT_FB, rather than ROOTSCH,
c to locate guesses for bisection of roots.  Handling of eigAAsav and eigFFsav
c that hold eigs from previous iteration also changed: add common/eigsav/eigAAsav,eigFFsav so last eigenvalues not lost
c
c Modif 05/01/03 08:07am to allow constraint of the sum of two rate constants
c as well as the ratio of two rates.  This is done with no changes in storage
c by setting efac() negative when SUM is to be constrained, with abs(efac())=
c total of the two rate constants. Changes made in hjcfit, checkqd, qset_hjc,
c qset_tru and (in \calc), qsetd, prtmod.  If this generates a negative rate
c it is reset to 1.d-4
c
c The version that uses SLOPESCH seems to be a bit slower than earlier version
c in cases where roots are NOT grouped.  This version of HJCFIT amalgamates
c the two earlier versions
c
c Modif to change logical excop(10) to real*4 iexcop(10) (same size) and
c  iexcop(j)=0 :  no open periods excluded (same as excop=false)
c  iexcop(j)=1 :  open periods excluded when gap on both sides longer than
c			specified (same as excop=true)
c  iexcop(j)=3 :  Exclude ALL isolated openings (bursts with one opening)
c			(usable only when tcrit supplied), and suitable only for high
c			conc records with essentially no mono-liganded openings
c
c
c Modif 02/15/03 07:04pm to allow 'grouped' bunches of 'identical' asymptotic
c roots -see notes in HJCASYMP and ROOTSCH.  NB no need to be keep groups
c separately for each data set, because groups are set for the current set at
c every iteration.  Also in HJCASYMP used stable=true after fit
c has settled own, so narrower guesses can be use for bisection
c
c Modif 01/28/02 06:35pm to allow simulation to be done with different model
c  from that used for fitting. To do this, the following things, needed in
c  hjcsim2, must be kept separately for the true model used for simulation
c  irate,jrate,npar,dgamma,kA,kB,kC,kD. In hjcsim2, QT is constructed from
c  npar,irate,jrate and commons
c	common/KBLK/kA,kB,kC,kD
c	common/cpar/ncdep,IX(100),JX(100),x
c	common/LIG/nlig,IL(100)
c  Versions for simulation denoted
c  irates,jrates,npars,dgammas,kAs,kBs,kCs,kDs
c	common/KBLKs/kAs,kBs,kCs,kDs
c	common/cpars/ncdeps,IXs(100),JXs(100),xs
c	common/LIGs/nligs,ILs(100)
c
c Modif 01/16/02 12:34pm to fit log of rate constants (so steps in simplex
c can be geometrically spaced)
c
c Modif 11/25/01 05:07pm to simulate the .scn file here: for model specified
c at start, using specified 'true' rates (returned to main prog as thtrue())
c For this option autosim=true (but simulat=false). For simulated data generated

c in SCSIM (iscan=-103 or -3001 for old files) SIMULAT=true, autosim=F)
c (NB = true if either autosim or simulat is true)
c
c Modif 11/22/01 12:08pm for new getqd, and jm() added to .ini
c
c Modif 11/14/01 03:41pm to insert assmax=maximum allowable value
c  for ANY association rate constant that is a free parameter (can't do
c  much about an assoc rate that is calc by micro rev, or via constraint)
c
c Modif 11/01/01 11:33am.  During calculation of errors, QT, QD  and theta
c could be altered.  Although arrays are called th1() etc in vmat_hjc, they
c theta() and QD in HJLIK, and QD, QT are in common in HJCLIK, and so
c get changed in main program.
c
c Modif 11/01/01 07:41am to make avamp,rms,ffilt,fc,trise all arrays rms(10) etc
c so false vent rate calculated correctly for each set when nset>1
c
c Modif 10/17/01 08:16am by addition of an extra array, logical chsvec(j)
c as well as burst(j), j=1,nset.
c Burst(j)=true if record for set #j is divided into bursts i.e. tcrit is
c 	is such as to exclude some of the shut times (actually superfluous
c	becase info is in tcrit which is set to 1 year for nchan=1)
c chsvec(j)=true  if start and end vectors are calculated as in C. Hawkes
c	 & Srodzinski, (1996, eqs 5.8, 5.11), false if steady state
c	vectors are used (so can have chsvec=T only if burst=T)
c
c MAJOR MODIF 03/28/01 06:05pm TO TAKE UP TO 100 STATES/10 LIGANDS
c Note: GETQD now returns QT without conc (or diagonals), so rather than using
c QNEWC to change conc (only), use QSETD or QSETC to set whole Q matrix (does
c not need the 'standard' conc, cA1, cB1, used in old version)
c Changes
c (1) pre*3 (not pre*1)
c (2) remove calls to GETIJ and use
c		   i=irate(m)
c		   j=jrate(m)
c (3) note common sizes in getqd
c	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
c	character*2 charmod(25,40)	!to print model
c	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c NB in HJCFIT use conc(10,10) for conc of ligand i for set j, then
c use conc(1,j) in call for subroutines that expect conc(10)
c (4) In HJCFIT the array sizes are major problem. There are many big arrays
c 	and 100x100x100 (real*8) = 8 Mb. The only solutions are
c	(a) Fix maximum kA to 10 (say) and make array declarations as 10x90 rather
c	   than 100x100
c or  (b) Give these dimensions (say kAm=10, kFm=90) right at the start and
c	   pass them to every subroutine.  This is better because declarations
c	   in main prog are all that would have to be changed (along with
c	   definition of kAm and kFm, to alter the program in future
c	   e.g. make common/dims/kmax,k,kAm,kFm that contains actual
c	   declared dimensions of various arrays (dimensions can be in
c	   common but array must be an argument; see test.for)
c	   In addition, all arrays not in common could be made allocatable.
c	   NB the actual declared dimensions of each array must be passed
c		(for 2D or more arrays)
c
c (5) the statement PARAMETER(kAx=20) occurs in HJCFIT, HJCLIK, HJCASYMP,
c    DARSDS, DETWA and DETWF to define max number of open states for some
c    arrays which it is convenient not to make allocatable -just alter all
c    these if max=20 needs to be changed.
c
c (6) All pon() eliminated from hjcfit.for
c (7) replace all GMAT3 with GMAT4
c	other mew subroutines
c	eqoc_hjc.obj
c	c:\fort90\calc\mattran1.obj
c	c:\fort90\calc\submat3d.obj
c	c:\fort90\calc\RAC3d.obj
c	qnew_hjc.obj
c
c Modif 03/20/00 05:42am Convert theta, simplex hjclik etc to real*8
c
c Modif 03/18/00 11:56am
c (1) Stability plots added
c (2) HJCFIT.INI doubled in size to 20480.  Addition of one record of 'bad bits'
c     actually makes hjcfit.ini 11093 bytes, but leave plenty of marginf for
c	further expansion!
c
c Modif 02/21/00 08:48am
c Resolutions made into array, real*4 tresol(10) (so
c now real*8 tresb is redundant in .ini file), and real*8 tresd(10)
c
c Modif 01/24/00 12:53pm to read definition of bad sections to be omitted
c from hjcfit.ini NB at present the .ini file holds one good bit for each set
c in isgood(j), iegood(j)
c
c Modif 07/20/98 12:45pm to use call to GETQD to get model, as in other theory
c progs. This caused various problems when model changed -must be careful
c not to use defaults from old model read from hjcfit.ini (which now
c includes old model # so this can be checked. To get nlig, IL() from
c getqd must be careful with common/lig/ because nlig is already in
c common/qdblk/ so use common/nligsav,IL  and set nlig=nligsav after call GETQD
c Also common/cpar here had IL() added to it, but not in getqd so now
c removed and IL() transmitted via common/lig/
c
c MAJOR MODIF 10/03/94 05:18pm to use new data disks and allocatable arrays
c Modified 11/05/92 06:40pm to keep disc partition, as well as expt #, for
c every experiment -kept in array ndevs*2(10)
c Modified 06/16/92 12:51pm
c	(1) Estimates assoc rate constants -conc dependence now fixed
c	    When QD first read (from QGEN2) it has conc incorporated in it
c	    but concentration removed straight away, to make array QT
c	    without conc (i.e. with association rate constants rather than
c	    actual transition rates).
c	(2) HJCFIT.INI now expected in same directory as .exp (but CHARQ.DAT
c	   and QDISC.DAT still on D:\)
c	(3) The data files used (and concentrations for them) kept in .INI
c     (4) Data read in in HJDATIN now (options for 1st latency removed).
c	(5) Use 2-D arrays tint(i,j),iampl(i,j) for jth set (e.g. conc)
c     (6) Normally oneset=false, but to do calcs for plots (HJCDISP)
c	    set oneset=true before calling HJCLIK to do calcs, so calcs
c		done only for the one data set to be plotted
c	(7) tcrit and 'burst' can be different for each data set
c
c Fits the model number (imod0) that is read in here: model must have
c been defined in window (ie in CHARMOD in QGEN2) previously
c for sc data
c=	integer*2 IAMPL(20480,10)
c=	real TINT(20480,10)		!up to 20 *1024 for each conc
c Declare fixed dimension arrays (for use in common/hjcblk/ and /ampblk/)
c	real*4 tint(50000,10),ampl(50000,10)
c	integer*1 iprops(50000,10)
c	real*4 tint(5120,10),ampl(5120,10)
c	integer*1 iprops(5120,10)
c Make allocatable as in EKDIST, but here they are 2D arrays
c
	PARAMETER(kAx=60) 	!max number of open states for following
c Declare allocatable arrays
	allocatable::tint,tint0
	allocatable::iampl0,ampl0,ampl
	allocatable::iprops,iprops0
	allocatable::index
	real*4 tint(:,:),tint0(:,:),ampl0(:,:),ampl(:,:)
	integer*2 iampl0(:,:)
	integer*1 iprops(:,:),iprops0(:,:)

	integer*4 index(:,:)
c To pad the .ini file (see below)
	allocatable::inipad
	integer*1 inipad(:)
c
c===make all arrays with dimension 10 (=max nset) allocatable so max
c===number of sets can be specified (problem is that it is fiddly to read
c===back arrays from .ini when they are not fixed size)
c	integer isgood1(10),iegood1(10)  !use points n1 to n2 for set j
c	allocatable isbad,iebad,nbad,nbad1,
c
c	integer*4 isbad(:,:),iebad(:,:)
c	integer*4 nbad(:),nbad1(:)
c	allocatable ampval,stabcut
c	real*4 ampval(:)
c	logical stabcut(:)
c	allocatable kfile,nfile,nfileb,nintt,nint
c	integer*4 kfile(20,:),nfile(:),nfileb(:),nintt(:),nint(:)
c	allocatable pfile,pfileb
c	character*33 pfile(20,:),pfileb(20,:)	!path names for SCANDAT files
c
c For stability plots
c  temp declaration for read of .ini
	integer isgood1(10),iegood1(10)  !use points n1 to n2 for set j
c
	integer isbad(20,10),iebad(20,10)	!for bad bits (see stabplot)
	integer nbad(10),nbad1(10)
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	logical samex,mono,stabcut(10)
	logical liksurf,logsurf
	real*8 alow,ahigh,blow,bhigh,da,db
c For cjumps
	logical cjump
	allocatable::kjumps0,kjumps
	integer*4 kjumps0(:),kjumps(:)

c Other declarations
	integer kfile(20,10),nfile(10),nfileb(10),nintt(10),nint(10)
	character*33 pfile(20,10),pfileb(20,10)	!path names for SCANDAT files
	character name*12,adcfil*30,qfile*40,inifile*40
	character fileout*12
	character ascfil*40
c	character qfile1*40
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
c=	real*4 concA(10),concB(10)		!to hold concentrations for each expt
c=	real*4 concA1(10),concB1(10)		!to hold concentrations for each expt
c NB in HJCFIT use conc(10,10) for conc of ligand i for set j, then
c use conc(1,j) in call for subroutines that expect conc(10)
	real*4 conc(10,10),conc1(10,10)	!to hold concentration ith ligand, jth set
	character*20 ligname(10)
	real*4 tcrit(10),tresol(10),tresolb(10)
	real*8 tresd(10),tres1
	logical burst(10),chsvec(10),badend(10),onechan,present,samexp
	real*4 tcbad(2,10)	!1=shut, 2=open
	integer ncbad(2,10)
	logical iopen,jopen,bind	!to print equilib constants
	character*7 kname 	!to print equilib constants
	logical first,errflag,setbad(2,10)
	logical sbin,shist,sres,sexp	!for DATIN
c The following are now arrays
	real*4 avamp(10),rms(10),fc(10),ffilt(10),trise(10)
c theta now real*8
c Theta jfix etc now have dimension 200
	integer jfix(200),jfix1(200),jcon(200),jmic(200),jbad(200)
	real*8 theta0(200),theta(200),thsav(200)
	real*8 thtrue,thsav1
	allocatable::thtrue(:),thsav1(:)
	real*8 setlik(10)		!likelihood for each data set (e.g.conc)
	common/sets/setlik,nset2		!for dspout in simphjc
c To store simulations for writting to binary file
	real*8 thetval,elmval,elmset
	allocatable thetval(:,:),elmval(:),elmset(:,:)	!for simulations
	allocatable::nintval,ixval,iyval,izval,nevals
	integer nintval(:,:),ixval(:),iyval(:),izval(:),nevals(:)
	allocatable::ec50val			!for simulation results
	real*8 ec50val(:)

c
	allocatable::var,covar,covlog,varlog,badpar,jcov
	real*8 var(:),covar(:,:),elmax,cor,den			!for HMAT
	real*8 covlog(:,:),varlog(:)
	logical badpar(:)							!for HMAT
	integer*4 jcov(:)
c	real*8 var(200),covar(200,200),elmax,cor,den			!for HMAT
c	logical badpar(200)							!for Hmat
	real*4 sd(200),sdlog(200)
c
	real*8 stpfac,confac,resfac,errfac,delmin,smin	!for simplex
	real*8 hjclik	!function now real*8
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges
	allocatable QDsav,QTsav
	real*8 QDsav(:,:),QTsav(:,:)		!QT has not got conc in
	real*8 QD(100,100),QT(100,100),Peq(100)         !QT has not got conc in
	real*8 dgamma(100)
	real*8 tres
	logical excamp	!for reshjc1
	logical monot	!for ec50
	logical plotdone
c From here on can make arrays like s1a(kAm),s1F(kFm)?
c Keep following arrays of fixed size (100) so can go in common
c (see also more fixed size arrays in HJCLIK)
	real*8 rootA(100),rootF(100)
	real*8 s1A(100),s2A(100),s1F(100),s2F(100)	!init guesses for roots
	real*8 s1Asav(kAx,10),s2Asav(kAx,10),s1Fsav(100,10),s2Fsav(100,10)
	real*8 eigAAsav(kAx,10),eigFFsav(100,10)
	real*8 rootAsav(kAx,10),rootFsav(100,10)
	real*8 phiA(1,100),phiF(1,100)
	common/phiblk/phiF,phiA
c Keep roots and init guesses for them separately for each data set so
c make arrays 2D rootA(i,j)= ith root for data set j
c New declarations, as in HJCLIK (allocate these, if not in common
	real*8 ci,cj
c For getqd etc
	character qfilem*40
	real*8 cfacd,vknd
	character*10 titlep(200)
	character*10 titpfree(200)	!titles for free parameters
	common/tpf/titpfree,iptit	!common with dspout (in simphjc)
	character*11 cdate,ctime
	character*2 charmod(25,40)	!to print model
	COMMON/QPAR/NCON,IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
c	integer inew(100)
	integer IQ(100,100)
	integer IQf(100,100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	COMMON/SPAR/NSPEC,IS,JS		!special parameters (not used)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	common/cpar/ncdep,IX,JX,x	!for hjclik, checkqd, qset_hjc, hjcdisp
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
	integer nbound(100,10)
	common/nbnd/nbound
	COMMON/qblk/IQ
	common/q0/irate,jrate		!for modec50
	common/KBLK/kA,kB,kC,kD
c 	-following needed for QSETD (see PDCALC)- make sure all set
	COMMON/KM2/AKA1,BA,PSTAR(4),KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)	!not used?
c=	integer iq0(20),jq0(20),iq1(20),jq1(20)
c iq0, jq0 are same as irate, jrate in new QETQD
c iq1 and jq1 now renamed irate1, jrate1
	integer irate(200),jrate(200),irate1(200),jrate1(200)
	integer IS(100),JS(100)	!declare for disc-write-not used now
	logical debug,caplock
	logical readini,oneset,inidef
	character*1 ans,UC,ans1,ans2,ans3,ans4,ans5
	character*2 ndev
	LOGICAL KMFAST		!for QSETD
	logical nofit,nodata
	logical curvonly
	common/ndata/nodata
	logical discprt,debprt,dpsav
c  For parsname etc
	character path1*30,pname*8,suffix*3	!for parsname
	character pnameb*8,suffixb*3
	logical nopath
	logical csfound,digchar
c	logical dprt
	character prtfil*40
	character defname*6,cnum*11,prefix*40,command*100
c
	character mtitle1*40,filnam*32,prtport*4		!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c add common for getqd
	character*74 mtitle	!title for MODWIND models
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print sim model
	common/models/imods,charmods,jlasts,ilasts,mtitles,imodolds
c
	common/sblk/sbin,shist,sres,sexp	!for datin,resint
	COMMON/RBLCK/treso,tresg,acrit,avamp1	!for resint61, stab_hjc
	common/sch/np0min,np0max,npmin,npmax	!for rootsch
	common/deb/idebug,idebug1
	common/deb1/debprt
	common/deb2/idebug2	!for printing in checkmr
	common/deb3/idebug3	!for check on solution of r*WF=0

	real*8 fcomp(10)		!j=1,nset (for hjclik only)
	common/lcomp/nset1,fcomp				!for SIMPLEX, HJCLIK
	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) for set j (for hjclik only)
	COMMON/HJCBLK/Nint,tcrit,burst,chsvec,badend		!for HJCLIK
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
c common/CBLK/ for HJCLIK, checkqd,qset_hjc,hjcdisp, prtrate
	common/CBLK/nset,conc,jsetlast
c    Used QT in QDBLK now (conc not incorp) for hjclik and qset_hjc
c==	common/QDBLK/QT,QD,npar,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/QDBLK1/QT,QD
	common/QDBLK2/npar,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/root/rootA,rootF	!hjclik,popadj,fcrqt,fmrqt,popadjc,depend,mopadjc
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots; hjclik only
	common/eigsav/eigAAsav,eigFFsav
	common/rootsav/rootAsav,rootFsav		!hjclik only
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav !for init guesses for roots (hjclik only)
c common/detw2/ was not optimally aligned -fixed here
	common/detw2/tres,km,nerr	!for DETWA,DETWF, hjclik, hjcasymp, hjcexact
	common/queue/qfile	!for vplot and vhist
	common/resblk/tresd	!for hjclik only
c
c For micro rev
	logical allmr
	logical useprim		!use spanning tree method
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c to store alpha, beta to allow check on correlation
	allocatable alpha2,beta2,aloglik
	real*8 alpha2(:),beta2(:),aloglik(:)
c	real*8 alpha2(10000),beta2(10000),aloglik(10000)
c alpha2,beta2 now alloc so not in common
c	common/absave/nab,alpha2,jalpha,beta2,jbeta,aloglik	!in hjclik, hjcdisp
	common/absave/nab,jalpha,jbeta		!in hjclik, hjcdisp
c For ec50 constraint
	logical dcmodel,dcmod,fixec50,prtec50
	real*4 xs
	real*8 ec50,ec501,xqlo,xqhi,ec50out,ec50out1,pmax
	common/ec/fixec50,nmod,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel
c	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	real*8 ec502,xqlo2,xqhi2
	real*4 conc_ec1(10),conc_ec2(10)	!conc of other ligands for ec50
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
c  common/ec/ is in checkqd, qset_hjc
c For penalty function
	logical penalty
	real*8 penfunc,penfac
	common/pen/penalty,penfunc,penfac 	!for hjclik, qset_hjc
c
c New declarations of arrays that were in commons, not previously in main prog,
c but now allocatable so must be made arguments of HJCLIK, HJCDISP etc
	allocatable::Z00A,Z10A,Z11A,Z00F,Z10F,Z11F
	real*8 Z00A(:,:,:),Z10A(:,:,:),Z11A(:,:,:)
	real*8 Z00F(:,:,:),Z10F(:,:,:),Z11F(:,:,:)
	allocatable::XAF,XFA,QEXPQA,QEXPQF
	real*8 XAF(:,:,:),XFA(:,:,:)
	real*8 QEXPQA(:,:),QEXPQF(:,:)
c
c Need to allocate temporary arrays to read old .ini files (suffix 2)
	ALLOCATABLE::jfix2,ie2,je2,efac2,if2,jf2
	ALLOCATABLE::jcon2,IM2,JM2,jmic2,conca,concb
	integer jfix2(:),ie2(:),je2(:),if2(:),jf2(:),
     & jcon2(:),IM2(:,:),JM2(:,:),jmic2(:)
	real*4 efac2(:),conca(:),concb(:)
c For max assoc rate
	real*8 assmax,ratemax
	common/amax/assmax,icdep(200),ratemax		!for hjclik
c For simulated data
	logical repeat,autosim,simulat,sim,apfile,endsim,restart
	character*40 simfile,simfile1	!file names for output of simulation
	logical abort
	common/abt/abort		!from simphjc
	common/rand/ix1,iy1,iz1
c For separate mechanism used for simulation (possibly different from that
c used for fit)
	integer irates(200),jrates(200)
	real*8 dgammas(100)
	common/KBLKs/kAs,kBs,kCs,kDs
	common/cpars/ncdeps,IXs(100),JXs(100),xs
	common/LIGs/nligs,ILs(100)
c
c For fitting log (rate constant)
	real*8 stpsav
	logical logfit,logsav
	common/logf/logfit
c Recent additions
	common/ires/ireset	!for hjclik
	real*8 perfac
	common/pert/ perfac
c For exclusion of openings
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
c=	logical excop(10),excopen
	logical checkadv
	logical excopen
	integer*4 iexcop(10)
	common/exop/iexcop,gaplo,gaphi,nskip		!for getoper
c For hjcasymp
	logical slopsch,checkgrp,grouped,stable
	real*8 rcrit,gfac1,gfac2
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	!for hjcasymp
c
c  For getqd1 to enable read/write of models defined in Windows version
	character*3 snumw(100)		! state number
	integer*4 icolwin(100)		! colour for open/schut
	real*4 wposx(100),wposy(100)	! position
	integer*4 nwidwin,nhghtwin	!window dimensions
	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
c
	common/timer/idebugt,itick,itlast,ndeb
c Added 03/28/06 07:05am to read windows ini files
	character*60 pathdata,pathmec
	character*8 ftype
c when iftype=100, from windows version, read
	character*60 qmec, pfiles(20,10)
c with long path, rather than character*33 pfileb(20,10) and character*40 qfilem
c for fast block case
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb
c
	external HJCLIK       !function
c
c
c	pon()=slock()
	debug()=caplock()
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
c Use F90 routine to prevent underflow crashes??
	errflag=.true.
	mono=icol.eq.0
	call UNDFL(errflag)
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	call GINO
	call vga
	call mode(3)
	filnam='HJCFIT.PRT'
	call WINPRINT		!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
c
	print 100
	if(discprt) write(8,100)
100	FORMAT(
     &' HJCFIT: Fit of model to open-shut times with missed events',/,
     &'  (Uses HJC distributions, exact for 1st 2 deadtimes then',/,
     &'  asymptotic, to calculate likelihood of record)',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 6,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,6) cdate,ctime(1:8),mtitle1
6	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' CAPS LOCK on for debugging')
c
c	ktd=200	!fixed size of theta, jfix etc
c	Now allocated below
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ireset=1	!for hjclik
	perfac=0.05d0	!maximum random perturbation
	ngpcheck=100
	excopen=.false.
c
	nab=0		!for save of alpha and beta values
c
c Define declared dimensions of tint() (could also define allocated dimensions
c likewise, if decide to not used fixed dimension for tint() -but this will
c require making a version of simplex which can pass data to HJCLIK as
c parameters).
	sbin=.false.	!for HJCDAT
	shist=.false.	!for HJCDAT
	sres=.false.	!for HJCDAT
	sexp=.false.	!for HJCDAT
	idiskq=-1		!until defined
	nset=1		!default number of data sets
c
c
c===Advanced options were here, now moved below .ini, but set defaults
c here in case .ini not read
c=====
	do j=1,10
	   iexcop(j)=0
	enddo
	np0min=100
	np0max=10000	!ditto for fine search
	npmin=50
	npmax=500000	!upper limit of number of search points for coarse search
c	Now allocated below
	idebug=1		!no debugging
	idebug1=1		!no debugging
	idebug1=1		!no debugging
	idebug2=0		!no print in chekmr (except at end)
	idebug3=0		!for check on solution of r*WF=0
	idsav=1		!ditto
c params for grouped (closely spaced) asymptotic roots
	slopsch=.false.
	checkgrp=.false.
	grouped=.false.
	rcrit=1.05d0	!identical within 5%
c params for using narrower guesses when fit is stabilised
	nstab1=200		!200 evals since last rootsch call
	gfac1=0.1d0	!guesses = rootF +/- 10% after nstab1 evaluations
	nstab2=500		!500 evals since last rootsch call
	gfac2=0.02d0	!guesses = rootF +/- 2% after nstab2 evaluations
	stable=.false.
	debprt=.false.
	liksurf=.false.
c========
c
	autosim=.false.
	nofit=.false.
	nodata=.false.
	prtec50=.false.
	penalty=.false.
	logfit=.false. 	!except when hjclik called from simplex
c    Initialise in case .ini not read
	nfix=0
	neq=0
	ncyc=0
	nmr=0			!number of rates actually set by MR
	do i=1,200		!ktd=200
	  jfix(i)=0		!to mark fixed params
	  jfix1(i)=0	!all zero, for Simplex call
	  jcon(i)=0		!to mark constrained params
	  jmic(i)=0		!to mark micro rev params
	enddo
c
c
c Initialise
	neq=0		!in case init file not read
	ans1='N'		!in case init file not read
	ndisp=10	!ditto
	fastblk=.false.
c	ihjc=1	!use HJC definition of resolution
c
c Read the INIT file (tres kept in microsec here)
c Read file names as fileb and pfileb() so can see whether this run uses
c same expts as last
452	readini=.false.
	ans3='N'
	ans4='Y'
	ans5='N'
	inifile='HJCFIT.INI'
c Now check if default from last run to be used (see advanced options)
	INQUIRE(file='HJCINI.INI',exist=present,flen=nlen,err=453)
	if(present.and.nlen.gt.0) then
         OPEN(unit=30,file='HJCINI.INI',status='UNKNOWN',
     &     	access='TRANSPARENT')
	   read(30,rec=1) inidef
	   if(inidef) then	!use inifile name from last run
	       read(30,rec=1) inidef,inifile
	   endif
	   CLOSE(unit=30)
	endif
c
	print 45
45	format(' Read defaults from .ini file on disc [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(ans.eq.'Y') then
	  call TITENT0(
     &    'Name for hjcfit.ini file:',inifile,40,.false.)
	  INQUIRE(file=inifile,exist=present,flen=nlen,err=452)
	  if(.not.present.or.nlen.eq.0) then
	    call BELL(1)
	    print 451
451	    format(' HJCFIT.INI not found')
	    ans='N'
	    goto 452
	  endif
	  if(discprt) write(8,111) inifile
111	  format(' Name of initialisation file: ',a40)
	  readini=.true.
        OPEN(unit=30,file='HJCINI.INI',status='UNKNOWN',
     &   access='TRANSPARENT')
	  write(30,rec=1) inidef,inifile
	  CLOSE(unit=30)
c Need to allocate temporary arrays to read old .ini files (suffix 2)
	  ALLOCATE(jfix2(20),ie2(10),je2(10),efac2(10),if2(10),jf2(10),
     &  jcon2(20),IM2(8,15),JM2(8,15),jmic2(20),conca(10),concb(10))
c
	  if(nlen.le.20480) then	!old file
	    if(nlen.eq.10240) then	!old file
		OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=10240)
c		read(19,rec=1) pfileb,tresb,nfix,jfix2,neq,IE2,JE2,EFAC2,
		read(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &  	IF2,JF2,jcon2,IM2,JM2,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &  	nrange,idiskq,
     &	nfileb,kfile,nset,concA,concB,tcrit,burst,irecq,
     & 	idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	isgood1,iegood1,tresolb
	    else if(nlen.eq.20480) then	!new .ini for old 10 state/2 ligand version
      	OPEN(unit=19,file=inifile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=20480)
		read(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &      IF2,JF2,jcon2,IM2,JM2,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &	nrange,idiskq,
     & 	nfileb,kfile,nset,concA,concB,tcrit,burst,irecq,
     & 	idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	nbad1,isbad,iebad,tresolb,ans3,ans4,
     & 	fixec50,ec50,i50,j50,m50,xqlo,xqhi
	    endif
	  	CLOSE(unit=19)
		call SYSTEM("copy "//inifile//", hjc0.ini")
		print 10,inifile
		if(discprt) write(8,10) inifile
10		format(/," Old hjcfit.ini file, ", a30,/,"copied as "
     &	"""hjc0.ini"", and new 60 kb hjcfit.ini will be made",/)
c          Transfer old .ini data to new arrays and deallocate old
		do j=1,10
		   conc(1,j)=concA(j)
		   conc(2,j)=concB(j)
		   ie(j)=ie2(j)
		   je(j)=je2(j)
		   efac(j)=efac2(j)
		   if(j)=if2(j)
		   jf(j)=jf2(j)
		enddo
		do i=1,20
		   jfix(i)=jfix2(i)
		   jcon(i)=jcon2(i)
		   jmic(i)=jmic2(i)
		enddo
		do i=1,8
		  do j=1,15
			im(i,j)=im2(i,j)
			jm(i,j)=jm2(i,j)
		   enddo
		enddo
		DEALLOCATE(jfix2,ie2,je2,efac2,if2,jf2,
     &	  jcon2,IM2,JM2,jmic2,conca,concb)
	  else	!new ini file for 100 state/10 ligand version
c check size initially by writing as transparent
c Transparent write shows new .ini is 56459 bytes, so keep as fixed
c record length of 60k=61440 bytes. BUT record length > 32k gives
c error, so easiest to use transparent access, but when writing
c .ini, pad it out to 61440 bytes, so if other things added later
c don't get 'read past end of record'.

c When iftype=100 (from windows) read pfiles and qmec which have the
c long path included
c character*60 qmec, pfiles(20,10)
c rather than DOS versions
c character*33 pfileb(20,10)
c character*40 qfilem

      OPEN(unit=19,file=inifile,status='UNKNOWN',
     &    access='TRANSPARENT')
	read(19,rec=1) iftype
	if(iftype.eq.100) then
      read(19,rec=1) iftype,pfiles,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc,
     &    (nsc(i),i=1,ncyc),
     &    ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &    ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &    assmax,qmec,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)

c now copy into pfiles, qmec into DOS version, pfileb, qfilem, removing path
c character*60 qmec, pfiles(20,10)
c rather than DOS versions
c character*33 pfileb(20,10)
c character*40 qfilem
	   do j=1,nset
	    	do i=1,nfileb(j)
		   call PARSNAM1(pfiles(i,j),path1,ndev,pname,suffix,
     &		fileout,nopath,60)
c		   pfileb(i,j)=pname//'.'//suffix
		   pfileb(i,j)=fileout

		  call DOSFILE(pfileb(i,j),33,pfileb(i,j),nerr)
		  if(nerr.ne.0) then
		     call BELL(1)
		     print 221,pfileb(i,j)
221		     format(' ERROR: ',a33,' is not DOS file format')
		     pause
		  endif
		enddo
	   enddo
	   call PARSNAM1(qmec,path1,ndev,pname,suffix,
     &		fileout,nopath,60)
	   qfilem=fileout
	   call DOSFILE(qfilem,40,qfilem,nerr)
	  if(nerr.ne.0) then
	     call BELL(1)
	     print 221,qfilem
c221	     format(' ERROR: ',a33,' is not DOS file format')
           pause
	  endif

	else

      read(19,rec=1) ftype
      if(ftype.eq.'windows') then
		read(19,rec=1) ftype,pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc,
     &    (nsc(i),i=1,ncyc),
     &    ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &    ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &    assmax,qfilem,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig),pathdata,pathmec

c  case where qfilem and pfileb are read with no path

	else
		read(19,rec=1) pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc,
     &    (nsc(i),i=1,ncyc),
     &    ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &    ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &    assmax,qfilem,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)

	   endif
	endif

c         OPEN(unit=19,file=inifile,status='UNKNOWN',
c     &     access='TRANSPARENT')
c	   read(19,rec=1) ftype
c	   if(ftype.ne.'windows') then
c	      read(19,rec=1) pfileb,nfix,jfix,neq,IE,JE,EFAC,
c     &   IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
c     &   nfileb,kfile,nset,conc,tcrit,burst,irecq,
c     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
c     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
c     &    fixec50,ec501,i50,j50,m50,xqlo,xqhi,kAm,kFm,
c     &    chsvec,ncyc,
c     &    (nsc(i),i=1,ncyc),
c     &    ((im(i,j),j=1,nsc(i)),i=1,ncyc),
c     &    ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
c     &    assmax,qfilem,nsim,irect,logsav,imodolds,
c     &    badend,iexcop,gaplo,gaphi,dcmod,
c     &    slopsch,checkgrp,rcrit,ngpcheck,
c     &    nstab1,gfac1,nstab2,gfac2,ans5,
c     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
c     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
c     &    (conc_ec1(i),i=1,nlig),
c     &    (conc_ec2(i),i=1,nlig)
c	   else !ftype='windows'
c	     read(19,rec=1) ftype,pfileb,nfix,jfix,neq,IE,JE,EFAC,
c     &  IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
c     &  nfileb,kfile,nset,conc,tcrit,burst,irecq,
c     &   idatyp,qfile,imodold,setbad,tcbad,onechan,
c     &   nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
c     &   fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
c     &   chsvec,ncyc,
c     &   (nsc(i),i=1,ncyc),
c     &   ((im(i,j),j=1,nsc(i)),i=1,ncyc),
c     &   ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
c     &   assmax,qfilem,nsim,irect,logsav,imodolds,
c     &   badend,iexcop,gaplo,gaphi,dcmod,
c     &   slopsch,checkgrp,rcrit,ngpcheck,
c     &   nstab1,gfac1,nstab2,gfac2,ans5,
c     &   nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
c     &   useprim,(isetmr(i),i=1,ncyc),ratemax,pathdata,pathmec
c	  endif

c
	    CLOSE(unit=19)
	    if(kAm.lt.1.or.kAm.gt.20) then
		kAM=10	!max number of open states
		kFm=90	!max number of shut states
		km=100	!total number of states
	    endif
	    allmr=.true.
c             use nsc and im(i,1) to encode obeymr, automr respectively
	    do i=1,ncyc		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
		if(nsc(i).lt.0) then
		   obeymr(i)=.false.
		   allmr=.false.
		   nsc(i)=iabs(nsc(i))
		else
		   obeymr(i)=.true.
		endif
		if(im(i,1).lt.0) then
		   automr(i)=.false.
		   im(i,1)=iabs(im(i,1))
		else
		   automr(i)=.true.
		endif
		if(nmr.lt.0.or.nmr.gt.ncyc) nmr=ncyc
	    enddo
	    if(assmax.lt.1.d4.or.assmax.gt.1.d12) assmax=1.d9	!max assoc rate
	    if(ratemax.lt.1.d3.or.ratemax.gt.1.d6) ratemax=1.d6   !max other rate (1/s)
	    if(ans3.ne.'Y'.and.ans3.ne.'N') ans3='N'	!undefined 1st time
	    if(ans4.ne.'Y'.and.ans4.ne.'N') ans4='Y'	!undefined 1st time
	    if(ans5.ne.'Y'.and.ans5.ne.'N') ans5='N'	!undefined 1st time
	  endif
c
	  if(idatyp.lt.1.or.idatyp.gt.4) idatyp=1
	  imodsav=imodold		!in case imodold changed in getqd
	  if(irecq.lt.1) irecq=1		!temp!
	  do j=1,nset
		tresol(j)=tresolb(j)
		do i=1,10
		   conc1(i,j)=conc(i,j) !save
		enddo
	  enddo
c  Checks in case rcrit etc not yet in ini file
	  if(rcrit.lt.1.01d0.or.rcrit.gt.5.d0) then	!assume not defined
		slopsch=.false.
		checkgrp=.false.
		grouped=.false.
		rcrit=1.05d0	!identical within 5%
c params for using narrower guesses when fit is stabilised
		nstab1=200		!200 evals since last rootsch call
		gfac1=0.1d0	!guesses = rootF +/- 10% after nstab1 evaluations
		nstab2=500		!500 evals since last rootsch call
		gfac2=0.02d0	!guesses = rootF +/- 2% after nstab2 evaluations
	  endif
	endif		!end of stuff done when hjcfit.ini read
c
	iopt=idatyp
	if(.not.readini) then
	    if(ans3.ne.'Y'.and.ans3.ne.'N') ans3='N'	!undefined 1st time
	    if(ans4.ne.'Y'.and.ans4.ne.'N') ans4='Y'	!undefined 1st time
	endif
c End of read of ini file
c
c Advanced options now AFTER read of .ini
	idebugt=0		!no timer
c	idebugt=1		!for debug
c
c Check if any of the advanced settings that are read from .ini differ from
c the default values. If so, give a warning, to avoid using non-default
c settings inadvertantly
c defaults are as follows
c	do j=1,10
c	   iexcop(j)=0
c	enddo
c	np0min=100
c	np0max=10000	!ditto for fine search
c	npmin=50
c	npmax=500000	!upper limit of number of search points for coarse search
cc	Now allocated below
c	idebug=1		!no debugging
c	idebug1=1		!no debugging
c	idebug1=1		!no debugging
c	idebug2=0		!no print in chekmr (except at end)
c	idsav=1		!ditto
cc params for grouped (closely spaced) asymptotic roots
c	slopsch=.false.
c	checkgrp=.false.
c	grouped=.false.
c	rcrit=1.05d0	!identical within 5%
cc params for using narrower guesses when fit is stabilised
c	nstab1=200		!200 evals since last rootsch call
c	gfac1=0.1d0	!guesses = rootF +/- 10% after nstab1 evaluations
c	nstab2=500		!500 evals since last rootsch call
c	gfac2=0.02d0	!guesses = rootF +/- 2% after nstab2 evaluations
c	stable=.false.
c	debprt=.false.
c	liksurf=.false.
	checkadv=.false.		!if true, check the advanced options
	excopen=.false.
	do j=1,10
	   if(iexcop(j).ne.0) excopen=.true.
	enddo
	if(excopen) checkadv=.true.
	if(idebug.ne.1.or.idebug1.ne.1.or.idebug2.ne.0) checkadv=.true.
	if(idsav.ne.1.or.nstab1.ne.200.or.nstab2.ne.500) checkadv=.true.
	if(liksurf.or.slopsch.or.checkgrp.or.grouped) checkadv=.true.
c    would be best if only the non-dfeault options were checked
c
461	iopt=1
	print 46,iopt
46	   format(/,
     &' OPTIONS:',/,
     &' (1) Fit data',/,
     &' (2) Show data and specified model (no fit)',/,
     &' (3) Show only curves for specified model (no data or fit)',/,
     &' (4) Advanced settings',/,
     &' Option number [',i2,'] = ')
	call INPUTi(iopt)
c
	if(iopt.eq.2.or.iopt.eq.3) nofit=.true.
	if(iopt.eq.3) nodata=.true.
	curvonly=nofit.and.nodata
c=====
	if(iopt.eq.4.or.checkadv) then
c  set debugging options
	   icase=0
448	   print 447,icase
447	   format(/,' ADVANCED SETTINGS:',/,
     &' (0) No more changes -carry on',/,
     &' (1) Switch on debugging options',/,
     &' (2) Debugging for search for roots of asymptotic pdf',/,
     &' (3) Check for closely-spaced asymptotic time constants?',/,
     &' (4) Definition of when fitting is ''stable''',/,
     &' (5) Settings to deal with negative rates',/,
     &' (6) Perturbation of '' previous best values''',/,
     &' (7) Exclude open times that have specified shut'
     &         ' time on BOTH sides',/,
     &' (8) Printing of checks on microscopic reversibility',/,
     &' (9) Switch on timer',/,
     &' (10) Save name of the .INI file as default',/,
     &' (11) Print checks on solution of r*WF(s)=0',/,
     &' Option number [',i3,'] = ')
	   call INPUTi(icase)
	   SELECT CASE(icase)
c
	   CASE(0)
	   print 150
150	   format(/,' Choose another option')
	   checkadv=.false.
	   iopt=1
	   goto 461
c
	   CASE(1)
		print 441,idebug
441		format(
     &' Debugging options:',/,
     &' (1) No debugging printout',/,
     &' (2) Print intermediates for calculation of HJC pdf',/,
     &' (3) Print eGAF(t) and eGFA(t) while calculating likelihood',/,
     &' (4) Print interval #, length and current row vector ditto ',/,
     &' (5) Both (3) and (4)',/,
     &' (6) All (2,3 and 4)',/,
     &' (7) Print Q matrix, roots and likelihood for each iteration',/,
     &' (8) Print exact and asymptotic pdfs for final fit only',/,
     &' (9) Print pdfs, and as (4), for final fit only',/,
     &' (10) Save alpha2, beta2 to display correlation',/,
     &' (11) Calculate surface for alpha2, beta2',/,
     &' (12) Print open periods and groups that are used',/,
     &' Option number [',i3,'] = ')
		call INPUTi(idebug)
		idsav=idebug
		if(idebug.gt.13.or.idebug.lt.1) idebug=1
		if(idebug.eq.8.or.idebug.eq.9) then
c		   dsav=idebug		!so debug can be switched on at end only
		   idebug=1
		endif
		if(idebug.eq.11) liksurf=.true.
		checkadv=.false.
c
	   CASE(2)
		print 445,idebug1
445		format(' Debug options:',/,
     &' (1) Print minimum details of search for asymptotic roots',/,
     &' (2) Print details of search for roots of asymptotic pdf',/,
     &' (3) Print also details of slope search for asymptotic roots',/,
     &' Option number [',i3,'] = ')
		call INPUTi(idebug1)
	      ans='Y'
	      call DCASK(
     & 'Write debug info for problems in root-location to print file',
     &  ans,ans)
	      debprt=ans.eq.'Y'
c
	   CASE(3)
	   ans='N'
	   if(slopsch) ans='Y'	!default
	   call DCASK(
     &'Optimise search for closely-spaced asymptotic time constants',
     &     ans,ans)
	   slopsch=ans.eq.'Y'
	   if(ans.eq.'Y') then
		print 449,np0min,np0max
449		format(
     &' Minimum, maximum number of points for course search [',
     &	   i4,','i6,'] = ')
		call INPUT2i(np0min,np0max)
		print 450,npmin,npmax
450		format(
     &' Minimum, maximum number of points for fine search [',
     &	   i4,','i6,'] = ')
		call INPUT2i(npmin,npmax)
c
		ans='N'
		if(checkgrp) ans='Y'	!default
	   call DCASK(
     &'Group closely-spaced asymptotic time constants into sets',
     &     ans,ans)
		checkgrp=ans.eq.'Y'
		if(ans.eq.'Y') then
		   print 13
13  		   format(
     & ' Group sets of closely-spaced asymptotic time constants [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
c		   checkgrp=.true.
		   xs=sngl(rcrit)
		   print 145,xs
145		format(
     & ' Roots are ''close'' if they differ by factor =< [',f7.2,'] = ')
		   call INPUTr(xs)
		   rcrit=dble(xs)
		   print 446,ngpcheck
446		   format(
     & ' If groups of closely-spaced roots are found, do search for',/,
     & ' ALL roots every n evaluations to ensure groups still OK ',/,
     & '   n [',i4,'] = ')
	         call INPUTi(ngpcheck)
c		   print 1451,istrat
c1451		   format(
c     & ' If signs of detW(s1) and detW(s2) are inconsistent with the',/,
c     & ' the number of roots expected in a group, then',/,
c     & '  (1) Do complete search for all routes to get guesses',/,
c     & '  (2) Use eigenvalues of QFF to scale rootF as guesses',/,
c     & '   Option number [',i2,'] = ')
c		call INPUTi(istrat)
		endif
	   endif
c
	   CASE(4)
		print 146,nstab1
146		format(
     & ' Number of likelihood evaluations before "stable'' [',i5,'] = ')
		call INPUTi(nstab1)
		xs=100.*sngl(gfac1)
		print 147,xs,xs
147		format(
     & ' When stable, reduce width of guesses to ',f7.2,
     & '% of last root [',f7.2,'] = ')
		call INPUTr(xs)
		gfac1=dble(xs)/100.d0
		print 148,nstab2
148		format(
     & ' Number of evaluations before ''very stable'' [',i5,'] = ')
		call INPUTi(nstab2)
		xs=100.*sngl(gfac2)
		print 149,xs,xs
149		format(
     & ' When very stable, reduce width of guesses to ',g7.2,
     & '% of last root [',f7.2,'] = ')
		call INPUTr(xs)
		gfac2=dble(xs)/100.d0
c
	   CASE(5)
	   i=ireset+1
	   print 442,i
442	   format(
     &   ' (1) Reset negative rates to previous best value',/,
     &   ' (2) Reset negative rates to zero',/,
     &   ' Option number [',i3,'] = ')
	   call INPUTi(i)
	   ireset=i-1 	!0 or 1
c
	   CASE(6)
	   xs=sngl(perfac)
	   print 444,xs
444	   format(
     &' Max fractional perturbation of '' previous best values'' [',
     &  f8.3,'] = ')
	   call INPUTr(xs)
	   perfac=dble(xs)
	   ans='Y'
	   call DCASK(
     & 'Write debug info for problems in root-location to print file',
     &  ans,ans)
	   debprt=ans.eq.'Y'
c
	   CASE(7)
c New option 06/03/02 05:50pm (ask for details below once nset known)
		ans='N'
		if(excopen) ans='Y'
		call DCASK(
     &' Exclude any open times',ans,ans)
		excopen=ans.eq.'Y'
c
	   CASE(8)
		i=1
	      print 455,i
455	      format(
     &   ' (1) Print MR check only at end',/,
     &   ' (2) Print all MR checks to screen only',/,
     &   ' (3) Print all MR checks to screen and print file',/,
     &   ' Option number [',i3,'] = ')
		call INPUTi(i)
		idebug2=i-1		!0,1,2
c
	   CASE(9)
		idebugt=1
		itlast=0
		ndeb=0

	   CASE(10)
		ans='Y'
		call DCASK(
     & ' Use name of ini file from this run as default for next',
     &     ans,ans)
		if(ans.eq.'Y') then
		   inidef=.true.
		else
		   inidef=.false.
		endif
         	OPEN(unit=30,file='HJCINI.INI',status='UNKNOWN',
     &     	access='TRANSPARENT')
		write(30,rec=1) inidef,inifile
		CLOSE(unit=30)
c
	   CASE(11)
		ans='Y'
		call DCASK(
     & ' Print checks on solution of r*WF(s)=0',
     &     ans,ans)
		if(ans.eq.'Y') then
		   idebug3=1
		else
		   idebug3=0
		endif
	   END SELECT
	   icase=0
 	   goto 448
	endif
c End of advanced options
c
c In this version, get model first (so ligname known)
c
c Move 'print 11' menu up, so autosim is defined before getqd called
	autosim=.false.
	if(.not.curvonly) then
	   if(idatyp.eq.1) then
		iopt=1
	   else if(idatyp.eq.2) then
		iopt=3
	   else if(idatyp.eq.3) then
		iopt=2
	   else
		iopt=idatyp
	   endif
	   print 11,iopt
11	format(/,
     & ' DEFINE THE FILE WITH FIT DATA (idealised dwell times etc.)',/,
     & ' (1) Read data from *.SCN file (from SCAN)',/,
     & ' (2) Simulate a one or more scan.scn file',/,
     & ' (3) Read data from old SCANDAT.DAT disc',/,
     & ' (4) Read data from a Dempster (*.EDE) file',/,
     & ' (5) Read data from an AXON (*.EVL) file',/,
     & ' Option number [',i2,']  = ')
	   call INPUTi(iopt)
	   autosim=.false.
	   if(iopt.eq.1) then
		idatyp=1
	   else if(iopt.eq.3) then
		idatyp=2 		!keep as in earlier versions
	   else if(iopt.eq.2) then
		idatyp=3
		autosim=.true.
	   else if(iopt.eq.4) then
		idatyp=4		!Dempster .EDE file
	   else if(iopt.eq.5) then
		idatyp=5		!Axon .EVL file
	   endif
	endif
c
c SPECIFY THE MODEL TO BE FITTED
	nvdep=0	!for fitting
	nspec=0	!in this prog
	kmfast=.false.
c Following are set by disc read: ncdep,ix,jx,il,ncyc,nsc,im,jm,kA,kB,kC,kD,
c titlep,ncon
      print 138
      if(discprt) write(8,138)
	call BELL(1)
c138	format(
c     &' ------------------------------------------------------------')
c	print 71
c71	format(/,' DEFINE THE REACTION MECHANISM')
c Note GETQD now returns QT without conc (or diagonals)
	iprint=3
	nchan=0
	kflag=0
	idest=0
59	continue	!return after getqd2 with idest=59 to try another file
	call GETQD1(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  autosim,irect,irates,jrates,npars,dgammas,kflag,idest,
     &  useprim,imodsav,dcmod,jcon,theta0,conc,nfix,jfix,jmic,nmr)
c======whether or not autosim?
	if(autosim) then
	   ks=kAs+kBs+kCs+kDs
	   if(idest.eq.591) then
		idest=0		!reset
		goto 591		!skip intermediate code 2nd time (?)
	   endif
	endif

      print 138
      if(discprt) write(8,138)
c
	k=kA+kB+kC+kD
	kF = k - kA
c=	cfac=float(nchan)*(vkin*0.001)*1.e12	!*dgamma (Siemens) gives pA
c Get rid of rounding errors
	vknd=dble(vkin*float(nchan))
	vknd=DROUND(vknd,0)	!exact double precision as long as vkin is integer
	cfacd=vknd*1.d-3*1.d12
c
c BITS dependent on model numbering
c Fix ec50 value? Ask about this below, after other constraints done
	nmod=imod0
	dcmodel=.false.
	if(nmod.eq.1.and.npar.eq.10) dcmodel=.true.
	if(nmod.eq.9.and.npar.eq.6) dcmodel=.true.
	if(nmod.eq.10.and.npar.eq.8) dcmodel=.true.
	if(nmod.eq.11.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.29.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.33.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.34.and.npar.eq.12) dcmodel=.true.
	if(nmod.eq.35.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.36.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.37.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.38.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.39.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.40.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.41.and.npar.eq.8) dcmodel=.true.
c NB need to set jalpha, jbeta=index of theta values to be saved
c This model numbering is for DC's qmodel only!
	if(dcmodel.and.(idebug.eq.10.or.idebug.eq.11)) then
	   nmod=imod0
	   if(nmod.eq.29.or.nmod.eq.33) then
		jalpha=1
		jbeta=2
	   else if(nmod.eq.1) then
		jalpha=5
		jbeta=6
	   else if(nmod.eq.11) then
		jalpha=7
		jbeta=8
	   endif
c==========add other dcmodels
	endif
c
	if(ncyc.gt.0) then
	   allmr=.true.
	   do i=1,ncyc
	      if(.not.obeymr(i)) allmr=.false.
	   enddo
	   if(allmr) then
		print 115
		if(discprt) write(8,115)
115		format(/,
     &   ' All cycles constrained to obey microscopic reversibility',/)
	   else
   		print 1141
		if(discprt) write(8,1141)
1141		format(/,' Rates may NOT obey microscopic reversibility',/)
c=	      ncyc=0
		do i=1,ncyc
		   if(obeymr(i).and.(.not.automr(i))) then
			print 200,i
			if(discprt) write(8,200) i
200			format(' Cycle ',i3,' set to obey micro rev')
		   else if(automr(i)) then
			print 2001,i
			if(discprt) write(8,2001) i
2001			format(' Cycle ',i3,' automatically obeys micro rev')
		   else
			print 202,i
			if(discprt) write(8,202) i
202			format(' Cycle ',i3,' does not obey micro rev')
		   endif
		   print 201,(jm(i,m),m=1,nsc(i))
		   if(discprt) write(8,201) (jm(i,m),m=1,nsc(i))
201		   format(' states: ',5(10(i3,4x),/))
		enddo
	   endif
	endif
	nlig=nligsav	!from common/lig/
	if(ncdep.eq.0) nlig=0
	k=kA+kB+kC+kD
c  End of getqd section
c
c This must must go after getqd, so ligname known
	if(curvonly) then
	   treso=tresol(1)	!default resolution from .ini in microsec in common
	   print 241,treso
241       format(/,
     & ' Resolution for HJC distributions (microsec) [',f8.2,'] = ')
	   call INPUTr(treso)
	   tresol(1)=treso	!keep (possibly altered) res in microsec for .ini
	   treso=treso*1.e-3	!ms
	   tresd(1)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	   tresol(1)=1.e3*treso	!keep (possibly altered) res in mus for .ini
	   do j=2,nset
		tresol(j)=tresol(1)	!same for all at moment
		tresd(j)=tresd(1)
	   enddo
c
c	   nset=1
	   print 412,nset
412	   format(/,
     &' Number of different concentrations (up to 10) [',i2,'] = ')
	   call INPUTi(nset)
c	   call GETCONC(conc,nset,nlig,ligname,readini,2)	!get conc below
c
	   if(.not.(allocated(thtrue))) allocate(thtrue(1))	!not used?
c
	   nd1=1
	   nd2=1
	   if(.not.allocated(tint)) then
		ALLOCATE(tint(nd1,nd2),ampl(nd1,nd2),
     &	iprops(nd1,nd2))
	   endif
c
	   goto 330
	endif
c
      print 138
      if(discprt) write(8,138)
c138	format(
c     &' ------------------------------------------------------------')
	call BELL(1)
	simulat=.false.
	if(.not.autosim) then
c First get expt file numbers and size
c This returns values of treso, tresg if latencies read (not yet done)
	   if(.not.(allocated(thtrue))) allocate(thtrue(1))	!not used with real data
	   print 505
	   if(discprt) write(8,505)
505	   format(' (2) DEFINE FILE FOR EXPERIMENTAL DATA (.scn)')
	   call HJCDAT1(idatyp,nfile,kfile,pfile,nval,irecs,calfacs2,
     &    nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,
     &    name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,
     &    qfile,adcfil,nfileb,pfileb,npatch,defname,samexp,
     &    cjump,nsweep,tzero,tsamp)
	    simulat=iscan.eq.-103.or.iscan.eq.-3001
	else if(autosim) then
	   simulat=.false.
	   print 506
	   if(discprt) write(8,506)
506	   format(' (2) DEFINE VALUES FOR SIMULATED DATA')
	   if(.not.(allocated(thtrue))) then
		allocate(thtrue(200))
		do i=1,200
		   thtrue(i)=0.d0
		enddo
	   endif
c Now get details for simulation of nset sets of observations, nintt(j) obs
c   in each. The actual simualtion (and hence setting of resolution has to
c   be done later  (in HJCSIM2) , after constraints etc are
c   known, so these can be applied to true parameters in thtrue().
	   call HJCSIM1(nsim,nsims,simfile,simfile1,nset,conc,nintt,
     & 	nfile,pfile,qfile,
     &	ligname,defname,readini,irect,nmax,apfile,repeat)
	   do j=1,nset
		do i=1,nlig
		   conc1(i,j)=conc(i,j)
		enddo
	   enddo
	   restart=nsims.gt.1
	   ALLOCATE(thetval(npar,nsim),nintval(nset,nsim),
     &    ixval(nsim),iyval(nsim),izval(nsim),elmval(nsim),
     &	elmset(nset,nsim),nevals(nsim))
	   ALLOCATE(ec50val(nsim))
	endif
      print 138
      if(discprt) write(8,138)
c
	sim=simulat.or.autosim
c
c If same expts as last run not used, then construct default qfile name
c (done in hjcsim1 in case of autosim)
	if((.not.samexp).and.(.not.autosim)) then
	   call PARSNAME(pfile(1,1),path1,ndev,pname,suffix,nopath,33)
	   csfound=.true.
	   do i=1,6
		ival=ICHAR(pname(i:i))
		if(.not.DIGCHAR(ival)) then
		   csfound=.false.
		   goto 41
		endif
	   enddo
41	   continue
	   if(csfound) then
		pname(7:7)='H'	!replace with H
		prefix=charnb(path1)//charnb(pname)
	   else
		if(npatch.ge.0.and.npatch.le.9) then
		   call INTCONV(npatch,cnum)
		   prefix=charnb(defname)//'H'//cnum(1:1)
		else
		   prefix=charnb(defname)//'HJ'
		endif
	   endif
	   suffix='plq'
	   qfile=charnb(prefix)//'.'//suffix(1:3)
	   call DOSFILE(qfile,40,qfile,nerr)	!replace invalid characters with 'x'
	   call TITENT0(
     &    'Name for plot queue file:',qfile,40,.false.)
	endif
c
c Now have number of intervals, so allocate arrays
	nset2=nset		!for common
	if(.not.autosim) then
	   ALLOCATE(tint0(nmax,nset),iampl0(nmax,nset),
     &	iprops0(nmax,nset),ampl0(nmax,nset),index(nmax,nset))
	else if(autosim) then 		!iampl0 is local, in hjcsim2
	   ALLOCATE(tint0(nmax,nset),
     &	iprops0(nmax,nset),ampl0(nmax,nset),index(nmax,nset))
	endif
c
c Allocate cjump arrays -not used yet but needed for resint
	cjump=.false.
	nsweep=1
	ALLOCATE(kjumps0(nsweep),kjumps(nsweep))
c
	if(.not.autosim) then
	   call HJCDAT2(tint0,iampl0,ampl0,iprops0,iscan,
     &     nintt,nfile,kfile,pfile,calfacs2,nval,irecs,nmax,nset)
	   DEALLOCATE(iampl0)
	endif
c
	if(idest.eq.-1) goto 99
c
c Allocate arrays for resolved data
	nd1=nmax
	nd2=nset
	ALLOCATE(tint(nmax,nset),ampl(nmax,nset),
     &	iprops(nmax,nset))
c
c If autosim, then get resolution settings only, but don't impose it (reshjc2)
c until after simulated date files generated later (hjcsim2)
	if(autosim) then
c set avamp from the largest of the specified conductances
	   gmax=0.0
	   do i=1,kA
		g=sngl(dgamma(i))
		if(g.gt.gmax) gmax=g
	   enddo
	   do j=1,nset	!do for each set
		avamp(j)=1.e12*gmax*0.1	!as in hjcsim2
		print 701,j
701		format(/,' SET THE RESOLUTION FOR SET #',i3)
		treso=tresol(j)	!default resolution from .ini in microsec in common
		avamp1=avamp(j)	!in common/rblck/
		call REShjc1(ffilt,fc,rms,trise,sres,sexp,cjump,
     &	 excamp,alo,ahi,treso,tresg,acrit,avamp(j),autosim)
c next 2 lines came after resint61, so they should be after reshjc2 (not after reshjc1)
c=		tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		tresol(j)=treso	!record in ms for now, in case different for each set
c=		tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	   enddo
	   goto 510
	endif
c
c Resolution setting when NOT autosim
c Now set resolution and define nint(), tint() etc (for simulated data
c may be able to skip RESINT if required resolution already imposed).
	if(.not.simulat) then
	   do j=1,nset	!do for each set
		print 701,j
c701		format(/,' SET THE RESOLUTION FOR SET #',i3)
		treso=tresol(j)	!default resolution from .ini in microsec in common
		tresg=treso
		avamp1=avamp(j)	!in common/rblck/
		call REShjc1(ffilt,fc,rms,trise,sres,sexp,cjump,
     &	 excamp,alo,ahi,treso,tresg,acrit,avamp(j),autosim)
 		call REShjc2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),
     &	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),
     &	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),
     &	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,
     &	 sim,sres,sexp,excamp,alo,ahi,treso,tresg,acrit,avamp(j))
c		call RESINT61(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),
c     &	  iprops0(1,j),iprops(1,j),nint(j),nintt(j),
c     & 	  imode,ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),
c     & 	  cjump,nsweep,kjumps0,kjumps)
		tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	   enddo
	else		!for simulated expts (resolution fixed in SCSIM)
	   tres=dble(treso)*1.0d-3 	!in seconds; value from SCAN.scn
	   print 63,treso*1000.,tresol(1),treso*1000.       !treso,tresg in ms
63	   format(/,
     & ' Resolution of ',f8.1,' microsec already imposed in SCSIM',/,
     & ' Resolution of ',f8.1,' microsec used in last run',/,
     & ' Use resolution of ',f8.1,' microsec [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		do j=1,nset	!do for each set
		  print 701,j
c701		  format(/,' SET THE RESOLUTION FOR SET #',i3)
		  treso=tresol(j)	!default resolution from .ini in microsec
		  avamp1=avamp(j)	!in common/rblck/
c		  call RESINT61(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),
c     &	  iprops0(1,j),iprops(1,j),nint(j),nintt(j),
c     & 	  imode,ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),
c     & 	  cjump,nsweep,kjumps0,kjumps)
		  call REShjc1(ffilt,fc,rms,trise,sres,sexp,cjump,
     &	   excamp,alo,ahi,treso,tresg,acrit,avamp(j),autosim)
 		  call REShjc2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),
     &	   iprops0(1,j),iprops(1,j),nint(j),nintt(j),
     &	   ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),
     &	   cjump,nsweep,kjumps0,kjumps,autosim,nsims,
     &	   sim,sres,sexp,excamp,alo,ahi,treso,tresg,acrit,avamp(j))
		  tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		  tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
		enddo
	   else	!use resolution imposed in SCSIM
c                 !so transfer data as is from tint0() etc to tint() etc
		do j=1,nset
		   nint(j)=nintt(j)
		   do i=1,nint(j)
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
		   enddo
		enddo
	   endif
	endif
c
	print 43
      if(discprt) write(8,43)
43	format(/,
     &' Resolution for HJC calculations')
	do j=1,nset
	   print 430,j,tresd(j)*1.d6
         if(discprt) write(8,430) j,tresd(j)*1.d6
430	   format(
     &'   Set ',i3,': ',f10.3,' microseconds')
	enddo
c
c Now option to show stability plots for each set
c NB nmax in call replaces nintt in stabplot (=dimension of array)
c Replace nfile with 1 so boundaries between pooled files not marked
c here and so iexstrt and iexend not need (just dummy integers in call)
c First check if any defined bad bits read form disk for current expt
c If bad sections to be removed (and resolution has not been changed)
c then do it (once!) here, now resolution has been imposed
c Check if same experiments and res as last run (for bad bits)
510	continue	!for autosim jump here, to skip resint
	do j=1,nset
	   if(readini.and.nbad1(j).ge.1) then
c check whether the bad bits were defined for the current expts or not
		samex=.true.
		diff=abs(tresol(j)-tresolb(j))
		if(diff.gt.0.1) samex=.false.	 !0.1 microsec
		if(nfile(j).ne.nfileb(j)) samex=.false.
		do i=1,nfile(j)
		   call PARSNAME(pfile(i,j),path1,ndev,pname,suffix,
     &		nopath,33)
		   call PARSNAME(pfileb(i,j),path1,ndev,pnameb,suffixb,
     &		nopath,33)
c		   if(pfile(i,j).ne.pfileb(i,j)) samex=.false.
		   if(pname//'.'//suffix.ne.pnameb//'.'//suffixb) then
			samex=.false.
		   endif
		enddo
	 	if(.not.samex) then
	          print 39
39		    format(' The bad sections on disc are for',
     &	    ' different experiments and/or resolution')
		    stabcut(j)=.false.       	!don't remove them
c		else if(first.and.samex) then
		else if(samex) then
		   call BELL(1)
	         print 37,j,nbad1(j),j,tresd(j)*1.d6
37		   format(' NOTE: For SET ',i3,': ',i3,
     &' bad sections have already been defined by stability plot for',/,
     & ' set ',i3,' (resolution ',f8.2,' microsec)',/,
     & ' bad section #     start point        end point  ')
		   do i=1,nbad1(j)
			print 371,i,isbad(i,j),iebad(i,j)
c			if(discprt) write(8,371) i,isbad(i,j),iebad(i,j)
371			format(5x,i3,16x,i9,10x,i9)
		   enddo
		   print 372
372		   format(
     & ' Remove these bad sections before analysis [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(UC(ans).eq.'Y') then
			nbad(j)=nbad1(j)
			stabcut(j)=.true.
			print 741,j
c741			format(/,' SET ',i3)
		      call REMBAD(tint(1,j),ampl(1,j),iprops(1,j),nint(j),
     &		nmax,nbad(j),isbad(1,j),iebad(1,j),index(1,j),.false.)
		   else
			stabcut(j)=.false.       	!don't remove them
			nbad(j)=0		!don't remove bad bits (but nbad1 preserved)
			print 331,j
			if(discprt) write(8,331) j
331	    		format(/,' Set ',i3,
     &  ': bad sections (from stability plot) are NOT omitted',/)
		   endif
		endif
c	   else if(readini.and.nbad1(j).eq.0) then
	   else
		stabcut(j)=.false.       	!don't remove them
		nbad(j)=0		!don't remove bad bits (but nbad1 preserved)
	   endif
c Now show stability plot(s)?
c NB if data are cut out, within stab_hjc, from one plot, they can't be cut out
c from the other too, so if stabcut=true on entry then stab_hjc allows display
c only, but no more cuts
c stability plots
         print 138
         if(discprt) write(8,138)
c138	   format(
c     &' ------------------------------------------------------------')
	   if(autosim) goto 65		!skip stab plots
	   tcfac=2.0	!as in ekdist
	   if(j.eq.1) then
c		ans5='Y'	!now in .ini
c		print 108
		call BELL(1)
		call DCASK(' Show stability plots',ans5,ans5)
	   endif
	   i1=0
	   if(ans5.eq.'Y') then
64		i1=i1+1
		if(i1.gt.3) i1=3
		print 62,j,i1
62		format(' SET ',i3,':',/,
     &	' (1) Show amplitude stability plot',/,
     &	' (2) Show open, shut time and Popen stability plots',/,
     &	' (3) No more stability plots -carry on',/,
     &	'  Option number [',i2,'] = ')
		call INPUTi(i1)
		if(i1.eq.1) then
		   iopt=8	 !for amps
		else if(i1.eq.2) then
		   iopt=7	 !for open/shut
		else
		   goto 66	!next set
		endif
		treso=tresol(j)	!in common/rblck
		tresg=tresol(j)	!in common/rblck
		avamp1=avamp(j)	!in common/rblck/
c NB acal not defined!
		call STAB_HJC(tint(1,j),ampl(1,j),iprops(1,j),nint(j),
     &       nmax,nbad(j),nbad1(j),isbad(1,j),iebad(1,j),
     &       namp,ampval,acal,trise(j),tcfac,iscan,idiskq,index(1,j),
     &	 iopt,mono,stabcut(j),j)
		goto 64
	   endif
66	   continue
	enddo		!end of j=1,nset
c
c
65	continue	!skip here if autosim
c
c If data contain long (e.g. desensitised) shut times, then cannot fit with
c nchan=1 (unless desens is included in the model).  Equally should not
c use 'burst tcrit' to exclude the desens times, because start and end
c of burst vector that are used in this case will also be wrong unless
c the model includes desensitisation explicitly.  However, it should
c be reasonable approximation (if there are not too many) to set
c all the long desens periods as bad gaps.
c
c Definitions
c Modif 10/17/01 08:16am by addition of an extra array, logical chsvec(j)
c as well as burst(j), j=1,nset.
c Burst(j)=true if record for set #j is divided into bursts i.e. tcrit is
c 	is such as to exclude some of the shut times (actually superfluous
c	becase info is in tcrit which is set to 1 year for nchan=1)
c chsvec(j)=true  if start and end vectors are calculated as in C. Hawkes
c	 & Srodzinski, (1996, eqs 5.8, 5.11), false if steady state
c	vectors are used (so can have chsvec=T only if burst=T)
c  setbad(1,j)=T if shut times set bad when longer that tcbad(1,j) -used
c		only for defaults, from .ini
c  setbad(2,j)=T if open times set bad when longer that tcbad(2,j) -used
c		only for defaults, from .ini
c Define tcrit, burst
	if(.NOT.readini) then
	   do j=1,10
		burst(j)=.false.
		chsvec(j)=.false.
		badend(j)=.true.
	   enddo
	endif
c
      print 138
      if(discprt) write(8,138)
	call BELL(1)
c138	format(
c     &' ------------------------------------------------------------')
	print 72
	if(discprt) write(8,72)
72	format(/,' DEFINE GROUPS OF OPENINGS (IF NOT ONE CHANNEL)')
	print 108
	do j=1,nset
	   if(burst(j)) then
		ans2='N'
	   else
		ans2='Y'
	   endif
c	   if(simulat.or.autosim) ans2='Y'
	   print 811,j
811	   format(/' SET ',i3)
c If answer to next question is yes, then burst=F and chsvec=F.  If answer is
c no then burst=t but chsvec=t only if long gaps are spent in states that
c are included in the mechanism
	   print 813,ans2
813	   format(
     & ' Did patch contain only one channel (AND all relevant states',/,
     &    '  are included in the mechanism being fitted) [',a1,'] ? ')
	   ans=ans2
	   call INPUTa(ans)
	   if(ans.eq.'Y') then
		burst(j)=.false.
		chsvec(j)=.false.		!irrelevant in this case!
		badend(j)=.true.		!bad gap is valid end of group
		tcrit(j)=3.1536e10		!msec=1 year! -see PRANGE
		setbad(1,j)=.false.	!unless set true below
	   else				!more than one channel
		print 81,j,tcrit(j)
81		format(
     &'     SET ',i3,':  critical shut time (tcrit) (ms) [',f9.1,'] = ')
		call INPUTr(tcrit(j))
c
		if(chsvec(j)) then
		   ans='Y'
		else
		   ans='N'
		endif
		print 812,ans
812		format(
     & ' Are long shut times spent in states that are included in',/,
     & '  the mechanism being fitted [Y means that CHS vectors ',/,
     & '  are used for start and end of groups (bursts)] [',a1,'] ? ')
c		ans=ans3
		call INPUTa(ans)
		ans3=ans		!for .ini
		if(ans.eq.'Y') then
		   burst(j)=.true.
		   chsvec(j)=.true.
		   setbad(1,j)=.false.
		else		!can't use CHS vector if states not in mech
		   burst(j)=.true.
		   chsvec(j)=.false.
c=		   burst(j)=.false.
c=		   tcrit(j)=3.1536e10		!msec=1 year! -see PRANGE
		   setbad(1,j)=.false.	!unless set true below
		endif
c		badend(j)=.true.	!now from .ini
		ans='Y'
		print 815,ans
815		format(
     &     ' Should a bad shut time be a valid end of a group [',
     &	a1,'] ? ')
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   badend(j)=.true.
		else
		   badend(j)=.false.
		endif
	   endif
c Now check whether to set long shut times bad for current set (should never
c be needed if tcrit() already set)
	   if(.not.burst(j)) then
		print 49,j
49	 	format(' Data set ',i3,/,
     &   ' Long shut times can be set as bad (e.g. because they',/,
     &   ' result from errors in fitting, or from states that are',/,
     &   ' very rarely visited, or from states that are not included',/,
     &   ' in the mechanism being fitted')
		ans=ans4
		call DCASK('Set very long shut times as bad',ans,ans)
		ans4=ans		!for .ini
		setbad(1,j)=.false.
	 	if(ans.eq.'Y') then
		   setbad(1,j)=.true.
		   print 44,j,tcbad(1,j)
44		   format(' Set ',i3,/,
     & 	 ' Set all shut times as bad if longer than t (ms) ['
     &	,g11.4,'] = ')
		   call INPUTr(tcbad(1,j))
		   n=0
		   do i=1,nint(j)
		    if(tint(i,j).ge.tcbad(1,j).and.ampl(i,j).eq.0.) then
			iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
			n=n+1
		    endif
		   enddo
		   print 47,j,n,tcbad(1,j)
		   if(discprt) write(8,47) j,n,tcbad(1,j)
47		   format(
     &	    ' Set ',i3,': ',i5,' shut times longer than ',g12.5,
     &          ' ms were set as bad')
		   ncbad(1,j)=n
		endif
	   endif
c      print what was done
	   if(burst(j)) then
	       if(discprt) write(8,84) j,tcrit(j)
84		 format(/,' SET ',i3,
     &': Critical gap length to define end of group = ',f8.1,
     &' milliseconds',/,
     &'  (defined so that all openings in a group prob come from same',
     &' channel)',/)
	   endif
	   if(chsvec(j)) then
      	print 431
      	if(discprt) write(8,431)
431		format(
     &' Initial and final vectors for bursts calculated as in C.,',/,
     &' Hawkes & Srodzinski, (1996, eqs 5.8, 5.11).',/)
	   else
		print 432
 		if(discprt) write(8,432)
432		format(/,
     &' Initial and final vectors for are calculated as for   ',/,
     &' steady state openings and shuttings (this involves a slight',/,
     &' approximation at start and end of bursts that are defined',/,
     &' by shut times that have been set as bad).',/)
	   endif
	   if(badend(j)) then
		print 4321
 		if(discprt) write(8,4321)
4321		format(
     &' A bad gap ends a group, but does not eliminate the whole group',
     &   /)
	   else
		print 4322
 		if(discprt) write(8,4322)
4322		format(
     &' A bad gap results in elimination of the whole group in which'
     & 	' it occurs',/)
	   endif
c
c Now check whether to set long open times bad for current set (whether
c or not tcrit() set for shut time)
	   ans='N'
	   if(setbad(2,j)) ans='Y'
	   print 491,j
491	   format(' Data set ',i3,/,
     &   ' Long open times can be set as bad (e.g. because they',/,
     &   ' result from errors in fitting, or from states that are',/,
     &   ' very rarely visited.')
	   call DCASK('Set very long open times as bad',ans,ans)
	   if(ans.eq.'N') then
		setbad(2,j)=.false.
	   else
		setbad(2,j)=.true.
		print 443,j,tcbad(2,j)
443		format(' Set ',i3,/,
     &  	' Set all open times as bad if longer than t (ms) ['
     &	,g11.4,'] = ')
		call INPUTr(tcbad(2,j))
		n=0
		do i=1,nint(j)
		   if(tint(i,j).ge.tcbad(2,j).and.ampl(i,j).ne.0.) then
			iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
			n=n+1
	   	   endif
		enddo
		print 471,j,n,tcbad(2,j)
		if(discprt) write(8,471) j,n,tcbad(2,j)
471		format(
     &	' Set ',i3,': ',i5,' open times longer than ',g12.5,
     &	' ms were set as bad')
		ncbad(2,j)=n
	   endif
	enddo		!end of j=1,nset loop
c
c Set excluded openings -now put after burst(j), tcrit(j) have been defined
c New option 06/03/02 05:50pm
	if(excopen.and.(.not.autosim)) then	 !excopen set under advanced options now
         print 138
         if(discprt) write(8,138)
c138	   format(
c     &' ------------------------------------------------------------')
	   call BELL(1)
	   ans='Y'
	   call DCASK(
     &' Exclude any open times',ans,ans)
	   if(ans.eq.'N') then
		do j=1,nset
c		   excop(j)=.false.
		   iexcop(j)=0
		enddo
	   else if(ans.eq.'Y') then
		do j=1,nset
		   print 811,j
c811		   format(/' SET ',i3)
		   iopt=iexcop(j)+1
133		   print 239,j,iopt
239		   format(
     &' (1) No openings excluded in set ',i3,/,
     &' (2) Exclude open times that have specified shut time on',
     &   ' BOTH sides',/,
     &' (3) Exclude all openings in groups that contain only',
     &   ' one opening',/,
     & ' Option number [',i2,'] = ')
		   call INPUTi(iopt)
		   iexcop(j)=iopt-1
c		   if(excop(j)) then 	!from .ini
c			ans='Y'
c		   else
c			ans='N'
c		   endif
c		   call DCASK(
c     &' Exclude open times that have specified shut time on BOTH sides',
c     &	ans,ans)
c		   if(ans.eq.'Y') then
		   if(iexcop(j).eq.1) then
c			excop(j)=.true.
			iexcop(j)=1
			print 238,j,gaplo(j),gaphi(j)
238			format(/,' SET ',i4,/,
     &	' Lower, upper limits for length of adjacent gap (ms) [',
     &	   f10.2,',',g11.2,'] = ')
			call INPUT2r(gaplo(j),gaphi(j))
		 	if(gaphi(j).lt.gaplo(j)) then
			   a1=gaphi(j)		!swap
			   gaphi(j)=gaplo(j)
			   gaplo(j)=a1
	      	endif
	            if(discprt) write(8,131) j,gaplo(j),gaphi(j)
131			format(/,' SET ',i3,/,
     & ' Open period excluded if shut time on BOTH sides is',/,
     &    '  between ',g13.6,' and ',g13.6,' ms')
		   else if(iexcop(j).eq.3) then
			if(.not.burst(j)) then
			   call BELL(1)
			   print 132,j
132			   format(' No tcrit defined for set ',i3)
			   goto 133
			else
			   gaplo(j)=tcrit(j)
			   gaphi(j)=3.1536e10		!msec=1 year! -see PRANGE
	               if(discprt) write(8,134) j,gaplo(j)
134			format(/,' SET ',i3,/,
     & ' Open period excluded if shut time on BOTH sides is above',/,
     & '  tcrit = ',g13.6,' ms, so all isolated openings excluded.')
			endif
		   endif
		enddo
	   endif
	endif
c
c Convert the data to alternating open PERIODS and shut times
	if((.not.nodata).and.(.not.autosim)) then
	   do jset=1,nset
		call GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2)
	   enddo
	endif
c
cc SPECIFY THE MODEL TO BE FITTED -now moved to start!
c
330	continue
c Now check ligand conc
cc Note GETQD now returns QT without conc (or diagonals)
c NB nlig is set in model (can't change here because conc dep rates would
c be set wrongly)
	if(readini) then
c	   print 300,nlig
c300	   format(' Number of ligands (same for all sets) [',i2,'] = ')
c	   call INPUTi(nlig)
	   do j=1,nset
		if(imod0.eq.imodsav) then
		   do i=1,nlig
			conc(i,j)=conc1(i,j)	!value from .ini
		   enddo
		endif
		print 741,j
741		format(/,' SET ',i3)
	      do i=1,nlig
		   xA=conc(i,j)*1.e6
		   print 74,ligname(i),xA
74		   format(
     &  	  ': Concentration of ',a10,' (micromolar) [',g13.6,'] = ')
		   call INPUTr(xA)
		   conc(i,j)=xA*1.e-6
		enddo
	   enddo	!end of j=1,nset
	else if(.not.readini) then
	   do j=1,nset
	      print 741,j
c741		format(' Set ',i3)
	      do i=1,nlig
		   print 76,ligname(i)
76		   format(
     &    '    Concentration of ',a10,' (micromolar) = ')
		   call INPUTr(xA)
		   conc(i,j)=xA*1.e-6
	      enddo
	   enddo
	endif
c
c
c In case of nodata, can jump back to here to repeat with different
c conc for set 1
993	continue

c Print the concentrations
      if(discprt) write(8,138)
c138  format(
c     &' ------------------------------------------------------------')
	do j=1,nset

	   if(discprt) write(8,741) j
c741	   format(' Set ',i3)
	   do i=1,nlig
		if(discprt) write(8,761) ligname(i),conc(i,j)*1.e6
761		format(
     &    '  Concentration of ',a10,' (micromolar) = ',g13.6)
	   enddo
	enddo
      if(discprt) write(8,138)
c
c Concentrations for each data set have been defined already (and stored in
c conc array) so no need to ask for conc here
	do i=1,ilast
	   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
67	   format(4x,35a2)
	enddo
      if(discprt) write(8,2431)ka,kb,kc,kd
2431	format(
     & ' No of states in each subset: kA,kB,kC,kD= ',4i3)
c	call PRTMOD(il)
c Get initial guesses (in QWIND3, or read as QT from Qgen2?- must do former
c because IQ(), which defines which q(i,j) corresponds to which titlep(m), and
c to which theta0(m), is defined in QWIND3 now.
c
c NOW use theta0(m) to hold ALL parameters, but theta(m) to hold fitted param
c 	The array IQ defines correspondence between param # (index of TITLEP and
c THETA(0),and elements of Q: IQ(i,j)=m where theta0(m) is the parameter that goes
c in QT(i,j),(though may be multiplied by a statistical factor in QT),
c ie theta0(IQ(i,j))=Q(i,j)
c
	if(debug()) then
	   print 34
34	   FORMAT(' Type IQ matrix [N]? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).EQ.'Y') call ATYPI(IQ,'  IQ =  ',k,k,km,km)
	endif
c Calculate arrays irate(m),jrate(m) to give inverse of IQ(i,j) (faster than using
c GETIJ call) Q[irate(m),jrate(m)]=theta0(m) for array of ALL parameters (for fitting
c theta(m) is fixed to contain only the parameters to be estimated -see below)
c NB irate(), jrate() now output from getqd so no need to calculate here
c	do 14 m=1,npar
c	 do 13 i=1,k
c	 do 13 j=1,k
c	   if(IQ(i,j).eq.m) then
c		irate(m)=i
c		jrate(m)=j
c		goto 14
c	   endif
c13	 continue
c	 call BELL(1)
c	 print 15,m
c15	 format(
c     & ' ERROR : parameter # ',i3,' not found in Q matrix')
c14	continue
c
c Print rates as aid to setting constraints (not to disc)
	print 130
c      if(discprt) write(8,130)
130	format(/,' Initial values of rate constants')
	print 33
33	FORMAT(' Type Q matrix [N]? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'N') then
	   if(nlig.gt.0) then
      	print 332
       	if(discprt) write(8,332)
332		format(' Q matrix without concentrations:')
		call ATYPD(QT,'  QT    ',k,k,km,km)
	   endif
	   ans='N'
	   call DCASK('Type Q matrix for each data set',ans,ans)
c	   print 333
c333	   format(' Type Q matrix for each data set [N] ? ')
c	   ans='N'
c	   call INPUTa(ans)
c  now use QT and incorporate conc (for set #jset) and micro rev
c (but not constraints) by call to QNEW_HJC (like \calc\qnewc, except
c that it sets micro rev too) (NB qset_hjc defines QD from theta, after fit)
	   if(UC(ans).eq.'Y') then
		do j=1,nset
		   call QNEW_HJC(QT,j,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
      	   print 3351,j
	         if(discprt) write(8,3351) j
3351		   format(/,' Set ',i3,': Q matrix at concentration(s) ')
		   do i=1,nlig
      		print 335,conc(i,j)*1.e6,ligname(i)
			if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
335			format(3x,g13.6,' micromolar of ',a20)
		   enddo
	         call ATYPD(QD,'  QD    ',k,k,km,km)
		enddo
	   endif
	endif
c
c Allocate QD elements to theta0(), using IQ (which has been reordered if
c states were reordered in MPERM)
      print 108
c	if(discprt) write(8,108)
108	format('/')
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   theta0(m)=QT(i,j)
	   thsav(m)=theta0(m)
         print 121,m,i,j,titlep(m),theta0(m)
c	   if(discprt) write(8,121) m,i,j,titlep(m),theta0(m)
121	   format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	enddo
c
	do i=1,200
	   jmic(i)=0	!zero in case mic rev parameter has been changed
	enddo
	nmr=0			!number of cycles that obey m.r. (.le.ncyc)
	if(ncyc.gt.0) then
	   do i1=1,ncyc
		i=isetmr(i1)	!actual cycle number
		if(obeymr(i).and.(.not.automr(i))) then	!if cycle #i does not obey mr (see GETREV)
		   m=IQ(im(i,1),jm(i,1))
		   jmic(m)=1		!fixed by micro rev
		   nmr=nmr+1
		endif
	   enddo
	endif
c
c Now allocate all arrays used for calcs -neede as args for hjclik,
c hjcdisp, simphjc etc
c Set size of allocatable arrays now model specified (make a bit bigger??)
	kAm=kA
	kFm=kF
	if(.not.allocated(Z00A)) then
	   ALLOCATE(Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km))
	   ALLOCATE(Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km))
	   ALLOCATE(XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm),
     &     QEXPQA(kFm,kAM),QEXPQF(kAm,kFm))
	endif
c
c To calculate likelihood surface here and bypass the rest
c This assumes that all rate constants except alpha2, beta2 are fixed
c at their ML values, and log(lik) calculate for range of alpha2, beta2
c NB fixed values already in QT so no need to set nfix, jfix which
c are not used in HJCLIK
	if(liksurf) then
	   call BELL(1)
	   print 504
c504	format(' DEFINE THE RATE CONSTANTS')
	   call GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &  nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &  theta0,jcon,autosim,thtrue,irect,irates,jrates,
     &  npars,dgammas,kflag,icon,useprim,nfix,jfix,idest)
c       In autosim case theta, theta0 and thsav refer to the fitted mech
	   do i=1,npar
		theta(i)=theta0(i)
		thsav(i)=theta0(i)
	   enddo
	   ik=0
	   do m=1,npar
	      ik=ik+1
	      i=irate(m)
	      j=jrate(m)
		irate1(ik)=i
		jrate1(ik)=j
		IQf(i,j)=m
	      theta(ik)=QT(i,j)
c 25/03/08 Fix icdep (was not defined in the liksurf case)
		icdep(ik)=0		!numbering for FITTED params
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j)) then
			icdep(ik)=1
		   endif
		enddo
	   enddo
	   print 800
	   if(discprt) write(8,800)
800	   format(/,
     & ' Calculation of log(likelihood) for range of values of',/,
     & ' two parameters with other rate constants fixed.')
	   do m=1,npar
		print 809,m,titlep(m),theta(m)
809		format(' (',i3,') ',a10,' = ',g13.6)
	   enddo
	   print 808,jalpha,jbeta
808	   format(
     &	' Specify parameter # for the rates to be varied [',
     &	i2,',',i2,'] = ')
	   call INPUT2i(jalpha,jbeta)
c
	   print 801,titlep(jalpha),theta(jalpha),titlep(jalpha)
801	   format(' Input value of ',a10,'is ',g13.6,/,
     &   '  specify low, high value for range of ',a10,' = ')
	   call INPUT2r(x1,x2)
	   alow=dble(x1)
	   ahigh=dble(x2)
	   print 802,titlep(jbeta),theta(jbeta),titlep(jbeta)
802	   format(' Input value of ',a10,' is ',g13.6,/,
     &   '  specify low, high value for range of ',a10,' = ')
	   call INPUT2r(x1,x2)
	   blow=dble(x1)
	   bhigh=dble(x2)
	   ncalc=30		!=900 values altogether
	   print 803,ncalc
803	   format(' Number of values for each [',i3,'] = ')
	   call INPUTi(ncalc)
c	Allocate arrays
	   kab=ncalc*ncalc
	   ALLOCATE(alpha2(kab),beta2(kab),aloglik(kab))
	   ans='Y'
	   call DCASK('Use log(rate)',ans,ans)
	   if(ans.eq.'Y') then
		logsurf=.true.
		da=(dlog10(ahigh)-dlog10(alow))/dfloat(ncalc-1)
		db=(dlog10(bhigh)-dlog10(blow))/dfloat(ncalc-1)
	   else
		logsurf=.false.
	      da=(ahigh-alow)/dfloat(ncalc-1)
	      db=(bhigh-blow)/dfloat(ncalc-1)
	   endif
	   first=.true.		!get openings/group etc in HJCLIK
	   oneset=.false.	!so calcs done for ALL data sets in HJCLIK
	   kf=npar		!call hjclik with kfit=all param so all copied from theta
	   do i=1,ncalc
		if(logsurf) then
		   theta(jalpha)=alow*(10d0**(dfloat(i-1)*da))
		else
		   theta(jalpha)=alow + dfloat(i-1)*da
		endif
		print 806,theta(jalpha)
806		format(
     &      ' Calculating for beta values for alpha = ',g13.6,' . . .')
		do j=1,ncalc
		   if(logsurf) then
			theta(jbeta)=blow*(10d0**(dfloat(j-1)*db))
		   else
			theta(jbeta)=blow + dfloat(j-1)*db
		   endif
   		   elmax=HJCLIK(kf,theta,
     & 		tint,ampl,iprops,nd1,nd2,
     &		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   print 8061,titlep(jalpha),theta(jalpha),
     &	    titlep(jbeta),theta(jbeta),elmax
8061		   format(1x,a10,' = ',g13.6,a10,' = ',g13.6,' LL = ',g13.6)
		enddo
	   enddo
c Now write values to disc for plot in Origin etc
c In hjclik, the current param values (not log) are written to alpha2(), beta2()
	   call TITENT0(
     &    'File name for ascii output of alpha, beta, log(lik):',
     &	ascfil,40,.false.)
	   OPEN(unit=12,file=ascfil,status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
	   write(unit=12,fmt=804) nab
804	   format(i11)
c       In logsurf case write the log rates for use in Origin
	   if(logsurf) then
		do i=1,nab
		   write(unit=12,fmt=805) alpha2(i),beta2(i),aloglik(i)
805		   format(3f15.5)
		enddo
	   else
		do i=1,nab
		   write(unit=12,fmt=805) dlog10(alpha2(i)),
     &		dlog10(beta2(i)),aloglik(i)
		enddo
	   endif
c write the values of the other npar-2 rates that are constant
	   do m=1,npar
		if(m.ne.jalpha.and.m.ne.jbeta) then
		   write(unit=12,fmt=8051) titlep(m),theta(m)
8051		   format(a10,f15.5)
		endif
	   enddo

	   CLOSE(unit=12)
	   if(discprt) then
		write(8,807) ncalc,alow,ahigh,ncalc,blow,bhigh,
     &	ncalc*ncalc,ascfil
807		format(
     &      1x,i5,' values of alpha2, in range ',g13.6,' to ',g13.6,/,
     &      1x,i5,' values of beta2,  in range ',g13.6,' to ',g13.6,/,
     &      1x,i5,' values written to ascii file ',a40,/)
		if(logsurf) then
		   write(8,8071)
8071		format(' Values logarithmically spaced')
		endif
	   endif
	   goto 99
	else if(idebug.eq.10) then
c       In this case a value of theta(jalpha) and theta(jbeta) is defined
c	  for every call to HJCLIK during fitting, so nimber not know
c 	  in advance
	   print 810,jalpha,jbeta
810	   format(/,
     &    ' Specify parameter # for the rates to saved during fit [',
     &    i2,',',i2,'] = ')
	   call INPUT2i(jalpha,jbeta)
	   kab=10000
	   if(.not.allocated(alpha2)) then
		ALLOCATE(alpha2(kab),beta2(kab),aloglik(kab))
	   endif
	   call TITENT0(
     &    'File name for ascii output of alpha, beta, log(lik):',
     &	ascfil,40,.false.)
	else
	   kab=1		!must allocate anyway
	   if(.not.allocated(alpha2)) then
		ALLOCATE(alpha2(kab),beta2(kab),aloglik(kab))
	   endif
	endif
c
	if(curvonly) then
	   do i=1,npar
		theta(i)=theta0(i)
	      irate1(i)=irate(i)	!now called irate1(i)
	      jrate1(i)=jrate(i)
	   enddo
	   kfit=npar
c check rates here when no data
         print 138
         if(discprt) write(8,138)
c138	   format(
c     &'  ------------------------------------------------------------')
	   call BELL(1)
	   print 504
c504	format(' DEFINE THE RATE CONSTANTS')
	   call GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &  nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &  theta0,jcon,autosim,thtrue,irect,irates,jrates,
     &  npars,dgammas,kflag,icon,useprim,nfix,jfix,idest)
c       In autosim case theta, theta0 and thsav refer to the fitted mech
	   do i=1,npar
		theta(i)=theta0(i)
		thsav(i)=theta0(i)
	   enddo
c 06/06/04 08:18pm Fix icdep (was not defined in the curvonly case)
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		irate1(m)=i
		jrate1(m)=j
		icdep(m)=0		!numbering for FITTED params
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j)) then
			icdep(m)=1
		   endif
		enddo
c		IQf(i,j)=m
c		theta(i)=QT(i,j)
c		titpfree(i)=titlep(m)		!title for free param
	   enddo
c
	   goto 991		!save ini?
	endif
c
c Carry on with normal fitting
c	if(readini.and.imod0.eq.imodold) then
c Move constraints to a subroutine so they can be set before MR (in getqd1)
c when PRIM3 is used for MR
	if(.not.useprim) then
	   call SETCONS(readini,imod0,imodsav,titlep,IQ,jcon,
     &	theta0,obeymr,.true.,npar,useprim,QT,km)
c Used fixed EC50 as constraint?
	   call SETEC50(readini,imod0,dcmod,imodsav,nlig,
     &	conc,titlep,IQ)
         print 138
         if(discprt) write(8,138)
c138     format(
c     &' ------------------------------------------------------------')
         print 108
	   call BELL(1)
	   ans='N'
	   call DCASK(' Correct for very fast channel block',ans,ans)
	   if(ans.eq.'Y') then
		fastblk=.true.
		call SETFBLK(npar,titlep,nlig,ligname)	!all the rest in commons
	   else
		fastblk=.false.
	   endif
	endif
c
c
c Parameter fixing now has to be done after getqd2 or value overwritten
c
c
c Check maximum value for any assoc rate constant
	call BELL(1)
	xs=sngl(assmax)
	print 502,xs
502	format(/,
     &' Value for any fitted association rate constant to be',/,
     &'  constrained to be not more than [',g13.6,'] (1/molar*sec) = ')
	call INPUTr(xs)
	assmax=dble(xs)
	if(discprt) write(8,503) assmax
503	format(/,
     &' Value for any association rate constant constrained ',/,
     &' to be not more than ',g13.6,' (1/molar*sec)',/)
c
c Check maximum value for any other rate constant
	xs=sngl(ratemax)
	print 514,xs
514	format(/,
     &' Value for any other fitted rate constant to be',/,
     &'  constrained to be not more than [',g13.6,'] (1/sec) = ')
	call INPUTr(xs)
	ratemax=dble(xs)
	if(discprt) write(8,515) ratemax
515	format(/,
     &' Value for any other rate constant constrained ',/,
     &' to be not more than ',g13.6,' (1/sec)',/)
c
c Print out micro rev, constrained and fixed values now
	if(ncyc.gt.0) then
	do 139 L1=1,ncyc
	 L=isetmr(L1)	!actual cycle number
	 if(discprt.and.obeymr(L)) then
	   write(8,164)L
164	   format(/,' Cycle # ',i3)
	   if(automr(L)) then
	    write(8,1651)(IM(L,M),JM(L,M),m=1,nsc(L))
1651	    format(' ',2(5(2i3,4x)),'(automatically obeys micro rev)',/)
	   else
	    write(8,165)im(L,1),jm(L,1)
165	    format(
     &  ' q(',i2,',',i2,')  calculated by microscopic reversibility')
	    write(8,166)(IM(L,M),JM(L,M),m=2,nsc(L))
166	    format('  rest of cycle: ',2(5(2i3,4x),/))
	   endif
	 endif
139	 continue
	endif
c
	neqsav=neq	!for debug -neq sometimes seems to get reset to zero?
	if(neq.gt.0) then
	  if(discprt) write(8,324)
324	  format(/,' The following parameters are constrained:')
	  do n=1,neq
		i=ie(n)
		j=je(n)
		m=IQ(i,j)
		i1=if(n)
		j1=jf(n)
		m1=IQ(i1,j1)
		if(efac(n).gt.0.) then
       	   if(discprt) write(8,321) m,i,j,titlep(m),efac(n),
     &	     m1,i1,j1,titlep(m1),theta0(m1)
321	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		else
       	   if(discprt) write(8,322) m,i,j,titlep(m),-efac(n),
     &	     m1,i1,j1,titlep(m1),theta0(m1)
322	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' minus rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		endif
	   enddo
	else
       if(discprt) write(8,162)
162	 format(' No parameters constrained')
	endif
c
	if(fixec50) then
	   do ifix=1,nfixec50
		if(ifix.eq.1) then
		   n=nlvar
		   i=i50
		   j=j50
		   m=m50
		   ec=ec501
		else
		   n=nlvar2
		   i=i502
		   j=j502
		   m=m502
		   ec=ec502
		endif
	      if(discprt) write(8,26) m,i,j,titlep(m),ligname(n),ec*1.d6,
     &	penfac
26		format(/,' Rate ',
     & i3,3x,' q(',i2,',',i2,') =',1x,a10,' is constrained to give',/,
     & '  EC50 for ',a10,' = ',g16.6,' micromolar,',/,
     & ' using penalty factor = ',g13.6,' if needed')
		if(nlig.gt.1) then
		   if(discprt) write(8,261)
261		   format(
     &	'    Concentration of other ligands for EC50 determination')
		   do i=1,nlig
			if(i.ne.n) then
			   if(ifix.eq.1) then
				x=1.e6*conc_ec1(i)
			   else if(ifix.eq.2) then
				x=1.e6*conc_ec2(i)
			   endif
			   if(discprt) write(8,325) ligname(i)(1:10),x
325			   format(
     &'    Conc of ',a10,' at which EC50 was determined (micromolar) = '
     & ,f9.4)
			endif
		   enddo
		endif
	  enddo		!ifix=1,nfixec50
	endif
c
c
c Confirm and print guesses (and true rates, if autosim)
c  now that all constraints defined
591	continue
      print 138
      if(discprt) write(8,138)
c138	format(
c     &' ------------------------------------------------------------')
	call BELL(1)
	print 504
504	format(' DEFINE THE RATE CONSTANTS',/)
	call GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &  nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &  theta0,jcon,autosim,thtrue,irect,irates,jrates,
     &  npars,dgammas,kflag,icon,useprim,nfix,jfix,idest)
      if(idest.eq.59) goto 59 	!try a different rate file
c
c Fix some parameters?
c Can now decide on fixed values
	if(.not.useprim) then
	   call SETFIX(readini,imod0,imodsav,npar,theta0,titlep,
     &	 QT,IQ,obeymr,nfix,jfix,km,useprim)
	endif
c
	do m=1,npar
	   thsav(m)=theta0(m)  !re-save initial guesses in case altered in getqd2
	enddo
c
c
c NUMBER OF PARAMS TO ESTIMATE=kfit, kept in theta(1),...,theta(kfit)
c Now put parameters to be estimated, kfit in number, into theta(), and
c record the corresponding values of q(i,j) in IQf(i,j), and in irate1(m),jrate1(m)
c exclude fixed, constrained params, and those calc by micro rev
c Also define here icdep(i)=0 if theta(i) is not an assoc rate, =1 if it is
c an assoc rate (in common/amax so max assoc rate can be constrained in hjclik)
c=	kfit=npar-nfix-neq-ncyc
c Next bit for debug
	if(neq.ne.neqsav) then
	   print 77,neq,neqsav
77	   format(' Problem: neq has been reset -please tell DC.',/,
     &   '	neq = ',i3,';  neqsav = ',i3)
	   neq=neqsav
	   pause
	endif
c
	kfit=npar-nfix-neq-nmr	!nmr=# of cycles constrained by mr (.le.ncyc)
	if(fixec50) kfit=kfit-nfixec50
	ik=0		!use to check vs kfit
	do m=1,npar
	 if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
	   if((.not.(fixec50.and.m.eq.m50)).and.
     &	  (.not.(nfixec50.eq.2.and.m.eq.m502))) then
		ik=ik+1
		i=irate(m)
		j=jrate(m)
		irate1(ik)=i
		jrate1(ik)=j
		icdep(ik)=0		!numbering for FITTED params
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j)) then
			icdep(ik)=1
		   endif
		enddo
		IQf(i,j)=m
		theta(ik)=QT(i,j)
		titpfree(ik)=titlep(m)		!title for free param
	   endif
	 endif
	enddo
	iptit=0	!for dspout
c
c	call BELL(2)
	print 135,npar,nfix,neq,nmr,nfixec50,kfit
	if(discprt) write(8,135) npar,nfix,neq,nmr,nfixec50,kfit
135	format(
     & '  Total number of rates = ',i6,/,
     & '    Number that are fixed       = ',i6,/,
     & '    Number that are constrained = ',i6,/,
     & '    Number set by micro rev     = ',i6,/,
     & '    Number set by fixed EC50    = ',i6,/,
     & '  Number of free rates to be estimated = ',i6,/)
	if(kfit.ne.ik) then
	   call BELL(2)
	   print 122,ik
	   if(discprt) write(8,122) ik
122	   format(' Number of free parameters = ',i6,/,
     & ' ERROR: too many parameters constrained or fixed? ',/)
	endif
c
991	continue	!jump here if nodata
	call DCASK('Write details to hjcfit.ini before fitting','N',ans)
c Transparent write shows new .ini is 56459 bytes, so keep as fixed
c record length of 60k=61440 bytes. BUT record length > 32k gives
c error, so easiest to use transparent access, but when writing
c .ini, pad it out to 61440 bytes, so if other things added later
c don't get 'read past end of record'.
      if(ans.eq.'Y') then
	  call TITENT0(
     &    'Name for hjcfit.ini file:',inifile,40,.false.)
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   do i=1,ncyc
		if(.not.obeymr(i)) then
		   nsc(i)=-iabs(nsc(i))
		endif
		if(.not.automr(i)) then
		   im(i,1)=-iabs(im(i,1))
		endif
	   enddo
	   n=0
	   do i=1,ncyc
		n=n+nsc(i)
	   enddo
	  if(autosim) then		!restore tresol
		do j=1,nset
		   tresol(j)=1.e3*tresol(j) !keep (possibly altered) res in mus for .ini
		enddo
	  endif
c Subtract space taken by nsc(),im,jm,assmax,qfilem,nsim,irect,logsav,imods,
c badend,iexcop,gaplo,gaphi,dcmodel
	   dcmod=dcmodel
	   npad=4941-4-4*ncyc-8*n-8-40-4-4-4-4-4*40-4
c and for
c slopsch,checkgrp,rcrit,ngpcheck,nstab1,gfac1,nstab2,gfac2,ans5
c     4       4      8     4        4      8      4      8    1 = 45 bytes
	   npad=npad - 45
c and for  nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2 =3*8+4*4 =40 bytes
	   npad=npad - 40
c  npad=4700 before isetmr added (for SS model)
	   npad=npad-4		!for useprim
	   npad=npad-4*ncyc	!for isetmr
	   npad=npad-4		!for ratemax
	   npad=npad-4		!for nmr
	   npad=npad-4		!for nlig
	   npad=npad-2*4*nlig	!for conc_ec1,conc_ec2
	   if(npad.lt.0) then
		call BELL(3)
		print 78
78		format(' PROBLEM -run out of space in scan.ini')
		STOP
	   endif
	   allocate(inipad(npad))
	   do i=1,npad
		inipad(i)=0
	   enddo
         OPEN(unit=19,file=inifile,status='UNKNOWN',
     &     access='TRANSPARENT')
c     &    access='DIRECT',form='UNFORMATTED',recl=61440)
c     &    access='DIRECT',form='UNFORMATTED',recl=20480)
	   write(19,rec=1) pfile,nfix,jfix,neq,IE,JE,EFAC,
     & IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     & nfile,kfile,nset,conc,tcrit,burst,irecq,
     & idatyp,qfile,imod0,setbad,tcbad,onechan,
     & nbad,isbad,iebad,tresol,ans3,ans4,nlvar,
     & fixec50,ec501,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     & chsvec,ncyc,
     & (nsc(i),i=1,ncyc),
     & ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     & ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     & assmax,qfilem,nsim,irect,logsav,imods,
     & badend,iexcop,gaplo,gaphi,dcmod,
     & slopsch,checkgrp,rcrit,ngpcheck,
     & nstab1,gfac1,nstab2,gfac2,ans5,
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     & (conc_ec1(i),i=1,nlig),
     & (conc_ec2(i),i=1,nlig),
     & inipad
	   CLOSE(unit=19)
	   deallocate(inipad)
	   do i=1,ncyc	     !reset nsc
		nsc(i)=iabs(nsc(i))
		im(i,1)=iabs(im(i,1))
	   enddo
	  if(autosim) then		!restore tresol
		do j=1,nset
		   tresol(j)=1.e-3*tresol(j) !keep (possibly altered) res in mus for .ini
		enddo
	  endif
	endif
c
c Now generate simulated data
c For data simulated within HJCFIT, generate the simulated data file here,
c just before simplex -followed by silent call to REShjc2 to impose resolution
c Det nsims=1 above now -may want to start at > 1 if run restarted
c	nsims=1	!number of simulated expts fitted =1,2,...,nsim
500	continue	!jump back here for repeated simulations
	if(autosim) then
c       Generate simulated tint() with zero resolution
c	   if(nsims.eq.1) then
		if(discprt) write (8,138)
c138	      format(
c     &' ------------------------------------------------------------')
		if(discprt) write (8,136)
136		format(' SIMULATION OF DATA')
c	   endif
	   ix1sav=ix1	!keep random seeds at start of run
	   iy1sav=iy1
	   iz1sav=iz1
c	   call HJCSIM2(thtrue,tint0,ampl0,iprops0,conc,nintt,
c     &   irate,jrate,npar,nmax,nset,dgamma,nsims,k)
c NB call must have values for mechanism used for simulation
	   call HJCSIM2(thtrue,tint0,ampl0,iprops0,conc,nintt,
     &    irates,jrates,npars,nmax,nset,dgammas,nsims,ks)
c       and impose resolution for simulated tint() (treso, tresg, in ms,
c       are already set by call to reshc1 above)
	   do j=1,nset	!do for each set
		avamp1=avamp(j)	!in common/rblck/
		treso=tresol(j)	!still in ms
		tresg=treso
 		call REShjc2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),
     &	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),
     &	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),
     &	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,
     &	 sim,sres,sexp,excamp,alo,ahi,treso,tresg,acrit,avamp(j))
c
		tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	   enddo
	endif		!end of autosim section
c
c Get likelihood for initial guesses eg to determine scale factor
c First call, with first=true, must be done also if NODATA (skips out via
c nodata in common after calculating distributions -no likelihood returned)
c
	first=.true.		!get openings/group etc in HJCLIK
	oneset=.false.	!so calcs done for ALL data sets in HJCLIK
c	if(idsav.eq.8.or.idsav.eq.9) idebug=idsav			!switch debug on
c==	if(.not.nodata) then
	   if(.not.autosim) then
      	print 112
      	if(discprt) write(8,112)
112		format(/,
     &	 ' For initial guesses: ')
	   endif
c	   prtec50=.true.		!print initial ec50 (whether fixec50 or not)
	   prtec50=.false.		!print initial ec50 (whether fixec50 or not)
c All ec50 printing now done in PRNTEC50, so prtec50, for qset_hjc, is
c  now superfluous
   	   elmax=HJCLIK(kfit,theta,
     &   tint,ampl,iprops,nd1,nd2,
     &   Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &   XAF,XFA,QexpQA,QexpQF,
     &   alpha2,beta2,aloglik,kab,
     &   kAm,kFm,km)
c========leave prtec50=true====temp=======
c	   prtec50=.false.
         print 138
         if(discprt) write(8,138)
	   call BELL(1)
c138	   format(
c        &' ------------------------------------------------------------')
	   print 1420
         if(discprt) write(8,1420)
1420	   format(/,' EQUILIBRIUM CALCULATIONS FOR INITIAL GUESSES')
	   call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
c
	   if(autosim.and.nsims.gt.1.and.(.not.nodata)) then	!skip printout
		if(restart) then
		   restart=.false.	!get stpfac etc again; reset restart
		else
		   goto 501
		endif
	   endif
c
	if(curvonly) goto 990
c
c Only the param to be estimated are in theta(), so use kfit in call, and
c call with JFIX1 set to zeros, so none of the kfit param are fixed
c	   if(idsav.eq.8.or.idsav.eq.9) idebug=1			!switch debug off
         print 1121,-elmax
         if(discprt) write(8,1121) -elmax
1121	   format(' log(likelihood) = ',g14.7)
	   do j=1,nset
      	print 12,j,fcomp(j),ngp(j),an(j),nscal(j)
      	if(discprt) write(8,12) j,fcomp(j),ngp(j),an(j),nscal(j)
12	   	format(' Set ',i3,': likelihood = ',g14.7,/,
     &	 1x,i4,' groups: mean no of openings/group = ',g13.6,/,
     &	 ' (likelihood scaled down ',i4,' times by 1.e-100)')
	   enddo
         print 113
         if(discprt) write(8,113)
113	   format(/,
     &'  Note: if a particular set of parameter values causes the',/,
     &' likelihood for a group to appear <0, the log(lik) for this',/,
     &' group is taken as zero, thus penalising these parameters',/)
c=	endif
c
	if(curvonly) goto 990
c
	print 1351,kfit,(titpfree(i)(1:8),theta(i),i=1,kfit)
	if(discprt) write(8,1351)kfit,(titpfree(i)(1:8),theta(i),i=1,kfit)
1351	format(/,' Fit ',i3,' parameters: initial free parameters = ',
     & 25(/,4(1x,a8,'=',g10.3)))
c
	if(nofit) goto 990
      print 108
	call DCASK('Skip fit and plot initial parameters',ans1,ans)
	ans1=ans		!new default
	if(ans1.eq.'Y') then
	   nofit=.true.
	   GOTO 990
	endif
c Set other inputs for Simplex
511	continue			!jump to here if autosim
	errfac=1.d-3
	delmin=-1.d0		!do not use delmin for convergence
c	if(defolt) goto 526
	print 525
525	format(/,
     & ' Settings for Simplex.',/,
     & '   Initial step size factor is usually fac=0.1 to 0.5',/,
     & '     (initial step = factor * guess), or, for log(rate)',/,
     & '     step in log rate = log(fac) where fac=1.1 to 10',/,
     & '   Contraction factor is usually 0.4 to 0.8',/)
c
c	ans='N'
c	if(logsav) ans='Y'
	ans='Y'
	call DCASK('Use log(rate constant) in Simplex',ans,ans)
	logsav=ans.eq.'Y'
c
c	confac=0.8d0		!parameter for simplex3 -for bad guesses
c	stpfac=0.5d0		!better for bad guesses?
	confac=0.5d0		!parameter for simplex3 -for bad guesses
	stpfac=0.2d0		!better for bad guesses?
	if(logsav) stpfac=5.d0
	resfac=10.0d0		!better for bad guesses?

	x=sngl(stpfac)
1403	print 140,x
140	format(' Initial step size factor [',f5.1,'] = ')
	call INPUTr(x)
	if(logsav.and.x.lt.1.05.or.x.gt.20.) then
	   call  BELL(1)
	   print 1401,x
1401	   format(' factor should be between 1.01 and 20',/)
	   x=2.0
	   goto 1403
	else if((.not.logsav).and.(x.lt.0.05.or.x.gt.0.9)) then
	   call  BELL(1)
	   print 1402,x
1402	   format(' factor should be between 0.05 and 0.95',/)
	   x=0.5
	   goto 1403
	endif
	stpfac=dble(x)
	if(logsav) then
	   stpsav=dlog(stpfac)
	endif
c
	print 16,confac
16	format(' Simplex contraction factor (0-1) [',f5.1,'] = ')
	x=sngl(confac)
	call INPUTr(x)
	confac=dble(x)
	print 17,resfac
17	format(' Restart step size=resfac*critstep: resfac [',f5.1,'] = ')
	x=sngl(resfac)
	call INPUTr(x)
	resfac=dble(x)
c
	irestrt=3
	print 312,irestrt
312	FORMAT(' Limit number of restarts to [',i2,']: n = ')
	call INPUTi(irestrt)
c
	print 523,errfac,ndisp
523	FORMAT(' ERROR=guess*',g13.6,/,
     & ' Print every Nth estimate [',i3,'] :  N= ')
	call INPUTi(i)
	if(i.ge.1) NDISP=i
c
	x=sngl(errfac)
	print 68,x
68	format(
     & ' Relative error [',f8.5,'] = ')
	call INPUTr(x)
	if(x.gt.1.e-10) errfac=dble(x)
c Choose convergence criterion -not necessary, iconv=1 seems OK
	iconv=1
	iconvsav=1
c	if(logsav) then
cc	  omit iconv=3 (though in simphjc) unless some rationalisation found for it
c	   print 42,iconv
c42	   format(
c     &  ' Select convergence criterion:',/,
c     &  ' (1) convergence judged by log(rates)',/,
c     &  ' (2) convergence judged by usual error fac*guess',/,
c     &  ' Option number [',i2,'] =  ')
c	   call INPUTi(iconv)
c	   iconvsav=iconv		!for .ini and restore
c	endif
c
	if(.not.logsav) then
	   print 120,stpfac,confac,resfac,errfac
         if(discprt) write(8,120) stpfac,confac,resfac,errfac
120	   format(/,' For simplex fit of rate constants',/,
     &  ' stpfac = ',f6.2,'    confac = ',f6.2,' resfac = ',f6.2,/,
     &  ' error = ',g13.6,' * guess')
	else
	   print 1201,stpfac,confac,resfac,errfac,iconv
         if(discprt) write(8,1201) stpfac,confac,resfac,errfac,iconv
1201	   format(/,' For simplex fit of LOG(RATE CONSTANTS)',/,
     &  ' stpfac = ',f6.2,'    confac = ',f6.2,' resfac = ',f6.2,/,
     &  ' error factor = ',g13.6,'; convergence criterion = ',i3)
	endif
c	pause 'Before Simplex'
c
501	continue	!skip to here for autosim
	nevm=-20000	!neg so iterations not printed to disc
c	nevm=20000	!iterations -> disk
	if(debug()) then
	   call DCASK('Print iterations to disk file',ans,ans)
	   if(ans.eq.'Y') then
		nevm=20000	!iterations -> disk
	   else
		nevm=-20000	!neg so iterations not printed to disc
	   endif
	endif
	abort=.false.
	logfit=logsav 	!logfit false except when hjclik called from simplex
	if(logfit) then
	   stpfac=stpsav
	   iconv=iconvsav		!restore
	   do i=1,kfit
		theta(i)=dlog(theta(i))
	   enddo
	endif
	if(autosim) then
	   dpsav=discprt
	   if(nevm.lt.0) discprt=.false.	!switch off during fit
	   print 512,nsims,ix1sav,iy1sav,iz1sav
	   if(discprt) write(8,512) nsims,ix1sav,iy1sav,iz1sav
512	   format(/,' STARTING SIMULATION # ',i7,' (seeds = ',3i8,')')
	   call WAIT(300)		!wait 3 seconds
	endif
	first=.true.
	call TIME(ctime)
	print 61,ctime
	if(discprt) write(8,61) ctime
61	format(/,' Simplex started at ',a11)
c
c NB all param now real*8 in simphjc
	call SIMPHJC(kfit,THETA,stpfac,errfac,nev,nevm,
     & smin,HJCLIK,Ndisp,jfix1,delmin,confac,irestrt,resfac,iconv,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & autosim,nsims,
     & kAm,kFm,km)
c
	if(logfit) then
	   do i=1,kfit
		theta(i)=dexp(theta(i))	!non-log from here on
	   enddo
	   logfit=.false.		!so later calls to hjclik don't expect log rates
	endif
	call TIME(ctime)
      print 5233,ctime
      if(discprt) write(8,5233) ctime
5233	FORMAT(/,' End of fitting: ',a11)
c
	if(autosim) discprt=dpsav	!switch back on

	endsim=.false.
	call BELL(5)
	if(abort.and.autosim.and.nsims.lt.nsim) then
	   print 509,nsims-1
509	   format(2x,i7,' simulations completed',/,
     &	' End the simulations now [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   endsim=ans.eq.'Y'
	endif
c
c For idebug=10, now save the alpha, beta values
	if(idebug.eq.10) then
	   OPEN(unit=12,file=ascfil,status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
	   write(unit=12,fmt=804) nab
c804	   format(i11)
	   do i=1,nab
	      write(unit=12,fmt=805) alpha2(i),beta2(i),aloglik(i)
c805		format(3f15.5)
	   enddo
	   CLOSE(unit=12)
	   if(discprt) then
		print 814, nab,ascfil
		write(8,814) nab,ascfil
814		format(
     &      1x,i5,' values of alpha2, beta2 and log(lik) written',/,
     &      ' to ascii file ',a40,/)
	   endif
	endif
c
c Print results
	print 24,nev,(titpfree(i)(1:8),theta(i),i=1,kfit)
	if(discprt) write(8,24) nev,
     &	(titpfree(i)(1:8),theta(i),i=1,kfit)
24	format(/,' Number of evaluations = ',i8,' final estimates = ',
     & 25(/,4(1x,a8,'=',g10.3)))
c
c Note that final parameter estimates are in THETA(); values in it may have
c been modified in Simplex since the last calculation of QD (e.g. to output
c best vertex, or abs min). Therefore now remake QT and QD from theta
c (use conc for jset=1, arbitrarily)
c
990	CONTINUE		!JUMP HERE TO SKIP FIT
	jset=1
	call QSET_HJC(jset,theta,QT,QD,kfit,k)	!QT,QD no longer in common/qblk
	if(autosim) then	!set theta0
	   jset=1
	   imode=1	 !so sets mr in QT and outputs theta0 with mr set
	   icons=0				!apply constraints (if any)
	   if(icon.eq.3) icons=1	!don't apply constraints to fitted rates
	   call QSET_TRU(QT,theta0,jset,conc,npar,irate,jrate,IQ,
     &	 imode,icons,k)
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
	   enddo
	   elmax=-smin
c===	   if(nsims.gt.1) goto 508  !and then skip print (or, for now, get printout of final params?)
	   if(nsims.ge.1) goto 508  !and then skip print (or, for now, get printout of final params?)
	endif
c
	call ATYPD(QT,'  QT    ',k,k,km,km)
	ans='N'
	if(fastblk) ans='Y'
	call DCASK('Type Q matrix for each data set',ans,ans)
c	print 333
cc333	   format(' Type Q matrix for each data set [N] ? ')
c	ans='N'
c	call INPUTa(ans)
	if(UC(ans).eq.'Y') then
	   do j=1,nset
		call QNEW_HJC(QT,j,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
      	print 3351,j
	      if(discprt) write(8,3351) j
c3351		format(/,' Set ',i3,': Q matrix at concentration(s) ')
		do i=1,nlig
      	   print 335,conc(i,j)*1.e6,ligname(i)
		   if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
c335		   format(3x,g13.6,' micromolar of ',a20)
		enddo
	      call ATYPD(QD,'  QD    ',k,k,km,km)
	   enddo
	endif
c
c
c Allocate QD elements to theta0(), using IQ
	print 137
      if(discprt) write(8,137)
137	format(/,' FINAL VALUES OF RATE CONSTANTS')
	do j=1,nset
	   tres1=1.d6*tresd(j)
	   if(nodata) then
		burst(j)=.false.
		chsvec(j)=.false.
	   endif
         print 340,j
	   if(discprt) write(8,340) j
340	   format(/,' Set ',i3)
	   if(burst(j)) then
		do i=1,nlig
		   print 335,conc(i,j)*1.e6,ligname(i)
		   if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
c335		   format(3x,g13.6,' micromolar of ',a20)
		enddo
		if(chsvec(j)) then
		   print 352,tcrit(j),tres1
      	   if(discprt) write(8,352) tcrit(j),tres1
352		   format(
     &	  '    Analysed in bursts, tcrit (ms) = ',g13.6,/,
     &        '    CHS vector used for start and end of bursts',/,
     &	  '    Resolution (microsec) = ',g13.6)
		else
		   print 3521,tcrit(j),tres1
      	   if(discprt) write(8,3521) tcrit(j),tres1
3521		   format(
     &     '    Analysed in bursts, tcrit (ms) = ',g13.6,/,
     &     '    Steady state vector used for start and end of bursts',/,
     &     '    Resolution (microsec) = ',g13.6)
		endif
	   else
		do i=1,nlig
		   print 335,conc(i,j)*1.e6,ligname(i)
		   if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
c335		   format(3x,g13.6,' micromolar of ',a20)
		enddo
		print 341,tres1
      	if(discprt) write(8,341) tres1
341		format(
     &	  '    Analysed as if from one channel only',/,
     &	  '    Resolution (microsec) = ',g13.6)
	   endif
	   if(badend(j)) then
		print 4321
 		if(discprt) write(8,4321)
c4321		format(/,
c     &' A bad gap ends a group, but does not eliminate the whole group')
	   else
		print 4322
 		if(discprt) write(8,4322)
c4322		format(/,
c     &' A bad gap results in elimination of the whole group in which'
c     & 	' it occurs')
	   endif
	   if(logsav) then
		print 3411
      	if(discprt) write(8,3411)
3411		format(' Simplex used log(rate constant) for searching')
	   endif
	   if(setbad(1,j)) then
		print 342,ncbad(1,j),tcbad(1,j)
      	if(discprt) write(8,342) ncbad(1,j),tcbad(1,j)
342		format('      ',i5,
     &	  ' shuttings longer than ',g13.6,' ms set bad')
	   endif
	   if(setbad(2,j)) then
		print 343,ncbad(2,j),tcbad(2,j)
      	if(discprt) write(8,343) ncbad(2,j),tcbad(2,j)
343		format('      ',i5,
     &	  ' openings longer than ',g13.6,' ms set bad')
	   endif
	   if(iexcop(j).eq.1) then
		print 1311,j,nskip(j),gaplo(j),gaphi(j)
		if(discprt) write(8,1311) j,nskip(j),gaplo(j),gaphi(j)
1311	      format(/,' SET ',i3,/,
     &   1x,i5,' open periods excluded because shut time on BOTH sides',
     &	/,'  is between ',g13.6,' and ',g13.6,' ms')
	   else if(iexcop(j).eq.2) then
		print 1312,j,nskip(j)
		if(discprt) write(8,1312) j,nskip(j)
1312	      format(/,' SET ',i3,/,
     &   1x,i5,' open periods excluded because they are in group with',
     &	/,' only one open period')
	   endif
	enddo
c
c EC50 now printed AFTER the rate constants
	kdim=100
c
	if(.not.autosim) then
         print 1371
         if(discprt) write(8,1371)
1371	   format(/,23x,'      initial        final')
	else
         print 1372
         if(discprt) write(8,1372)
1372	   format(/,27x,' true        initial       final')
	endif
	do m=1,npar
	 i=irate(m)
	 j=jrate(m)
	 theta0(m)=QT(i,j)
	 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
         print 1210,m,i,j,titlep(m),thsav(m),theta0(m)
         if(discprt) write(8,1210) m,i,j,titlep(m),thsav(m),theta0(m)
1210	 format(1x,i4,1x,' q(',i2,',',i2,') = ',1x,a10,2g13.6)
	 else
c NB do not want desens parameters to be printed for DC mech 39 when
c  fitting of simulated results done with mech 29
c This is fixed here only
c for the case where mod=39 for simulation, and mod=29 for fit
c In this case the two desens parameters theta(3) and theta(4) need
c to be omitted from thtrue() for print out
	   th=thtrue(m)
	   if(autosim.and.imod0.eq.29.and.imods.eq.39.and.m.gt.2) then
		th=thtrue(m+2)
	   endif
         print 1211,m,i,j,titlep(m),th,thsav(m),theta0(m)
         if(discprt) write(8,1211) m,i,j,titlep(m),th,thsav(m),
     &	theta0(m)
1211	 format(i3,1x,' q(',i2,',',i2,') = ',1x,a10,3g13.6)
	 endif
	 if(jmic(m).eq.1) then
         print 1212
         if(discprt) write(8,1212)
1212	   format('& (micro-rev)')
	 else if(jcon(m).eq.1) then
         print 1213
         if(discprt) write(8,1213)
1213	   format('& (constrained)')
	 else if(fixec50.and.m.eq.m50) then
         print 1215,1.d6*ec501
         if(discprt) write(8,1215) 1.d6*ec501
1215	   format('& (from EC50=',f8.3,' muM)')
	 else if(fixec50.and.m.eq.m502) then
         print 1215,1.d6*ec502
         if(discprt) write(8,1215) 1.d6*ec502
c1215	   format('& (from EC50=',f8.3,' muM)')
	 else if(jfix(m).eq.1) then
         print 1214
         if(discprt) write(8,1214)
1214	   format('& (fixed)')
	 endif
	enddo		!end of m=1,npar
      print 23,nev,-smin
      if(discprt) write(8,23) nev,-smin
23	format(
     &' Number of evaluations = ',i8,' Max log(likelihood) = ',g14.7,/,
     &'  Press any key to continue')
	if(.not.nofit) then
	   if(istab.eq.0) then
		print 231
		if(discprt) write(8,231)
231		format(
     & ' (Fit NOT ''stable'' at time of convergence)')
	   else if(istab.eq.1) then
		print 232
		if(discprt) write(8,232)
232		format(
     & ' (Fit ''stable'' at time of convergence)')
	   else if(istab.eq.2) then
		print 233
		if(discprt) write(8,233)
233		format(
     & ' (Fit ''very stable'' at time of convergence)')
	   endif
	endif

	call ANYKEY()
c
c Now print also equilibrium constants
	print 92
	if(discprt) write(8,92)
92	format(/,
     &' Equilibrium constants calculated for fitted rate constants')
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
		   eqK=sngl(QT(j,i)/QT(j,i))
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
	   print 7,kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
	   if(discprt) write(8,7)
     &		kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
7	   format(
     &      1x,a7,' = q(',i2,',',i2,')/q(',i2,',',i2,') = ',
     &	a10,'/',a10,' = ',g13.6)
	   if(isfac1.gt.0.and.isfac2.gt.0) then
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
c Print MR check here, after final rates
	i=idebug2	!save
	idebug2=2	!so prints to screen and disk
	call CHECKMR(QD)
	idebug2=i	!restore
c
c Now print ec50(s) for final parameters AFTER the rate constant estimates
c (now replaced by call to PRNTEC50()
c
      print 138
      if(discprt) write(8,138)
	call BELL(1)
c138	format(
c     &' ------------------------------------------------------------')
	print 142
      if(discprt) write(8,142)
142	 format(/,' EQUILIBRIUM CALCULATIONS FOR FINAL RATES')
	call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
	if(ncdep.gt.0) then
c	 do ifix=1,nfixec50
c	   if(ifix.eq.1) then
c	    nlv=nlvar
c	   else if(ifix.eq.2) then
c	    nlv=nlvar2
c	   endif
c	   print 142,nlv,ligname(nlv)
c         if(discprt) write(8,142) nlv,ligname(nlv)
c142	   format(/,' EC50 calculations for final parameter values',/,
c     &  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
cc Conc of other ligands is as in set 1 at the moment -could make loop for nset
c	   if(nlig.gt.1) then
c		do j=1,nlig
c		   if(j.ne.nlv) then
c			print 143,j,ligname(j),conc(j,1)*1.e6
c			if(discprt) write(8,143) j,ligname(j),conc(j,1)*1.e6
c143			format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
c     &			' micromolar)')
c		   endif
c		enddo
c	   endif
c	   kdim=100
c	   if(dcmodel.and.nlig.eq.1) then
c		call MODEC50(imod0,QT,npar,kdim,pmax,ec50out1)
c		call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
c     &	  pmax,nlv,QT,jset,conc,k,kdim,nerr)
c	   else
c		call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
c     &       pmax,nlv,QT,jset,conc,k,kdim,nerr)
c	   endif
c	   if(nerr.ne.0) then
c		call BELL(1)
c		print 590,nerr
c		if(discprt) write(8,590) nerr
c590		format('  Error in bisection for EC50: nerr = ',i2)
c	   endif
c	   print 593,ligname(nlv),cur0,pop0
c	   if(discprt) write(8,593) ligname(nlv),cur0,pop0
c593	   format(
c     & ' At zero concentration of ',a10,', current (pA) = ',g12.5,
c     &	', Popen = ',g12.5)
c	   if(monot) then
c		print 594,curinfs,pmax,ligname(nlv)(1:10),ec50out*1.e6
c      	if(discprt) write(8,594) curinfs,pmax,ligname(nlv)(1:10),
c     &		ec50out*1.e6
c594		format(
c     & ' Equilibrium response-concentration curve is monotonic',/,
c     & ' Maximum response (pA) = ',g11.4,' Maximum Popen = ',f9.6,/,
c     & '    Conc of ',a10,' for 50% of this maximum (EC50) (muM) = ',
c     &     g11.4,/,
c     &   ' --------------------------------------------------')
c	   else	!not monotonic (never true for binding curve)
c      	print 592,curmax,concmax*1.e6,pmax,
c     &		ligname(nlv)(1:10),ec50out*1.e6,curinfs
c      	if(discprt) write(8,592) curmax,concmax*1.e6,pmax,
c     &		ligname(nlv)(1:10),ec50out*1.e6,curinfs
c592		format(
c     &    ' Equilibrium response-concentration curve has maximum.',/,
c     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
c     &    ' Maximum Popen = ',f9.6,/,
c     &    '   Conc of ',a10,
c     &    ' for 50% of this max. current (muM) (left of max) = ',
c     &        g12.5,/,
c     &    '   Response at conc -> infinity = ',g12.5,' pA',/,
c     &   ' --------------------------------------------------')
c	   endif
c   	   if(dcmodel.and.nlig.eq.1) then
c		print 3461,1.d6*ec50out1
c		if(discprt) write(8,3461) 1.d6*ec50out1
c3461		format(
c     & ' Check: EC50 by explicit calculation = ',f9.3,' micromolar')
c	   endif
c	 enddo	!do ifix=1,nfixec50
c	endif		!ncdep.gt.0
c
c	if(fixec50) then
c	   do ifix=1,nfixec50
c		if(ifix.eq.1) then
c		   n=nlvar
c		   i=i50
c		   j=j50
cc		   m=m50
c		   ec=ec501
c		else
c		   n=nlvar2
c		   i=i502
c		   j=j502
cc		   m=m502
c		   ec=ec502
c		endif
c		print 345,i50,j50,1.d6*ec50
c		if(discprt) write(8,345) i,j,ligname(n),1.d6*ec
c345	      format(
c     &'    Q(',i2,',',i2,') calculated from EC50 (',a10,') = ',f9.3,
c     &' micromolar')
c		if(nlig.gt.1) then
c		   if(discprt) write(8,261)
cc261		   format(
cc     &	'    Concentration of other ligands for EC50 determination')
c		   do i=1,nlig
c			if(i.ne.n) then
c			   if(ifix.eq.1) then
c				x=1.e6*conc_ec1(i)
c			   else if(ifix.eq.2) then
c				x=1.e6*conc_ec2(i)
c			   endif
c			   if(discprt) write(8,325) ligname(i)(1:10),x
cc325			   format(
cc     & '  Conc of ',a10,' at which EC50 was determined (micromolar) [',
cc     & f9.4,'] = ')
c			endif
c		   enddo
c		endif
c	   enddo		!ifix=1,nfixec50
	   if(penalty) then
	      print 344,penfunc
            if(discprt) write(8,344) penfunc
344	      format(' Penalty function in effect: ',f8.3,
     &        ' log likelihood units')
	   endif
	endif
c
508	if(autosim) then
	   do i=1,nset
		elmset(i,nsims)=setlik(i)
	   enddo
	   if(abort) then
		ans='N'
		call DCASK(' Save the last (aborted) fit',ans,ans)
		if(ans.eq.'N') goto 513
	   endif

	   kdim=100
c For autosim need to fix to keep ec50s for 2 ligands
c========fix ec50 for autosim with nlig>1.
c========jset now in call to ec50_hjc, but not defined here

	   if(dcmodel.and.nlig.eq.1) then
		call MODEC50(imod0,QT,npar,kdim,pmax,ec50out)
	   else
		jset1=1		!for now
		call EC50_HJC(EC50out,curinf,curmax,concmax,cur0,pop0,
     &	  pmax,nlvar,QT,jset1,conc,k,kdim,nerr)
c===print ec50 for autosim?
		if(nerr.ne.0) then
		   call BELL(1)
		   print 590,nerr
		   if(discprt) write(8,590) nerr
590		   format('  Error in bisection for EC50: nerr = ',i2)
		endif
	   endif
	   call SIMWRT(
     &    nsims,simfile,simfile1,imod0,k,npar,nset,nlig,titlep,
     &    thtrue,thsav,conc,kfit,fixec50,ec50,stpfac,confac,errfac,nevm,
     &    cdate,ctime,apfile,treso,mtitle1,nsim,
     &    ix1sav,iy1sav,iz1sav,nint,nev,elmax,theta0,
     &    thetval,nintval,ixval,iyval,izval,elmval,elmset,abort,
     &    imods,npars,mtitles,nevals,ec50out,penfunc,ec50val)
513	   nsims=nsims+1
	   if(nsims.le.nsim.and.(.not.endsim)) then
c     Restore initial guess
		ik=0
		do m=1,npar
		 if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
		   if(.not.(fixec50.and.m.eq.m50)) then
			ik=ik+1
			theta(ik)=thsav(m)
		   endif
		 endif
		enddo
		iwrite=2	!keep current  ix1 etc but don't change ixlast
		print 516,nsims-1,simfile1
		if(discprt) write(8,516) nsims-1,simfile1
516		format(/,' Simulation #',i7,' recorded in ',a40,/)
		call RANDSK(ix1,iy1,iz1,iwrite,repeat)		!write IX,IY,IZ
		goto 500		!do next fit
	   else
		do j=1,nset
		   tresol(j)=1.e3*treso	!keep res in mus for .ini
		enddo
		iwrite=1	!replace both ix1 and ixlast (unless repeat=T)
		call RANDSK(ix1,iy1,iz1,iwrite,repeat)		!write IX,IY,IZ
	   endif
	endif
c
c  Do calculations again with final estimates to print pdfs etc if
      print 108
      if(discprt) write(8,108)
c108	format('/')
      print 109
      if(discprt) write(8,109)
109	format(' VALUES CALCULATED FROM FINAL FIT')
	do jset=1,nset
         print 267,jset
         if(discprt) write(8,267) jset
267      format(/,
     & ' Equilibrium values for set number ',i3,': ',/,
     & ' concentration(s) =')
	   do i=1,nlig
		print 335,conc(i,jset)*1.e6,ligname(i)
		if(discprt) write(8,335) conc(i,jset)*1.e6,ligname(i)
c335		format(3x,g13.6,' micromolar of ',a20)
	   enddo
         print 138
         if(discprt) write(8,138)
138	   format(
     &' ------------------------------------------------------------')
c	   xA0=concA(jset)
c	   xB0=concB(jset)
c Make QD for the specified concentration(s)
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
c (a) Calc equilibrium occupancies
c=	   call EQOCCd(QD,k,k-1,km,peq)
	   call EQOC_HJC(QD,peq,k,km,km)
	   call SLIFEd1(QD,Peq,km,km)
	enddo
c (b) Print final HJC distributions
c -set idebug=8 so HJCLIK prints values (for all data sets)
c	if(idsav.eq.8.or.idsav.eq.9) then
c Now hjclik fixed so can call with nodata=true to calculate distributions
c Also, when idebug=8, check the calculation of amplitudes for asyptotic
c HJC distributions in next call to HJCLIK.
c Are rW=0 calculations reasonably free of rounding errors
C at the time of convergence?
c
c==	if(.not.nodata) then
c	do j=1,nset
	   if(nodata) then
		first=.true.
	   endif
	   oneset=.false.
	   idebug=idsav			!save idebug
	   idebug=8				!switch debug on
   	   elmax=-HJCLIK(kfit,theta,
     &    tint,ampl,iprops,nd1,nd2,
     &    Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &    XAF,XFA,QexpQA,QexpQF,
     &    alpha2,beta2,aloglik,kab,
     &    kAm,kFm,km)		!for final estimates from final QD
	   idebug=idsav			!restore idebug
c	enddo
c==	endif
c
c
c Before calculating errors, ask if QT to be saved for use in SCBST etc
c (now moved before plots too)
	print 8
8	format(
     &' The fitted rate constants can be saved in a qmechs file',/,
     &' (e.g. QTEMP.MEC) which functions like qmechs.dat and can be',/,
     &' used as input to HJCFIT, SCALCS, SCBST etc',/)
	ans='Y'
	call DCASK('Save fitted rate constants',ans,ans)
	if(ans.eq.'Y') then
	   kflag=-1	!signal to write QT into qtemp.dat
	   call GETQD2(npar,titlep,ligname,QT,conc,irate,jrate,
     &    nchan,dgamma,vkin,vhold,vref,iprint,readini,irecq,qfilem,
     &    theta0,jcon,autosim,thtrue,irect,irates,jrates,
     &    npars,dgammas,kflag,icon,useprim,nfix,jfix,idest)
	endif
c
c Now options to plot data with final fit superimposed on it (based on EKDIST)
c Call HJCDISP with QT and put required concentrations later
	call BELL(1)
	ans='Y'
	call DCASK('Estimate errors for parameters before plotting',
     &	ans,ans)
	if(ans.eq.'Y') then
	   plotdone=.false.	!so returns to do plots after errors if nec
	   go to 70		!calc errors
	endif
c
71	continue	!return to do plots after errors if nec
	iplot=1
c
	if(nodata) then	!must allocate tint anyway before call
	   nd1=1
	   nd2=1
	   if(.not.allocated(tint)) then
		ALLOCATE(tint(1,1),ampl(1,1),iprops(1,1))
	   endif
	endif
c NB call with theta (not theta0), for hjclik call in hjcdisp
c 01/14/02 05:01pm nodata removed from args and put in common (for hjclik)
	first=.true.
	call HJCDISP(iplot,nint,burst,chsvec,rootA,rootF,theta,
     & tres,tcrit,QT,nlig,ylo,yhi,nrange,readini,kfit,k,
     & tint,ampl,iprops,nd1,nd2,nintt,iscan,idsav,idiskq,
     & qfile,tresd,nofit,curvonly,ligname,ec50out,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,badend,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,kAx,km)
c
	plotdone=.true.
c
	oneset=.false.		!in case more fits done
c
c After plots done safely, ask if errors to be done, and do ec50 calc
c Calculate predicted EC50 (see scalcs)
c EC50 etc tnow calculated in PRNTEC50 (above, but can move to here,
c after plots, if it causes crashes
c
94	continue
c If nodata may want to try different conc (easier to have several sets
c with different conc?)
	if(nodata) then
	   ans='Y'
	   call DCASK('Repeat with a different concentration',ans,ans)
	   if(ans.eq.'N') then
		goto 992
	   else
		do j=1,nset
	        print 741,j
c741		  format(' Set ',i3)
	        do i=1,nlig
		   print 76,ligname(i)
c76		   format(
c     &    '    Concentration of ',a10,' (micromolar) = ')
		   call INPUTr(xA)
		   conc(i,j)=xA*1.e-6
	        enddo
		enddo
	      goto 993		!repeat calcs
	   endif
	endif
c
c Now errors
	ans='Y'
	call DCASK('Estimate errors for parameters',ans,ans)
70	if(ans.eq.'Y') then
c NB theta0() contains all param, but theta() contains only the estimated
c params.
c Modif 11/01/01 11:33am.  During calculation of errors, QT, QD  and theta
c could be altered.  Although arrays are called th1() etc in vmat_hjc, they
c theta() and QD in HJLIK, and QD, QT are in common in HJCLIK, and so
c get changed in main program.  Simplest fix is just to save ML values here
c and restore after errors calc
	   if(allocated(QTsav)) DEALLOCATE(QTsav,QDsav)
	   ALLOCATE(QTsav(100,100),QDsav(100,100),thsav1(200))
	   if(allocated(var)) then
		deallocate(var,covar,covlog,varlog,badpar,jcov)
	   endif
	   ALLOCATE(var(200),covar(200,200),
     &	 covlog(200,200),varlog(200),badpar(200),jcov(200))
	   do i=1,k
		thsav1(i)=theta0(i)
		do j=1,k
		    QTsav(i,j)=QT(i,j)
		    QDsav(i,j)=QD(i,j)
		enddo
	   enddo
c Now returns both covar() for rates and covlog()=covar matrix for log(rates)
	   call HMAT_HJC(theta,var,elmax,kfit,badpar,kgood,
     &    covar,covlog,varlog,npar,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
c  restore ML values of QT etc
	   do i=1,k
		theta0(i)=thsav1(i)
		do j=1,k
		    QT(i,j)=QTsav(i,j)
		    QD(i,j)=QDsav(i,j)
		enddo
	   enddo
c	   DEALLOCATE(QTsav,QDsav,thsav1)
c
c Define jbad(i), i=1, npar
c Also define jcov(i), i=1,npar, such that jcov(i)=n where n=row/col number
c in covar matrix that corresponds to parameter #i, and jcov(n)=-1 if
c parameter #i does not appear in the covar matrix
	   i1=0
	   i2=0
	   do m=1,npar
		jcov(m)=-1
		jbad(m)=0
		sd(m)=-1.0		!unless good value defined
		sdlog(m)=-1.0		!unless good value defined
		if(jcon(m).eq.0.and.jmic(m).eq.0.and.jfix(m).eq.0) then	!free
		   if(.not.(fixec50.and.m.eq.m50)) then		!free
			i1=i1+1
	      	if(badpar(i1)) jbad(m)=1
			if(.not.badpar(i1)) then
			   i2=i2+1
			   if(var(i2).gt.0.0d0) then
				sd(m)=sngl(dsqrt(var(i2)))
				sdlog(m)=sngl(dsqrt(varlog(i2)))
				jcov(m)=i2
			   endif
			endif
		   endif
		endif
	   enddo
c
c Now print covar() here rather than in hmat, so parameter names can be
c put at rows and of cols more easily
c If can print titlep + 10 values per row then each row of matrix
c will occupy 1+kgood/9 rows of printout
	   print 3711
	   if(discprt) write(8,3711)
3711	   FORMAT(/,' Correlation matrix= ')
	   npr=10
c	   nr=1 + kgood/(npr-1)
c  col titles
	   ir=0
	   do j=1,npar
		if(jbad(j).eq.0.and.sd(j).gt.0.0) then
		   ir=ir+1
		   if(ir.eq.1) then
		      print 50,titlep(j)(1:6)
			if(discprt) write(8,50) titlep(j)(1:6)
50			format(12x,a6)		!space for row titles
		   else
			print 51,titlep(j)(1:6)
			if(discprt) write(8,51) titlep(j)(1:6)
51			format('& ',a6)
		   endif
		   if(mod(ir,npr).eq.0) then
			ir=0
		   endif
		endif
	   enddo
c now print matrix
c
	   ii=0
	   do i=1,npar
	      if(jbad(i).eq.0.and.sd(i).gt.0.0) then
		   print 53,titlep(i)		!row label
		   if(discprt) write(8,53) titlep(i)
53		   format(1x,a10)
		   ii=ii+1
		   jj=0
		   ir=0	!count number on each row
		   do j=1,npar	!print row i
			if(jbad(j).eq.0.and.sd(j).gt.0.0) then
			   ir=ir+1
			   jj=jj+1
			   ci=covar(ii,ii)
			   cj=covar(jj,jj)
			   if(ci.le.1.d300/cj.and.ci.gt.1.d-300/cj) then
			 	den=dsqrt(covar(ii,ii)*covar(jj,jj))
				if(den.gt.1.d-30) then
				   cor=covar(ii,jj)/den
				   print 54,cor
				   if(discprt) write(8,54) cor
54				   format('& ',f6.3)
				else
				   print 55
				   if(discprt) write(8,55)
55				   format('&  n.d. ')
				endif
			   else
				print 55
				if(discprt) write(8,55)
			   endif
			   if(mod(ir,npr).eq.0) then
				ir=0
				print 52
				if(discprt) write(8,52)
52				format('             ')	!start new row
			   endif
			endif
		   enddo
		endif
	   enddo
c
c Print settings etc
	   print 1373,nev,-smin
         if(discprt) write(8,1373) nev,-smin
1373	   format(/,' FINAL VALUES OF RATE CONSTANTS',/,
     &' Number of evaluations = ',i8,' Max log(likelihood) = ',g14.7)
	   do j=1,nset
		tres1=1.d6*tresd(j)
		if(nodata) then
		   burst(j)=.false.
		   chsvec(j)=.false.
	      endif
      	print 350,j
		if(discprt) write(8,350) j
350		format(/,' Set ',i3)
		if(burst(j)) then
		   do i=1,nlig
			print 335,conc(i,j)*1.e6,ligname(i)
			if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
c335		      format(3x,g13.6,' micromolar of ',a20)
		   enddo
		   print 351,tcrit(j),tres1
      	   if(discprt) write(8,351) tcrit(j),tres1
351		   format(
     &	  '    Analysed in bursts, tcrit (ms) = ',g13.6,/,
     &	  '    Resolution (microsec) = ',g13.6)
		else
		   do i=1,nlig
			print 335,conc(i,j)*1.e6,ligname(i)
			if(discprt) write(8,335) conc(i,j)*1.e6,ligname(i)
c335			format(3x,g13.6,' micromolar of ',a20)
		   enddo
		   print 341,tres1
      	   if(discprt) write(8,341) tres1
c341		   format(
c     &	  '    Analysed as if from one channel only',/,
c     &	  '    Resolution (microsec) = ',g13.6)
		endif
		if(logsav) then
		   print 3411
      	   if(discprt) write(8,3411)
c3411		   format(' Simplex used log(rate constant) for searching')
		endif
		if(setbad(1,i)) then
		   print 342,ncbad(1,i),tcbad(1,i)
      	   if(discprt) write(8,342) ncbad(1,i),tcbad(1,i)
c342		   format('      ',i5,
c     &	  ' shuttings longer than ',g13.6,' ms set bad')
		endif
		if(setbad(2,i)) then
		   print 343,ncbad(2,i),tcbad(2,i)
      	   if(discprt) write(8,343) ncbad(2,i),tcbad(2,i)
c343		   format('      ',i5,
c     &	  ' openings longer than ',g13.6,' ms set bad')
		endif
		if(iexcop(j).eq.1) then
		   print 1311,j,nskip(j),gaplo(j),gaphi(j)
		   if(discprt) write(8,1311) j,nskip(j),gaplo(j),gaphi(j)
c1311	         format(/,' SET ',i3,/,
c     &   1x,i5,' open periods excluded because shut time on BOTH sides',
c     &	/,'  is between ',g13.6,' and ',g13.6,' ms')
		else if(iexcop(j).eq.2) then
		   print 1312,j,nskip(j)
		   if(discprt) write(8,1312) j,nskip(j)
c1312	         format(/,' SET ',i3,/,
c     &   1x,i5,' open periods excluded because they are in group with',
c     &	/,' only one open period')
	   endif
	   enddo
c Print estimates and SD
c NB for simulations, cannot print rates here if data was simulated
c with a different model (# imods) from that used for fitting (# imod0)
	   if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
      	print 30
      	if(discprt) write(8,30)
30		format(/,23x,
     &	'  initial         final       approx SD       CV(%)')
	   else
      	print 31
      	if(discprt) write(8,31)
31		format(/,23x,
     & '  true        initial        final      approx SD       CV(%)')
	   endif
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
		cv=100.*sd(m)/sngl(theta0(m))
		if(jbad(m).eq.0.and.sd(m).gt.0.0) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 32,m,i,j,titlep(m),thsav(m),theta0(m),sd(m),cv
	         if(discprt) write(8,32) m,i,j,titlep(m),thsav(m),
     &		theta0(m),sd(m),cv
32		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &	   t25,g13.6,t38,g13.6,t51,g13.6,t65,f8.2)
		 else
      	   print 35,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m),
     &		sd(m),cv
      	   if(discprt) write(8,35) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m),sd(m),cv
35	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &	   t25,g13.6,t38,g13.6,t51,g13.6,t65,g13.6,f8.2)
		 endif
		else if(jbad(m).eq.1) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 36,m,i,j,titlep(m),thsav(m),theta0(m)
	         if(discprt) write(8,36) m,i,j,titlep(m),thsav(m),
     &			theta0(m)
36		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &		t25,g13.6,t38,g13.6,t54,'  not def   ')
c Print 'not def' only if can't get 2nd deriv for Hessian
		 else
		   print 38,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
      	   if(discprt) write(8,38) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m)
38	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,
     &		t54,'  not def   ')
		 endif
c if constrained, print the error for the value that is constrained
c to be equal to
		else
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 361,m,i,j,titlep(m),thsav(m),theta0(m)
	         if(discprt) write(8,361) m,i,j,titlep(m),thsav(m),
     &			theta0(m)
361		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &		t25,g13.6,t38,g13.6,t54,'  -         ')
c Print 'not def' only if can't get 2nd deriv for Hessian, but
c for rates that are constrained or fixed print '-'
		 else
		   print 381,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
      	   if(discprt) write(8,381) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m)
381	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,
     &		t65,'  -         ')
		 endif
		endif
		if(jmic(m).eq.1) then
      	   print 1212
      	   if(discprt) write(8,1212)
c1212		   format('&   (micro-rev)')
		else if(jcon(m).eq.1) then
      	   print 1213
      	   if(discprt) write(8,1213)
c1213		   format('&   (constrained)')
		 else if(fixec50.and.m.eq.m50) then
	         print 1215,1.d6*ec501
	         if(discprt) write(8,1215) 1.d6*ec501
c1215		   format('& (from EC50=',f8.3,' muM)')
		 else if(fixec50.and.m.eq.m502) then
	         print 1215,1.d6*ec502
	         if(discprt) write(8,1215) 1.d6*ec502
c1215		   format('& (from EC50=',f8.3,' muM)')
		else if(jfix(m).eq.1) then
      	   print 1214
      	   if(discprt) write(8,1214)
c1214		   format('&   (fixed)')
		endif
	   enddo
c
c  Now repeat print for log(rates)
c		sd(log(x))= CV(x) approximately
c		and from this get approx sd(x)=x*CV(x)
c
c Print estimates and SD
	   first=.true.	 !calc covar(rate); when false, calc covar(log(rate))
	   if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
      	print 301
      	if(discprt) write(8,301)
301		format(/,
     &    ' Errors from log(rate) means that 2nd derivs in Hessian',/,
     &    ' are multiplied by theta(i)*theta(j), to get 2nd deriv',/,
     &    ' with respect to log(rate)',/,/,
     &     '  FINAL VALUES WITH ERRORS FROM log(rate)',/,23x,
     &     '  initial         final         CV(%)       SD')
	   else
      	print 311
      	if(discprt) write(8,311)
311	     format(/,
     &    ' Errors from log(rate) means that 2nd derivs in Hessian',/,
     &    ' are multiplied by theta(i)*theta(j), to get 2nd deriv',/,
     &    ' with respect to log(rate)',/,/,
     &    '  FINAL VALUES WITH ERRORS FROM log(rate)',/,23x,
     &     '  true        initial        final         CV(%)     SD')
	   endif
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
		if(jmic(m).eq.1) then	!calc CV for MR rates
		   ktd=200
		   call MRVAR(i,j,covlog,jcov,kgood,obeymr,IQ,cvmr,ktd)
		    sdlog(m)=cvmr
		endif
c
		if(jbad(m).eq.0.and.sdlog(m).gt.0.0) then
		 cv=100.*sdlog(m)
		 sx=sdlog(m)*sngl(theta0(m))
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 313,m,i,j,titlep(m),thsav(m),theta0(m),cv,sx
	         if(discprt) write(8,313) m,i,j,titlep(m),thsav(m),
     &		theta0(m),cv,sx
313		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &	   t25,g13.6,t38,g13.6,t51,g13.6,t65,g13.6)
		 else
      	   print 315,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m),
     &		cv,sx
      	   if(discprt) write(8,315) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m),cv,sx
315	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
     &	   t25,g13.6,t38,g13.6,t51,g13.6,t65,f8.2,g13.6)
		 endif
		else if(jbad(m).eq.1) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 36,m,i,j,titlep(m),thsav(m),theta0(m)
	         if(discprt) write(8,36) m,i,j,titlep(m),thsav(m),
     &			theta0(m)
c36		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
c     &		t25,g13.6,t38,g13.6,t54,'  not def   ')
		 else
		   print 38,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
      	   if(discprt) write(8,38) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m)
c38	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,
c     &		t54,'  not def   ')
		 endif
		else
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         print 361,m,i,j,titlep(m),thsav(m),theta0(m)
	         if(discprt) write(8,361) m,i,j,titlep(m),thsav(m),
     &			theta0(m)
c361		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,
c     &		t25,g13.6,t38,g13.6,t54,'     -      ')
c Print 'not def' only if can't get 2nd deriv for Hessian, but
c for rates that are constrained or fixed print '-'
		 else
		   print 381,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
      	   if(discprt) write(8,381) m,i,j,titlep(m),thtrue(m),
     &		thsav(m),theta0(m)
c381	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,
c     &		t65,'     -      ')
		 endif
		endif
		if(jmic(m).eq.1) then
      	   print 1212
      	   if(discprt) write(8,1212)
c1212		   format('&   (micro-rev)')
		else if(jcon(m).eq.1) then
      	   print 1213
      	   if(discprt) write(8,1213)
c1213		   format('&   (constrained)')
		 else if(fixec50.and.m.eq.m50) then
	         print 1215,1.d6*ec501
	         if(discprt) write(8,1215) 1.d6*ec501
c1215		   format('& (from EC50=',f8.3,' muM)')
		 else if(fixec50.and.m.eq.m502) then
	         print 1215,1.d6*ec502
	         if(discprt) write(8,1215) 1.d6*ec502
c1215		   format('& (from EC50=',f8.3,' muM)')
		else if(jfix(m).eq.1) then
      	   print 1214
      	   if(discprt) write(8,1214)
c1214		   format('&   (fixed)')
		endif
	   enddo
c  Repeat with direct calculation of COVLOG as test
	   if(first) then
		first=.false.
            print 108
	      ans='Y'
	      call DCASK(
     &   'Check errors by direct calculation from log(rates)',ans,ans)
		if(ans.eq.'Y') then
		   call HMAT_LOG(theta,varlog,elmax,kfit,badpar,kgood,
     & 	   COVLOG,tint,ampl,iprops,nd1,nd2,
     & 	   Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	   XAF,XFA,QexpQA,QexpQF,
     & 	   alpha2,beta2,aloglik,kab,
     & 	   kAm,kFm,km)
c		   goto ***
	         print 48
		   if(discprt) write(8,48)
48		   format(/,' Approx CV (%) calc directly from log(rates)')
		   do i=1,kgood
			if(varlog(i).gt.0.0d0) then
			   sdlog(i)=sngl(dsqrt(varlog(i)))
		         print 40,i,100.*sdlog(i)
			   if(discprt) write(8,40) i,100.*sdlog(i)
40			   format(' ',i3,' CV (%) = ',f9.3)
			endif
		   enddo
		endif
	   endif
c
	   DEALLOCATE(QTsav,QDsav,thsav1)
	   call DCASK('Another error calculation','N',ans)
	   if(ans.eq.'Y') goto 70
	endif		!end of if errors
c
992	continue
c
	if(allocated(var)) deallocate(var,covar,covlog,varlog,badpar,jcov)
c
	if(.not.plotdone) goto 71	!now do the plots
c
	print 108
c
99	continue
c Write defaults back to disc?
	print 108
	call DCASK('Write defaults to disc (hjcfit.ini)','Y',ans)
c	if(readini) then
c	   call DCASK('Write defaults to disc','N',ans)
c	else
c	   call DCASK('Write defaults to disc','Y',ans)
c	endif
cc NB write ndev2 back to .INI file
c Write irecq=position of rate data, back to .INI
c Transparent write shows new .ini is 56459 bytes, so keep as fixed
c record length of 60k=61440 bytes. BUT record length > 32k gives
c error, so easiest to use transparent access, but when writing
c .ini, pad it out to 61440 bytes, so if other things added later
c don't get 'read past end of record'.
c	irecq=(jrecq+1)/2		!irecq is output from GETQD
      if(ans.eq.'Y') then
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   call TITENT0(
     &    'Name for hjcfit.ini file:',inifile,40,.false.)
	   do i=1,ncyc
		if(.not.obeymr(i)) then
		   nsc(i)=-iabs(nsc(i))
		endif
		if(.not.automr(i)) then
		   im(i,1)=-iabs(im(i,1))
		endif
	   enddo
	   n=0
	   do i=1,ncyc
		n=n+nsc(i)
	   enddo
c Subtract space taken by nsc(),im,jm,assmax,qfilem,nsim,irect,logsav,imods,
c badend,iexcop,gaplo,gaphi,dcmodel
	   dcmod=dcmodel
	   npad=4941-4-4*ncyc-8*n-8-40-4-4-4-4-4*40-4
c and for
c slopsch,checkgrp,rcrit,ngpcheck,nstab1,gfac1,nstab2,gfac2,ans5
c     4       4      8     4        4      8      4      8    1 = 45 bytes
	   npad=npad - 45
c and for  nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2 =3*8+4*4 =40 bytes
	   npad=npad - 40
c  npad=4700 before isetmr added (for SS model)
	   npad=npad-4		!for useprim
	   npad=npad-4*ncyc	!for isetmr
	   npad=npad-4		!for ratemax
	   npad=npad-4		!for nmr
	   npad=npad-4		!for nlig
	   npad=npad-2*4*nlig	!for conc_ec1,conc_ec2
	   if(npad.lt.0) then
		call BELL(3)
		print 77
c-77		format(' PROBLEM -run out of space in scan.ini')
		STOP
	   endif
	   allocate(inipad(npad))
	   do i=1,npad
		inipad(i)=0
	   enddo
         OPEN(unit=19,file=inifile,status='UNKNOWN',
     &     access='TRANSPARENT')
c     &    access='DIRECT',form='UNFORMATTED',recl=61440)
c     &    access='DIRECT',form='UNFORMATTED',recl=20480)
	   write(19,rec=1) pfile,nfix,jfix,neq,IE,JE,EFAC,
     & IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     & nfile,kfile,nset,conc,tcrit,burst,irecq,
     & idatyp,qfile,imod0,setbad,tcbad,onechan,
     & nbad,isbad,iebad,tresol,ans3,ans4,nlvar,
     & fixec50,ec501,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     & chsvec,ncyc,
     & (nsc(i),i=1,ncyc),
     & ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     & ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     & assmax,qfilem,nsim,irect,logsav,imods,
     & badend,iexcop,gaplo,gaphi,dcmod,
     & slopsch,checkgrp,rcrit,ngpcheck,
     & nstab1,gfac1,nstab2,gfac2,ans5,
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     & (conc_ec1(i),i=1,nlig),
     & (conc_ec2(i),i=1,nlig),
     & inipad
	   CLOSE(unit=19)
	   deallocate(inipad)
	   do i=1,ncyc	     !reset nsc
		nsc(i)=iabs(nsc(i))
		im(i,1)=iabs(im(i,1))
	   enddo
	endif
c
	if(allocated(tint0)) then
	   DEALLOCATE(tint0,tint,iprops,iprops0,ampl0,index)
	endif
	if(allocated(kjumps0)) then
	   DEALLOCATE(kjumps0,kjumps)
	endif
	call ENDPRINT
c Save print file with default name
c and also make copy of .ini file with same name
	call PARSNAME(qfile,path1,ndev,pname,suffix,nopath,40)
	csfound=.true.
	do i=1,6
	   ival=ICHAR(pname(i:i))
	   if(.not.DIGCHAR(ival)) then
		csfound=.false.
		goto 4
	   endif
	enddo
4	continue
	if(csfound) then
	   pname(7:7)='H'	!replace with H
	   prefix=charnb(path1)//charnb(pname)
	else
	   if(npatch.ge.0.and.npatch.le.9) then
		call INTCONV(npatch,cnum)
		prefix=charnb(defname)//'H'//cnum(1:1)
	   else
		prefix=charnb(defname)//'HJ'
	   endif
	   i=ichar(prefix(1:1))
	   if(i.eq.0.or.i.eq.32) then		!name starts with blank
		prefix='HJCxxx'
	   endif
	endif
c Next bit not needed now -hjcfit.ini named above (also CALL COPY does
c not seem to wrok with Win2000 or WinMe -need to use call SYSTEM as
c done below for .prt file.
c      ans='Y'
c      call DCASK(
c     & 'Make copy of defaults file (HJCFIT.INI)',ans,ans)
c	if(ans.eq.'Y') then
c	   suffix='ini'
c	   prtfil=charnb(prefix)//'.'//suffix(1:3)
c	   call TITENT0(
c     &	'Name for copy of HJCFIT.INI:',prtfil,40,.false.)
c		call COPY('hjcfit.ini',prtfil)
c	      OPEN(unit=8,file=filnam,status='UNKNOWN',
c     &     ACCESS='APPEND',FORM='FORMATTED',CARRIAGE CONTROL='FORTRAN')
c		print 92,prtfil
c		write(8,92) prtfil
c92		format(' Defaults file, HJCFIT.INI, renamed as ',a40)
c		CLOSE(unit=8)
c	   endif
c
	if(discprt) then
	   ans='Y'
453	   call DCASK(
     &   'Make copy of disc printout file with default name',ans,ans)
	   if(ans.eq.'Y') then
		suffix='prt'
		prtfil=charnb(prefix)//'.'//suffix(1:3)
		call DOSFILE(prtfil,40,prtfil,nerr)	!replace invalid characters with 'x'
		call TITENT0(
     &	'Name for copy of HJCFIT.PRT:',prtfil,40,.false.)
		INQUIRE(file=prtfil,exist=present,flen=nlen,err=453)
		iopt=2 	!if file does not already exist
		if(present.and.nlen.gt.0) then
		   call BELL(1)
		   print 454,prtfil
454		   format(/,1x,a40,' already exists:',/,
     &		' (1) append new file to old one',/,
     &		' (2) overwrite old file with new one',/,
     &		' (3) cancel copy',/,
     &		' Option number [1] = ')
		   iopt=1
		   call INPUTi(iopt)
		endif
		if(iopt.eq.1) then
c		  e.g. 'copy 991129h1.prt+hjcfit.prt 991129h1.prt'
		   command='copy '//charnb(prtfil)//'+'//charnb(filnam)//
     &		 ' '//charnb(prtfil)
		   call SYSTEM(command)
		else if(iopt.eq.2) then
		   command='copy '//charnb(filnam)//' '//charnb(prtfil)
		   call SYSTEM(command)
c=		   call COPY(filnam,prtfil)
		else if(iopt.eq.3) then
		   goto 999
		endif
	      OPEN(unit=8,file=prtfil,status='UNKNOWN',
     &     ACCESS='APPEND',FORM='FORMATTED',CARRIAGE CONTROL='FORTRAN')
		if(iopt.eq.1) then
		   print 93,prtfil
		   write(8,93) prtfil
93		   format(
     & '==============================================================='
     &	   ,/,' Print file, HJCFIT.PRT, appended to ',a40)
		else if(iopt.eq.2) then
		   print 91,prtfil
		   write(8,91) prtfil
91		   format(' Print file, HJCFIT.PRT, renamed as ',a40)
		endif
		CLOSE(unit=8)
	   endif
	endif
999	continue
c
	end


