	program HJCFIT
c To fit model specied by q(i,j) to a set of open-shut intervals using
c HJC distributions (exact for 2 deadtimes, then asymptotic).
c Lahey V5.n version
c (converted from version called HJCFIT1 for V2.n compiler, and HJCLIK1
c  (& HJCDISP1 similarly renamed).  Modified to use new QGEN.INI, QDAT.DAT
c and QMODEL.DAT files (see QGEN and QDFIX)
c All three files (QGEN.INI, QDAT.DAT, QMODEL.DAT) now kept in current root
c segment (they need to be available to several different progs)
c
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
	integer*2 IAMPL(20480,10)
	real TINT(20480,10)		!up to 20 *1024 for each conc
	integer kfile(20,10),nfile(10),nintt(10),nint(10)
	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	real*4 tcrit(10)
	logical burst(10)
	logical first,sublev
	logical sbin,shist,sres,sexp	!for DATIN
c
	real*4 theta0(20),theta(20),thsav(20),thtrue(20),data(100)
	real*4 ylo(20),yhi(20)
	real*8 QD(10,10),QT(10,10),Peq(10),tres	!QT has not got conc in
	real*8 rootA(10),rootF(10)
	real*8 s1A(10),s2A(10),s1F(10),s2F(10)	!init guesses for roots
c Keep roots and init guesses for them separately for each data set so
c make arrays 2D rootA(i,j)= ith root for data set j
	real*8 rootAsav(10,10),rootFsav(10,10)
	real*8 s1Asav(10,10),s2Asav(10,10),s1Fsav(10,10),s2Fsav(10,10)
	integer jfix(20),jfix1(20),jcon(20),jmic(20)
	character*10 titlep(50),titlep1(50)
	character*11 cdate,ctime
c	character*74 pdtitle(20)	!names of pdf types
	character*74 mtitle	!title for MODWIND models
	character*74 mtits(41)	!to read all ditto from disc
	character*2 charmod(20,30)
	integer IC(2,20),jmod(20)
	integer NSC(8),IM(8,15),JM(8,15),IX(10),JX(10),IL(10)
	integer im1(8,15),jm1(8,15)
	integer IQ(10,10),iq0(20),jq0(20)
	integer IQf(10,10),iq1(20),jq1(20)
	integer inew(10)
	real*4 EFAC(10)
c	integer jomit(20)
	integer IS(10),JS(10)	!declare for disc-write-not used now
	integer NEQ,IE(10),JE(10),IF(10),JF(10)	!ditto
	integer SCRNBUF0(1024)
	logical pon,slock,debug,caplock
	logical samenum,readin,oneset
	character*1 ans,UC,ans1
	character infil*14
	character*2 ndev,ndev1,ndev2,ndev21,ndev3,ndevs(20,10)
	LOGICAL KMFAST		!for QSETD
	logical discprt
c
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp	!for datin,resint
	COMMON/RBLCK/treso,tresg,iacrit,mavamp	!for resint
	common/deb/idebug
	common/lcomp/nset1,fcomp(10)				!for SIMPLEX, HJCLIK
	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) etc for set j
c	common/nblk/ng,an,nscal,first
	COMMON/AMPBLK/iampl
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	common/CBLK/nset,concA,concB,jsetlast		!for HJCLIK
	COMMON/CPAR/NCDEP,IX,JX,IL,X
c	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/SPAR/NSPEC,IS,JS		!special parameters
	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/iqblk/IQ
c Used QT in QBLK now (conc not incorp)
	common/QBLK/QT,QD,npar,IQf,iq1,jq1,xA1,xB1,nlig
c==?	COMMON/fitblk/ndist,idist,ncom,nomit,jomit,k,xA,xB,IL,V1,km
c NB npar=number of param; k=dimension of Q=kA+kB+kC+kD
	common/KBLK/kA,kB,kC,kD
c -following needed for QSETD (see PDCALC)- make sure all set
	COMMON/KM2/AKA1,BA,PSTAR(4),KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	COMMON/QPAR/NCON,IC
	COMMON/HJCBLK/TINT,Nint,tcrit,acal,burst
	common/root/rootA,rootF
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots
	common/rootsav/rootAsav,rootFsav
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav	  !for init guesses for roots
	common/detw2/km,tres,nerr			!for HJCLIK,DETWA,DETWF
c
	external HJCLIK       !function
c
c
	pon()=slock()
	debug()=caplock()
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	filnam='HJCFIT.PRT'
	call WINPRINT		!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
c
	print 100
	if(pon()) write(7,100)
	if(discprt) write(8,100)
100	FORMAT(
     &' HJCFIT: Fit of model to open-shut times with missed events',/,
     &'  (Uses HJC distributions, exact for 1st 2 deadtimes then',/,
     &'  asymptotic, to calculate likelihood of record)',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 6,cdate,ctime(1:8),mtitle1
	if(pon()) write(7,6) cdate,ctime(1:8),mtitle1
	if(discprt) write(8,6) cdate,ctime(1:8),mtitle1
6	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
101	format(a1)
2	format(i8)
	km=10		!in COMMON
	first=.true.	!first call to DATIN
	sbin=.false.	!for DATIN
	shist=.false.	!for DATIN
	sres=.false.	!for DATIN
	sexp=.false.	!for DATIN
	idiskq=-1		!until defined
	nset=1		!default number of data sets
c
	idebug=1		!no debugging
	idsav=1		!ditto
	call DCASK('Switch on debugging options','n',ans)
	if(ans.eq.'Y') then
	   print 441
441	   format(
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
     &' Option number [1] = ')
	  read 2,idebug
	  if(idebug.gt.9.or.idebug.lt.1) idebug=1
	  if(idebug.eq.8.or.idebug.eq.9) then
		idsav=idebug		!so debug can be switched on at end only
		idebug=1
	  endif
	endif
c
c Initialise
	do 132 i=1,20
	  jfix(i)=0		!to mark fixed params
	  jfix1(i)=0	!all zero, for Simplex call
	  jcon(i)=0		!to mark constrained params
	  jmic(i)=0		!to mark micro rev params
132	continue
	neq=0		!in case init file not read
	ans1='N'		!in case init file not read
	ndisp=20	!ditto
	ihjc=1	!use HJC definition of resolution
c
c Read the INIT file (tres kept in microsec here)
	readin=.false.
	print 45
45	format(' Read defaults from disc [Y] ? ')
	read 101,ans
      if(UC(ans).ne.'N') then
	   readin=.true.
         OPEN(unit=19,file='HJCFIT.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=3072)
c	   read(19,rec=1) ndevs,tres,nfix,jfix,neq,IE,JE,EFAC,
c     & IF,JF,jcon,IM,JM,jmic,ndisp,ndev3,ans1,ylo,yhi,nval,idiskq,
c     & nfile,kfile,nset,concA,concB,tcrit,burst
cc ndev3=disc for rates (so not overwritten by ndev2 from QDISC.DAT below)
cc (1429 bytes up to ans1)
	   read(19,rec=1) ndevs,tres,nfix,jfix,neq,IE,JE,EFAC,
     & IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nval,idiskq,
     & nfile,kfile,nset,concA,concB,tcrit,burst
	if(irecq.lt.1) irecq=1		!temp!
c irecq=file # for rates
	   CLOSE(unit=19)
	endif
c GET THE DATA TO BE FITTED (iampl() is in common/ampblk/)
	 call HJDATIN(TINT,NINTt,ACAL,IAVAMP,idisk,
     & SUBLEV,jfilt,rms,idest,nfile,kfile,trise,fc,iscan,idiskq,
     & treso,tresg,ndevs,ndev1,first,nset,concA,concB,readin)
	if(idest.eq.-1) goto 99
	if(iscan.ne.-3001) then
	   imode=1		!for HJCFIT
	   ihjc=1		!use HJC method
	   do 62 j=1,nset	!do for each set
	     call RESINT(TINT(1,j),nintt(j),nint(j),acal,iavamp,imode,
     &   	iampl(1,j),sublev,jfilt,fc,rms,trise,ihjc)
		ihjc=-1	!so no questions after 1st set done
62	   continue
	   tres=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	else		!for simulated expts (resolution fixed in SCSIM)
	   print 63
63	   format(' Impose resolution here (if not done in SCSIM) [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
	      imode=1		!for HJCFIT
		ihjc=1		!use HJC method
		do 64 j=1,nset	!do for each set
	        call RESINT(TINT(1,j),nintt(j),nint(j),acal,iavamp,imode,
     &   	   iampl(1,j),sublev,jfilt,fc,rms,trise,ihjc)
		  ihjc=-1	!so no questions after 1st set done
64		continue
	      tres=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	   else
		do 641 j=1,nset
641		nint(j)=nintt(j)
		print 41,sngl(tres)
41		format(
     &' Resolution (dead time) already imposed (musec) = ',f8.1,
     & 	' O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 42
42		   format(
     &	   ' Resolution (dead time) (microsec) = ')
		   call INPUTr(x)
		   if(x.gt.0.) then
		      tres=dble(x)*1.d-6	!value typed in (seconds)
		   else if(x.lt.-0.5) then
		      tres=0.d0			!to allow zero res
		   endif
		else
		   tres=tres*1.d-6	!value from INIT file, conv to seconds
		endif
	   endif
	endif
c
      if(pon()) write(7,43) tres*1.d6
      if(discprt) write(8,43) tres*1.d6
43	format(/,
     &' Resolution for HJC calculations = ',f8.1,' microseconds')
c
c Define tcrit, burst
	do 80 j=1,nset
	 if(readin) then
	    if(burst(j)) then
	      print 81,j,tcrit(j)
81		format(' Set ',i3,
     &    ': t(crit) (ms) = ',f8.1,' (not all one channel) O.K. [Y] ? ')
	    else
	      print 82,j,tcrit(j)
82		format(' Set ',i3,
     &   ': t(crit) (ms) = ',f8.1,' (all from one channel) O.K. [Y] ? ')
	    endif
	    read 101,ans
	    if(UC(ans).eq.'N') then
		print 83,j
83		format(' Set ',i3,
     &      ': t(crit) (ms) = ')
		call INPUTr(tcrit(j))
		if(tcrit(j).lt.500.) then
		   burst(j)=.true.
		   print 421
421	   	   format('&Does this tcrit include ALL shuttings [N] ')
	   	   read 101,ans
		   if(UC(ans).eq.'Y') burst(j)=.false.
	      else
		   burst(j)=.false.
		   print 422
422		   format('&Does this tcrit include ALL shuttings [Y] ')
		   read 101,ans
		   if(UC(ans).eq.'N') burst(j)=.true.
		endif
	    endif
	 else		!not readin
	  print 83,j
	  call INPUTr(tcrit(j))
	  if(tcrit(j).lt.100.) then
	     print 421
	     read 101,ans
	     burst(j)=UC(ans).ne.'N'
	  else
	     print 422
	     read 101,ans
	     burst(j)=.not.(UC(ans).ne.'N')
	  endif
	 endif
       if(pon()) write(7,84) j,tcrit(j)
       if(discprt) write(8,84) j,tcrit(j)
84	 format(/,' Set ',i3,
     &': Critical gap length to define end of group = ',f8.1,
     &' milliseconds',/,
     &'  (defined so that all openings in a group prob come from same',
     &' channel)',/)
	 if(burst(j)) then
         print 431
         if(pon()) write(7,431)
         if(discprt) write(8,431)
431	   format(
     &'&Initial and final vectors for bursts used on assumption that',/,
     &' this tcrit defines 1-channel sections of data, and excludes',
     &' long shut times',/)
	 else
         print 432
         if(pon()) write(7,432)
         if(discprt) write(8,432)
432	   format(
     &'&Analysis assumes that tcrit is long enough to include all',/,
     &' relevant shut time components thet are present in the data',/)
	 endif
80	continue	!end of jset=1,nset loop
c====
c	print 42,tcrit
c42	format(
c     &' Specify t(crit) such that gaps<tcrit imply all openings from',
c     & ' one channel',/,' tcrit (ms) [',f8.1,'] = ')
c	call INPUTr(x)	!real*4, in milliseconds as req in HJCLIK
c	if(x.gt.0.) tcrit=x
c	if(tcrit.lt.100.) then
c	   print 421
c421	   format(' Does this tcrit define single bursts [Y] ')
c	   read 101,ans
c	   burst=UC(ans).ne.'N'
c	else
c	   print 422
c422	   format(' Does this tcrit include ALL shuttings [Y] ')
c	   read 101,ans
c	   burst=.not.(UC(ans).ne.'N')
c	endif
c
c
c SPECIFY THE MODEL TO BE FITTED
	nvdep=0	!for fitting
	nspec=0	!in this prog
c
c	xA=0.1	!to prevent error in qsetd
	kmfast=.false.
c Following are set by disc read: ncdep,ix,jx,il,ncyc,nsc,im,jm,kA,kB,kC,kD,
c titlep,ncon
c Read titles of models that are defined
c      OPEN(unit=14,file='D:\CHARQ.DAT',status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=3072)
      OPEN(unit=14,file='\QMODEL.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=3072)
	read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
      CLOSE(unit=14)
cc Read QDISC to find which disc used in last run (and also imod for
cc reading param later) -now replaced by QGEN.INI
c      OPEN(unit=17,file='D:\QDISC.DAT',status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=512)
cc array IMOD(i) contains model number (imod0) on the ith disc (i=1,2,3,4,5,6
cc for disc=A,B,C,D,E,F)
c	read(17,rec=1) idisc,ndev2,imod
c	CLOSE(unit=17)
c	if(readin) then
c	   ndev21=ndev2
c	   ndev2=ndev3		!use default from HJCFIT.INI
c	endif
      OPEN(unit=17,file='\QGEN.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(17,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=17)
	if(readin) then		!use default from HJCFIT.INI
	   jrecq=2*irecq-1	!record # in QDAT for 1st record of rate data
	else
	   jrecq=jreclast
	endif
c	i=ichar(ndev2(1:1))	!test if ndev2=A,...,Z
c	if(i.lt.65.or.i.gt.90) ndev2='**'
c	print 71,ndev2
c71	format(' Read model and rates from disc ',a2,' [Y] ? ')
c	read 101,ans
c      if(UC(ans).eq.'N') then
c73	   print 72
c72	   format(
c     &	' (1) Specify different disc for QDAT2.DAT',/,
c     &	' (2) Specify different model',/,
c     &	' Option number = ')
c	   read 2,i
c	   if(i.lt.1.or.i.gt.2) goto 73
c	   if(i.eq.2) then
c	      goto 59
c	   else if(i.eq.1) then
c	     call DISCNUM1(idisc,ndev21,0,1) !disc to read from-default in ndev21
c	     ndev2=ndev21		!new ndev2
c	   endif
c	endif
c==========should specify default model here!
	call DCASK('Read model and rates from disc ','y',ans)
	if(ans.eq.'N') then
	   print 72
72	   format(' Specify different model:')
	   goto 59		!get another model
	endif
c Read QDAT2, record 1 to get QD=final Q matrix with conc etc incorp,
c from last QGEN2 run, before CHARQ.DAT can be read
c	INFIL=ndev2//'\QDAT2.DAT'
182	continue
c      OPEN(unit=18,file=INFIL,status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=2048)
c	read(18,rec=1) QD,Peq,k,kA,kB,kC,kD,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig
       OPEN(unit=18,file='\QDAT.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	read(18,rec=jrecq) QD,Peq,k,kA,kB,kC,kD,nmod,imod0,mtitle,
     &   xa0,xa1,xb0,xb1,nlig
	iflag=3		!so numerical values shown in QWINDs
	CLOSE(unit=18)
3	continue	!return with new imod0 if necessary
c      OPEN(unit=14,file='D:\CHARQ.DAT',status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=3072)
      OPEN(unit=14,file='\QMODEL.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=3072)
c	read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)		!read above now
	read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     & ncyc,nsc,im,jm,kA,kB,kC,kD,titlep1,ncon
      CLOSE(unit=14)
	npar=2*ncon
	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
	irecq=(jrecq+1)/2
	print 669,mtitle,imod0,irecq
669	format(1x,a74,/,
     & ' Model #',i3,':  Rates from file #',i3,':  O.K. [Y] ? ')
	read 101,ans
59	if(UC(ans).eq.'N') then
58	   do 55 i=1,nmodc	!list the model titles
55	   print 56,i,mtits(i)
56       format(' (',i2,')',a74)
	   print 57
57	   format(' Fit model number = ')
	   read 2,imod0
	   if(imod0.lt.1.or.imod0.gt.nmodc) goto 58
	   if(pon()) write(7,670)mtitle,imod0
	   if(discprt) write(8,670)mtitle,imod0
670	   format(/,1x,a74,/,' Model number = ',i3,/)
	   print 301
301	   format(
     & ' Read rate constants from disc [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		call QZERO(QD,k)		!if not read
		iflag=1	!rate constant values initially blank, names shown
		goto 330
	   endif
C Read Q etc from disc	(default for read/write defined above as always
c write but may not always read)
cc List which disc has which model's data (from IMOD, read above)
cc IDISC=1,2,3,4,5,6  for A:,B:,C:,D:,E:,F: (or idisc=1-12 for DELL)
c	   do 180 i=1,ndisc+2	!ndisc=number of hard disc partitions
c	    if(imod(i).ne.0) then
c		print 181,char(i+64)//':',imod(i),mtits(imod(i))(1:72)
c181		format(1x,a2,' (',i2,')',a72)
c		if(imod(i).eq.imod0) idisc=i	!use as default
c	    endif
c180	   continue
c	   if(readin) then
c	     call DISCNUM1(idisc,ndev2,0,1)	!disc to read from-default in ndev2
c	   else
c	     call DISCNUM1(idisc,ndev2,0,2)	!disc to read from-default in idisc
c	   endif
c	   if(idisc.eq.-1) goto 99
c	   INFIL=ndev2//'\QDAT2.DAT'
c List files containing rate data from QDAT.DAT
	   print 180
180      format(' file #  Model #     Name of model')	!column headings
	   do i=1,nrecq
		if(jmod(i).ne.0) then
		   print 181,i,jmod(i),mtits(jmod(i))(1:72)
181		   format(1x,i2,2x,i2,1x,a72)
		   if(jmod(i).eq.imod0) irecq=i	!use as default
		endif
	   enddo
	   print 18,irecq
18	   format(' File # for data [',i2,'] = ')
	   call INPUTi(irecq)
	   jrecq=2*irecq-1
	   goto 182		!read rates
	endif
c
330	continue
c Now check ligand conc
	if(readin) then
	  do 551 j=1,nset
	   if(nlig.eq.1) then
		print 74,j,concA(j)*1.e6
74		format(' Set ',i3,
     &    ': Concentration (micromolar) = ',g13.6,' O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 75,j
75		   format(' Set ',i3,
     &       ': Concentration (micromolar) = ')
		   call INPUTr(xA)
		   concA(j)=xA*1.e-6
		endif
	   else if(nlig.eq.2) then
		print 741,j,concA(j)*1.e6,concB(j)*1.e6
741		format(' Set ',i3,
     &': Concentrations (micromolar) = ',g13.6,',',g13.6,' O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 751
751		   format(
     &       ' Concentrations (micromolar) of A, B = ')
		   call INPUT2r(xA,xB)
		   concA(j)=xA*1.e-6
		   concB(j)=xB*1.e-6
		endif
	   endif
551	  continue
	else if(.not.readin) then
	  do 552 j=1,nset
	   if(nlig.eq.1) then
	      print 76,j
76	      format(' Set ',i3,
     &      ': Concentration (micromolar) = ')
	      call INPUTr(xA)
	      concA(j)=xA*1.e-6
	   else if(nlig.eq.2) then
		print 761
761		format(
     &      ' Concentrations (micromolar) of A, B = ')
		call INPUT2r(xA,xB)
		concA(j)=xA*1.e-6
		concB(j)=xB*1.e-6
	   endif
552	  continue
	endif

	cA1=xA1	!save conc at which QD calculated in case conc altered
	cB1=xB1
c NOW REMOVE CONC FROM QD by calling QNEWC with 'new conc'=1 and keep as QT
c Concentrations for each data set have been defined already (and stored in
c concA, concB arrays) so no need to ask for conc here
	nlig=1		!one ligand only for now
	call QNEWC(QD,cA1,cB1,QT,1.0,1.0,ncdep,nlig,IL,IX,JX,k,km)
c Calc equilib occs for each set here? Need 2D array in place of PEQ?
c	do 331 i=1,nset
c	   xA0=concA(i)
c	   xB0=concB(i)
c	   call QNEWC(QT,1.0,1.0,QD,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
c	   call EQOCCd(QD,k,k-1,km,peq)	!calc new init occs at xA0
c331	continue
c
	do 671 i=1,ilast
c	print 67,(charmod(i,j),j=1,jlast)
	if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
	if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
67	format(4x,10a2)
671	continue
c
c===
c==	ncdep=0		!for now; incorp conc in initial guesses
c
	if(pon()) write(7,2431)ka,kb,kc,kd
      if(discprt) write(8,2431)ka,kb,kc,kd
2431	format(
     & ' No of states in each subset: kA,kB,kC,kD= ',4i3)
c	call PRTMOD(il)
c Get initial guesses (in QWIND2, or read as QT from Qgen2?- must do former
c because IQ(), which defines which q(i,j) corresponds to which titlep(m), and
c to which theta0(m), is defined in QWIND2 now.
c NOW use theta0(m) to hold ALL parameters, but theta(m) to hold fitted param
c 	The array IQ defines correspondence between param # (index of TITLEP and
c THETA(0),and elements of Q: IQ(i,j)=m where theta0(m) is the parameter that goes
c in QT(i,j),(though may be multiplied by a statistical factor in QT),
c ie theta0(IQ(i,j))=Q(i,j)
c Now show model + QWIND
	npar=2*ncon		!number of parameters
	ni=npar		!number of rows
	nidisp=ni
c	nidisp=4	!to test scrolling
	nr1=1		!posn of QWIND on screen
	nc1=2*jlast+7
	call CAPTSCN(scrnbuf0)	!before putting up model
	call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
c Call CHARQ to get connections (in IC(2,20)) and kA,kF
	call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
	k=kA+kF		!total number of states
	call QWIND3(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep1,IQ,DATA,itry)
	call DISPSCN(scrnbuf0)  !restore orig screen
	if(itry.eq.1) then			!re-read rates
	   ans='N'
	   goto 59
	endif
c	if(debug()) then
c	  pause
c	  do 31 m=1,npar
c         print 30,m,titlep1(m),data(m)
c         if(pon()) write(7,30) m,titlep1(m),data(m)
c         if(discprt) write(8,30) m,titlep1(m),data(m)
c30	   format(i3,3x,a10,2x,g13.6)
c31	  continue
c	endif
c
c Check allocation to subsets; states may need renumbering, and permute
c IQ also, accordingly (Peq removed for now)
	call MPERM1(QT,IQ,INEW,samenum,km)       !kA,..,kD IN COMMON
c
c NB Cycles, and conc dep parameters are read from disc and are needed
c in HJCLIK. If states renumbered, these must be too. (Constraints, fixed
c param are dealt with now, so 'renumbered' state numbers will be specified)
c No conc dep incorporated at present so just alter state numbers in IM,JM
	if(ncyc.eq.0.or.samenum) goto 36
	do 37 L=1,ncyc
	do 38 m=1,nsc(L)
	do 39 i=1,k
	if(im(L,m).eq.inew(i)) im1(L,m)=i
	if(jm(L,m).eq.inew(i)) jm1(L,m)=i
39	continue
	im(L,m)=im1(L,m)
	jm(L,m)=jm1(L,m)
38	continue
37    continue
      print 40
      if(pon()) write(7,40)
      if(discprt) write(8,40)
40	format(' After renumbering of states the cycles are:')
	do 136 L=1,ncyc
	print 126, L
	print 1311,im(L,1),jm(L,1)
	print 127,(im(L,m),jm(L,m),m=2,nsc(L))
	if(pon()) then
	  write(7,126)L
	  write(7,1311)im(L,1),jm(L,1)
	  write(7,127)(im(L,m),jm(L,m),m=2,nsc(L))
	endif
	if(discprt) then
	  write(8,126)L
	  write(8,1311)im(L,1),jm(L,1)
	  write(8,127)(im(L,m),jm(L,m),m=2,nsc(L))
	endif
126	format(' Cycle # ',i3)
1311	format(2i3,'  (calc by micro rev)')
127	format(2(5(2i3,4x),/))
136	continue
c
36	continue
c
	if(debug()) then
	   print 34
34	   FORMAT(' Type IQ matrix [N]? ')
	   read 101,ans
	   if(UC(ans).EQ.'Y') call IATYP(IQ,'  IQ =  ',pon(),k,k,km,km)
	endif
c Calculate arrays iq0(m),jq0(m) to give inverse of IQ(i,j) (faster than using
c GETIJ call) Q[iq0(m),jq0(m)]=theta0(m) for array of ALL parameters (for fitting
c theta(m) is fixed to contain only the parameters to be estimated -see below)
	do 14 m=1,npar
	 do 13 i=1,k
	 do 13 j=1,k
	   if(IQ(i,j).eq.m) then
		iq0(m)=i
		jq0(m)=j
		goto 14
	   endif
13	 continue
	 call BELL(1)
	 print 15,m
15	 format(
     & ' ERROR : parameter # ',i3,' not found in Q matrix')
14	continue
c
	print 130
      if(pon()) write(7,130)
      if(discprt) write(8,130)
130	format(/,' Initial values of rate constants')
	print 33
33	FORMAT(' Type Q matrix [Y]? ')
	read 101,ans
	if(UC(ans).ne.'N') then
         print 332
         if(pon()) write(7,332)
         if(discprt) write(8,332)
332	   format(' Q matrix without concentrations:')
	   call DATYP(QT,'  QT    ',pon(),k,k,km,km)
	   print 333
333	   format(' Type Q matrix for each data set [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then
	   do 334 i=1,nset
		xA0=concA(i)
		xB0=concB(i)
		call QNEWC(QT,1.0,1.0,QD,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
      	print 335,i,concA(i)*1.e6
	      if(pon()) write(7,335) i,concA(i)*1.e6
	      if(discprt) write(8,335) i,concA(i)*1.e6
335		format(/,1x,i3,': Q matrix at concentration = ',g13.6)
	      call DATYP(QD,'  QD    ',pon(),k,k,km,km)
334	   continue
	   endif
	endif
c
c Allocate QD elements to theta0(), using IQ (which has been reordered if
c states were reordered in MPERM)
      print 108
      if(pon()) write(7,108)
      if(discprt) write(8,108)
108	format('/')
	do 111 m=1,npar
c	 call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	 i=iq0(m)
	 j=jq0(m)
	 theta0(m)=sngl(QT(i,j))
	 thsav(m)=theta0(m)
	 titlep(m)=titlep1(IQ(i,j))	!reordered names of param
       print 121,m,i,j,titlep(m),theta0(m)
       if(pon()) write(7,121) m,i,j,titlep(m),theta0(m)
       if(discprt) write(8,121) m,i,j,titlep(m),theta0(m)
121	 format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
111	continue
c
	if(ncyc.gt.0) then
	 do 133 i=1,ncyc
	  m=IQ(im(i,1),jm(i,1))
	  jmic(m)=1		!fixed by micro rev
133	 continue
	endif
c
	if(readin) then
	  if(neq.gt.0) then
	   print 324
c324	   format(' The following parameters are constrained:')
	   do 323 n=1,neq
		i=ie(n)
		j=je(n)
		m=IQ(i,j)
c		jcon(m)=1	!define jcon (already read from init file)
		i1=if(n)
		j1=jf(n)
		m1=IQ(i1,j1)
       	print 321,m,i,j,titlep(m),efac(n),
     &	 m1,i1,j1,titlep(m1),theta0(m1)
321	 	format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
323	   continue
	   print 1161 		!OK?
c1161	   format(' Are these O.K [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') goto 212	!jfix already done
	  endif	!end of neq.ne.0
	else		!not readin
	   neq=0
	endif
2121	print 216
216	format(' No of rates constrained to be equal to a fixed ',/,
     & '    multiple of another [0] = ')
	do 316 i=1,20
316	jcon(i)=0		!in case old jcon read from disc
	read 2,neq
	if(neq.eq.0) goto 212
	 do 217 n=1,neq
	  print 218,n
218	  format(' #',i2,' . Rate # n1 = factor times rate #n2',/,
     & '   Specify: n1, factor (real), n2= ')
	  read 219,n1,efac(n),n2
219	  format(i8,g13.6,i8)
	  ie(n)=iq0(n1)
	  je(n)=jq0(n1)
	  if(n)=iq0(n2)
	  jf(n)=jq0(n2)
c Check that constrained parameter, q(ie,je), is not one of the params
c calc by micro rev
	 if(ncyc.gt.0) then
	  do 222 m=1,ncyc
	    if(ie(n).eq.im(m,1).and.je(n).eq.jm(m,1)) then
		call BELL(1)
		print 221,ie(n),je(n),m
221		format(
     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
     & ' (cycle #',i3,'). Do not constrain it!')
		goto 2121
	    endif
222	  continue
	 endif
c   Constrained param OK, so print it
	 i=ie(n)
	 j=je(n)
	 m=IQ(i,j)
	 jcon(m)=1	!define jcon
	 i1=if(n)
	 j1=jf(n)
	 m1=IQ(i1,j1)
       print 321,m,i,j,titlep(m),efac(n),
     &    m1,i1,j1,titlep(m1),theta0(m1)
	 print 1163
1163	 format(' Is this O.K [Y] ? ')
	 read 101,ans
	 if(UC(ans).eq.'N') goto 2121
217	continue
c
212	continue
c
c Fix some parameters?
c
	if(readin) then
	  if(nfix.gt.0) then
	   print 224
c224	   format(' The following parameters are fixed:')
	   do 223 m=1,npar
	    if(jfix(m).eq.1) then
		i=iq0(m)
		j=jq0(m)
       	print 121,m,i,j,titlep(m),theta0(m)
	    endif
223	   continue
	   print 1161
1161	   format(' Are these O.K [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then	!print values
c	    do 2231 m=1,npar
c	    if(jfix(m).eq.1) then
c		i=iq0(m)
c		j=jq0(m)
c       	if(pon()) write(7,121) m,i,j,titlep(m),theta0(m)
c      	if(discprt) write(8,121) m,i,j,titlep(m),theta0(m)
c	    endif
c2231	    continue
	    goto 225	!jfix already done
	   endif
	  endif
	else
	   nfix=0
	endif
1162	print 116
116	FORMAT('&Number of parameters to be fixed [0]= ')
	read 2,nfix
	do 3161 i=1,20
3161	jfix(i)=0		!in case old jfix read from disc
	if(nfix.gt.0) then
	  do 119 n=1,nfix
	   print 118,n
118	   format('&(',i2,'):   parameter #= ')
	   read 2,m
	   print 1191,titlep(m),theta0(m)
1191	   format('&',1x,a10,' fixed at ',g13.6,' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 1162
	   JFIX(m)=1		!ELEMENT OF JFIX=1 IF PARAM FIXED
c        print 120,m,theta(m)
119	  continue
	endif
225	continue
c
c Print out micro rev, constrained and fixed values now
	if(ncyc.gt.0) then
	do 139 L=1,ncyc
	 if(pon()) then
	   write(7,164)L
164	   format(/,' Cycle # ',i3)
	   write(7,165)im(L,1),jm(L,1)
165	   format(
     &  ' q(',i2,',',i2,')  calculated by microscopic reversibility')
	   write(7,166)(IM(L,M),JM(L,M),m=2,nsc(L))
166	   format('  rest of cycle: ',2(5(2i3,4x),/))
	 endif
	 if(discprt) then
	   write(8,164)L
	   write(8,165)im(L,1),jm(L,1)
	   write(8,166)(IM(L,M),JM(L,M),m=2,nsc(L))
	 endif
139	 continue
	endif
c
	if(neq.gt.0) then
	  if(pon()) write(7,324)
	  if(discprt) write(8,324)
324	  format(/,' The following parameters are constrained:')
	  do 3231 n=1,neq
		i=ie(n)
		j=je(n)
		m=IQ(i,j)
		i1=if(n)
		j1=jf(n)
		m1=IQ(i1,j1)
       	if(pon()) write(7,321)m,i,j,titlep(m),efac(n),
     &	 m1,i1,j1,titlep(m1),theta0(m1)
       	if(discprt) write(8,321)m,i,j,titlep(m),efac(n),
     &	 m1,i1,j1,titlep(m1),theta0(m1)
3231	  continue
	else
       if(pon()) write(7,162)
       if(discprt) write(8,162)
162	 format(' No parameters constrained')
	endif
c
	if(nfix.gt.0) then
       if(pon()) write(7,224)
       if(discprt) write(8,224)
224	 format(/,' The following parameters are fixed:')
	 do 160 m=1,npar
	  if(jfix(m).eq.1) then
	   i=iq0(m)
	   j=jq0(m)
         if(pon()) write(7,121) m,i,j,titlep(m),theta0(m)
         if(discprt) write(8,121) m,i,j,titlep(m),theta0(m)
	  endif
160	 continue
	else
       if(pon()) write(7,161)
       if(discprt) write(8,161)
161	 format(/,' No parameters fixed')
	endif
c
c NUMBER OF PARAMS TO ESTIMATE=kfit, kept in theta(1),...,theta(kfit)
c Now put parameters to be estimated, kfit in number, into theta(), and
c record the corresponding values of q(i,j) in IQf(i,j), and in iq1(m),jq1(m)
c exclude fixed, constrained params, and those calc by micro rev
	kfit=npar-nfix-neq-ncyc
	ik=0		!use to check vs kfit
	do 131 m=1,npar
	if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
	   ik=ik+1
	   i=iq0(m)
	   j=jq0(m)
	   iq1(ik)=i
	   jq1(ik)=j
	   IQf(i,j)=m
	   theta(ik)=sngl(QT(i,j))
	endif
131	continue
	if(kfit.ne.ik) then
	   call BELL(2)
	   print 135,ik,kfit
135	   format(' ERROR ik,kfit = ',2i8)
	endif
c
	print 1351,kfit,(theta(i),i=1,kfit)
	if(pon()) write(7,1351) kfit,(theta(i),i=1,kfit)
	if(discprt) write(8,1351) kfit,(theta(i),i=1,kfit)
1351	format(/,' Fit ',i3,' parameters: initial theta() = ',/,
     & 5g13.6,/,5g13.6)
c Set other inputs for Simplex
	errfac=1.e-3
	delmin=-1.		!do not use delmin for convergence
	stpfac=0.1
	nevm=15000
c	if(defolt) goto 526
	print 525
525	format(' Relative error [1.e-3] = ')
	call INPUTr(x)
	if(x.gt.1.e-10) errfac=x
cc	if(errfac.lt.0.) delmin=-errfac	!use delmin criterion
c526	continue
c
	print 523,errfac,ndisp
523	FORMAT(' ERROR=guess*',g13.6,/,
     & ' Print every Nth estimate [',i3,'] :  N= ')
	read 2,i
	if(i.ge.1) NDISP=i
c
c Get likelihood for initial guesses eg to determine scale factor
	first=.true.		!get openings/group etc in HJCLIK
	oneset=.false.	!so calcs done for ALL data sets in HJCLIK
c	if(idsav.eq.8.or.idsav.eq.9) idebug=idsav			!switch debug on
	sm=HJCLIK(kfit,THETA)		!for init guesses
c Only the param to be estimated are in theta(), so use kfit in call, and
c call with JFIX1 set to zeros, so none of the kfit param are fixed
c	if(idsav.eq.8.or.idsav.eq.9) idebug=1			!switch debug off
      print 112,-sm
      if(pon()) write(7,112) -sm
      if(discprt) write(8,112) -sm
112	format(/,
     & ' For initial guesses:  log(likelihood) = ',g13.6)
	do 110 j=1,nset
         print 12,j,fcomp(j),ngp(j),an(j),nscal(j)
         if(pon()) write(7,12) j,fcomp(j),ngp(j),an(j),nscal(j)
         if(discprt) write(8,12) j,fcomp(j),ngp(j),an(j),nscal(j)
12	format(' Set ',i3,': likelihood = ',g13.6,/,
     & 1x,i4,' groups: mean no of openings/group = ',g13.6,/,
     & ' (likelihood scaled down ',i4,' times by 1.e-100)')
110	continue
      print 113
      if(pon()) write(7,113)
      if(discprt) write(8,113)
113	format(/,
     &'  Note: if a particular set of parameter values causes the',/,
     &' likelihood for a group to appear <0, the log(lik) for this',/,
     &' group is taken as zero, thus penalizing these parameters',/)
c
	call DCASK('Skip fit and plot initial parameters',ans1,ans)
	ans1=ans		!new default
	if(ans1.eq.'Y') GOTO 990		!TO TEST GRAPHICS
c
c
c	pause 'Before Simplex'
	call TIME(ctime)
	print 61,ctime
	if(discprt) write(8,61) ctime
61	format(/,' Simplex started at ',a11)
	call SIMPLEX(kfit,THETA,stpfac,errfac,nev,nevm,
     & smin,HJCLIK,Ndisp,jfix1,delmin)
	call TIME(ctime)
      print 5233,ctime
      if(pon()) write(7,5233) ctime
      if(discprt) write(8,5233) ctime
5233	FORMAT(/,' End of fitting: ',a11)
c
990	CONTINUE		!JUMP HERE TO TEST GRAPHICS
c True params for CH82 model
	thtrue(1)=3000.
	thtrue(2)=15.
	thtrue(3)=5.e8
	thtrue(4)=0.666667
	thtrue(5)=500.
	thtrue(6)=15000.
	thtrue(7)=4000.
	thtrue(8)=5.e8
	thtrue(9)=2000.
	thtrue(10)=1.e8	!=2k(+1)
c Print results
      print 24,(theta(j),j=1,kfit)
      if(pon()) write(7,24) (theta(j),j=1,kfit)
      if(discprt) write(8,24) (theta(j),j=1,kfit)
24	format(' theta = ',5g13.6,/,5g13.6)
c
c Note that final parameter estimates are in THETA(); values in it may have
c been modified in Simplex since the last calculation of QD (e.g. to output
c best vertex, or abs min). Therefore now remake QD from theta (use jset=1,
c arbitrarily)
c
	jset=1
	call QSET_HJC(jset,theta,kfit,k)
c Now remove conc from QD by calling QNEWC with 'new conc'=1 and keep as QT
	if(ncdep.gt.0) then
c	  xA=concA(jsetlast)	!=conc last used to define QD
c	  xB=concB(jsetlast)	!=conc last used to define QD
	  xA=concA(jset)	!=conc last used to define QD
	  xB=concB(jset)	!=conc last used to define QD
	  call QNEWC(QD,xA,xB,QT,1.0,1.0,ncdep,nlig,IL,IX,JX,k,km)
	endif
	print 137
      if(pon()) write(7,137)
      if(discprt) write(8,137)
137	format(/,' Final values of rate constants')
	call DATYP(QT,'  QT    ',pon(),k,k,km,km)
	print 333
c333	format(' Type Q matrix for each data set [Y]? ')
	read 101,ans
	if(UC(ans).ne.'N') then
	   do 336 i=1,nset
		xA0=concA(i)
		xB0=concB(i)
		call QNEWC(QT,1.0,1.0,QD,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
      	print 335,i,concA(i)*1.e6
	      if(pon()) write(7,335) i,concA(i)*1.e6
	      if(discprt) write(8,335) i,concA(i)*1.e6
c335		format(/,1x,i3,': Q matrix at concentration = ',g13.6)
	      call DATYP(QD,'  QD    ',pon(),k,k,km,km)
336      continue
	endif
c Allocate QD elements to theta0(), using IQ
	if(iscan.ne.-3001) then
         print 1371
         if(pon()) write(7,1371)
         if(discprt) write(8,1371)
1371	   format(/,23x,' initial         final')
	else
         print 1372
         if(pon()) write(7,1372)
         if(discprt) write(8,1372)
1372	   format(/,27x,' true        initial       final')
	endif
	do 138 m=1,npar
	 i=iq0(m)
	 j=jq0(m)
c	 theta0(m)=sngl(QD(i,j))
	 theta0(m)=sngl(QT(i,j))
	 if(iscan.ne.-3001) then
         print 1210,m,i,j,titlep(m),thsav(m),theta0(m)
         if(pon()) write(7,1210) m,i,j,titlep(m),thsav(m),theta0(m)
         if(discprt) write(8,1210) m,i,j,titlep(m),thsav(m),theta0(m)
1210	 format(i3,1x,' q(',i2,',',i2,')=',1x,a10,2g13.6)
	 else
         print 1211,m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
         if(pon()) write(7,1211) m,i,j,titlep(m),thtrue(m),thsav(m),
     &	theta0(m)
         if(discprt) write(8,1211) m,i,j,titlep(m),thtrue(m),thsav(m),
     &	theta0(m)
1211	 format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6)
	 endif
	 if(jmic(m).eq.1) then
         print 1212
         if(pon()) write(7,1212)
         if(discprt) write(8,1212)
1212	   format('& (micro-rev)')
	 else if(jcon(m).eq.1) then
         print 1213
         if(pon()) write(7,1213)
         if(discprt) write(8,1213)
1213	   format('& (constrained)')
	 else if(jfix(m).eq.1) then
         print 1214
         if(pon()) write(7,1214)
         if(discprt) write(8,1214)
1214	   format('& (fixed)')
c	 else
c         print 108
c         if(pon()) write(7,108)
c         if(discprt) write(8,108)
c108	   format('/')
	 endif
138	continue
c
      print 23,nev,-smin
      if(pon()) write(7,23) nev,-smin
      if(discprt) write(8,23) nev,-smin
23	format(/,
     &' Number of evaluations = ',i8,' Max log(likelihood) = ',g13.6)
	pause
c  Do calculations again with final estimates to print pdfs etc if
      print 108
      if(pon()) write(7,108)
      if(discprt) write(8,108)
c108	format('/')
      print 109
      if(pon()) write(7,109)
      if(discprt) write(8,109)
109	format(' VALUES CALCULATED FROM FINAL FIT')
	do 114 jset=1,nset
         print 267,jset,concA(jset)*1.e6
         if(pon()) write(7,267) jset,concA(jset)*1.e6
         if(discprt) write(8,267) jset,concA(jset)*1.e6
267      format(/,
     &' Equilibrium values for set number ',i3,': concentration = ',
     &  g13.6,/,
     &' ------------------------------------------------------------')
	   xA0=concA(jset)
	   xB0=concB(jset)
c Make QD for the specified concentration(s)
	   call QNEWC(QT,1.0,1.0,QD,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
c (a) Calc equilibrium occupancies
	   call EQOCCd(QD,k,k-1,km,peq)
	   call SLIFEn(QD,Peq,inew,km,km)
114	continue
c (b) Print final HJC distributions
c -set idebug=8 so HJCLIK prints values (for all data sets)
c	if(idsav.eq.8.or.idsav.eq.9) then
	   idebug=idsav			!save idebug
	   idebug=8				!switch debug on
	   sm=HJCLIK(kfit,THETA)	!for final estimates from final QD
	   idebug=idsav			!restore idebug
c	endif
c
c Now options to plot data with final fit superimposed on it (based on EKDIST)
c Call HJCDISP with QT and put required concentrations later
	iplot=1
c
	call HJCDISP(iplot,tint,nint,burst,rootA,rootF,theta,
     & tres,tcrit,QT,nlig,ylo,yhi,nval,readin,kfit,k,km)
	oneset=.false.		!in case more fits done
c
c Write defaults back to disc?
	if(readin) then
	   call DCASK('Write defaults to disc','N',ans)
	else
	   call DCASK('Write defaults to disc','Y',ans)
	endif
cc NB write ndev2 back to .INI file
c Write irecq=position of rate data, back to .INI
	irecq=(jrecq+1)/2
      if(ans.eq.'Y') then
         OPEN(unit=19,file='HJCFIT.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=3072)
	   write(19,rec=1) ndevs,tres*1.d6,nfix,jfix,neq,IE,JE,EFAC,
     & IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,ylo,yhi,nval,idiskq,
     & nfile,kfile,nset,concA,concB,tcrit,burst
	   CLOSE(unit=19)
	endif
c
99	continue
	call ENDPRINT
	end

	integer*2 function IAVAL(i,j)
c (Unlike version in EKDIST, this returns int*2 value)
c VERSION for HJCLIK with 2D array
c Version with IAMPL in COMMON can now replace IACAL
C To get amplitude in ADC units from IAMPL(i). Removes 10000 if
C this has been added  (for pos amp) or subtracted (for neg amp)
C to indicate 'dubious' value .
	integer*2 IAMPL(20480,10)
	integer*2 ix
	COMMON/AMPBLK/iampl
C
	ix=0
	if(i.eq.0) goto 1
	ix=IAMPL(i,j)
	if(ix.gt.9999) ix=ix-10000
	if(ix.lt.-9999) ix=ix+10000
1	IAVAL=ix
	RETURN
	END


