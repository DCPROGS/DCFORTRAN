	subroutine QDEF(QD,PI,pinf,QT,IQ,titlep,titles,npar,
     & nlig,IL,readp)
c
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
c viz: write(18,rec=4) nmod,theta,nchan,gamma,nvdep,iv,jv,hpar
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
c     & HPAR,IV,JV,NVDEP,nchan,gamma
c QDAT.DAT (recl=4096)
c rec=1 to pass stuff to other progs
c After 01/21/91 08:04am added ncdep,IL(),IX(),JX(10) to rec=1 to enable
c calcs to be done with conc that varies with time (in EPSCSIM)
c	   write(18,rec=1) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
c     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
c Values in records #2,3 needed for rerun of prog
c	write(18,rec=2) QD,QT,Pinf,k,ka,kb,kc,kd,nmod,vref,Vkin,IR,
c     & PZERO,PSTAR,KMFAST,KMCON,aka1,aka2,BA
c	write(18,rec=3) NCON,IC,NCYC,NSC,ICYC,IM,JM,nspec,IS,JS,neq,
c     & IE,JE,IF,JF,EFAC,ncdep,IX,JX,nchan,HPAR,IV,JV,NVDEP,gamma
c THUS NOW USE:
c	CHARQ.DAT (recl=3072)
c       read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
c     & ncyc,nsc,im,jm,kB,kC,kD,titlep,ncon
c	QDAT2.DAT (recl=2048 is big enough- 4096 not needed)
c	(1) rec=2 for rerun
c	   (18,rec=2) QT,Pinf,nmod,imod0,vref,Vkin,PZERO,PSTAR,KMFAST,KMCON,
c     & HPAR,IV,JV,NVDEP,nchan,gamma
c	(2) rec=1 for other progs (same as orig QDAT.DAT rec=1 except that
c		imod0 added)
c	   write(18,rec=1) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
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
	real*8 QT(10,10),QD(10,10),P0(10),Pinf(10),pzero(10)
	real*8 Peq(10),gamma(10)	!use Peq as arg for EQOCC
	real*8 Qdum(10,10)
	character*10 titlep(50),titlep1(50)
	character*8 titles(10)
	character*74 mtitle	!title for MODWIND models
	character*74 mtits(100)	!to read all ditto from disc
c	character*74 rtitle(40)	!title to describe rate constants
	character*74 rtitle(100)	!title to describe rate constants
	character*2 charmod(20,30),charmod1(20,30)
	real*4 PI(10,10)
	integer SCRNBUF0(1024)
c	integer IQ(10,10),jmod(40)
	integer IQ(10,10),jmod(100)
C QM IS TO HOLD MINORS OF Q IN EQOCC. SEEMS THAT IT MUST BE
C DECLARED IN MAIN PROG FOR CORRECT CALCS (SEE COMMENTS IN EQOCC)
C **Do not ned this when DETD or DETV rather than DETERM used to
c calc determinants in EQOCC
	REAL*4 PSTAR(4)		!KM2 OCCS
c	real*4 THETA(25)	!contains all param
	integer IC(2,20)
	integer NSC(8),IM(8,15),JM(8,15),IX(10),JX(10),IL(10)
	integer NSC1(8),IM1(8,15),JM1(8,15),IX1(10),JX1(10),IL1(10)
	integer kmcon(9)
	character*1 ans,UC
	LOGICAL READP,KMFAST,ranrate,newmod,sameq,vjump,cjump
	logical pon,slock,debug,caplock
	logical discprt
c
	common/dp/discprt
	COMMON/KM2/AKA1,BA,PSTAR,KMCON,KMFAST,aka2,arat	!for km2occ
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	common/Vval/gamma,vhold,vkin,vref,enchan		!V in mV
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
	integer IS(10),JS(10)	!declare for disc-write-not used now
	COMMON/SPAR/NSPEC,IS,JS		!special parameters
	COMMON/dimblk/K,KMAX,ir,nmod
	COMMON/KBLK/kA,kB,kC,kD
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
	common/equil/ak1,ak2,ba2,bab,akb,akd	!for km2par,main,qdef1
c
c for eqoccd, getqd
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed

c
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	sameq=.false.	!hangover from earlier version
	nlig=1		!one ligand only, unless redefined below
	do i=1,10
	  IL(i)=1		!ligand #1 unless redefined
	enddo
c
	vref=-80.
	print 36,vref
36	format(' Reference potential (mV)= ',f9.2,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   print 38
38	   format(' Reference potential (mV)= ')
	   call INPUTr(vref)
	endif
	if(pon()) write(7,33)vref
      if(discprt) write(8,33)vref
33	format(' Reference potential (mV)= ',f9.2)
c (end of bit from old QMOD)
	nmod=0		!hangover from old QMOD2
	kmax=10
	neq=0			!no constraints
	nspec=0
	ranrate=.false.	!random rate constants (nmod=0 only)
	Vkin=Vref		!unless reset
	readp=.false.
c===	if(ichan2.eq.2) sameq=.true.	!to omit questions 2nd time
c
c Check whether to use macro rev before calling GETQD
	ans='Y'
	obeymr=.true.
	call DCASK(
     & 'Constrain rates to obey microscopic reversibility',ans,ans)
	if(ans.eq.'N') then
	   print 114
	   if(discprt) write(8,114)
114	   format(/,' Rates may NOT obey microscopic reversibility',/)
	   obeymr=.false.
	   ncyc=0
	else
	   print 115
	   if(discprt) write(8,115)
115	   format(/,
     &	' Rates constrained to obey microscopic reversibility',/)
	endif
	call BELL(1)
	print 40
40	format(/,
     & ' NB The following files are now expected to be present in',/,
     & ' the directory that contains this program: qgen.ini, ',/,
     & ' qmodel.dat, qdat.dat and (if number of models > 41) ',/,
     & ' then also qtitle.dat'/)
	pause
c
c SECTION FOR NMOD=0
	kmfast=.false.	!for all models so far
c
c Section to use model specified in window (nmod=0,iopt=2)
673	iflag=0   !not read from disc- new one typed into window
	nrow=20	!for MODWIND
	ncol=30
c New version: replace QDISC with QGEN.INI, and replace multiple QDAT2.DAT
c files with a single multi-record file called QDAT.DAT (see QDFIX.FOR)
c In old version:
c	QDAT2 used two records, of recl=2048 (3rd record used only for 2-channel
c	case which is not in at present -make a sep file for it when needed)
c	QDISC recorded which model number had data (QDAT2) on each disk
c	 -array IMOD(i) = model number (imod0) on the ith disc (i=1-6
c 	 for disc=A-F Tosh, or i=1-12 for A: to L: on the DELL) -also recorded
c	 idisc=ndev for disc used for QDAT2 in last run.
c New version: Rather than keeping disc, keep record number in QDAT where
c	data is: irecq=1,2,3,...,(actual 1st record # is jrecq=2*irecq-1=1,3,5,...
c	since each data set uses 2 records). Thus JMOD(irecq)=model # (imod0)
c	that has data located at point in QDAT.DAT defined by irecq
c	(irecq=1,2,3,...,nrecq; nrecq=number of data sets currently defined)
c
      OPEN(unit=17,file='QGEN.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(17,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=17)
c
c Read all titles for sets of rate constants initially here (otherwise
c reading rtitle() involves read also QT, nchan etc so any values of these
c set here may be overwritten
      OPEN(unit=18,file='QDAT.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	do i=1,nrecq
	   jrecq=2*i	!read SECOND record of data
	   read(18,rec=jrecq) QT,Pinf,nmod,imod1,vref,Vkin,PZERO,PSTAR,
     &	 KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,gamma,rtitle(i)
	enddo
	CLOSE(unit=18)
c Bit done when new or stored model requested (not sameQ)
	iflag=1		!so charmod displayed
      OPEN(unit=14,file='QMODEL.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=3072)
c Modify so that 1st record contains number of models recorded (nmodc)
c + titles of them- list these first. Details of model #i kept in rec=i+1.
c The 1st record has space for 41 titles + nmodc
c Modif 01/11/00 03:47pm If nmodc>41 then look for titles in a new
c file QTITLE.DAT, which is big enough to hold 100 titles
c This is more economical than increasing the record length in
c qmodel.dat because 3072 bytes is long enough for all records except
c the first: size of qmodel.dat=(nmodc+1)*3072
	read(14,rec=1) nmodc
	if(nmodc.gt.41) then	!look for qtitles.dat
         OPEN(unit=15,file='QTITLE.DAT',status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=10240)
	   read(15,rec=1) nmodc,(mtits(i),i=1,nmodc)
	   CLOSE(unit=15)
	else
	   read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
	endif
58	continue
	newmod=.false.
	print 561,0
561   format(' (',i2,')',' New model to be defined')
	do i=1,nmodc	!list the model titles
	   print 56,i,mtits(i)
56       format(' (',i2,')',a74)
	   if(mod(i,20).eq.0) then
		call BELL(1)
		print 11
11		format(' Hit any key for more')
		call ANYKEY
	   endif
	enddo
	call BELL(2)
	print 57
57	format('  Model number = ')
c577	call INPUTi(imod0)
c577	call INPUTi(imod0)
	read*,imod0
	if(imod0.gt.nmodc) goto 58
	if(imod0.le.0) then
	   imod0=nmodc		!so says next model #=nmodc+1 below
	   iflag=0   !not read from disc- new one typed into window
	   newmod=.true.
         CLOSE(unit=14)
	   goto 3
	endif
	mtitle=mtits(imod0)
	read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     & ncyc,nsc,im,jm,kA,kB,kC,kD,titlep,ncon
      CLOSE(unit=14)
	nrow=ilast+6	!for MODWIND
	ncol=jlast+6
3	continue
	call MODWIND(charmod,nrow,ncol,iflag,ilast,jlast)
	call CQLAST(charmod,ilast,jlast)
c Call CHARQ to get connections (in IC(2,20)) and kA,kF
	call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
	k=kA+kF		!total number of states
c Can print CHARMOD on printer to keep record of model used?
c Even if existing model was read, it may have been altered in MODWIND so
c check that cycles etc are OK;
c iflag=0 = it was read but may have been altered, so kB,kC,nlig etc may need
c	 changing OR new model, in which case nlig etc must be set
c iflag=1 means read, and not changed- just print values
	if(iflag.eq.0.or.iflag.eq.1) then
323	   print 2011,kA,kF
2011	   FORMAT('  number of open and shut states, kA, kF = ',2i5,/)
	   if(newmod) goto 155
	   print 2012,kB,kC,kD
2012	   format(' kB,kC,kD = ',3i5,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') goto 156
155	   print 202	!kB=
202	   FORMAT('&  number of short shut states, kB= ')
	   call INPUTi(kB)
	   print 203   !kC=
203	   FORMAT('&  number of long-lived shut states C, kC= ')
	   call INPUTi(kC)
	   print 204   !kD=
204	   FORMAT(
     &    '&  number very long (between cluster) shut states, kD= ')
	   call INPUTi(kD)
	   if((kA+kB+kC+kD).ne.k) goto 323
156	   continue
	   if(pon()) write(7,2431)ka,kb,kc,kd
         if(discprt) write(8,2431)ka,kb,kc,kd
2431	   format(
     & ' No of states in each subset: kA,kB,kC,kD= ',4i3)
	endif
	if(iflag.eq.0) then	!read, but possibly changed/or not read
	   call CYCQ(k,ncon,ic,ncyc,nsc,im,jm)
c In GET*** subroutines:
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c in GETCYC iopt=3 asks for rates to be calc by micro rev only
	   call GETCYC(3)	!get micro rev rate only
	   call GETLIG(2,il,nlig)	!ditto for ligands
	else if(iflag.eq.1) then 	!read from disk and not changed
	   call GETCYC(0)	!set micro rev states
	   call GETLIG(0,il,nlig)	!ditto for ligands
	endif
c#####check connections with getcon?
c Print final details for model
c	print 670,mtitle,imod0
59	continue
	if(.not.newmod) then
	   if(pon()) write(7,670)mtitle,imod0
	   if(discprt) write(8,670)mtitle,imod0
670	   format(/,1x,a74,/,' Model number = ',i3,/)
	   do 671 i=1,ilast
c	   print 67,(charmod(i,j),j=1,jlast)
	   if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
	   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
67	   format(4x,10a2)
671	   continue
	endif
	call PRTMOD(il)
	call DCASK('This model O.K.','y',ans)
	if(ans.eq.'N') then
         OPEN(unit=14,file='QMODEL.DAT',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=3072)	!re-open file
	   goto 58
	endif
c
c Model finished; now check whether to read numerical values from disc
c for display in QWIND2
c When iflag=3 input values of both rate constant names and values shown
c      iflag=2 rate constant names  initially blank, values shown
c      iflag=1 rate constant values initially blank, names shown
c      iflag=0 both cols initially blank.
c
	iflag=1
	if(ranrate) then
c	   Random allocation of rates
C        ALLOCATE ALL Q(I,J) RANDOMLY WITH SLOW RATES FOR LEAVING C-STATES ETC
	   ncdep=0
	   nvdep=0
	   call GETCYC(3)	!get micro rev rate only
	   call GETRAN(QT,ic)		!allocate QT
	   goto 330
	endif
c
305	continue
	print 301,vref
301	FORMAT(
     & ' Read rate constants (at Vref=',f4.0,'mV) from disc [Y] ? ')
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
	do i=1,nrecq
	   if(jmod(i).eq.imod0) n=n+1
	enddo
	if(n.eq.0) then
	   print 191
191	   format(' No rate constants have been stored for this model')
c      iflag=1 rate constant values initially blank, names shown
	   iflag=1
	else
	   print 190,imod0,mtits(imod0)(1:74)
190	   format(' For model #',i3,': ',a74,/,
     &   ' the following rate constants have been stored:',/,
     &   '   file #:                      Name of rate file')
	   do i=1,nrecq
		if(jmod(i).eq.imod0) then
		   print 192,i,rtitle(i)
192		   format(1x,i3,': ',a74)
		   irecq=i	!use as default
		endif
	   enddo
	   print 15,irecq
15	   format(' File # for rates [',i2,'] = ')
	   call INPUTi(irecq)
c	   imodsav=imod0
	   jrecq=2*irecq-1
	endif
c	print 180
c180	format(' Model #:                          Name of model',/,
c     &	 '  (Rate constants  file #:         Name of rates)')
cc180	format(' file #  Model #     Name of model')	!column headings
c	irecq=nrecq+1	!default unless redefined below
c	do i=1,nrecq
c	   if(jmod(i).ne.0) then
c		jrecq=2*i	!read SECOND record of data
c		print 181,jmod(i),mtits(jmod(i))(1:69),i,rtitle(i)(1:65)
c181		format(' Model ',i2,':',1x,a69,/,
c     &	' Rate file ',i2,':',1x,a65,/)
cc181		format(1x,i2,':',1x,a74,/,'  (',i2,':',1x,a72,')')
ccc		print 181,i,jmod(i),mtits(jmod(i))(1:72)
ccc181		format(1x,i2,2x,i2,1x,a72)
c		if(jmod(i).eq.imod0) irecq=i	!use as default
c		if(mod(i,6).eq.0) then
c		   print 11
c11		   format(' Hit any key for more')
c		   call ANYKEY
c		   print 180		!repeat headings
c		endif
c	   endif
c	enddo
c	print 182,nrecq+1,irecq
c
c Read data
      OPEN(unit=18,file='QDAT.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	jrecq=2*irecq	!read SECOND record of data
	read(18,rec=jrecq) QT,Pinf,nmod,imod1,vref,Vkin,PZERO,PSTAR,
     & KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,gamma,rtitle(irecq)
	CLOSE(unit=18)
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
c	if(nmod.gt.0) goto 306
c
c Now show model + QWIND
	ni=2*ncon	!number of rows
	npar=ni
	nidisp=ni
c	nidisp=4	!to test scrolling
	nr1=1		!posn of QWIND on screen
	nc1=2*jlast+7
	call CAPTSCN(scrnbuf0)	!before putting up model
	call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
	call QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,itry)
	call DISPSCN(scrnbuf0)  !restore orig screen
	if(itry.eq.1) goto 305	!reread rates (from another disc?)
c
641	print 4
4	format(/' Save model on disc [N] ? ')
	read 101,ans
      if(UC(ans).ne.'Y') goto 5
c
	if(newmod) then
c	   call LABENT(' Enter title for model',mtitle,74)
	   call TITENT0(' Enter title for model',mtitle,74,.true.)
	else
	   call TITENT0(' Enter title for model',mtitle,74,.false.)
c	   print 6721,mtitle
c6721	   format(1x,a74,/,' :  title O.K. [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).eq.'N') then
c		call LABENT(' Enter title for model',mtitle,74)
c	   endif
	endif
c
676	continue
	iw=0		!initialise
	if(newmod) then
	   print 8,nmodc+1
8        format('  Next unused model number =',i3,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then	!yes
		nmodc=nmodc+1		!increment # of models
		imod0=nmodc		!new model number
		mtits(imod0)=mtitle		!new title
		goto 802		!write to disc
	   endif
	else
	   print 801,imod0,nmodc+1,imod0
801      format(
     & ' Present model number = ',i3,': next unused model number = ',i3,
     &   /,' Replace present model #',i3,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then	!yes
		mtits(imod0)=mtitle		!in case title changed
		goto 802		!write to disc as imod0, nmodc unchanged
	   endif
	endif
c If default model number above was NOT approved (ans=no) then arrive here
	print 675,nmodc
675	format('&model number (=<',i3,') = ')
	call INPUTi(imod0)
	if(imod0.gt.nmodc) goto 676
c NB if moved up then model numbers for rate constants etc in QDAT2.DAT will
c be wrong
	print 674,imod0,mtits(imod0)
674	format(' Present model #',i3,' has title =',/,1x,a74,')',/,
     &   ' (1) Overwrite present model',/,
     &   ' (2) Move present model, and all above it, up one place',/,
     &   ' (3) Abandon ',/,
     &   ' Option number [3] = ')
	call INPUTi(iw)
	if(iw.le.0.or.iw.ge.3) goto 641
	if(iw.eq.2) nmodc=nmodc+1
	if(iw.eq.1) mtits(imod0)=mtitle		!new title
c
802	continue
c For new model, print model # and charmod here
	if(newmod) then
	   if(pon()) write(7,670)mtitle,imod0
	   if(discprt) write(8,670)mtitle,imod0
	   do 672 i=1,ilast
	   if(pon()) write(7,67) (charmod(i,j),j=1,jlast)
	   if(discprt) write(8,67) (charmod(i,j),j=1,jlast)
672	   continue
	endif
c
c now get details of model from QMODEL.DAT (prev called CHARQ)
      OPEN(unit=14,file='QMODEL.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=3072)
c If iw=2 then all existing models with numbers => imod0 must be read in
c and moved up one place. Also update imod() which records which model
c has data (rates etc) on each disc
	if(iw.eq.2) then
	   do 50 i=nmodc-1,imod0,-1	!nmodc already inc so nmodc-1=orig nmodc
		mtits(i+1)=mtits(i)
		read(14,rec=i+1) charmod1,ilast1,jlast1,ncdep1,ix1,jx1,il1,
     &      ncyc1,nsc1,im1,jm1,kA1,kB1,kC1,kD1,titlep1,ncon1
		write(14,rec=i+2) charmod1,ilast1,jlast1,ncdep1,ix1,jx1,il1,
     &      ncyc1,nsc1,im1,jm1,kA1,kB1,kC1,kD1,titlep1,ncon1
50	   continue
	   mtits(imod0)=mtitle		!insert new title after others moved up
	   do 51 i=1,10
		if(jmod(i).ge.imod0) jmod(i)=jmod(i)+1
51	   continue
	endif
c
	write(14,rec=1) nmodc
	if(nmodc.gt.41) then	!write mtits() to qtitles.dat
         OPEN(unit=15,file='QTITLE.DAT',status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=10240)
	   write(15,rec=1) nmodc,(mtits(i),i=1,nmodc)
         CLOSE(unit=15)
	else
	   write(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
	endif
	write(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     & ncyc,nsc,im,jm,kA,kB,kC,kD,titlep,ncon
c NB recl is in bytes (4*value for PDP)
      CLOSE(unit=14)
	if(nmodc.le.41) then
         print 8031,imod0
         if(discprt) write(8,8031) imod0
8031     format(
     &  ' Details of model stored, as model #',i3,
     &  ' , on disk (in QMODEL.DAT)',/)
	else
         print 8032,imod0
         if(discprt) write(8,8032) imod0
8032     format(
     &  ' Details of model stored, as model #',i3,
     &  ' , on disk (in QMODEL.DAT and QTITLE.DAT)',/)
	endif
5	continue
c
	call GETCHAN(nchan,gamma,readp,sameq,idest)
	if(pon()) write(7,320) NCHAN,kA,(1.0d12*gamma(i),i=1,kA)
      if(discprt) write(8,320) NCHAN,kA,(1.0d12*gamma(i),i=1,kA)
320	FORMAT(/,' Number of channels= ',i10,/,
     & 3X,I3,' open states, with gamma (pS)=', 5G13.6,/)
	CFkin=nchan*Vkin*1.0E-3*1.0E9		!FOR nA at potential of expt
c Now V-dependence
	call GETVDEP(nmod,k,titlep,npar,IQ,readp,sameq,
     & vkin,vhold,vref,idest)
140	continue
c	goto 30		!get conc etc
c end of NMOD=0 section
c30	continue
C OMIT CONSTRAINTS FOR NOW- but leave arrays in to avoid extensive
c modifs to QSETD (not kept on disc now)
	NEQ=0
c2121	print 216
c216	format(' No of q(i,j) constrained to be equal to a fixed ',/,
c     & '    multiple of another (-1 for old)= ')
c	read 10,L
c	if(L.lt.0) goto 212
c	neq=L
c	if(neq.eq.0) goto 212
c	do 217 L=1,neq
c	print 218,L
c218	format(' #',i2,' . Element i,j= fac times element 1i,j1',/,
c     & '   Specify: i,j,fac,i1,j1= ')
c217	read 219,IE(L),JE(L),EFAC(L),IF(L),JF(L)
c219	FORMAT(2I8,G13.6,2I8)
c
	if(nvdep.eq.0) goto 37		!use Vkin=Vref set above
	print 1001,vkin
1001	FORMAT(
     & ' Membrane potential (E-Erev) (real mV)= ',f6.1,':  O.K.[Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 324
	print 1002
1002	FORMAT(
     & ' Membrane potential (E-Erev) (real mV)= ')
	call INPUTr(vkin)
324	continue
	if(pon()) write(7,32) vkin
      if(discprt) write(8,32) vkin
32	FORMAT(/,' Membrane potential (E-Erev) = ',g13.6,' mV')
  	vhold=vkin		!pot at which kinetics measured
	V1=VHOLD-Vref		!V1 DEC WITH HYPERPOL- define for QSETD
	CFkin=float(NCHAN)*Vkin*1.0E-3*1.0E9	!FOR nA at potential of expt
C
C NOW GET AGONIST CONC.
37	if((.not.kmfast).and.ncdep.eq.0) goto 504	!conc not needed
24	continue
311	print 16
16	FORMAT(' Concentration (micromolar) = ')
	call INPUTr(xA)
2	FORMAT(G13.6)
	if(pon()) write(7,163)1,xa
      if(discprt) write(8,163)1,xa
163	FORMAT(' Ligand #',i2,': concentration (micromolar) = ',g13.6)
	XA=XA*1.0E-6		!=MOLAR
c	xa0=xa            !for cjump may be redefined as xa1 below
	if(nlig.eq.1) goto 241
	print 161
161	FORMAT(' Concentration of ligand #2 (micromolar) = ')
	call INPUTr(xB)
	if(pon()) write(7,163)2,xb
      if(discprt) write(8,163)2,xb
	xb=xb*1.e-6		!molar
c	xb0=xb            !for cjump may be redefined as xb1 below
	if(nlig.ne.1.and.nlig.ne.2) print 162
162	format(' ERROR: not yet written for more than 2 ligands')
c
241	continue
c
	IF(KMFAST) CALL KM2OCC(XA)
504	CONTINUE
c calc QD to get init occs
	call QZERO(QD,k)
	call QSETD(XA,XB,IL,V1,QT,QD,K,.false.)
c
c Print out final values of rate constants used (inc those determined
c by micro rev?)
c   -problem here if conc=0. (eg as initial conc for a conc jump) since
c qd/xA=r/xA=0/0. Solve this by doing special call to QSETD with conc=1.0,
c and call result QDUM, used only for printing rate constants (also removes
c need to check which rates are conc-dep) (in fact can just set epsim=true
c so that rates are not multiplied by conc, so xA,xB ignored)
	call QZERO(Qdum,k)
	call QSETD(1.0,1.0,IL,V1,QT,Qdum,k,.true.)
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
	CALL EQOCCd(QD,K,K-1,KMAX,Peq)		!CALC EQUILIB OCCS
c
C CALC CURRENT FOR THIS (or these) conc (now all in SLIFED1)
	call SLIFEd1(QD,Peq,KMAX,KMAX)
	CUR=0.0
	DO 319 I=1,kA
319	CUR=CUR+sngl(Peq(I)*gamma(I))
	CUR=CUR*CFkin
	IF(KMFAST) CUR=CUR*PSTAR(1)	!FRACT OF STATE 1 THAT IS OPEN
	print 421,Vkin,1.e3*CUR
421	FORMAT( ' Current (pA), at ',F6.1,' mV= ',g13.6)
3161	continue
c
	IF(ncdep.eq.0) GOTO 317
	print 316
316	FORMAT( '  O.K.[Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 311   !GET NEW CONC
C
C Calc PI and TYPE RESULTS
317	continue
	DO 20 I=1,K
	DO 20 J=1,K
	pi(i,j)=-1.		!in case denom=0
	if(dabs(qd(i,i)).lt.1.d-25) goto 20
	PI(I,J)=SNGL(QD(I,J)/(-QD(I,I)))
20	continue
	print 400
400	FORMAT( ' Type Q and PI matrices [Y]? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 45
	CALL DATYP(QD,'    QD  ',pon(),K,K,KMAX,KMAX)		!TYPE Q
	CALL ATYPE2(PI,'    PI  ',pon(),K,K,KMAX,KMAX)
45	continue
	call flush(7)
C
c Write Q etc back to disc
81	continue
	print 332
332	format(' Save rate constants on disc [Y] ? ')
	read 101,ans
      if(UC(ans).eq.'N') goto 99
	print 811
811	format(
     & ' Choose file number to write rates to [type ESC to omit]:')
	print 180
180	format(' Model #:                          Name of model',/,
     &	 '  (Rate constants  file #:         Name of rates)')
	irecq=nrecq+1	!default unless redefined below
	do i=1,nrecq
	   if(jmod(i).ne.0) then
		jrecq=2*i	!read SECOND record of data
		print 181,jmod(i),mtits(jmod(i))(1:69),i,rtitle(i)(1:65)
181		format(' Model ',i2,':',1x,a69,/,
     &	' Rate file ',i2,':',1x,a65,/)
		if(jmod(i).eq.imod0) irecq=i	!use as default
		if(mod(i,10).eq.0) then
		   print 11
c11		   format(' Hit any key for more')
		   call ANYKEY
		   print 180		!repeat headings
		endif
	   endif
	enddo
	print 182,nrecq+1,irecq
182	format(' Next vacant rate file # = ',i4,/,
     & ' File # for rates [',i2,'] = ')
	call INPUTi(irecq)
	call TITENT0(' Enter title for rates',rtitle(irecq),74,.false.)
c Write the data to chosen file (pair of records) in QDAT
c NB In record 1 single channel progs (SCBST etc) read only up to MTITLE; the
c rest of the values in recd #1 are needed in for noise/jumps
       OPEN(unit=18,file='QDAT.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	jrecq=2*irecq-1	!record # for FIRST record of data
c	write(18,rec=jrecq) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
c     &   xa0,xa1,xb0,xb1,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
c     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
c NB in this version, xa0,xa1 not distinguished, cur0, P0() not defined
c and vjump, cjump not defined, but simpler to leave them in here
c to avoid altering read statements in other progs. Likewise in 2nd
c record pzero not defined
	vjump=.false.
	cjump=.false.
	write(18,rec=jrecq) QD,PINF,K,ka,kb,kc,kd,nmod,imod0,mtitle,
     &   xa,xa,xb,xb,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
	write(18,rec=jrecq+1) QT,Pinf,nmod,imod0,vref,Vkin,PZERO,PSTAR,
     &   KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,gamma,rtitle(irecq)
	jreclast=jrecq
	CLOSE(unit=18)
c
      print 14,imod0,irecq,rtitle(irecq)
      if(pon()) write(7,14) imod0,irecq,rtitle(irecq)
      if(discprt) write(8,14) imod0,irecq,rtitle(irecq)
14	format(/,
     & ' Rate constants for model # ',i3,' stored in file # ',i3,
     & ' of QDAT.DAT with title:',/,1x,a74)
c
c Record in QGEN.INI which data was used last
	jmod(irecq)=imod0
	if(irecq.gt.nrecq) nrecq=irecq
      OPEN(unit=17,file='QGEN.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	write(17,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=17)
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
	INTEGER IC(2,20)
      character*1 ans,UC
	COMMON/QPAR/NCON,IC
c	COMMON/SPAR/NSPEC,IS,JS		!special parameters
c	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
c
101   format(a1)
	ans='N'
c
	if(iopt.eq.2) goto 20
	print 123,(IC(1,j),ic(2,j),j=1,ncon)
123	format(' Connections= ',/,4(5(2i3,4x),/))
	if(iopt.eq.1) RETURN
	print 2
2	format('& O.K. [Y] ? ')
      read 101,ans
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


	subroutine GETLIG(iopt,il,nlig)
C Define index for values to be mult by conc
c***Have problem if more than one ligand present- conc of WHICH ligand?
c so define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate (defined only if NLIG>1)
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
	integer IX(10),JX(10),IL(10)		!for ligand type
      character*1 ans,UC
	COMMON/CPAR/NCDEP,IX,JX,X
c
101   format(a1)
	ans='N'
	if(iopt.eq.2) goto 3
	if(ncdep.eq.0) then
	   print 1
1	   format(' No concentration-dependent rates')
	else
	   print 133,(IX(L),JX(L),IL(L),L=1,ncdep)
133	   format(' Concentration-dependent elements:',/,
     &   '  i   j     ligand #',/,10(2i3,5x,i3,/))
c	   Define nlig from the input values of IL() -if all IL() are =1 then
c	   nlig=1, otherwise nlig=2
	   if(ncdep.eq.1) then
		nlig=1
	   else
		do 4 i=2,ncdep
4		if(il(i).ne.il(1)) nlig=2
	   endif
	endif
	if(iopt.eq.1) RETURN
	print 2
2	format('& O.K. [Y] ? ')
      read 101,ans
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
214		FORMAT('&#',i2,': i,j= ')
		call INPUT2i(i,j)
		IX(L)=I	!index for C-dep Qij values
		JX(L)=J
		if(nlig.gt.1) then
		   print 211
211		   format('&Ligand number= ')
	   	   call INPUTi(IL(L))
		endif
210		continue
	   endif
231	continue
	endif
	RETURN
	end

	subroutine GETCYC(iopt)
	integer ICYC(20),NSC(8),IM(8,15),JM(8,15)
      character*1 ans,UC
	COMMON/MPAR/NCYC,NSC,IM,JM
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c iopt=3 asks for rates to be calc by micro rev only
c
101   format(a1)
	ans='N'
c
	if(iopt.eq.2) goto 5
	if(iopt.eq.3) goto 6
132	if(ncyc.eq.0) then
	   print 1
1	   format(' No cycles in the model',/)
	else
	   do 125 L=1,ncyc
	   print 126,L
126	   format(' Cycle # ',i3)
	   print 1311,im(L,1),jm(L,1)
1311	   format(2i3,'  (calc by micro rev)')
	   print 127,(IM(L,M),JM(L,M),m=2,nsc(L))
127	   format(2(5(2i3,4x),/))
125	   continue
	endif
	if(iopt.eq.1) RETURN
c
	print 2
2	format('& O.K. [Y] ? ')
      read 101,ans
5     if(UC(ans).eq.'N') then
	print 21
21	format(
     & ' Alter ONLY the rates to be calc by microscopic rev. [Y] ? ')
      read 101,ans
      if(UC(ans).ne.'N') goto 6
	   print 61
61	   FORMAT( ' No of cycles (up to 8) = ')
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
	   DO 64 M=3,NSC(L)
	   print 651,M
651	   FORMAT( ' state #',I2,' = ')  !EG, 1,3,4,7 IN CORRECT ORDER
	   call INPUTi(icyc(m))
64	   CONTINUE
C SET IM,JM. IM(L,1)=I,JM(L,1)=J for Q(i,j) to be calc by micro-rev
c in the Lth cycle. The rest of the q(i,j) in the cycle, going round
c in the same direction, have i,j in the other IM,JM
	   DO 641 M=1,NSC(L)-1
	   IM(L,M)=ICYC(M)
	   JM(L,M)=ICYC(M+1)
641	   CONTINUE
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
	do 120 i=1,ncyc
17	   print 123,i
123	   format(' Cycle #',i3)
	   do 121 j=1,nsc(i)
121	   icyc(j)=im(i,j)	!states for current cycle
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',20i3)
	   print 67
67	   FORMAT( '  route, q(i,j), to be calc by micro rev: i,j= ')
	   call INPUT2i(i1,j1)
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do 124 m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
124	   continue
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),20)
	   do 16 m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
16	   continue
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
	   do 131 m=1,nsc(i)-1
	   im(i,m)=icyc(m)
	   jm(i,m)=icyc(m+1)
131	   continue
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
120	continue		!next cycle
c
	RETURN	!from GETCYC
	end


c	subroutine GETVDEP(nmod,k,titlep,npar,IQ,readp,sameq,idest)
c	character*10 titlep(50)
c	character*1 ans,UC
c	integer IQ(10,10)
c	logical readp,sameq
c	logical discprt,pon,slock
c	common/dp/discprt
c	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
cc
c	pon()=slock()
cc
c101	format(a1)
c	IF(.NOT.READP) GOTO 307
c	IF(NVDEP.EQ.0) GOTO 501
c	DO 321 L=1,NVDEP
c321	print 322,IV(L),JV(L),HPAR(L)
c322	FORMAT(' For q(',2I2,')   H(mV)= ',F8.1)
c501	if(sameq) goto 309
c	print 308,nvdep
c308	FORMAT(' Same (',i3,' ) H values [Y] ? ')
c	read 101,ans
c	if(UC(ans).ne.'N') GOTO 309
c307	print 107
c107	FORMAT('&Number of V-dependent rates= ')
c	call INPUTi(nvdep)
c	IF(NVDEP.EQ.0) GOTO 309
cC RESULT DECREASES WITH HYPERPOL (INCREASES WITH V) IF H POSITIVE,
cC I.E. RATE GETS SLOWER (TAU LONGER) WITH HYPERPOL
c	print 1082
c1082	format('&If H is positive rate slows (tau longer) with hyperpol')
ccc type parameter names if nmod>0
cc	if(nmod.gt.0) print 28,(i,titlep(i),i=1,npar) !have titles for nmod=0 now
c	print 28,(i,titlep(i),i=1,npar)
c28	format(10(1x,i2,': ',a10,/))
c	do 1081 L=1,nvdep
c82	print 88
c88	format('&parameter number= ')		!for nmod>0
c	call INPUTi(m)
c	call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
c	IV(L)=I
c	JV(L)=J
c	print 2081,I,J
c2081	FORMAT(' H(',2I2,') (mV)= ')
c	call INPUTr(Hpar(L))
c2	format(g13.6)
c1081	continue
c309	CONTINUE	!JUMP TO HERE IF H VALUES FROM DISK USED
cc define cfkin here
c	if(.not.pon().and.(.not.discprt)) goto 13
c	if(pon()) write(7,323) nvdep
c      if(discprt) write(8,323) nvdep
c323	format(/,1x,i3,' voltage-dependent rate constants')
c	if(nvdep.gt.0) then
c	   do 11 L=1,NVDEP
c	   i=iv(L)
c	   j=jv(L)
c	   m=IQ(i,j)
c	   if(pon()) write(7,12)i,j,titlep(m),hpar(L)
c         if(discprt) write(8,12)i,j,titlep(m),hpar(L)
c12	   FORMAT(' For q(',2i2,')= ',a10,':   H (mV)= ',F9.1)
c11	   continue
c	endif
c13	continue
cc Array IQ defines correspondence
cc between elements of THETA and elements of Q: IQ(i,j)=m where
cc theta(m) is the parameter that goes in QT(i,j) (though may be
cC NOW HAVE H VALUES
c	RETURN		!from GETVDEP
c	END

	subroutine GETCHAN(nchan,gamma,readp,sameq,idest)
	REAL*8 gamma(10)
	logical readp,sameq
	character*1 ans,UC
	COMMON/KBLK/kA,kB,kC,kD
c
101	format(a1)
	ir=kA
	k=kA+kB+kC+kD
	if(.not.readp) goto 407
	print 320,NCHAN,IR,(1.0d12*gamma(I),I=1,IR)
320	FORMAT(' Number of channels= ',i10,/,
     & 3X,I3,' open states, with gamma (pS)=', 5G13.6)
	if(sameq) goto 402
	print 43
43	FORMAT( ' Use same no of channels, and gamma values [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') GOTO 402	!IF NOT OVERWRITE WITH NEW ONES
C
407	continue
	DO 100 I=1,K	!INITIALISE
100	gamma(I)=0.0d0
	print 403
403	FORMAT('&number of channels (integer)= ')
	call INPUTi(nchan)
c
	DO 406 I=1,IR
	print 405,I
405	FORMAT( '&gamma(',i2,') (pS)= ')
	call INPUTr(g)
	gamma(i)=dble(g)*1.0d-12		!IN SIEMENS
406	CONTINUE
402	continue
	RETURN		!return from GETCHAN
	END

	subroutine GETRAN(QT,ic)
	real*8 QT(10,10)
	integer IC(2,20)
	logical route
	common/rand/ix,iy,iz
	COMMON/KBLK/kA,kB,kC,kD
c
	k=kA+kB+kC+kD
	DO 72 I=1,KA		!FIRST KA ROWS
	DO 721 J=1,K		!ALL COLS
	i1=i
	j1=j
	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 721		!NO ROUTE FROM I TO J
	IF(J.LE.KA) QT(I,J)=DBLE(100.*(1.+9.*random()))  !QAA=100-1000
	IF(J.GT.KA) QT(I,J)=DBLE(100.*(5.+45.*random()))
C						!QAB,QAC=500-5000
721	CONTINUE
72	CONTINUE
C
	DO 73 I=KA+1,KA+KB		!NEXT KB ROWS
	DO 731 J=1,K		!ALL COLS
	i1=i
	j1=j
	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 731	!NO ROUTE FROM I TO J
	IF(J.LE.KA.OR.J.GT.KA+KB) QT(I,J)=DBLE(1000.*(5.+95.*random()))
C						!QBA,QBC=5000-100000.
	IF(J.GT.KA.AND.J.LE.KA+KB) QT(I,J)=DBLE(10.*(1.+9.*random()))
C						!QBB=10-100
731	CONTINUE
73	CONTINUE
C
	DO 74 I=KA+KB+1,K		!LAST KC ROWS
	DO 741 J=1,K		!ALL COLS
	i1=i
	j1=j
	IF(.NOT.ROUTE(i1,j1,IC)) GOTO 741		!NO ROUTE FROM I TO J
97	IF(J.LE.KA+KB) QT(I,J)=DBLE((1.+9.*random()))   !QCA,QCB=1-10
	IF(J.GT.KA+KB) QT(I,J)=DBLE(10.*(1.+9.*random()))
C							  !QCC=10-100
741	CONTINUE
74	CONTINUE
	RETURN	!from GETRAN
	end

