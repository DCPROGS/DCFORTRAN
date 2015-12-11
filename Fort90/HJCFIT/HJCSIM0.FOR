	subroutine HJCSIM1(nset,conc,THTRUE,cjump,iscan,qfilem,
     & nintt,nfile,nfileb,pfile,pfileb,calfacs2,nval,irecs,nmax,
     & titlep,ligname,irate,jrate,dgamma,readini,vkin,vhold,vref,
     & irect,nbstmax,repeat)

c To set up everything for generating a simulated .scn file in HJCIT
c Actually simulation to make tinto0 etc is done in call to HJCSIM2
c Model to be used (imod0, in common) and initial guesses already
c specifed by imod etc from call to getqd in hjcfit.
c  Here must specify
c (a) nset= number of sets, and conc(i,j)
c (b) true rates (thtrue() in hjcfit) used to make simulated obs
c Rather than defining icur(i,j) as current amplitude for state i, channel j
c just use dgamma().  Amplitudes irrelevant in HJCFIT
c As in hjcdat1,
c  NINTT(j)=total no of intervals in set j before resolution imposed
c  NMAX=maximimum nintt(j) (=allocated dimension of tint0() etc).
c
c NB all constraints, micro rev etc already set?
c
	allocatable::QT
	real*8 QT(:,:),r
	real*8 thtrue(200)
	real*4 conc(10,10)
	real*8 dgamma(100)
	integer irate(200),jrate(200)
	character qfilem*40
	integer nfile(10),nfileb(10),nintt(10)
	character*20 ligname(10)
	character*79 heading,title
	character*11 cdata(50,3),cnum
	character*7 kname 	!to print equilib constants
	logical readini,repeat,cjump
	character*33 pfile(20,10),pfileb(20,10)	!path names for .SCN files
c For qmechs
	integer jstart(500)
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer jmod(500)
	logical indmod,bind,monot
	logical iopen,jopen	!to print equilib constants
	character*74 rtitle,rtitles(500)	!title to describe rate constants
	character*10 titlep(200)
	character string*74,pre*3,ans*1
	integer SCRNBUF0(1024)
C
	character*2 charmod(25,40)	!to print model
c==	character*2
cmodnew(25,40)	!for NEWMOD
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c Commons with getqd
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
c=	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/LIG/nlig,IL(100)
	integer IQ(100,100)
	COMMON/qblk/IQ
	logical discprt
	common/dp/discprt
c Addition for getbound
	integer nbound(100,10)
	common/nbnd/nbound
	logical obeymr(50)
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c For EC50
	real*8 cfacd,vknd
c
c
	kdim=100
	allocate(QT(100,100))
	print 2
2	format(' SPECIFY TRUE RATES FOR SIMULATION')
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
15	format(
     & ' File # for INITIAL GUESSES for rate constants [',i2,'] = ')
	call INPUTi(ir)
	irecq=ir
	if(autosim) then
	   if(readini) then
		ir=irect		!file # for TRUE rates
	   endif
	   print 151,ir
15	   format(
     &' File # for TRUE rate constants used for simulation [',i2,'] = ')
	   call INPUTi(ir)
	   irect=ir
	endif
c
c Now read chosen file
	irec=jstart(irect)	!start byte for data in record #irecq
	irecsav=irec
	irectsav=irect
c    Erase any previosly-read charmod()
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
	read(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat
	if(.not.indmod) then
	   nsetq=0
	   kstat=k
	endif
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
	nchan=1 	!always (not kept in qmechs.dat)
	kfit=npar
c Define thtrue() to contain parameters
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   thtrue(m)=QT(i,j)
	enddo
c
c Need QT (temp local file) for QWIND, but only thtrue is returned from here)
	jset=1
	call SETMR(QT,jset,conc,kdim)
c
	iflag=3		!so numerical values shown in QWIND2
c
	CLOSE(unit=15)
	imod0=imod
	mtitle=mtits(imod0)
	k=kA+kB+kC+kD
c	kF=k-kA
c	if(.not.newrate) then
	   print 669,imod0,mtitle,irect,rtitles(irect)(1:74)
669	   format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Rates from file #',i3,':',/,
     &  1x,a74,/,
     & '  O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
c	else
c	   ans='Y'
c	endif
c
	mtitle=mtits(imod0)
c IQ etc already defined
c display rates, alter, print (and re-store?) for true mechanism
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
	string=char(244)//' = calculated by microscopic rev'
	print 13,string
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
	else
	   vhold=-80.	!so defined to calculate current and noise
	   vref=-80.
	   vkin=-80.
	endif
c
c check the rate constants
	   call CAPTSCN(scrnbuf0)	!before putting up model
c	call MODWIND(cmodnew,ilast+2,jlast+2,2,ilast,jlast)	!display model
	call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)	!display model
	if(.not.indmod) then
c===		ni=2*ncon	!number of rows
		ni=npar		!number of rows (OK for independent models too)
		nidisp=ni
		if(nidisp.gt.20) nidisp=20
		nr1=0		!posn of QWIND on screen
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
		iflagc=nj0		!show all cols
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
	if(discprt) write(8,6691)imod0,
     &	mtitle,irect,rtitles(irect)(1:74)
6691	format(
     & ' Mechanism #',i3,':',/,
     &  1x,a74,/,
     & ' Rates from file #',i3,':',/,
     &  1x,a74,/)
c NB now work with QT throughout so no need to remove conc
	do m=1,npar
	   pre='   '
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
	   if(r.eq.thtrue(m).or.idest.ne.105) then
      	print 12,m,pre,i,j,titlep(m),r
	      if(discprt) write(8,12)m,pre,i,j,titlep(m),r
12		format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	   else
      	print 121,m,pre,i,j,titlep(m),r,thtrue(m)
	      if(discprt) write(8,121)m,pre,i,j,titlep(m),r,thtrue(m)
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
c nbmax=max # of ops/bst in scsim! (called nbstmax here)
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
c
c insert ec50 here
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
     & 	conc,nlvar,vhold,vref,cfacd,dgamma,k,kdim)
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
c==========
c	if(.not.renum) then
c	   call DCASK('Re-save the model and rates on disc','n',ans)
c	else
c	   goto 99		!if renumbered
c	endif
c==	if(ans.eq.'N') goto 99
c
c True rates for simulation now defined
	print 412,nset
412	format(
     &' Number of different data sets (eg concentrations) [',i2,'] = ')
	call INPUTi(nset)
	nchan=1		!always
	nmax=0
	do j=1,nset
	   nfile(j)=1	!always for simulation
	   if(nlig.gt.0.and.readini) then
		print 163,j
163		format(' SIMULATED DATA FOR SET ',i2,
     &	' Last run used concentration(s) (micromolar):')
		do i=1,nlig
		   xs=1.e6*conc(i,j)
		   print 164,i,ligname(i),xs
164		   FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		enddo
		call DCASK('Change the concentration(s)','n',ans)
	   else
		ans='Y'
	   endif
	   if(nlig.gt.0) then
		if(ans.eq.'Y') then
		   do i=1,nlig
			xs=1.e6*conc(i,j)
			print 159,i,ligname(i),xs
159			FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
			call INPUTr(xs)
			conc(i,j)=xs*1.0e-6
		   enddo
		   if(discprt) write(8,108)
108		format(/,'------------------------------------------------')
		   do i=1,nlig
			xs=1.e6*conc(i,j)
			if(discprt) write(8,164) i,ligname(i),xs
c164		   FORMAT(/,
c     &      ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		   enddo
		   if(discprt) write(8,108)
		endif
	   endif
c	   call INTCONV(j,cnum)
	   pfile(1,j)='sim'//charnb(cnum)//'.dat'	!define this for plotq
c	   call TITENT0('Name for simulated .scn file:',
c     &	pfile(1,j),33,.false.)
c
	   nintt(j)=20000		!default
	   print 3,nintt(j)
3	   format(' Number of intervals to be simulated [',i6,'] = ')
	   call INPUTi(nintt(j))
	   if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!j=1,nset
c
c Now calculate amean(), peq() and picum() arrays that are used for actual
c simulation (these never change so calculate here). BUT they are different
c for each set, so perhaps better to calculate them at same time as simulation
c (time taken to do so is negligible compared with that needed for fitting)
c
c
	nbstmax=5000
	print 110,nbstmax
110	format(' Maximum number of openings/burst expected [',i8,'] = ')
	call INPUTi(nbstmax)
c
c Get random seeds
c (use ix1 to avoid clash with ix(10) array!)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from DL1:
	call RANDSK(ix1,iy1,iz1,0,repeat)		!read IX,IY,IZ
	DEallocate(QT)
	RETURN
	END

