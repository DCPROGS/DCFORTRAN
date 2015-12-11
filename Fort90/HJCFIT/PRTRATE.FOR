	subroutine PRTRATE(QT,npar,irate,jrate,titlep,jcon,nchan,
     &    ligname,dgamma,qfilem,k,kflag,idest,kA,kB,kC,kD,
     &    imod0,charmod,jlast,ilast,mtitle,
     &    ncon,IC,ncdep,IX,JX,IQ,nlig,IL,nlvar,vhold,vref,imode)
c
c Subroutine in HJCFIT (called in GETQD2) to print rate constants, taken
c from QT (in call this may be QT or QTtrue) and corresponding equilibrium
c  constants and EC50,with option to re-save the rate constants
c
c Modif 09/04/04 05:20pm so EC50 no longer printed here -all done from
c calls to PRNTEC50, from HJCFIT. FROM 11/05/04 01:08pm PRNTEC50 also
c called in GETQD2 to allow check of EC50 for initial guesses.
c
c Modif 01/27/03 09:14am to add conc(i.j) in common/CBLK/nset,conc,jsetlast
c -with more than one ligand it is essential to know the conc of the
c  others for the EC50 calc
c
c Modif 01/29/02 10:46pm to make IQ,IC, IL all parameters, rather than
c in common, so disprate can be called with arrays approriate for mech
c used for simulation. Also imode for qwindhjc (=0 normally; =1 when
c constraints and m-r not to be used, e.g for simulation model when it
c differs from fitted model (NB there is already an 'imode'
c in getqd2)
	REAL*8 QT(100,100),dgamma(100)
	character*2 charmod(25,40)	!to print model
	real*4 pstar(4)
	character*40 qfilem,qfilout
	logical samefil
	integer jcon(200)
c Declarations for modif 03/05/95 06:16pm
	character*10 titlep(200)
c New arrays for qmechs.dat
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle,rtitles(500)	!title to describe rate constants
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod,present
c For print of eq constants
	logical iopen,jopen,bind	!to print equilib constants
	character*7 kname 	!to print equilib constants
c
	logical kmfast,renum
	character*1 ans
	character*74 mtitle,mtitle1	!title for model
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
cc For ec50
c	real*8 EC50,cfacd,vknd,pmax
c	real*4 curmax,curinfs,concmax,pop0
c	logical monot
c
c Commons as in getqd1
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	common/indblk1/nsub,kstat0,npar0,kcon,npar1,ncyc0
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
c
	integer IC(2,200)			!now parameter
c	COMMON/QPAR/NCON,IC(2,200)
	integer IX(100) ,JX(100)	!now parameter
c	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer IQ(100,100)			!now parameter
c	COMMON/qblk/IQ(100,100)
	integer IL(100)			!now parameter
	real*4 conc(10,10)
	common/CBLK/nset,conc,jsetlast	!for HJCLIK, checkqd,qset_hjc,hjcdisp
c	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
c	common/KBLK/kA,kB,kC,kD
c
c	character*79 wtitle
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c
c Add common in case charmod etc needed in calling prog
c==========? remove?
c=	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	common/model2/chardef,indmod,nrateq,kstat,irecsav,irecqsav,
     & iver,nmods,boundef,jmod,mtits,statname,ksav
c	common/mod/imod
c
c
	character*40 mtitle2*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle2 !for WINPRINT,ENDPRINT,DISCNUM
	common/db/ideb		!debug level
	common/ren/renum		!so can be kept in .ini if req
c For statmod
	common/sm/nsmax		!for getqd/charq
c
c
c	nlig=nligsav
	if(idest.eq.100) then	!when kflag=-1
	   idest=0
	   goto 100
	endif
c
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
		do n1=1,ncyc
		   n=isetmr(n1)
		   if(obeymr(n).and.im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
		   endif
		enddo
	   endif
         print 12,m,pre,i,j,titlep(m),r
	   if(discprt) write(8,12)m,pre,i,j,titlep(m),r
12	   format(i4,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
c Add '&' in first position if parameter constrained
	   if(jcon(m).eq.1) pre(1:1)='&'
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
c Insert ec50 here -now from PRNTEC50 in hjcfit (and from GETQD2 for init rates)
c
c RE-SAVE model (e.g. if parameter names changed)
92	continue
101	if(.not.renum) then
	   call DCASK('Re-save the model and rates on disc','n',ans)
	else
	   goto 99		!if renumbered
	endif
	if(ans.eq.'N') goto 99
c
100	continue	!jump here to write if kflag=-1
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
c
99	continue
	imod=imod0	!restore
	idest=0
	kflag=0
	RETURN
	end

