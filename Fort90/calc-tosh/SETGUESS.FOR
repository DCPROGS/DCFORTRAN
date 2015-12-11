	subroutine SETGUESS(kt,npar,theta,titlep,loguess,higuess,QD,cA1,
     & repeat,prt,idprt,jfix,imode)
c To set random initial guesses in MECHFIT
c imode=0 to define lower and upper limits for the guesses, and produce
c 		a random theta()
c imode=1 to define a random theta() only, using previously defined limits
c NB loguess(50),higuess(50) are stored without conc (for c-dep rates)
c
c Modif 06/29/99 06:22pm to remove all reference to cA1 -rates have now
c NOT got concentration in them until needed in SSDMF and MFOUT for actual
c calcs.  In any case any conc used here would be arbitrary since actual
c conc may differ from one set to another
c
c Modif 06/22/99 12:52pm so first run uses the input rate constants without
c randomising
c
	real*4 theta(50),loguess(50),higuess(50)
	integer jfix(50)
	logical repeat,prt,discprt
	logical concdep,micset,conset		!functions
	logical cdep,set,setass
	character string*74,pre*2,ans*1
	real*8 QD(10,10),QT(10,10)
	character*10 titlep(50)
	common/dp/discprt
	common/KBLK/kA,kB,kC,kD
	COMMON/CPAR/NCDEP,IX(10),JX(10),X
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	common/qblk/IQ(10,10)
	common/Vval/vhold,vkin,vref,gamma(10)		!V in mV
	COMMON/LIG/nlig,IL(10)
	common/rand/ix1,iy1,iz1
c
	if(imode.eq.1) goto 10		!limits already defined
c
	k=kA+kB+kC+kD
	call RANDSK(ix1,iy1,iz1,0,repeat)		!read IX,IY,IZ
c=	u=random()		!uniform(0,1)
	print 20
20	format(' Options for default limits on initial guesses',/,
     & ' (1) Lower=parameter guess/factor; upper=guess*factor',/,
     & ' (2) Set all limits manually (default as for last run)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   print 21
21	   format(' Factor [10] = ')
	   fac=10.
	   call INPUTr(fac)
	else
	   fac=100.		!for defaults for manual entries
	endif
	ans='Y'
	call DCASK(' Set association rate constants separately',ans,ans)
	setass=ans.eq.'Y'
	if(setass) then
	   asslo=1.e5
	   print 22,asslo
22	   format(' Set lower limit for all assoc rate constants to [',
     &   g10.3,'] = ')
	   call INPUTr(asslo)
	   asshi=1.e10
	   print 23,asshi
23	   format(' Set upper limit for all assoc rate constants to [',
     &   g10.3,'] = ')
	   call INPUTr(asshi)
	endif
c	if(iopt.eq.1) then
	   do m=1,kt
		r=theta(m)
		if(m.le.npar) then 		!check for conc-dep and micro rev for q(i,j)
		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		   if(CONCDEP(i,j)) then
c=			r=r/cA1
			if(setass) then
			   loguess(m)=asslo
			   higuess(m)=asshi
			else
			   loguess(m)=r/fac
			   higuess(m)=r*fac
			endif
		   else
			loguess(m)=r/fac
			higuess(m)=r*fac
		   endif
		else		!m>npar
		   loguess(m)=r/fac
		   higuess(m)=r*fac
		endif
		if(iopt.eq.1) then
               print 2, m,loguess(m),higuess(m)
               if(discprt) write(8,2) m,loguess(m),higuess(m)
		endif
	   enddo
	   if(iopt.eq.1) goto 441		!skip manual entry
c	endif
c
c Now manual entry of limits
	do m=1,kt
	   r=theta(m)
	   cdep=.false.			!current parameter (#m) not conc-dep
	   if(m.le.npar) then 		!check for conc-dep and micro rev for q(i,j)
		call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		if(CONCDEP(i,j)) then
c=		   r=r/cA1
		   cdep=.true.	!current parameter (#m) is conc-dep
		endif
c check if either if i,j is a micro rev route
		if(MICSET(i,j)) then
		   print 12,m
      	   if(prt) write(7,12) m
		   if(discprt) write(8,12) m
12		   format(
     &   		  ' Parameter ',i2,' set by microscopic reversibilty')
		endif
c check if i,j is set by constraint
		if(CONSET(i,j)) then
		   print 18,m
      	   if(prt) write(7,18) m
		   if(discprt) write(8,18) m
18		   format(
     &   		  ' Parameter ',i2,' constrained')
c		   goto 17  !end loop: no -define limits in case not fixed next time!
		endif
	   endif		!end of q(i,j) parameters
c
	   if(jfix(m).eq.1) then
	      print 11,m,theta(m)
      	if(prt) write(7,11) m,theta(m)
	      if(discprt) write(8,11) m,theta(m)
11		format(' Parameter ',i2,' fixed at ',g13.6)
c		goto 17  !end loop -no -define limits in case not fixed next time!
	   endif
	   if(setass) then
		loguess(m)=asslo
		higuess(m)=asshi
	   endif
	   print 1,m,r,loguess(m),higuess(m)
1	   format(
     &   ' Param (',i2,') (',g11.4,'):',/,
     &   '   low, high limits for initial guess [',
     &	g11.4,',',g11.4,'] = ')
	   call INPUT2r(loguess(m),higuess(m))
         if(prt) write(7,2) m,loguess(m),higuess(m)
         if(discprt) write(8,2) m,loguess(m),higuess(m)
2	   format(
     &     ' Param (',i2,') low, high limits for guess = ',2g13.6)
c
17	   continue		!next parameter
	enddo		!end of parameter=1,...,kt loop
c
441	print 44
44	format(/,' Select format output of results of fit:',/,
     & ' (1) Print distributions only',/,
     & ' (2) Print distributions + values at fitted time points',/,
     & ' (3) Print distributions + values at specified times',/,
     & ' Option number [1] = ')
	idprt=1
	call INPUTi(idprt)
	RETURN
c
10	continue
	do m=1,kt
	   set=.true.
	   x1=loguess(m)
	   x2=higuess(m)
	   if(m.le.npar) then
		call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		if(MICSET(i,j).or.CONSET(i,j)) set=.false.
c=		if(CONCDEP(i,j)) then
c=		   x1=x1*cA1
c=		   x2=x2*cA1
c=		endif
	   endif
	   if(jfix(m).eq.1) set=.false.
	   if(set) then
		u=random()		!uniform(0,1)
c		theta(m)=x1 + u*(x2 - x1)
c interpolate guess on log scale! If x1=1.e5, x2=1.e10 then all guesses close
c to 1.e10 eg if u=0.5 then theta=5.e9 with arithmetic interp, but with
c log interp theta=3.1e7
		xl1=log(x1)
		xl2=log(x2)
		theta(m)=exp(xl1 + u*(xl2-xl1))
	   endif
	enddo
c
c Now allocate QD from the random theta(), and fix everything by call
c to QSETD
	call QZERO(QT,k)
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   QT(i,j)=dble(theta(m))           !define QD
	enddo
c incorp constraints, micro rev etc into QD
	call QZERO(QD,k)
c call QSETD with epsim=true so comcentrations ignored -they are already
c incorporated in Q.
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(xA,xB,IL,V1,QT,QD,k,.true.)   !xA,xB ignored
c
c Now print the random theta(), and allocate it to QD
	string=
     & ' * = association rate; '//char(240)//' = calc by micro rev'
	print 13,string
      if(prt) write(7,13) string
      if(discprt) write(8,13) string
13	format(/,' Random initial values of rate constants',/,a74)
	do m=1,npar
	   pre='  '
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   theta(m)=sngl(QD(i,j))           !redefine theta from final QD
	   r=theta(m)
	   if(CONCDEP(i,j)) then
		pre='* '		!asterisk indicates rate to be mult by conc
c=		r=r/cA1
	   endif
c check if either if i,j is a micro rev route
	   if(MICSET(i,j)) then
		pre(2:2)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
	   endif
         print 15,m,pre,i,j,titlep(m),r
         if(prt) write(7,15)m,pre,i,j,titlep(m),r
	   if(discprt) write(8,15)m,pre,i,j,titlep(m),r
15	   format(i3,2x,a2,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	enddo
c and print guesses for enchan, if any
	do m=npar+1,kt
         print 16,m,theta(m)
         if(prt) write(7,16)m,theta(m)
	   if(discprt) write(8,16)m,theta(m)
16	   format(i3,2x,' Number of channels = ',f8.3)
	enddo
c
	RETURN
	end

