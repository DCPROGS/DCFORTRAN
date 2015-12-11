	subroutine SETLIMIT(kt,npar,lowval,highval,pertval,theta,
     &	jfix,cA1,prt)
c To set lower and upper limits for fitted parameters in MECHFIT
c NB lowval(50),highval(50) are stored WITH conc (for c-dep rates)  (unlike
c loguess, higuess), because must have conc incorporated when used
c in simplex and ssdmf (standard conc, cA1).  Before storing in .INI
c the conc factor is removed
c Modif 06/29/99 06:22pm to remove all reference to cA1 -rates have now
c NOT got concentration in them until needed in SSDMF and MFOUT for actual
c calcs.  In any case any conc used here would be arbitrary since actual
c conc may differ from one set to another
c
	real*4 theta(50)
	real*4 lowval(50),highval(50),pertval(50)		!parameter
	integer jfix(50)
	logical prt,discprt,setass
c	logical default
	logical concdep,micset,conset		!functions
	character ans*1
	common/dp/discprt
	common/KBLK/kA,kB,kC,kD
	common/qblk/IQ(10,10)
c

	k=kA+kB+kC+kD
c Suggest default limits if present values are zero
c	x0=1.e-30
c	default=abs(lowval(1)).lt.x0.and.abs(highval(1)).lt.x0.and.
c     &	abs(pertval(1)).lt.x0
c	if(.not.default) then
c	   ans='N'
c	   call DCASK(' Set all limits to defaults',ans,ans)
c	   default=ans.eq.'Y'
c	endif
c
	print 20
20	format(' Options for default limits on parameter estimates',/,
     & ' (1) Lower=parameter guess/factor; upper=guess*factor',/,
     & ' (2) Wide limits: lower = 1.e-5, upper=1.e15',/,
     & ' (3) Set all limits manually (default as for (1))',/,
     & ' (4) Set all limits manually (default as for last run)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)

	if(iopt.eq.1.or.iopt.eq.3) then
	   print 21
21	   format(' Factor [100] = ')
	   fac=100.
	   call INPUTr(fac)
c	else
c	   fac=1000.		!for defaults for manual entries
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
	if(iopt.eq.1.or.iopt.eq.3) then
	   do m=1,kt
		r=theta(m)
		if(m.le.npar) then 		!check for conc-dep and micro rev for q(i,j)
		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		   if(CONCDEP(i,j)) then
c=			r=r/cA1
			if(setass) then
			   lowval(m)=asslo
			   highval(m)=asshi
			   pertval=0.1*r
			else
			   lowval(m)=r/fac
			   highval(m)=r*fac
			   pertval=0.1*r
			endif
		   else
			lowval(m)=r/fac
			highval(m)=r*fac
			pertval=0.1*r
		   endif
		else		!m>npar
		   lowval(m)=r/fac
		   highval(m)=r*fac
		   pertval=0.1*r
		endif
	   enddo
	else if(iopt.eq.2) then	!no need to ask
	   do m=1,kt
		r=theta(m)
		if(m.le.npar) then 		!check for conc-dep and micro rev for q(i,j)
		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		   if(CONCDEP(i,j)) then
c=			r=r/cA1
			if(setass) then
			   lowval(m)=asslo
			   highval(m)=asshi
			   pertval=0.1*r
			else
			   lowval(m)=1.e-5
			   highval(m)=1.e15
c			   lowval(m)=r/fac
c			   highval(m)=r*fac
			   pertval=0.1*r
			endif
		   else
			lowval(m)=1.e-5
			highval(m)=1.e15
c			lowval(m)=r/fac
c			highval(m)=r*fac
			pertval=0.1*r
		   endif
		else		!m>npar
		   lowval(m)=1.e-5
		   highval(m)=1.e15
c		   lowval(m)=r/fac
c		   highval(m)=r*fac
		   pertval=0.1*r
		endif
	   enddo
	   goto 99
	endif
	if(iopt.eq.1.or.iopt.eq.2) then	!print values and skip manual setting
	   do m=1,kt
      	print 2, m,lowval(m),highval(m),pertval(m)
      	if(discprt) write(8,2) m,lowval(m),highval(m),pertval(m)
	   enddo
	   goto 99
	endif
c
c Now manual entry with defaults set above (iopt=3) or from .ini (iopt=4)
	do m=1,kt
	   r=theta(m)
	   if(m.le.npar) then 		!check for conc-dep and micro rev for q(i,j)
		call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
c=		if(CONCDEP(i,j)) then
c=		   r=r/cA1
c=		endif
c check if either if i,j is a micro rev route
		if(MICSET(i,j)) then
		   print 12,m
      	   if(prt) write(7,12) m
		   if(discprt) write(8,12) m
12		   format(
     &   	    ' Parameter ',i2,' set by microscopic reversibility',/)
		   lowval(m)=r
		   highval(m)=r
		   pertval(m)=0.2*r
		   goto 17	!end of loop -set anyway in case free next time
		endif
c check if i,j is set by constraint
		if(CONSET(i,j)) then
		   print 18,m
      	   if(prt) write(7,18) m
		   if(discprt) write(8,18) m
18		   format(
     &   		  ' Parameter ',i2,' constrained',/)
		   lowval(m)=r
		   highval(m)=r
		   pertval(m)=0.2*r
		   goto 17	!end of loop -set anyway in case free next time
		endif
	   endif		!end of q(i,j) parameters
c
	   if(jfix(m).eq.1) then
	      print 11,m,theta(m)
      	if(prt) write(7,11) m,theta(m)
	      if(discprt) write(8,11) m,theta(m)
11		format(' Parameter ',i2,' fixed at ',g13.6,/)
		lowval(m)=r
		highval(m)=r
		pertval(m)=0.0
		goto 17	!end of loop -set anyway in case free next time
	   endif
c=	   if(default) then
c=		lowval(m)=0.01*r
c=		highval(m)=100.*r
c=		pertval(m)=0.2*r
c=		if(concdep(i,j)) then
c=		   lowval(m)=1.e5
c=		   highval(m)=1.e10
c=		   pertval(m)=1.e6
c=		endif
c=	   endif
	   print 1,m,r,lowval(m),highval(m)
1	   format(
     & ' Param (',i2,') (',g11.4,'):',/,
     & '   low, high limits for fitted value [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(lowval(m),highval(m))
	   print 3,pertval(m)
3	   format(
     &   '&   perturbation increment when simplex flattens [',
     &	g11.4,'] = ')
	   call INPUTr(pertval(m))
         if(prt) write(7,2) m,lowval(m),highval(m),pertval(m)
         if(discprt) write(8,2) m,lowval(m),highval(m),pertval(m)
2	   format(
     &  ' Param (',i2,') low, high limits for fitted value = ',2g13.6,/,
     & '   perturbation increment when simplex flattens = ',g13.6)
c
17	   continue		!next parameter
	enddo		!end of parameter=1,...,kt loop
c
c Multiply assoc rates by conc
99	continue
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
c=	   if(CONCDEP(i,j)) then
c=		lowval(m)=lowval(m)*cA1
c=		highval(m)=highval(m)*cA1
c=		pertval(m)=pertval(m)*cA1
c=	   endif
	enddo
c
	RETURN
	end

