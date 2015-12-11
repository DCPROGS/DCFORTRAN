	subroutine CJOUT(nev,smin,jfix,fitted,titlep,
     & vramp,vmin,vmax,Erev)
c
c Output of results of fitting for CJFIT
c Modif 03/15/95 10:47am to calculate reversal pot for I/V by bisection
c (theta now in common)
	real theta(20)
	character*10 TITLEP(20)		!names of params for fit
	integer jfix(20)
	logical fitted,pon,vramp
c	character*1 ans,UC
	LOGICAL CONSTR
	logical discprt,slock
	real*4 tau(10),amp(10)
	common/dp/discprt
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/vrev/km,theta
	EXTERNAL EFUNC	!at end of CJOUT
c
	pon()=slock()
c
c In case of exponential fit, sort time constants into ascending order
c because this needed for suitable xcal() values in CJVDU
	if(nmod.eq.0) then
	   do k=1,ncomp
		tau(k)=theta(2*k-1)	!theta(2*i-1) tau=theta 1,3,5,7,9
		amp(k)=theta(2*k)
	   enddo
	   call SORT2r(tau,amp,ncomp,10,.true.)
	   do k=1,ncomp
		theta(2*k-1)=tau(k)	!theta(2*i-1) tau=theta 1,3,5,7,9
		theta(2*k)=amp(k)
	   enddo
	endif
	km=kmax	!for common/vrev
	do 40 i=1,kmax
	if(jfix(i).eq.1.or.(.not.fitted)) then
	   print 412,I,TITLEP(i),THETA(I)
	   if(pon()) write(7,412)I,TITLEP(i),THETA(I)
         if(discprt) write(8,412)I,TITLEP(i),THETA(I)
412	   format(' Parameter ',I2,': ',a10,'= ',G13.6,' (fixed)')
	else
	   print 414,I,TITLEP(i),THETA(I)
	   if(pon()) write(7,414)I,TITLEP(i),THETA(I)
         if(discprt) write(8,414)I,TITLEP(i),THETA(I)
414	   FORMAT(' Parameter ',I2,': ',a10,'= ',G13.6)
	endif
40	continue
c
	if(pon()) write(7,42)NEV,SMIN
      if(discprt) write(8,42)NEV,SMIN
	print 42,NEV,SMIN
42	FORMAT(' No of function eval= ',i8,/,' Minimum SSD=',G13.6,/)
c
c Calculate reversal potential by brute force bisection
	if(vramp) then
c Calculate Erev here, by brute force bisection
	   if(vmin*vmax.le.0.) then   !voltage range spans zero
		xlo=vmin
		xhi=vmax
		y=0.0
		epsx=0.001		!0.001 mV accuracy
		epsy=-1.
		ndisp=-1
		call BISEC0(EFUNC,xlo,xhi,y,Erev,Yout,epsx,epsy,
     &	   nerr,ndisp,.false.)
		if(nerr.eq.0) then
c		   call DCFORMAT(Erev,7,3,cnum)
c		   call WDIALOG(1,
c     &	  'Fitted reversal potential = '//charnb(cnum)//' mV',11)
 	         print 43,Erev
      	   if(pon()) write(7,43) Erev
	         if(discprt) write(8,43) Erev
43		   format(' Fitted reversal potential = ',g12.5)
		else
c		   call WDIALOG(1,
c     &	  'Error in fitting reversal potential',12)
 	         print 44
      	   if(pon()) write(7,44)
	         if(discprt) write(8,44)
44		   format(' Error in fitting reversal potential')
		endif
	   endif
	endif
c ANCILLARY CALCS WITH FITTED PARAM FOR VARIOUS MODELS
	if(nmod.eq.4) then	!GHK with Mg block
c Calc permeabilities from single channel cond
	   gNa=theta(1)		!conductance in pS
	   gCs=theta(2)		!conductance in pS
	   v1=25.249		!mV at 20 deg
	   F=96484.56		!Coulomb/mole
	   cNa=0.15		!molar
	   cCs=0.14		!molar
	   PNa=(gNa*v1)/(F*cNa)    !pS*mV=femtoamps
	   PCs=(gCs*v1)/(F*cCs)    !pS*mV=femtoamps
	   PNa=PNa*1.e-15
	   PCs=PCs*1.e-15
	   print 211,PNa,PCs
	   if(pon()) write(7,211) PNa,PCs
	   if(discprt) write(8,211) PNa,PCs
211	   format(
     &   '&Permeability (1/(sec*molar)) for Na, Cs = ',2g13.6)
	   Erev1=v1*alog((PNa*cNa)/(PCs*cCs))
	   Eoff=theta(3)	!offset in Erev, relative to GHK value
	   Erev=Erev1+Eoff
	   print 212,Erev1,Erev
	   if(pon()) write(7,212) Erev1,Erev
	   if(discprt) write(8,212) Erev1,Erev
212	   format(
     & ' Reversal potential (mV): predicted, fitted = ',2g13.6,/)
c can also look at K1/K2 and its V-dep
	endif
	call flush(7)
c	if(defolt) goto 999
c
	RETURN
	end


	function EFUNC(x)
	common/vrev/km,theta(20)
	ival=1
	EFUNC=YCALCJ(km,theta,x,ival)
	RETURN
	end


