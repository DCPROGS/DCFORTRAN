	function YCALCJ(k,theta,xv1,ival)
C
C CALCULATES I'TH VALUE FOR MODEL #NMOD IN CJFIT
c Lahey V5 version 12/09/94 06:32pm.  Xfit cannot be in common now.  Rather
c than making it a parameter, altered so that xv1 is parameter - YCALCJ will
c use x=xv (from common/block1) if ival<1 in call, otherwise it will use
c x=xv1 (parameter) which has been set before call to xfit(i)
	real THETA(20)
	LOGICAL CONSTR
c for exponentials when rising phase fitted
	logical fixy0
	common/fix/fixy0
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/block2/cMg
c
c
	YCALCJ=0.0
c=	if(i1.ge.1) xv=Xfit(i1)
	if(ival.ge.1) xv=xv1
c If ival<1 then value of XV in common is used
c
	if(nmod.eq.0) goto 50		!exponentials
	if(nmod.eq.1) goto 200
	if(nmod.eq.2.or.nmod.eq.3.or.nmod.eq.4) goto 329
	if(nmod.eq.5) goto 21
c	if(nmod.eq.16.or.nmod.eq.17.or.nmod.eq.18) goto 329
c	if(nmod.eq.21) goto 21
	pause 'Error in nmod'
	STOP
c
c Exponentials
50	continue
c amps= theta 2,4,6,8,10; theta(2*i-1) tau=theta 1,3,5,7,9

	ycalcj=0.
	if(fixy0) then		!last amp=sum of others
	   sa=0.0		!sum of all amps but last
	   do i=1,ncomp-1
		i1=2*i
		tau=theta(i1-1)
		amp=theta(i1)
		sa=sa+amp
		ycalcj=ycalcj + amp*exp(-xv/tau)
	   enddo
c  now add in last exponential
	   tau=theta(kmax-1)
	   amp=-sa
	   ycalcj=ycalcj + amp*exp(-xv/tau)
	else
	   do i=1,ncomp
		i1=2*i
		tau=theta(i1-1)
		amp=theta(i1)
		ycalcj=ycalcj + amp*exp(-xv/tau)
	   enddo
	endif
	ycalcj=ycalcj+theta(kmax)		!add Y(inf)
	RETURN
c
c Polynomial
200	continue
c see NUMERICAL RECIPES for polynomial evaluation
c Also return the slope in SLOPE (in COMMON)
	if(ip2.gt.0) goto 221		!disjoint polynomial
c Degree of polynomial = ip1 = kmax-1
	slope=0.
	ycalcj=theta(kmax)
	do 20 i=kmax-1,1,-1
	slope=slope*xv + ycalcj
20	ycalcj=ycalcj*xv + theta(i)
	RETURN
221	continue
c For disjoint polynomial degree=ip1 at x<xjoin; degree=ip2 at x>xjoin
c ***not yet fixed for junction at other than x=0.
c	xv1=xv-xjoin		!=0. at junction
	xv1=xv
	slope=0.			!initialise for slope
	if(xv.gt.xjoin) goto 222
	ycalcj=theta(ip1+1)
 	do 223 i=ip1,1,-1
	slope=slope*xv1 + ycalcj
223	ycalcj=ycalcj*xv1 + theta(i)
	RETURN
222	ycalcj=theta(ip2+1)
 	do 24 i=ip2,1,-1
	slope=slope*xv1 + ycalcj
24	ycalcj=ycalcj*xv1 + theta(i)
	RETURN
c
c Model 2
329	continue
	Erev=theta(1)
	g0=theta(2)
	H=theta(3)
	if(nmod.eq.3) goto 330
	if(nmod.eq.4) goto 331
	ycalcj= (xv-Erev)*g0*exp(xv/H)
	RETURN
330	ca=theta(4)		!normalised agonist conc x/K1
c Mod 3: saturation with beta/alpha V-dep. Assume K1 not V-dep
	ba=theta(5)		!beta/alpha at 0 mV
	baV=ba*exp(xv/H)	!beta/alpha at xv mV
	popen=(bav*ca*ca)/(1. + 2.*ca + ca*ca*(1.+bav))
	ycalcj= (xv-Erev)*g0*popen
	RETURN
c Mod 4: saturation with K1 V-dep. Assume beta/alpha not V-dep
331	ca=theta(4)		!normalised agonist conc at E=0
	ba=theta(5)		!beta/alpha
	caV=ca*exp(xv/H)	!normalised at xv mV
	popen=(ba*caV*caV)/(1. + 2.*caV + caV*caV*(1.+ba))
	ycalcj= (xv-Erev)*g0*popen
	RETURN
c Model 5:  I/V for GHK with Mg block
21	continue
	gNa=theta(1)		!conductance in pS
	gCs=theta(2)		!conductance in pS
	Eoff=theta(3)	!offset in Erev, relative to GHK value
	aK1=theta(4)		!in mM
	aK2=theta(5)		!in mM
	H1=theta(6)
	H2=theta(7)
	v1=25.249		!mV at 20 deg
	F=96484.56		!Coulomb/mole
	cNa=0.15		!molar
	cCs=0.14		!molar
	PNa=(gNa*v1)/(F*cNa)    !pS*mV=femtoamps
	PCs=(gCs*v1)/(F*cCs)    !pS*mV=femtoamps
	E=xv-Eoff			!xv,Eoff in mV
c problem- if E=0 then e1/(1-exp(-e1)) goes to 0/0; limiting value
c is actually 1.0. If e1<0.0001 then use 1st order approx!
	e1=E/v1
	ex=exp1(-e1)		!use exp1.for to prevent under/overflow
	if(abs(e1).gt.0.0001) then
	   e2=e1/(1.0-ex)
	else
	   e2=1.0/(1.0-0.5*e1)
	endif
c	cur=(0.001*E*F/v1)*(1./(1.-ex))*(PCs*cCs - PNa*cNa*ex)
	cur=(0.001*F*e2)*(PCs*cCs - PNa*cNa*ex)
c For Mg block from inside: k(+1)=---> , k(-1)= <---- so K1= k(-1)/k(+1)
c for dissoc outwards k(-2) = --->.   Define K2=k(-2)/k(+1) (all at 0 mV)
c K1,K2 have V dep given by H1,H2 in mV
c	cMg=setx(j)		!Mg conc for set j in mM; in common/block2/ here
	e1=E/H1
	if(e1.gt.87.) e1=87.	!exp1 prevents underflow but not overflow
	e2=E/H2
	if(e2.gt.87.) e2=87.	!exp1 prevents underflow but not overflow
	aKeff=aK1*exp1(e1) + aK2*exp1(e2)
	pMg=cMg/(cMg+aKeff)		!Mg occupancy
	ycalcj=cur*(1.0-pMg)
	RETURN
c
	END


