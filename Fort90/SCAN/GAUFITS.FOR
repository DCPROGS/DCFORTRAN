	function GAUFITs(k,theta)
c For fitting Gaussians to point-amplitude histograms.
c This is version for SCAN (called for scamhist) -same as \ekdist\gaufit1
c except for common
c
c Data are in ifreq(i), (i=nb1,nb2) = frequency of occurrence of
c current level=float(i)*calfac2.  'Bin' width = 1 element of ifreq
c =calfac2 pA, so can consider bin as extending from (i-0.5)*calfac2
c to (i+0.5)*calfac2, and integrate over this bin.
c
c 07/27/94 02:09pm Modified for new SD constraints (via isdcon)
c (not yet fixed for excluded region -see MLFITS)
c
c 06/03/89 05:56pm Lahey version with data now in COMMON as originally
c Modif Nov 86 for up to 5 components
c
	REAL THETA(K)
	integer ja(10)		!local
	REAL R(10),A(10),sd(10)	!LOCAL
	LOGICAL AMPOS,excreg
	integer jafix(10)
	real*4 freqs(-5000:5000)		!in common/pablk/ for GAUFIT
	common/FIXBLK/nfix,jfix(50)
	COMMON/FITBLK/Ylow,Yhigh,Ncomp,Nfit,ampos,excreg,
     &  yexclow,yexchigh,isdcon,anfit
	common/PABLK/freqs,calfac2,ilow,ihigh
c
c
C***6-NOV-82 ORDER OF PARAMETERS CHANGED TO (MEAN(I),SD(I),AREA(I))...,
C I-1,NCOMP TO AGREE WITH NEW MLFIT
c
c
	if(ncomp.eq.1) then
	   R(1)=THETA(1)	!=mean(1)
	   sd(1)=theta(2)	!sd
	   A(1)=1.0	!AREA
	else
	   sa=0.	!TOTAL AREA EXCEPT FOR LAST COMP
	   j=0
	   do 20 i=1,ncomp
		j=j+1
		R(I)=THETA(J)		! mean(i)
		if(isdcon.eq.3) then
		   sd(i)=theta(2)		!if SD fixed, otherwise redefined
		else if(isdcon.eq.2) then
		   sd(i)=theta(2)*r(i)	!theta(2)=sdfac in this case
		else				!isdcon=1
		   j=j+1
		   if(theta(j).lt.1.e-30) theta(j)=1.e-7	!sd must be pos
		   sd(i)=theta(j)
		endif
		j=j+1
		ja(i)=j	!index in theta of area of component i (inc i=ncomp)
		IF(I.EQ.NCOMP) GOTO 20	!LAST AREA NOT A PARAMETER
		A(I)=THETA(J)
		IF(.NOT.AMPOS) GOTO 21
		IF(A(I).GT.1.) A(I)=0.99999
		IF(A(I).LT.0.) A(I)=1.0E-6
21		SA=SA+A(I)
20	   continue
C
C EACH AREA (I=1,NCOMP-1) IS NOW BETWEEN 0 AND 1 IF AMPOS. BUT IF THEIR
C SUM IS GREATER THAN 1 THEN SCALE ALL DOWN SO THAT THEIR SUM IS 1 AND
C THUS AREA(NCOMP)=0
C Each area (i=1,ncomp-1; sum=SA) is now between 0 and 1 if AMPOS. But if their
C sum is greater than 1 then scale all down so that their sum is 1, and
C thus AREA(ncomp)=0
c If an area has been fixed at a largish value (eg in iteration to get
c a likelihood interval for an area) then do not really want to sqeeze out
c AREA(ncomp) altogether: try scaling area(1)...area(ncomp) so they add
c to 0.99 and area(ncomp)=0.01
c NB Must NOT alter areas if they are FIXED parameters! To avoid this
c happening, firstly add all the fixed areas=safix, so have 0.99-safix as the
c total adjustable area; then add all the non-fixed areas=sadj, and scale
c them so sadj=0.99-safix.
c NB JAFIX,NAFIX defined by calling prog; nafix=number of fixed AREAS and
c JAFIX(i)=0,1 according as area(i), i=1,ncomp-1, is free, fixed resp.
	   IF(.NOT.AMPOS) GOTO 24
	   nafix=0
	   if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
		do i=1,ncomp-1
		   jafix(i)=0
		   if(jfix(ja(i)).eq.1) then
			nafix=nafix+1	!number of fixed areas
			jafix(i)=1	!area(i) is fixed
		   endif
	      enddo
	   endif
	   if(SA.ge.1.0) then
		if(nafix.eq.0) then		!no fixed areas
		   do i=1,ncomp-1
			A(i)=0.99*A(i)/SA
		   enddo
		   SA=0.99
		else			!some areas fixed
		   safix=0.0
		   sadj=0.0
		   do i=1,ncomp-1
			if(jafix(i).eq.0) then
			   sadj=sadj+a(i)
			else
			   safix=safix+a(i)
			endif
		   enddo
		   sa1=sadj/(0.99-safix)
		   sa=0.0
		   do i=1,ncomp-1
			if(jafix(i).eq.0) a(i)=a(i)/sa1
			sa=sa+A(i)
		   enddo
		endif
	   endif
c Reassign any altered values to theta
	   do i=1,ncomp-1
		theta(ja(i))=A(i)		!REASSIGN ALTERED VALUE
	   enddo
c
24	   A(ncomp)=1.0-SA	!NOT A PARAMETER
c
	endif		!end of ncomp>1
c
C Calc denominator=PROB(ylow<Y<yhigh)
	st=sqrt(2.0)
	d=0.
	ylow=(float(ilow)-0.5)*calfac2	!lower edge of first 'bin'
	yhigh=(float(ihigh)+0.5)*calfac2
	do i=1,ncomp
	   ulo=(ylow-r(i))/(sd(i)*st)
	   uhi=(yhigh-r(i))/(sd(i)*st)
	   d=d + a(i)*0.5*(erfs(uhi)-erfs(ulo))
	enddo
C
C NOW CALC MINUS LOG-LIKELIHOOD
	s=0.
c	do i=1,nfit
c	do i=ilow,ihigh
c	   Ylo=calfac2*(float(i)-0.5)		!lower edge for 'bin' #1 (pA)
c	   Yhi=Ylo+calfac2				!upper edge for 'bin' #1 (pA)
c	   t=0.
c	   do j=1,ncomp
c		ulo=(ylo-r(j))/(sd(j)*st)
c		uhi=(yhi-r(j))/(sd(j)*st)
c		t=t + a(j)*0.5*(erfs(uhi)-erfs(ulo))
c	   enddo
c	   if(t.lt.1.e-30) t=1.e-30
c	   S=S-freqs(i)*alog(t)	!each i value occurs freq(i) times
c	enddo
c Alternative version as done on PDP
	c=2.5066283		!=sqrt(2*pi)
	do i=ilow,ihigh
	   Y=calfac2*float(i)
	   t=0.
	   do j=1,ncomp
c		u=(y-r(j))/sd(j)
c		t=t + a(j)*exp1(-0.5*u*u)/(c*sd(j))
		u=(y-r(j))/sd(j)
		x=0.5*u*u
		t=t + (a(j)/(c*sd(j)))*exp1(-x)
	   enddo
	   if(t.lt.1.e-30) t=1.e-30
	   S=S-freqs(i)*alog(t)	!each i value occurs freq(i) times
	enddo
c
	S=S+float(nfit)*alog(d)
	GAUFITs=S
	RETURN
	end

