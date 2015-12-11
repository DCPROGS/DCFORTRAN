	function GAULIK(k,theta)
c 06/03/89 05:56pm Lahey version with data now in COMMON as originally
c 07/27/94 02:09pm Modified for new SD constraints (via isdcon),
c and for excluded range (via excreg etc)
c Modif Nov 86 for up to 5 components
	REAL THETA(K)
	integer ja(10)		!local
	REAL R(10),A(10),sd(10)	!LOCAL
	LOGICAL AMPOS,gamdst,fastf,logt,excreg
	integer jafix(10)
	real*4 freqs(-5000:5000)		!for pahist and Patlak
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS,SCVDU
	common/FIXBLK/nfix,jfix(50)
	integer nyval(10),ncomp(10)
	integer nfit(10)
	real*4 ylow(10),yhigh(10)
	real*4 yexclow(10),yexchigh(10)
	COMMON/FITBLK/yval(500000,10),xaxis(510),freq(510),Nyval,
     & ylow,yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(51200),anfit,
     & nfitt,yvalt(5120),iconst
C LIKELIHOOD FUNCT FOR FITTING GAUSSIANS
C N.B. A VALUES ARE REL AREAS OF COMPONENTS HERE
C***6-NOV-82 ORDER OF PARAMETERS CHANGED TO (MEAN(I),SD(I),AREA(I))...,
C I-1,NCOMP TO AGREE WITH NEW MLFIT
	C=2.5066283	!=SQRT(2*PI)
c

	if(ncomp(1).eq.1) then
	   R(1)=THETA(1)	!=mean(1)
	   if(theta(2).lt.1.e-15) theta(2)=1.e-7		!sd
	   sd(1)=theta(2)	!sd
	   A(1)=1.0	!AREA
	else
	   sa=0.	!TOTAL AREA EXCEPT FOR LAST COMP
	   j=0
	   do 20 i=1,ncomp(1)
		j=j+1
		R(I)=THETA(J)		! mean(i)
		if(i.eq.1) then
		   j=j+1		!so j=2 here
		   if(theta(j).lt.1.e-15) theta(j)=1.e-7	!sd must be pos
		   if(isdcon.eq.2) then
			sd(i)=theta(j)*r(1)	!theta(2)*r(1)
		   else
			sd(i)=theta(j)		!=theta(2)
		   endif
		else		!i>1
		   if(isdcon.eq.1) then
			j=j+1		!increment j only if isdcon=1 (all SD are param)
			if(theta(j).lt.1.e-15) theta(j)=1.e-7	!sd must be pos
			sd(i)=theta(j)
		   else if(isdcon.eq.2) then
			sd(i)=theta(2)*r(i)	!theta(2)=sdfac=SD/mean in this case
		   else if(isdcon.eq.3) then
			sd(i)=theta(2)		!if SD fixed, otherwise redefined
		   endif
		endif
c
		j=j+1		!incerement j to get areas
		ja(i)=j	!index in theta of area of component i (inc i=ncomp)
		IF(I.EQ.NCOMP(1)) GOTO 20	!LAST AREA NOT A PARAMETER
		A(I)=THETA(J)
		if(ampos) then
		   if(a(i).gt.1.) a(i)=0.99999
		   if(a(i).lt.0.) a(i)=1.0e-6
		endif
		SA=SA+a(i)
20	   continue
	   IF(.NOT.AMPOS) GOTO 24
	   nafix=0
	   if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
		do i=1,ncomp(1)-1
		   jafix(i)=0
		   if(jfix(ja(i)).eq.1) then
			nafix=nafix+1	!number of fixed areas
			jafix(i)=1	!area(i) is fixed
		   endif
	      enddo
	   endif
	   if(SA.ge.1.0) then
		if(nafix.eq.0) then		!no fixed areas
		   do i=1,ncomp(1)-1
			A(i)=0.99*A(i)/SA
		   enddo
		   SA=0.99
		else			!some areas fixed
		   safix=0.0
		   sadj=0.0
		   do i=1,ncomp(1)-1
			if(jafix(i).eq.0) then
			   sadj=sadj+a(i)
			else
			   safix=safix+a(i)
			endif
		   enddo
		   sa1=sadj/(0.99-safix)
		   sa=0.0
		   do i=1,ncomp(1)-1
			if(jafix(i).eq.0) a(i)=a(i)/sa1
			sa=sa+A(i)
		   enddo
		endif
	   endif
c Reassign any altered values to theta
	   do i=1,ncomp(1)-1
		theta(ja(i))=A(i)		!REASSIGN ALTERED VALUE
	   enddo
c
24	   A(ncomp)=1.0-SA	!NOT A PARAMETER
c
	endif		!end of ncomp>1
c New version
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
c
C Calc denominator=PROB(ylow<Y<yhigh)
c or, if range excluded, PROB(ylow<Y<yexclow.or.yexchigh<Y<yhigh)
	st=sqrt(2.0)
	d=0.
	if(excreg) then
	 do i=1,ncomp(1)
	   ulo=(ylow(1)-r(i))/(sd(i)*st)
	   uhi=(yhigh(1)-r(i))/(sd(i)*st)
	   uxlo=(yexclow(1)-r(i))/(sd(i)*st)
	   uxhi=(yexchigh(1)-r(i))/(sd(i)*st)
	   d=d + a(i)*0.5*(erfs(uhi)-erfs(uxhi)+erfs(uxlo)-erfs(ulo))
	 enddo
	else		!no excluded region (as earlier version)
	 do i=1,ncomp(1)
	   ulo=(ylow(1)-r(i))/(sd(i)*st)
	   uhi=(yhigh(1)-r(i))/(sd(i)*st)
	   d=d + a(i)*0.5*(erfs(uhi)-erfs(ulo))
	 enddo
	endif
C
C NOW CALC MINUS LOG-LIKELIHOOD
	if(fastf) then		!bin fitting used for Patlak plot
c  Mid-values are current=0.5*(float(i-1)+float(i))/100.
c  for bin #i, e.g. for freqs(2) mid-value for current=0.015 pA (see PATPLOT)
	   s=0.
	   do i=nb1,nb2
		t=0.
c		y=0.5*(float(i-1)+float(i))/100.  !mid-value of current for bin #i
		y=(float(i-1)+float(i))/200.  !mid-value of current for bin #i
		do  j=1,ncomp(1)
		   t1=(y-r(j))/sd(j)
		   t=t + a(j)*exp1(-0.5*t1*t1)/(c*sd(j))
		enddo
		if(t.lt.1.e-30) t=1.e-30
		s=s-freqs(i)*alog(t/d)
	   enddo
	   gaulik=s
	   RETURN
	endif
c Normal calculation
	s=0.
	if(anfit.lt.0.) then	!normal calc
	  do i=1,nfit(1)
	   Y=yval(i,1)
	   t=0.
	   do j=1,ncomp(1)
		t1=(y-r(j))/sd(j)
		t=t + a(j)*exp(-0.5*t1*t1)/(c*sd(j))
	   enddo
	   if(t.lt.1.e-30) t=1.e-30
	   S=S-ALOG(T)
	  enddo
	else				!weighted calc
	  do i=1,nfit(1)
	   Y=yval(i,1)
	   t=0.
	   do j=1,ncomp(1)
		t1=(y-r(j))/sd(j)
		t=t + a(j)*exp(-0.5*t1*t1)/(c*sd(j))
	   enddo
	   t=t*wfreq(i)
	   if(T.lt.1.e-30) T=1.e-30
	   S=S-ALOG(T)
	  enddo
	endif
c
c	if(d.lt.1.e-37.or.d.gt.1.e38) then
c	   print 70,d,(theta(i),i=1,k)
c70	   format(' d = ',g13.6,/,' theta = ',50g13.6)
c	endif
c
	s=s+float(nfit(1))*alog(d)
	gaulik=s
	RETURN
	END

