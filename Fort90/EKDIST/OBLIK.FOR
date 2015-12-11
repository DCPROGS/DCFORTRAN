	FUNCTION OBLIK(k,THETA)
	REAL THETA(K)
c	real area(10),w(10),rho(10)	!local
	real*4 amean(10,10),area(10,10),rho(10,10),w(10)	!local
	integer jafix(10)
	LOGICAL AMPOS,gamdst,fastf,logt,excreg
	logical errflag,constrained
c	logical debug,caplock
c for constraints when nset>1
	integer itcon(10,10),ialoc(10,10),itloc(50)	!see below
	integer*4 nset,neq,ieq(50),jeq(50),ieq0(50),jeq0(50)
	common/const/nset,neq,ieq0,jeq0,ieq,jeq,itcon,ialoc,itloc
	real*4 setlik(10)
	common/logliks/setlik	!likelihood for each set separately
c
	integer nyval(10),ncomp(10)
	integer nfit(10)
	real*4 ylow(10),yhigh(10)
	real*4 yexclow(10),yexchigh(10)
	COMMON/FITBLK/yval(500000,10),xaxis(510),freq(510),Nyval,
     & ylow,yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(51200),anfit,
     & nfitt,yvalt(5120),iconst
	common/FIXBLK/nfix,jfix(50)
c
c 06/03/89 06:33pm Lahey version of OBLIK. Data now in COMMON
c
c Modif 12/29/00 05:51pm for nset>1
c
c*** Modif March 89 for any number of components
c	Mean no of openings/burst for component= theta 1,3,5,...
c	areas=theta 2,4,6,...
C VERSION OF OBLIK WITH YVAL AS PARAM NOT COMMON FOR EKDIST,PTSCH5
C CALCULATES THE MINUS LOG LIKELIHOOD FOR FITTING SUM
C OF 2 GEOMETRICS TO RANDOM VARIABLE IN YVAL.
C 	DATA BETWEEN YLOW AND YHIGH  (INCLUSIVE) ARE USED (SEE
C	MLFIT). **NB YLOW MUST NOT BE LESS THAN ONE OR CALC OF
C	PROB(Y<YLOW)IS RUBBISH- THIS NOW CHECKED IN SCVDU1
C
C NOTE THAT RHO(I) MUST BE BETWEEN 0 AND 1 SO MEAN(I) (MEAN FOR
C COMPONENT I ALONE) MUST BE GREATER THAN 1.
c	debug=caplock()
c
c If(neq.gt.0) then check if the current amean is constrained -if so it is not
c present in theta so skip theta element and set amean(i,j) to be equal to
c whatever value is defined in ieq() etc -problem is that this value may
c not yet have been read from theta(), so must get all values from theta
c first
c next bit corrected (see PTYP1)
c	m=0
c	do j=1,nset 	!log likelihood calc sep for each set, and summed at end
c	   do i=1,ncomp(j)
c		if(neq.gt.0) then		!check if the current amean is constrained
c		   do n=1,neq
c			if(i.eq.ieq(n).and.j.eq.jeq(n)) then
c			   amean(i,j)=amean(ieq0(n),jeq0(n))
c			   rho(i,j)=1.0 - (1.0/amean(i,j))
c			   goto 21
c			endif
c		   enddo
c		endif
c		m=m+1
c		amean(i,j)=theta(m)
c		rho(i,j)=1.0 - (1.0/amean(i,j))
c21		continue
c		if(i.lt.ncomp(j)) then
c		   m=m+1
c		   area(i,j)=theta(m)
c		endif
c         enddo			!end of i=1,ncomp(j)
c	enddo			!end of j=1,nset
	if(nset.eq.1) then
	   m=0
	   do i=1,ncomp(1)
		m=m+1
		if(theta(m).lt.1.0) theta(m)=1.00001	!mean >= 1
		amean(i,1)=theta(m)
		rho(i,1)=1.0 - (1.0/theta(m))
		if(i.lt.ncomp(1)) then
		   m=m+1
		   area(i,1)=theta(m)
		endif
	   enddo
	endif
c For nset>1, first set all the non-constrained amean(i,j) (and the areas,
c never constrained)
	if(nset.gt.1) then
	   m=0
	   do j=1,nset 	!log likelihood calc sep for each set, and summed at end
		do i=1,ncomp(j),2
		   amean(i,j)=theta(j)		!set amean() -overwritten if constrained
		enddo
		do i=1,ncomp(j)
		   m=itcon(i,j)
		   if(m.ne.0) then
			amean(i,j)=theta(m)
			rho(i,j)=1.0 - (1.0/amean(i,j))
		   endif
		   n=ialoc(i,j)
		   if(n.ne.0) then
			area(i,j)=theta(n)
		   endif
		enddo
	   enddo
c  now set the constrained amean(i)
	  m=0
	  do j=1,nset 	!log likelihood calc sep for each set, and summed at end
	   do i=1,ncomp(j)
		if(neq.gt.0) then		!check if the current tau is constrained
		   do n=1,neq
			constrained=i.eq.ieq(n).and.j.eq.jeq(n)
			if(constrained) then
			   amean(i,j)=amean(ieq0(n),jeq0(n))
			   rho(i,j)=1.0 - (1.0/amean(i,j))
			endif
		   enddo
		endif
         enddo			!end of i=1,ncomp(j)
	  enddo			!end of j=1,nset
	endif
c All amean(i,j) set, and all area(i,j) except for last -set that now for each set
c Now add likelihoods for each set individually
	oblik=0.0	!initialise
c
	do j=1,nset
	   if(ncomp(j).eq.1) then
		area(1,j)=1.0
	   else
		sa=0.0	!total area exc last
		n=0
		if(nset.eq.1) then
		   do i=1,ncomp(j)
			if(amean(i,j).lt.1.0) then
			   amean(i,j)=1.0001		!mean>1
			   m=itcon(i,j)
			   theta(m)=amean(i,j)		!reset corresponding theta
			endif
			rho(i,j)=1.0 - (1.0/amean(i,j))
			if(ampos) then
			   if(area(i,j).gt.1.) area(i,j)=0.99999
			   if(area(i,j).lt.0.) area(i,j)=1.0e-6
			   if(i.lt.ncomp(j)) then	!last component not in theta
				m=ialoc(i,j)
				theta(m)=area(i,j)	!put altered value back in theta()
			   endif
			endif
			if(i.lt.ncomp(j)) then
			   sa=sa + area(i,j)
			endif
		   enddo
		else
		   do i=1,ncomp(j)
			if(itcon(i,j).ne.0) then	!amean free
			   if(amean(i,j).lt.1.0) then
				amean(i,j)=1.0001		!mean>1
				rho(i,j)=1.0 - (1.0/amean(i,j))
				m=itcon(i,j)
				theta(m)=amean(i,j)	!reset corresponding theta
			   endif
			endif
			rho(i,j)=1.0 - (1.0/amean(i,j))
			if(ampos) then
			   if(area(i,j).gt.1.) area(i,j)=0.99999
			   if(area(i,j).lt.0.) area(i,j)=1.0e-6
			   if(i.lt.ncomp(j)) then	!last component not in theta
				m=ialoc(i,j)
				theta(m)=area(i,j)	!put altered value back in theta()
			   endif
			endif
			if(i.lt.ncomp(j)) then
			   sa=sa + area(i,j)
			endif
		   enddo	!end of i=1,ncomp(j)
c All free amean(ij), area(i,j) now defined. If any are constrained, set them now
		   if(neq.gt.0) then
			do n=1,neq
			   i1=ieq(n)
			   j1=jeq(n)
			   amean(i1,j1)=amean(ieq0(n),jeq0(n))
			   rho(i1,j1)=1.0 - (1.0/amean(i1,j1))
			enddo
		   endif		!end of if(nset,gt,1)
		endif
	   endif	!end of ncomp>1
c COPE WITH FIXED AREAS AS IN EXPLIK
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
c NB at present all areas are in theta() (whether free or fixed)

	   if(ampos) then
		nafix=0		!number of fixed areas (in current set j)
		if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
		   do i=1,ncomp(j)-1
			jafix(i)=0
			m=ialoc(i,j)	!index in theta of area(i,j)
			if(jfix(m).eq.1) then
			   nafix=nafix+1	!number of fixed areas
			   jafix(i)=1	!area(i) is fixed
			endif
		   enddo
		endif		!end of nfix>0
		if(SA.ge.1.0) then
		   if(nafix.eq.0) then		!no fixed areas
			do i=1,ncomp(j)-1
			   area(i,j)=0.99*area(i,j)/SA
			   m=ialoc(i,j)
			   theta(m)=area(i,j)	!put altered value back in theta()
			enddo
			SA=0.99
		   else			!some areas fixed
			safix=0.0
			sadj=0.0
			do i=1,ncomp(j)-1
			   if(jafix(i).eq.0) then
				sadj=sadj+area(i,j)
			   else
				safix=safix+area(i,j)
			   endif
			enddo
			sa1=sadj/(0.99-safix)
			sa=0.0
			do i=1,ncomp(j)-1
			   if(jafix(i).eq.0) area(i,j)=area(i,j)/sa1
			   m=ialoc(i,j)
			   theta(m)=area(i,j)	!put altered value back in theta()
			enddo
		   endif
		endif		!end if if(sa.gt.1.)
24	   endif	!end of 'if(ampos)'
c
	   area(ncomp(j),j)=1.0 - SA	!NOT A PARAMETER
c
C NOTE THAT YVAL HAS BEEN REDEFINED IN MLFIT TO HOLD ONLY THE NFIT
C VALUES BETWEEN YLOW AND YHIGH INCLUSIVE. YVAL VALUES ARE NO OF
C OPENINGS/BURST, CONVERTED (IN GETDIS) TO FLOATING POINT.
C CALC DENOM=PROB(Y IS BETWEEN YLOW,YHIGH INCLUSIVE)
	   d=0.0
	   do i=1,ncomp(j)
		w(i)=area(i,j)*(1.0 - rho(i,j))	!w(i) for current set (# j)
	      y=ylow(j)-1.0
c	      if(debug) then
c		   print 700,i,rho(i,j),area(i,j),ylow(j),yhigh(j),y
c700		   format(i4,' rho(i,j),area(i,j),ylow(j),yhigh(j),y = ',5g13.6)
c		endif
		if(abs(y).le.0.00001) then
		   ry=1.0
		else
		   ry=rho(i,j)**y
		endif
		d=d + area(i,j)*(ry - (rho(i,j)**yhigh(j)))
c	      call UNDFL(errflag)
c		if(.not.errflag) then
c		   print 321,rho(i),y
c321		   format('Underflow in OBLIK; rho, y = ',2g13.6)
c		endif
	   enddo	!end of i=1,ncomp(j)
c
c Calc log(likelihood) for geometrics
	   setlik(j)=0.0
	   do i=1,nfit(j)
		y=yval(i,j) - 1.0		!op/bst-1
		t=0.
		do n=1,ncomp(j)
		   t=t + w(n)*(rho(n,j)**y)
		enddo
		if(t.lt.1.e-30) t=1.e-30
		setlik(j)=setlik(j) - ALOG(t)
	   enddo
	   if(d.lt.1.e-37) then
		print 34,d
34		format(' ERROR in OBLIK: d = ',g13.6)
		do i=1,ncomp(j)
		   print 35, i,area(i,j),w(i),amean(i,j)
35		   format(1x,i2,': area, w, mean = ',3g13.6)
	      enddo
	   endif
c
	   setlik(j)=setlik(j) + float(nfit(j))*ALOG(d)	!nfit is constant-but needed for errors
c
	   oblik=oblik + setlik(j)		!total likelihood to be maximized
c
	enddo		!end of j=1,nset
	RETURN
C
	END

