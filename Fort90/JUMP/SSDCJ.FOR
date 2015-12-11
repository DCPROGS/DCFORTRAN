	function SSDCJ(k,theta,xfit,yfit,ndv1)
C SUM OF SQUARES FOR FITTING MODEL #NMOD WITH CJFIT
C
	real THETA(20)
	LOGICAL CONSTR
	real*4 Xfit(ndv1),Yfit(ndv1)		!now alloc in main prog
c=	common/data/Xfit(2048),Yfit(2048)
	real*8 S,s1,dev
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/minpar/thmin(20)
c
	S=0.0d0
	if(constr) then
	  do 2 i=1,k
2	  if(theta(i).lt.thmin(i)) theta(i)=thmin(i)
	else if(nmod.eq.0) then		!check tau for exponentials anyway
	  do 21 i=1,k-1,2		!tau=1,3,5,...
21	  if(theta(i).lt.thmin(i)) theta(i)=thmin(i)
	endif
C
	do 40 i=1,nfit
	i1=i
	xv1=Xfit(i)		!see new Ycalcj
	Yc=Ycalcj(kmax,THETA,xv1,i1)
	dev=dble(Yfit(i)-Yc)
	S=S + dev*dev
40	continue
c
	s1=dabs(s)
	if(s1.gt.1.18d-38.and.s1.lt.3.4d38) then
	   SSDCJ=sngl(S)
	else if(s1.le.1.18d-38) then
	   sign=1.0
	   if(s.lt.0.0d0) sign=-1.0
	   SSDCJ=sign*1.18e-38
	else if(s1.ge.3.4d38) then
	   sign=1.0
	   if(s.lt.0.0d0) sign=-1.0
	   SSDCJ=sign*3.4e38
	endif
	RETURN
	END

