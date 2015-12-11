subroutine make_number(z,x,y,xaxis,ijus,it,dxs,dys,ipow,string,expon,cnum,cexpxy,str,&
logx,logy)
	

	logical xaxis,expon,logx,logy
	character cexpxy*10,cnum*10,str*20,string*11,cnum1*10
	cnum1=''
	cnum=''
	if(expon) then
	left=1
	center=0
	right=-1
	
	if(xaxis) then
	   if(it.eq.1.or.it.eq.0) then
		ijus=0
		y=y-abs(8.*dys)! down to leave room for superscript
		if(it.eq.0)  y= y-abs(dys)	!down further if tics central
	   else if(it.eq.-1) then
		ijus=0
		y=y-abs(8.*dys)
	   endif
	else if(.not.xaxis) then
	   y=y-dys
	   if(it.eq.1.or.it.eq.0) then
		ijus=1
		x= x-3.*dxs	!left to leave room for superscript
		if(it.eq.0)  x= x-dxs	!left further if tics central
	   else if(it.eq.-1) then
		ijus=1
		x= x-4.*dxs	!left further if tics central
	   endif
	endif

	cnum='10'
!	call SETSIZE(size)	!this defines ISIZE
	nmax=0	!max number of sig figs after decimal point
	call CHAEXI(0.8,0.8,0.6,0.3)
	call FIXDEC1(z,m0,n1,NMAX)
	call DCFORMAT(z,m0+n1+3,0,cexpxy)	!0 figs after dec point
	ns=len_trim(cexpxy)
	if(cexpxy(ns:ns).eq.'.') ns=ns-1	!remove decimal point
	ifs=2
	if(z.gt.0) ifs=3		!omit first char if pos
	cexpxy=cexpxy(ifs:ns)
	nl=len_trim(cexpxy)
	str=cnum(1:2)//char(42)//char(69)//cexpxy(1:nl)
	cnum=str(1:10)
	else
	if(xaxis) then
		ijus=0
		Y=Y-8.*dys
		x=x-2.*dxs
	else if(.not.xaxis) then
		ijus=1
		x=x-3.*dxs
		y=y-dys
	endif
	nmax=3  !max number of sig figs after decimal point

	call FIXDEC1(z,m0,n1,NMAX)
	call DCFORMAT(z,m0+n1+3,n1,cnum)

	ns=len_trim(cnum)
	if0=2
	if(m0.eq.0.and.z.lt.0.0) if0=1

	if(n1.eq.0.and.ns.gt.1.and.cnum(ns:ns).eq.'.') ns=ns-1
	cnum=cnum(if0:ns)
	
	nl=len_trim(cnum)
	
	do i=1,nl
		if(cnum(i:i).eq.'.') nle=i 
	enddo
	nri=nl-nle
	if(nri.gt.3) then
		nri=3
		nl=nle+nri
		do j=1,nl
			cnum1(j:j)=cnum(j:j)
		enddo
		!cnum=cnum(1:nl+1)
	endif
	nls=len_trim(string)
	if(ipow.ne.-10000.and.xaxis) then
	    cnum=cnum(1:nl)//string(1:nls)
	    x=x+0.5*dxs
	endif
	endif
	
	
	RETURN
	end