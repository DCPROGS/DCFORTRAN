	subroutine ADDFREQ(idata,nbuf,novlap,ifreq1,imin,imax,
     & ispnt1,inpnt1,ispnt2,inpnt2,bpnt1,bpnt2,ipsec1,isec,
     & icw1,nrlast,nsec,ioff,nread,sfac2,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,cjump,invert)
c
c To add points to all point amp histo in when SCAN is used
c all point amp histo with no fitting (last 2 lines of param are
c just for call to readsec
c
	integer*2 idata(1-novlap:nbuf+novlap)
	integer*4 ifreq1(imin:imax)
	character cnum*11
	logical cjump,invert,discprt
	common/dp/discprt
c
	ibase=ifixr(0.5*(bpnt1+bpnt2))	!mean baseline
	if(ipsec1.eq.isec) then		!all data in current section
	   do i=ispnt1,ispnt2
		j=idata(i)-ibase
		ifreq1(j)=ifreq1(j) + 1
	   enddo
	else
	   do msec=ipsec1,isec
     		call INTCONV(msec,cnum)
		n=NBLANK(cnum)
		call WDIALOG(1,
     &		'Reading section '//cnum(1:n)//
     &		' from disk . . .',-icw1)
		call READSEC(msec,idata,nrlast,nsec,ioff,nbuf,
     &	nread,novlap,sfac2,in1,in2,jn1,jn2,is1,is2,
     &	 js1,js2,cjump,invert)
		i1=ispnt1
		i2=is2
		if(msec.eq.ipsec1) i1=ispnt1
		if(msec.eq.isec) i2=ispnt2
		do i=i1,i2
		   j=idata(i)-ibase
		   ifreq1(j)=ifreq1(j) + 1
		enddo
	   enddo
	endif
	n1=inpnt2-inpnt1+1
     	call INTCONV(n1,cnum)
	n=NBLANK(cnum)
	call WDIALOG(1,cnum(1:n)//
     &  ' points added to all point amp histogram',icw1)
	 if(discprt) write(8,82) n1,inpnt1,ispnt1,ipsec1,
     &   inpnt2,ispnt2,isec
82	 format(/,1x,i9,
     & ' points added to all-point amplitude histogram',/,
     & ' From point ',i10,'(',i10,' in section',i6,')',/,
     & '   to point ',i10,'(',i10,' in section',i6,')')
c
	RETURN
	end

