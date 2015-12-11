	logical function ASSOC(ip,id,imod)
c Set true of theta(ip) is an association rate constant (for imod=29, 39 (CJH)
c and mod36 (DC) only at the moment)
	if(imod.ne.29.and.imod.ne.39.and.imod.ne.36) then
c	   print 1,imod
c1	   format(' Model # ',i3,' not yet recognised')
	   ASSOC=.false.
	   RETURN
	endif
	ASSOC=.false.
	if(ip.eq.8+id.or.ip.eq.10+id.or.
     &		ip.eq.12+id.or.ip.eq.14+id) then
	   ASSOC=.true.
	endif
	RETURN
	end

	logical function EQCONST(ip,npar,imod)
c Set true of theta(ip) is an association rate constant (for imod=29, 39 (CJH)
c and mod36 (DC) only at the moment)
	if(imod.ne.29.and.imod.ne.39.and.imod.ne.36) then
c	   print 1,imod
c1	   format(' Model # ',i3,' not yet recognised')
	   EQCONST=.false.
	   RETURN
	endif
	EQCONST=.false.
	if(ip.eq.npar+5.or.ip.eq.npar+6.or.
     &   		ip.eq.npar+7.or.ip.eq.npar+8) then
	   EQCONST=.true.
	endif
	RETURN
	end
	subroutine PAR(ypar,ip,npar,nsims,thetval,nsim1,exclude,imod0,
     &   ipex,p1,p2,elmval,elmset,ixval,nABORTW,nevals,nset,ips,id,
     &   iomit,nomit)
	real*8 thetval(npar,nsims),pv,p1,p2,pt
	real*8 elmval(nsims),elmset(nset,nsims)
	integer nevals(nsims),ixval(nsims),iomit(nsims)
	real*4 ypar(nsims)
	logical exclude
	logical ABORTW
	logical assoc,eqconst	!function
c
c To return ypar(i), i=1,nsims with values of parameter # ipar
c for mod29 only!  Assoc rates multiplied by 1.e-8
c If exclude=.true. then values for which parameter # ipex is outside
c the range p1 to p2 are excluded, and the number of good values is returned
c as nsim1
	n=0
	nABORTW=0
	do j=1,nsims
	   ABORTW=ixval(j).lt.0	!code used to indicate ABORTWed run in simwrt
	   if(ABORTW) then
		nABORTW=nABORTW+1
		goto 9
	   endif
c
	   if(iomit(j).eq.1) goto 9	!exclude run #j
c
	   if(exclude.and.imod0.eq.0) then
		if(ipex.le.npar) then
		   pt=thetval(ipex,j)
		else if(ipex.eq.npar+1) then
		   if(ips.eq.0) then
			pt=elmval(j)
		   else
			pt=elmset(ips,j)
		   endif
		else if(ipex.eq.npar+2) then
		   pt=dfloat(nevals(j))
		endif
c
		if(pt.le.p1.or.pt.gt.p2) goto 9
   	   else if(exclude.and.
     &	 imod0.ne.29.and.imod0.ne.39.and.imod0.ne.36) then
		if(ipex.le.npar) then
		   pt=thetval(ipex,j)
		else if(ipex.eq.npar+1) then
		   pt=thetval(2,j)/thetval(1,j)
		else if(ipex.eq.npar+2) then
		   pt=thetval(4+id,j)/thetval(3+id,j)
		else if(ipex.eq.npar+3) then
		   pt=thetval(6+id,j)/thetval(5+id,j)
		else if(ipex.eq.npar+5) then
		   if(ips.eq.0) then
			pt=elmval(j)
		   else
			pt=elmset(ips,j)
		   endif
		else if(ipex.eq.npar+6) then
		   pt=dfloat(nevals(j))
		endif
c
		if(pt.le.p1.or.pt.gt.p2) goto 9
c
	   endif	!end of if(exclude)
c
	   n=n+1
c
	   if(ip.le.npar) then
		pv=thetval(ip,j)
		if(ASSOC(ip,id,imod0)) then
		   pv=pv*1.d-8
		endif
		if(EQCONST(ip,npar,imod0)) then
		   pv=pv*1.d6
		endif
	   endif
	   if(imod0.eq.0) then
	      if(ip.eq.npar+1) then
		   if(ips.eq.0) then
			pv=elmval(j)
		   else
			pv=elmset(ips,j)
		   endif
		else if(ip.eq.npar+2) then
		   pv=dfloat(nevals(j))
		endif
	   else if(imod0.ne.29.and.imod0.ne.39.and.imod0.ne.36) then
		if(ip.eq.npar+1) then
		   pv=thetval(2,j)/thetval(1,j)
		else if(ip.eq.npar+2) then
		   pv=thetval(4+id,j)/thetval(3+id,j)
		else if(ip.eq.npar+3) then
		   pv=thetval(6+id,j)/thetval(5+id,j)
		else if(ip.eq.npar+4) then
		   pv=thetval(7+id,j) + thetval(9+id,j)
      	else if(ip.eq.npar+5) then
		   pv=1.d6*thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      	else if(ip.eq.npar+6) then
		   pv=1.d6*thetval(9+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
      	else if(ip.eq.npar+7) then
		   pv=1.d6*thetval(11+id,j)/thetval(12+id,j)	!K1a
      	else if(ip.eq.npar+8) then
		   pv=1.d6*thetval(13+id,j)/thetval(14+id,j)	!K1b
		else if(ip.eq.npar+9) then
		   if(ips.eq.0) then
			pv=elmval(j)
		   else
			pv=elmset(ips,j)
		   endif
		else if(ip.eq.npar+10) then
		   pv=dfloat(nevals(j))
		endif
	   endif
	   ypar(n)=sngl(pv)
9	   continue
	enddo
	nsim1=n
c
	RETURN
	end


	subroutine PAR2(ypar,ip1,ip2,npar,nsims,thetval,nsim1,exclude,
     & imod0,
     & ipex,p1,p2,ixval,elmval,elmset,nevals,nABORTW,nset,ips,itype,id,
     & iomit,nomit)
	 use menu_f90
	real*8 thetval(npar,nsims),pv1,pv2,p1,p2,pt
	real*4 ypar(nsims)
	real*8 elmval(nsims),elmset(nset,nsims)
	integer nevals(nsims),ixval(nsims),iomit(nsims)
	character*11 cnum
	logical exclude
	logical ABORTW
	logical assoc,eqconst	!function
c
c Version of PAR for itype=3,4,5,6 To return ypar(i), i=1,nsims containing
c the sum, diff, product or ratio of two parameters, #ip1 and #ip2 (mod 29)
c For mod29 only!
c Assoc rates multiplied by 1.e-8
c  Only for ip1,ip2 .le.npar
c If exclude=.true. then values for which parameter # ipex is outside
c the range p1 to p2 are excluded, and the number of good values is returned
c as nsim1
c Modif 07/30/02 12:12pm. Now fixed up to ip=npar+8
	if(ip1.gt.npar+8.or.ip2.gt.npar+8) then
	  call intconv(npar+8,cnum)
	  imes=gmdisplaymessagebox('',
     &	  ' This can be done only for parameters up to #'//cnum,
     &	   ginformation,gok)
	   ip1=1
	   ip2=2
	endif
	n=0
	nABORTW=0
	do j=1,nsims
	   ABORTW=ixval(j).lt.0	!code used to indicate ABORTWed run in simwrt
	   if(ABORTW) then
		nABORTW=nABORTW+1
		goto 9
	   endif
c
	   if(iomit(j).eq.1) goto 9	!exclude run #j
c
	   if(exclude.and.imod0.eq.0) then
		if(ipex.le.npar) then
		   pt=thetval(ipex,j)
		else if(ipex.eq.npar+1) then
		   if(ips.eq.0) then
			pt=elmval(j)
		   else
			pt=elmset(ips,j)
		   endif
		else if(ipex.eq.npar+2) then
		   pt=dfloat(nevals(j))
		endif
		if(pt.le.p1.or.pt.gt.p2) goto 9
   	   else if(exclude.and.
     &	 imod0.ne.29.and.imod0.ne.39.and.imod0.ne.36) then
		if(ipex.le.npar) then
		   pt=thetval(ipex,j)
		else if(ipex.eq.npar+1) then
		   pt=thetval(2,j)/thetval(1,j)
		else if(ipex.eq.npar+2) then
		   pt=thetval(4+id,j)/thetval(3+id,j)
		else if(ipex.eq.npar+3) then
		   pt=thetval(6+id,j)/thetval(5+id,j)
		else if(ipex.eq.npar+5) then
		   if(ips.eq.0) then
			pt=elmval(j)
		   else
			pt=elmset(ips,j)
		   endif
		else if(ipex.eq.npar+6) then
		   pt=dfloat(nevals(j))
		endif
c
		if(pt.le.p1.or.pt.gt.p2) goto 9
	   endif	!end of if(exclude)
	   n=n+1
c  Only for ip1,ip2 .le.npar
c Now fixed up to ip=npar+8
c===	   pv1=thetval(ip1,j)
	   if(ip1.le.npar) then
		pv1=thetval(ip1,j)
		if(ASSOC(ip1,id,imod0)) then
		   pv1=pv1*1.d-8
		endif
		if(EQCONST(ip1,npar,imod0)) then
		   pv1=pv1*1.d6
		endif
	   endif
	   if(imod0.ne.29.and.imod0.ne.39.and.imod0.ne.36) then
		if(ip1.eq.npar+1) then
		   pv1=thetval(2,j)/thetval(1,j)
		else if(ip1.eq.npar+2) then
		   pv1=thetval(4+id,j)/thetval(3+id,j)
		else if(ip1.eq.npar+3) then
		   pv1=thetval(6+id,j)/thetval(5+id,j)
		else if(ip1.eq.npar+4) then
		   pv1=thetval(7+id,j) + thetval(9+id,j)
      	else if(ip1.eq.npar+5) then
		   pv1=1.d6*thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      	else if(ip1.eq.npar+6) then
		   pv1=1.d6*thetval(9+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
      	else if(ip1.eq.npar+7) then
		   pv1=1.d6*thetval(11+id,j)/thetval(12+id,j)	!K1a
      	else if(ip1.eq.npar+8) then
		   pv1=1.d6*thetval(13+id,j)/thetval(14+id,j)	!K1b
		endif
	   endif
c
c===	   pv2=thetval(ip2,j)
	   if(ip2.le.npar) then
		pv2=thetval(ip2,j)
		if(ASSOC(ip2,id,imod0)) then
		   pv2=pv2*1.d-8
		endif
		if(EQCONST(ip2,npar,imod0)) then
		   pv2=pv2*1.d6
		endif
	   endif
	   if(imod0.ne.29.and.imod0.ne.39.and.imod0.ne.36) then
	    if(ip2.eq.npar+1) then
		pv2=thetval(2,j)/thetval(1,j)
	    else if(ip2.eq.npar+2) then
		pv2=thetval(4+id,j)/thetval(3+id,j)
	    else if(ip2.eq.npar+3) then
		pv2=thetval(6+id,j)/thetval(5+id,j)
	    else if(ip2.eq.npar+4) then
		pv2=thetval(7+id,j) + thetval(9+id,j)
          else if(ip2.eq.npar+5) then
		pv2=1.d6*thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
          else if(ip2.eq.npar+6) then
		pv2=1.d6*thetval(9+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
          else if(ip2.eq.npar+7) then
		pv2=1.d6*thetval(11+id,j)/thetval(12+id,j)	!K1a
          else if(ip2.eq.npar+8) then
		pv2=1.d6*thetval(13+id,j)/thetval(14+id,j)	!K1b
	    endif
	   endif
c
	   if(itype.eq.3) then
		ypar(n)=sngl(pv1+pv2)
	   else if(itype.eq.4) then
		ypar(n)=sngl(pv1-pv2)
	   else if(itype.eq.5) then
		ypar(n)=sngl(pv1*pv2)
	   else if(itype.eq.6) then
		ypar(n)=sngl(pv1/pv2)
	   endif
9	   continue
	enddo
	nsim1=n
c
	RETURN
	end
