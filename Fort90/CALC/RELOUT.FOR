	subroutine RELOUT(cfac,dgamma,pinf,bmj,eigen,t0,bm,tau,
     & cur0,curinf,cur0tot,bmrel,atotd,nlig,ligname,pon)
c
c To print details of current relaxation in SCALCS. All param up to
c t0 are inputs, rest are outputs.
c Modif 10/13/98 09:18pm to print components of binding curve
c Modif 03/10/95 08:46am to print areas (pC) rather than rel amp
c (but bmrel still returned as parameter)
c If t0 is negative then prints normally
c If t0 is positive then prints also current at time t0
	real*8 Pinf(100),dgamma(100)
	real*8 eigen(100)
	real*8 bmj(100,100),bm(100),area(100),atotd,amptotd
	real*8 bmrel(100),bmax(10),bmax1
	real*8 cur0(100),tau(100),curinf,cur0tot,t,ct,cfac1
	character*20 ligname(10)
	logical discprt,pon
	common/dp/discprt
	common/KBLK/kA,kB,kC,kD
c additions for components of binding curve
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	real*8 bound(100,10),binf
	integer nbound(100,10)
	common/nbnd/nbound
c
c
	k=kA+kB+kC+kD
	cfac1=dble(cfac)
c
	do m=1,k-1
	  tau(m)=-1.d3/eigen(m)
	enddo
c
	curinf=0.0d0
	do j=1,kA
	   curinf=curinf+cfac1*dgamma(j)*pinf(j)
	enddo
c
	print 36
      if(pon) write(7,36)
      if(discprt) write(8,36)
c36	format(/,' Time course for Current',/,
c     & ' Component  eigenvalue        tau (ms)      amp at t=0 (pA)',
c     & '    Area (pC)   ')
cc     & '    Relative amp')
36	format(/,' Time course for Current',/,
     & ' Comp  Eigenvalue    Tau (ms)     Amp (t=0, pA)   Rel amp  ',
     & '    Area (pC)')
	cur0tot=0.0d0
	atotd=0.0d0
	bmax1=-1.d200
	do m=1,k-1
	    bm(m)=0.0d0
	    do j=1,kA
	       bm(m)=bm(m) + dgamma(j)*bmj(m,j)
	    enddo
	    if(dabs(bm(m)).gt.bmax1) bmax1=dabs(bm(m))
	    cur0(m)=cfac1*bm(m)
	    area(m)=cur0(m)*tau(m)
	    cur0tot=cur0tot + cur0(m)
	    atotd=atotd+area(m)
	enddo
	do m=1,k-1
	    bmrel(m)=bm(m)/bmax1
c==	    print 39,m,sngl(eigen(m)),tau(m),cur0(m),bmrel(m)
c==       if(pon) write(7,39) m,sngl(eigen(m)),tau(m),cur0(m),bmrel(m)
c==       if(discprt) write(8,39) m,sngl(eigen(m)),tau(m),cur0(m),
c==     & bmrel(m)
c=	    print 39,m,sngl(eigen(m)),sngl(tau(m)),
c=     &	sngl(bm(m)),sngl(area(m))
c=          if(discprt) write(8,39) m,sngl(eigen(m)),sngl(tau(m)),
c=     &	sngl(bm(m)),sngl(area(m))
	    print 39,m,sngl(eigen(m)),sngl(tau(m)),
     &	cur0(m),bmrel(m),sngl(area(m))
          if(discprt) write(8,39) m,sngl(eigen(m)),sngl(tau(m)),
     &	cur0(m),bmrel(m),sngl(area(m))
c39	    format(i8,3x,g13.6,3x,g13.6,3x,g13.6,4x,g11.4)
39	    format(i3,2x,g12.5,1x,g12.5,3x,g12.5,3x,f9.5,4x,g11.4)
	enddo
	cur0tot=cur0tot+curinf	!add asymptote!
	print 391,cur0tot,curinf,atotd
      if(pon) write(7,391) cur0tot,curinf,atotd
      if(discprt) write(8,391) cur0tot,curinf,atotd
391	format(
     & ' Total current at t=0 (pA) = ',g13.6,/,
     & ' Total current at equilibrium (pA) = ',g13.6,/,
     & ' Total area (pC) = ',g11.4)
c If t0 >0 then print current at t0 also
	if(t0.gt.1.e-10) then
	  t=dble(t0*1.d3)		!time in ms
	  ct=0.0d0
	  do 11 m=1,k-1		!the k-1 components
	   ct=ct+cur0(m)*dexp(-t/tau(m))
11	  continue
	  ct=ct + curinf		!total current
        print 12,t,ct
        if(pon) write(7,12) t,ct
        if(discprt) write(8,12) t,ct
12	  format(
     & ' At t= ',g11.4,'ms, current is ',g13.6,'pA')
	endif
c
	if(ncdep.eq.0) RETURN		!no binding
c Now  print components of ligand binding
	n=1		!ligand number
c
	do n=1,nlig
461	  print 46,n,ligname(n)
         if(pon) write(7,46) n,ligname(n)
         if(discprt) write(8,46) n,ligname(n)
46	   format(/,
     & ' Time course of BINDING for ligand number ',i2,'(',a10,'):',/,
     & ' Component  eigenvalue        tau (ms)      amp at t=0    ',
     & '    Area (ms)   ')
	   bmax(n)=-1.d200
	   do i=1,k
		bound(i,n)=dble(float(nbound(i,n)))
		if(bound(i,n).gt.bmax(n)) bmax(n)=bound(i,n)
	   enddo
	   binf=0.0d0		!equilib binding occ
	   do j=1,k
		binf=binf + bound(j,n)*pinf(j)
	   enddo
	   binf=binf/bmax(n)
	   amptotd=0.0d0	!now total binding occ at t=0
	   atotd=0.0d0		!now total area (t=0 to inf) under binding curve
	   do m=1,k-1
		bm(m)=0.0d0
		do j=1,k
		   bm(m)=bm(m) + bound(j,n)*bmj(m,j)
		enddo
		bm(m)=bm(m)/bmax(n)
		amptotd=amptotd+bm(m)
		area(m)=bm(m)*tau(m)
		atotd=atotd+area(m)
	   enddo
	   do m=1,k-1
		print 40,m,sngl(eigen(m)),sngl(tau(m)),
     &	 sngl(bm(m)),sngl(area(m))
      	if(pon) write(7,40) m,sngl(eigen(m)),sngl(tau(m)),
     &	 sngl(bm(m)),sngl(area(m))
      	if(discprt) write(8,40) m,sngl(eigen(m)),sngl(tau(m)),
     &	 sngl(bm(m)),sngl(area(m))
40		format(i8,3x,g13.6,3x,g13.6,3x,g13.6,3x,g13.6)
	   enddo
	   amptotd=amptotd+binf
	   print 41,n,amptotd,binf
         if(pon) write(7,41) n,amptotd,binf
         if(discprt) write(8,41) n,amptotd,binf
41	   format(' Ligand number ',i2,/,
     & ' Total binding occupancy at t=0 = ',g13.6,/,
     & ' Total binding occupancy at equilibrium = ',g13.6)
	enddo
c
	RETURN
	end

