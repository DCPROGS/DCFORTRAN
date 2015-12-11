	subroutine HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     & aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     & aKmax,Ymmax,hillmax,dans)
c Tp display a particular expt in hypfit.for
	real c(100),w(100),yobs(100)
	real*4 aK(nrun,6),Ym(nrun,6),hill(nrun)
	real*4  sdK(nrun),sdY(nrun),aKlo(nrun),aKhi(nrun),
     & Ylo(nrun),Yhi(nrun),cor(nrun)
	real*4 aKlo2(nrun),aKhi2(nrun),Ylo2(nrun),Yhi2(nrun)
	character*1 UC,dans
	logical discprt
	common/dp/discprt
c
	      print 73,irun
73	      format(/,' Run ',i5,/,'      x             y           w')
		do i=1,n
		   print 74,c(i),yobs(i),w(i)
74		   format(g13.6,2x,g13.6,2x,g13.6)
		enddo
		print 75
75		format(' Estimates:       K         Ymax ')
		do j=1,5
		   print 76,j,aK(irun,j),Ym(irun,j)
76		   format(' Method ',i2,3x,g13.6,2x,g13.6)
		enddo
		print 761,6,aK(irun,6),Ym(irun,6),hill(irun)
761		format(' Method ',i2,3x,g13.6,2x,g13.6,2x,'nH = ',g13.6)
		print 77
77		format(' For least squares estimates of hyperbola')
		print 78,sdK(irun),sdY(irun),cor(irun)
78		format(' s(K) = ',g13.6,' s(Ymax) = ',g13.6,' r = ',g13.6)
		print 79,aKlo(irun),aKhi(irun),Ylo(irun),Yhi(irun)
79		format(' m=0.5 intervals:',/,
     &	' For K ',2g13.6,' For Ymax: ',2g13.6)
		print 80,aKlo2(irun),aKhi2(irun),Ylo2(irun),Yhi2(irun)
80		format(' m=2.0 intervals:',/,
     &	' For K ',2g13.6,' For Ymax: ',2g13.6)
		print 81,UC(dans)
81		format(' Display the simulated experiment [',a1,'] ? ')
		call INPUTa(dans)
		if(dans.eq.'Y') then
		   call HYPDISP1(aK,Ym,hill,irun,nrun,c,yobs,w,n,
     &		aKtrue,Ymtrue,aKmax,Ymmax,hillmax)
		endif
		RETURN
	end
