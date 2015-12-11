	subroutine LOGBIN(xaxis,nbmax,tres,nbin,nbdec,xwbase,ymax)
c To set log-bins for x and y in EKDIST (used in CDIST3D to set bins for
c x and y axis
	real*4 xaxis(nbmax)
	character*22 cha
	logical discprt
	common/dp/discprt

c
	dx=exp1(alog(10.)/float(nbdec))
	xend=1. + ymax - amod(ymax,1.)
	call realtoch(xend,cha,22)
      ians=gmdisplymessagebox('',' Last x value (ms)= '//cha,
     &    gquestion,gyesno)
	
	if(ians.eq.gyesbutton) goto 71
75	print 72
72	format('&Last x value (ms) = ')
	call INPUTr(xend)

71	nbin=1+ifix(alog(xend/xaxis(1))/alog(dx))
	if(nbin.gt.nbmax) nbin=nbmax
	xend=(dx**nbin)*xaxis(1)
	print 1047,nbin,xend
1047	format('&',1x,i5,' bins; last x reset to ',g13.6,': O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'N') then
	   do n=nbin-9,nbin
		xend=(dx**n)*xaxis(1)
		print 10,n,xend
10		format(' nbin = ',i4,' goes up to ',g13.6,' ms')
	   enddo
	   print 11,nbin
11	   format(' number of bins [',i3,'] = ')
	   call INPUTi(nbin)
	endif


	do i=1,nbin
	   xaxis(i+1)=xaxis(1)*(dx**i)
	enddo
73	xwbase=alog10(dx)	  !in log10 units to scale fit
	print 1045,nbin,dx,xaxis(1),xend
	if(discprt) write(8,1045) nbin,dx,xaxis(1),xend
1045	format(
     &' Distribution of log(t) displayed- ',i6,' bins, factor= ',g11.4,
     &'   from ',g11.4,' ms to ',g11.4,' ms')
c
	RETURN
	end

