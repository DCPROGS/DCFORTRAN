	subroutine MINMAX1(xval,yval,ndat,xmin1,xmax1,ymin1,ymax1,
     & logx,logy,ndimd,ncurvd,icurvd)
c Version of minmax for VHIST.
c 07/27/92 10:48am Small problem -for histo DATA (but not calc curves)
c have ndat()=nbin, but data goes up to nbin+2 (if 'hi bin' present), or
c up to nbin+1 (otherwise). To solve this MINMAX1 now called
c with ndat1()=ndat()+2 in VHIST1
c NB if XVAL(0:511,ndimd) used in call, then results OK but
c elements numbered 1-512 here (see TDIM.FOR)
c Ndimd=2nd dimension in main prog of xval,yval
	dimension XVAL(512,ndimd),YVAL(512,ndimd)
	integer ndat(ndimd)	!contain number of points in each array
	integer icurvd(ndimd)	!contain numbers of arrays actually used
	logical logx,logy
c
c For Yval want the largest/smallest values in any of the arrays that
c are used
	ymin1=1.e37
	xmin1=1.e37
	ymax1=-1.e37
	xmax1=-1.e37
	do 2 j1=1,ncurvd		!scan ncurvd data sets
	j=icurvd(j1)		!data set to be scanned
	do 1 i=1,ndat(j)
	if(logy.and.yval(i,j).lt.1.e-37) goto 1
	if(logx.and.xval(i,j).lt.1.e-37) goto 1
	if(xval(i,j).lt.xmin1) xmin1=xval(i,j)
	if(xval(i,j).gt.xmax1) xmax1=xval(i,j)
	if(yval(i,j).lt.ymin1) ymin1=yval(i,j)
	if(yval(i,j).gt.ymax1) ymax1=yval(i,j)
1	continue
2	continue
	RETURN
	end

