	program TVSAMP
	character adcfil*33
	logical consamdef,colseq,reject
	logical mouse_on,mous_set
	real*4 yline(10)
	common/mousval/mouse_on,nbutton
c
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	mous_set=.false.	!initialise
	consamdef=.false.
c
	adcfil='d:\scdat\cmc\990129c3.dat'
c	print *, 'idelt = '
c	call INPUTi(idelt)
c	idelt=0.
	i1= 430000
	i2= 530000
	ib1=477000
	ib2=477500
	j1=477100
	j2=477300
	colseq=.true.
c	i1=i1+idelt
c	i2=i2+idelt
c	ib1=ib1+idelt
c	ib2=ib2+idelt
	ybase=-100.		!so baseline estimated internally
c	nline=0
	nline=4
	yline(1)=-2.
	yline(2)=-4.
	yline(3)=-6.
	yline(4)=-8.
	imode=1	!estimate baseline internally in VSAMP
	amp=-1.		!scale internally
	reject=.true.
	call VSAMP(i1,i2,ib1,ib2,consamdef,adcfil,
     &   nline,yline,ybase,amp,imode,colseq,j1,j2,'Message here',reject)
c     &  nline,yline,ybase,amp,imode,colseq,j1,j2,'')
	print 1,reject
1	format(' reject = ',L4)
	end

