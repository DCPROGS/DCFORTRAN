

subroutine store_record3d(isens,iplotype,modplot,xval1,yval1,z1,bad,nx,ny,ndx,ndy,&
     	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,&
        quarter,idev,plot,iplot,kcol,posneg,isetcol,icol,&
     	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
        xlop,xhip,ylop,yhip,main,ixg,iyg,ipos,ixposv,iyposv,&
     	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,&
        ijust,ijusx,ijusy,ijusz,&
        xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
        xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,&
        ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,&
        nxstep,nystep,istyle,isup,&
        fill,inter,axis,fillbad,autplt,cross,mark)


!ILOG=0 for arithmetic plot,
!	 =1 for plot Y vs log(x)
!	 =2 for plot log(Y) vs x
!	 =3 for plot log(Y) vs log(x)
!	 =4 for Hill plot
!	 =5 for sqrt(y) vs x
!	 =6 for sqrt(y) vs log(x)
    use hjcrecords
    TYPE (RECORD_ATTRIBUTES) oldrecords(25)
    real*4 z1(ndx,ndy)
	real*4 xval1(ndx),yval1(ndy)
	logical bad(ndx,ndy)
    logical fillbad,autplt
	integer*4 icol(100)
	character*75 xtitle,ytitle,ztitle
	character*75 title1
	logical all,cross,after,posneg,monsav
	logical mono,plot,fill,inter,present,landplot,axis
	logical quarter,mark
    INTEGER ISTYLE(6),ISUP(6)
if(isens.eq.1) then
	oldrecords(modplot)%iplot=iplot
	do i=1,100
        oldrecords(modplot)%attributes%ICOL(i)=iCOL(i)
    enddo
    oldrecords(modplot)%attributes%ICOL(100)=kCOL
	oldrecords(modplot)%iplotype=iplotype

	oldrecords(modplot)%STR(1)=title1

	oldrecords(modplot)%STR(3)=xtitle
	oldrecords(modplot)%STR(4)=ytitle
	oldrecords(modplot)%STR(5)=ztitle
	oldrecords(modplot)%xmin=xmin
	oldrecords(modplot)%ymin=ymin
	oldrecords(modplot)%ymax=ymax
	oldrecords(modplot)%xmax=xmax
	oldrecords(modplot)%Wxmin=Wxmin
	oldrecords(modplot)%wymin=wymin
	oldrecords(modplot)%wymax=wymax
	oldrecords(modplot)%wxmax=wxmax
	
	oldrecords(modplot)%attributes%IFNT(1)=ifnt
	oldrecords(modplot)%attributes%IJUS(1)=ijust
	oldrecords(modplot)%attributes%angle(1)= alfa
	oldrecords(modplot)%attributes%Idraw(1)=idrawt
	oldrecords(modplot)%attributes%ICOL(1)=iCOL(25)
	oldrecords(modplot)%attributes%rx(1)=xst
	oldrecords(modplot)%attributes%ry(1)=yst

	oldrecords(modplot)%attributes%IFNT(3)=ifnl
	oldrecords(modplot)%attributes%IJUS(3)=ijusx
	oldrecords(modplot)%attributes%angle(3)= beta
	oldrecords(modplot)%attributes%Idraw(3)=idrawx
	oldrecords(modplot)%attributes%ICOL(3)=iCOL(23)
	oldrecords(modplot)%attributes%rx(3)=xspx
			oldrecords(modplot)%attributes%ry(3)=yspx
	
	oldrecords(modplot)%attributes%IFNT(4)=ifnl
	oldrecords(modplot)%attributes%IJUS(4)=ijusy
	oldrecords(modplot)%attributes%angle(4)= gama
	oldrecords(modplot)%attributes%Idraw(4)=idrawy
	oldrecords(modplot)%attributes%ICOL(4)=iCOL(23)
	oldrecords(modplot)%attributes%rx(4)=xspy
			oldrecords(modplot)%attributes%ry(4)=yspy
	
	oldrecords(modplot)%attributes%IFNT(5)=ifnl
	oldrecords(modplot)%attributes%IJUS(5)=ijusz
	oldrecords(modplot)%attributes%angle(5)= delta
	oldrecords(modplot)%attributes%Idraw(5)=idrawz
	oldrecords(modplot)%attributes%ICOL(5)=iCOL(23)
	oldrecords(modplot)%attributes%rx(5)=xspz
			oldrecords(modplot)%attributes%ry(5)=yspz
	
	oldrecords(modplot)%numbers%numbx=numx
	oldrecords(modplot)%numbers%numby=numy
	oldrecords(modplot)%numbers%inumx=nxstep
	oldrecords(modplot)%numbers%inumy=nystep
	oldrecords(modplot)%param_axis%ntx=nx
	oldrecords(modplot)%param_axis%nty=ny
	oldrecords(modplot)%param_axis%nx1=nx1
	oldrecords(modplot)%param_axis%ny1=ny1
	oldrecords(modplot)%param_axis%itx=ndx
	oldrecords(modplot)%param_axis%ity=ndy
	oldrecords(modplot)%param_axis%xtic=xtic
	oldrecords(modplot)%param_axis%ytic=ytic
	oldrecords(modplot)%param_axis%numaxi=numaxi
	oldrecords(modplot)%param_axis%nwidth=nwidth
	oldrecords(modplot)%param_axis%npow=npow
	oldrecords(modplot)%param_axis%nplace=nplace

	do i=1,ndx
	oldrecords(modplot)%xVal(i,1)=xval1(i)
	enddo
	do i=1,ndy
	oldrecords(modplot)%yVal(i,1)=yval1(i)
	enddo
	do i=1,ndx
	do j=1,ndy
	oldrecords(modplot)%g3dpar%bad(i,j)=bad(i,j)
	oldrecords(modplot)%g3dpar%z(i,j)=z1(i,j)
	enddo
	enddo
	oldrecords(modplot)%g3dpar%all=all
	oldrecords(modplot)%g3dpar%cross=cross
	oldrecords(modplot)%g3dpar%fill=fill
	oldrecords(modplot)%g3dpar%fillbad=fillbad
	oldrecords(modplot)%g3dpar%mark=mark
	oldrecords(modplot)%g3dpar%posneg=posneg
	oldrecords(modplot)%g3dpar%inter=inter
	oldrecords(modplot)%g3dpar%ratio=ratio
	oldrecords(modplot)%g3dpar%rat=rat
	oldrecords(modplot)%g3dpar%radius=radius
	oldrecords(modplot)%g3dpar%ifram=ifram
	oldrecords(modplot)%g3dpar%theta=theta
	oldrecords(modplot)%g3dpar%phi=phi
	do i=1,6
	oldrecords(modplot)%g3dpar%istyle(i)=istyle(i)
	oldrecords(modplot)%g3dpar%isup(i)=isup(i)
	enddo
	
else
    iplot=oldrecords(modplot)%iplot
    do i=1,100
        iCOL(i)=oldrecords(modplot)%attributes%ICOL(i)
     enddo
     kcol=oldrecords(modplot)%attributes%ICOL(100)
	iplotype=oldrecords(modplot)%iplotype

	title1=oldrecords(modplot)%STR(1)

	xtitle=oldrecords(modplot)%STR(3)
	ytitle=oldrecords(modplot)%STR(4)
	ztitle=oldrecords(modplot)%STR(5)
	xmin=oldrecords(modplot)%xmin
	ymin=oldrecords(modplot)%ymin
	ymax=oldrecords(modplot)%ymax
	xmax=oldrecords(modplot)%xmax
	wxmin=oldrecords(modplot)%Wxmin
	wymin=oldrecords(modplot)%wymin
	wymax=oldrecords(modplot)%wymax
	wxmax=oldrecords(modplot)%wxmax
	
	ifnt=oldrecords(modplot)%attributes%IFNT(1)
	ijust=oldrecords(modplot)%attributes%IJUS(1)
	alfa=oldrecords(modplot)%attributes%angle(1)
	idrawt=oldrecords(modplot)%attributes%Idraw(1)
	iCOL(25)=oldrecords(modplot)%attributes%ICOL(1)
	xst=oldrecords(modplot)%attributes%rx(1)
	yst=oldrecords(modplot)%attributes%ry(1)

	ifnl=oldrecords(modplot)%attributes%IFNT(3)
	ijusx=oldrecords(modplot)%attributes%IJUS(3)
	beta=oldrecords(modplot)%attributes%angle(3)
	idrawx=oldrecords(modplot)%attributes%Idraw(3)
	iCOL(23)=oldrecords(modplot)%attributes%ICOL(3)
	xspx=oldrecords(modplot)%attributes%rx(3)
	yspx=oldrecords(modplot)%attributes%ry(3)
	
	ifnl=oldrecords(modplot)%attributes%IFNT(4)
	ijusy=oldrecords(modplot)%attributes%IJUS(4)
	gama=oldrecords(modplot)%attributes%angle(4)
	idrawy=oldrecords(modplot)%attributes%Idraw(4)
	icol(23)=oldrecords(modplot)%attributes%ICOL(4)
	xspy=oldrecords(modplot)%attributes%rx(4)
	yspy=oldrecords(modplot)%attributes%ry(4)
	
	ifnl=oldrecords(modplot)%attributes%IFNT(5)
	ijusz=oldrecords(modplot)%attributes%IJUS(5)
	delta=oldrecords(modplot)%attributes%angle(5)
	idrawz=oldrecords(modplot)%attributes%Idraw(5)
	icol(23)=oldrecords(modplot)%attributes%ICOL(5)
	xspz=oldrecords(modplot)%attributes%rx(5)
	yspz=oldrecords(modplot)%attributes%ry(5)
	
	numx=oldrecords(modplot)%numbers%numbx
	numy=oldrecords(modplot)%numbers%numby
	nxstep=oldrecords(modplot)%numbers%inumx
	nystep=oldrecords(modplot)%numbers%inumy
	nx=oldrecords(modplot)%param_axis%ntx
	ny=oldrecords(modplot)%param_axis%nty
	nx1=oldrecords(modplot)%param_axis%nx1
	ny1=oldrecords(modplot)%param_axis%ny1
	ndx=oldrecords(modplot)%param_axis%itx
	ndy=oldrecords(modplot)%param_axis%ity
	xtic=oldrecords(modplot)%param_axis%xtic
	ytic=oldrecords(modplot)%param_axis%ytic
	numaxi=oldrecords(modplot)%param_axis%numaxi
	nwidth=oldrecords(modplot)%param_axis%nwidth
	npow=oldrecords(modplot)%param_axis%npow
	nplace=oldrecords(modplot)%param_axis%nplace

	do i=1,ndx
	xval1(i)=oldrecords(modplot)%xVal(i,1)
	enddo
	do i=1,ndy
	yval1(i)=oldrecords(modplot)%yVal(i,1)
	enddo
	do i=1,ndx
	do j=1,ndy
	bad(i,j)=oldrecords(modplot)%g3dpar%bad(i,j)
	z1(i,j)=oldrecords(modplot)%g3dpar%z(i,j)
	enddo
	enddo
	all=oldrecords(modplot)%g3dpar%all
	cross=oldrecords(modplot)%g3dpar%cross
	fill=oldrecords(modplot)%g3dpar%fill
	fillbad=oldrecords(modplot)%g3dpar%fillbad
	mark=oldrecords(modplot)%g3dpar%mark
	posneg=oldrecords(modplot)%g3dpar%posneg
	inter=oldrecords(modplot)%g3dpar%inter
	ratio=oldrecords(modplot)%g3dpar%ratio
	rat=oldrecords(modplot)%g3dpar%rat
	radius=oldrecords(modplot)%g3dpar%radius
	ifram=oldrecords(modplot)%g3dpar%ifram
	theta=oldrecords(modplot)%g3dpar%theta
	phi=oldrecords(modplot)%g3dpar%phi
	do i=1,6
	istyle(i)=oldrecords(modplot)%g3dpar%istyle(i)
	isup(i)=oldrecords(modplot)%g3dpar%isup(i)
	enddo
	
endif
end
		