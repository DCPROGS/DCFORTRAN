	subroutine dev1(ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,
     &nrows,icvalve,icshad,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,ic,icfill,
     &nbut,iym,ixm,ixmo,iymo)


c	input  : ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,nrows
c     output : x,y,xp,yp,ncolomns

	integer*4  	x(nvalves),y(nvalves),xp(nvalves),yp(nvalves)
	integer*4  	xc(nvalves),yc(nvalves),iym(nbut),iymo(nbut)
	integer*4   ym(5),y_rack(5)
	character*5	ch,cha
	ncol=int(nvalves/5)
	if((nvalves-ncol*5).ne.0) ncol=ncol+1
	ixhi=640
	ixhi1=0.8*ixhi
	idxm=ixhi-ixhi1
	idx=ixhi1/ncol
	x(1)=0.2*idx
	xc(1)=0.5*idx
	xm=0.2*idxm
	lv=0.6*idx
	lm=0.6*idxm

	idy=64
	y(1)=344
	y_rack(1)=y(1)-8
	yc(1)=y(1)-16
	ym(1)=yc(1)

	ihv=24
	ih_rack=2*ihv
	ihm=40
	iwv=16
	iwm=16

	do i=2,5
	   y(i)=y(1)-(i-1)*idy
	   y_rack(i)=y_rack(1)-(i-1)*idy
	   ym(i)=ym(1)-(i-1)*idy
	   yc(i)=yc(1)-(i-1)*idy
	enddo
	do i=1,ncol
	   x(i)=x(1)+i*idx
	enddo
	do i=1,5
	   call fillwin(ixlo+8,y_rack(i),ixhi1,y_rack(i)+ih_rack,11)
	   call hline(ixlo+8,ixhi1,y_rack(i),15)
	   call vline(ixhi1,y_rack(i),y_rack(i)+ih_rack,15)
      enddo
	do i=1,nvalves
	   call fillcircle(xc(i),yc(i),irc,8,7)
	   call draw_valve(x(i),y(i),lv,ihv,iwv,11,3,8)
	   call realtoch(float(i),ch,5)
	   cha=ch(1:2)
	   if(i.lt.10) cha=ch(1:1)
	   call wrstring18(x(i)+lv/2-8,y(i)+8,cha,0,11)
	enddo
	do i=1,nbut
	   call draw_valve(xm,ym(i),lm,ihm,iwm,15,11,8)
	enddo
	end
