c======================================================================
	subroutine solbox(ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,
     & nrows,icvalve,icshad,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,ic,icfill,
     & nbut,ixm,iym,ixmo,iymo,ixdra,klm,kihm,kiwm)
c======================================================================
c	input  : ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,nrows
c     output : x,y,xp,yp,ncolomns
c======================================================================

	integer*4  	x(nvalves),y(nvalves),xp(nvalves),yp(nvalves)
	integer*4  	xc(nvalves),yc(nvalves),iym(5),iymo(5)
	character*5	ch,cha

	length=ixhi-ixlo
	iheigth=iyhi-iylo
	lm=int(0.1*length)
	ihm=int(0.4*iheigth/5)
	iwm=int(0.4*lm)
	colm=float(nvalves)/float(nrows)
	rest=colm-int(colm)
	ncolumns=int(colm)
	if(rest.ne.0.0) ncolumns=ncolumns+1
	idx=0.8*length/ncolumns
	idy=0.8*iheigth/nrows
	iyhi1=iyhi-0.2*iheigth
	ixhi1=ixhi-0.2*length
	lv=int(0.7*idx)
	ihv=idy/4
	iwv=0.6*lv
	irc=idy/8
	k=1
	do j=1,nrows
	   do i=1,ncolumns
		x(k)=ixlo+(4*i-3)*idx/4
		xp(k)=x(k)+lv/4
		xc(k)=ixlo+(2*i-1)*idx/2+idx/4
		y(k)=iyhi1-(2*j-1)*idy/2
		yp(k)=y(k)-lv/4
c		yc(k)=iyhi1-(4*j-3)*idy/4
		yc(k)=y(k)-idy/4
		k=k+1
		if(k.gt.nvalves) goto 1
	   enddo
	enddo
1	continue
	ixm=ixhi1+24
	ixmo=ixm+lm/4

	do i=1,5
	   iym(i)=iyhi1-1.7*ihm*i
	   iymo(i)=iym(i)-lm/4
c	   print*,ixm,iym(i)
c	   pause
	enddo
c	call zbar(ixlo,iylo,length,iheigth,iwidth,icborder,icbox)

	call fillwin(ixlo,iylo,ixhi,iyhi,3)

	call fillwin(ixlo+2,iyhi1+2,ixhi-2,iyhi-2,8)

	call hline(ixlo+2,ixhi-2,iyhi1+2,15)
	call vline(ixhi-2,iyhi1+2,iyhi-2,15)
	call hline(ixlo+2,ixhi-2,iyhi-2,15)
	call vline(ixlo+2,iyhi1+2,iyhi-2,15)
c	call fillwin(ixhi1+16,iylo+8,ixhi-8,iyhi1-8,8)
	call hline(ixhi1+16,ixhi-8,iyhi1-8,11)
	call hline(ixhi1+16,ixhi-8,iylo+8,11)
	call vline(ixhi1+16,iylo+8,iyhi1-8,11)
	call vline(ixhi-8,iylo+8,iyhi1-8,11)
	do i=1,nrows
	   iyn=iyhi1-(2*i-1)*idy/2-4
	   call fillwin(ixlo+8,iyn,ixhi1+8,iyn+int(idy*0.45),11)
	   call hline(ixlo+8,ixhi1+8,iyn,15)
c	   call vline(ixlo+8,iyn,iyn+int(idy*0.45),15)
	   call vline(ixhi1+8,iyn,iyn+int(idy*0.45),15)

      enddo
	call hline(ixlo,ixhi,0,11)
	call hline(ixlo,ixhi,479,11)
	call hline(ixlo,ixhi,iyhi1,11)
	call vline(0,iylo,iyhi,11)
	call vline(639,iylo,iyhi,11)
	do i=1,nvalves

	   call fillcircle(xc(i),yc(i),irc,7,8)
	   call draw_valve(x(i),y(i),lv,ihv,iwv,11,3,8)
	   call realtoch(float(i),ch,5)
	   cha=ch(1:2)
	   if(i.lt.10) cha=ch(1:1)
	   call wrstring18(x(i)+lv/2-8,y(i)+ihv/2,cha,8,11)
	enddo
	ind=1
	if(nbut.eq.3) ind=2
	do i=1,5,ind
	   call draw_valve(ixm,iym(i),lm,ihm,iwm,15,11,8)
	enddo
	if(nbut.eq.5) then
	   call wrstring18(ixm+lm/2-8,iym(2)+ihm/2-8,'Exit',1,15)
	   call wrstring18(ixm+lm/2-8,iym(4)+ihm/2,'O.K.',1,15)
	   call wrstring18(ixm+lm/2,iym(4)+ihm/2+8,'\/',2,15)
      endif
	call wrstring18(ixm+lm/2-8,iym(1)+ihm/2-8,'Start',8,15)
	call wrstring18(ixm+lm/2-8,iym(3)+ihm/2-16,'Help',8,15)
	call wrstring18(ixm+lm/2-16,iym(5)+ihm/2,'Cancel',8,15)
	if (nbut.eq.3) then
	   call fillcircle(ixhi1+int(0.1*length),yc(1),irc,7,8)
	   call fillcircle(ixhi1+int(0.1*length),yc(1+ncolumns),irc,7,8)
	endif
	call fillcircle(ixhi1+int(0.1*length),iylo+int(2*irc),irc,7,
     &   8)
	call wrstring18(ixm+lm/2,iym(5)+ihm/2+8,'X',12,15)
	call wrstring18(ixm+lm/2,iym(3)+ihm/2,'?',12,15)
	if (nbut.eq.3) then
		iym(2)=iym(3)
		iym(3)=iym(5)
	endif
	ixdra=ixhi1+lm
	klm=lm
	kihm=ihm
	kiwm=iwm
	end
