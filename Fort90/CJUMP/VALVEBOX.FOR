c======================================================================
	subroutine valvebox(ixlo,iylo,ixhi,iyhi,nvalves,x,y,klv,kihv,kiwv,
     & nbut,ixm,iym,klm,kihm,kiwm,xc,yc,irc,xcm,ycm,
     & icbox,icv,icm,icc,icb,ncolumns,iyhi1)
c======================================================================
c	input  : ixlo,iylo,ixhi,iyhi,nvalves,nbut,icbox,icv,icm,icc,icb
c     output : x,y,klv,kihv,kiwv,ixm,iym,klm,kihm,kiwm,xc,yc,irc,xcm,ycm,
c              ncolumns,iyhi1
c======================================================================

	integer*4  	x(nvalves),y(nvalves),xc(nvalves),yc(nvalves)
	integer*4  	iym(5),ixm(5),xcm(5),ycm(5)
	character*5	ch,cha
	nrows=nbut
	icw=12
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
		xc(k)=ixlo+(2*i-1)*idx/2+idx/4
		y(k)=iyhi1-(2*j-1)*idy/2
		yc(k)=y(k)-idy/4
		k=k+1
		if(k.gt.nvalves) goto 1
	   enddo
	enddo
1	continue

	do i=1,5
	   iym(i)=iyhi1-1.7*ihm*i
	   ixm(i)=ixhi1+24
	enddo

	call fillwin(ixlo,iylo,ixhi,iyhi,icbox)

	call fillwin(ixlo+2,iyhi1+2,ixhi-2,iyhi-2,icc)

	call hline(ixlo+2,ixhi-2,iyhi1+2,icm)
	call vline(ixhi-2,iyhi1+2,iyhi-2,icm)
	call hline(ixlo+2,ixhi-2,iyhi-2,icm)
	call vline(ixlo+2,iyhi1+2,iyhi-2,icm)
	call hline(ixhi1+16,ixhi-8,iyhi1-8,icv)
	call hline(ixhi1+16,ixhi-8,iylo+8,icv)
	call vline(ixhi1+16,iylo+8,iyhi1-8,icv)
	call vline(ixhi-8,iylo+8,iyhi1-8,icv)
	do i=1,nrows
	   iyn=iyhi1-(2*i-1)*idy/2-4
	   call fillwin(ixlo+8,iyn,ixhi1+8,iyn+int(idy*0.45),icv)
	   call hline(ixlo+8,ixhi1+8,iyn,icm)
	   call vline(ixhi1+8,iyn,iyn+int(idy*0.45),icm)
      enddo
	call hline(ixlo,ixhi,0,icv)
	call hline(ixlo,ixhi,iyhi,icv)
	call hline(ixlo,ixhi,iyhi1,icv)
	call vline(0,iylo,iyhi,icv)
	call vline(ixhi,iylo,iyhi,icv)
	do i=1,nvalves
	   call fillcircle(xc(i),yc(i),irc,icb,icc)
	   call draw_valve(x(i),y(i),lv,ihv,iwv,icv,icbox,icc)
	   call realtoch(float(i),ch,5)
	   cha=ch(1:2)
	   if(i.lt.10) cha=ch(1:1)
	   call wrstring18(x(i)+lv/2-8,y(i)+ihv/2,cha,icc,icv)
	enddo
	ind=1
	if(nbut.eq.3) ind=2
	do i=1,5,ind
	   call draw_valve(ixm(i),iym(i),lm,ihm,iwm,icm,icv,icc)
	enddo
	if(nbut.eq.5) then
	   call wrstring18(ixm(2)+lm/2-8,iym(2)+ihm/2-8,'Exit',icc,icm)
	   call wrstring18(ixm(4)+lm/2-8,iym(4)+ihm/2,'O.K.',icc,icm)
	   call wrstring18(ixm(4)+lm/2,iym(4)+ihm/2+8,'\/',icc,icm)
      endif
	call wrstring18(ixm(1)+lm/2-16,iym(1)+ihm/2-8,'On/Off',icc,icm)
	call wrstring18(ixm(3)+lm/2-8,iym(3)+ihm/2-16,'Help',icc,icm)
	call wrstring18(ixm(5)+lm/2-16,iym(5)+ihm/2,'Cancel',icc,icm)
	if (nbut.eq.3) then
	   call fillcircle(ixhi1+int(0.1*length),yc(1),irc,icb,icc)
	   call fillcircle(ixhi1+int(0.1*length),yc(1+ncolumns),irc,icb,
     &   icc)
	endif
	call fillcircle(ixhi1+int(0.1*length),iylo+int(2*irc),irc,icb,
     &   icc)
	call wrstring18(ixm(1)+lm/2-16,iym(1)+ihm/2,'Timer',icc,icm)
	call wrstring18(ixm(5)+lm/2,iym(5)+ihm/2+8,'X',icw,icm)
	call wrstring18(ixm(3)+lm/2,iym(3)+ihm/2,'?',icw,icm)
	if (nbut.eq.3) then
		iym(2)=iym(3)
		iym(3)=iym(5)
		ixm(2)=ixm(3)
		ixm(3)=ixm(5)
	endif
	do i=1,nbut
	   ycm(i)=yc(1+(i-1)*ncolumns)
	   xcm(i)=ixhi1+lm
	enddo
	klm=lm
	kihm=ihm
	kiwm=iwm
	klv=lv
	kihv=ihv
	kiwv=iwv
	end
