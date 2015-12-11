	subroutine SCALCURV(xval,yval,ndat,xcal,ycal,ncal,ncurvd,icurvd,
     & ncurvc,icurvc,xscale,xoffset,yscale,yoffset,xdone,ydone,
     & xoff,yoff,xmin1,xmax1,ymin1,ymax1,ndv1,ndimd,ndc1,ndimc,ndims)
c For scale or offset of Y values, and scaling of x values in VPLOT (rescale
c menu).  Asks which data set/calc curve (j value) if more than one
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c for data
	integer*4 ndat(ndimd),icurvd(ndimd)
c for calc curves
	integer*4 ncal(ndimc),icurvc(ndimc)
	real*4 xscale(ndims),xoffset(ndims),
     & yscale(ndims),yoffset(ndims)
	character defolt*30,cans*30	!to hold text & result of QDIALOG
	logical help,mono,xdone,ydone
	character*1 charout	!for popmenu
	character*16 title 	!for popmenu
	character*16 string(6)	!for popmenu
	character*30 helpst(8)	!for popmenu
	common/hlp/help		!for QDIALOG
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	COMMON/cols/icol(100),mono
c
c
	xdone=.false.	!x values have been changed
	ydone=.false.	!y values have been changed
	ict=11
	sx=1.0		!in case 'do scaling' hit straight away
	sy=1.0
	off=0.0
	title=    'Choose operation'
	string(1)='Scale y values  '
	string(2)='Invert y values '
	string(3)='Offset y values '
	string(4)='scale X values  '
	string(5)='offset x Values '
	string(6)='Do the scaling  '
c	string(5)='Do the scaling  '
	nval=6
	helpst(1)='Scale means multiply all     '
	helpst(2)=' values by a constant factor.'
	helpst(3)='Offset means add a constant  '
	helpst(4)=' amount to all values.       '
	helpst(5)='Do the scaling -exits; then  '
	helpst(6)=' hit redraw to see new scale.'
	nhelp=6
	iline=1
c
c	if(ncurvd.eq.0.and.ncurvc.gt.0) goto 3
c
	if(ncurvd.gt.1) then
	   call SELDATA(ncurvd,j1,icol,0)		!choose which data line
	   if(j1.eq.-1) goto 99		!cancel
	else
	   j1=1
	endif
	if(j1.gt.0) then
	   j2=icurvd(j1)		!selected set
	else if(j1.eq.0) then	!all sets
	   j2=0
	endif
c
c Decide which operation required
1	call POPMENU(nxlo,nylo,nyhi,string,nval,ictm,icfm,icupm,ibkm,
     & title,helpst,nhelp,iline,charout,ival)
	if(iline.eq.0) goto 99	!esc=cancel
	if(iline.eq.6) goto 2
c Set defaults=present values
	if(j2.gt.0) then
	   if(iline.eq.1.or.iline.eq.2) then
		s=yscale(j2)
	   else if(iline.eq.3) then
		off=yoffset(j2)
	   else if(iline.eq.4) then
		s=xscale(j2)
	   else if(iline.eq.5) then
		off=xoffset(j2)
	   endif
	else
	   s=1.0
	   off=0.0
	endif
	if(iline.eq.1.or.iline.eq.4) then
	   s=1.0
	   call DEFOLTr(s,defolt)
	   call QDIALOG(1,'Scale factor',defolt,ict,cans)
	   call GETINPr(cans,s)
	else if(iline.eq.2) then
	   s=-s
	else if(iline.eq.3.or.iline.eq.5) then
	   off=0.0
	   call DEFOLTr(off,defolt)
	   call QDIALOG(1,'Offset by',defolt,ict,cans)
	   call GETINPr(cans,off)
	endif
c Set xscale() etc.
	if(iline.eq.1.or.iline.eq.2) then		!scale y
	   ydone=.true.	!y values have been changed
	   if(j2.gt.0) then
		s1=yscale(j2)	!save orig value
		yscale(j2)=s
		sy=s/s1
	   else
		do j1=1,ncurvd
		   j=icurvd(j1)
		   s1=yscale(j)	!save orig value
		   yscale(j)=s
		   sy=s/s1
		enddo
	   endif
	else if(iline.eq.3) then	!offset y
	   ydone=.true.	!y values have been changed
	   if(j2.gt.0) then
		off1=yoffset(j2)	!save orig value
		yoffset(j2)=off
		off=off-off1
	   else
		do j1=1,ncurvd
		   j=icurvd(j1)
		   off1=yoffset(j)	!save orig value
		   yoffset(j)=off
		   off=off-off1
		enddo
	   endif
	else if(iline.eq.4) then	!scale x
	   xdone=.true.	!x values have been changed
	   if(j2.gt.0) then
		s1=xscale(j2)	!save orig value
		xscale(j2)=s
		sx=s/s1
	   else
		do j1=1,ncurvd
		   j=icurvd(j1)
		   s1=xscale(j)	!save orig value
		   xscale(j)=s
		   sx=s/s1
		enddo
	   endif
	else if(iline.eq.5) then	!offset x
	   xdone=.true.	!x values have been changed
	   if(j2.gt.0) then
		off1=xoffset(j2)	!save orig value
		xoffset(j2)=off
		off=off-off1
	   else
		do j1=1,ncurvd
		   j=icurvd(j1)
		   off1=yoffset(j)	!save orig value
		   xoffset(j)=off
		   off=off-off1
		enddo
	   endif
	endif
c
	iline=6
	goto 1		!any more values to be altered?
c
c Do the scaling when all factors set
2	continue
	if(.not.ydone) goto 4
	yoff=off
	if(j2.gt.0) then
	   if(ncurvd.gt.0) then
		do i=1,ndat(j2)
		   yval(i,j2)=sy*yval(i,j2) + off
      	enddo
	   endif
	   if(ncurvc.gt.0) then
		j2=icurvc(j1)
		do i=1,ncal(j2)
		   ycal(i,j2)=sy*ycal(i,j2) + off
      	enddo
	   endif
	else
	   if(ncurvd.gt.0) then
		do j1=1,ncurvd
		   j=icurvd(j1)
		   do i=1,ndat(j)
			yval(i,j)=sy*yval(i,j) + off
      	   enddo
		enddo
	   endif
	   if(ncurvc.gt.0) then
		do j1=1,ncurvc
	 	   j2=icurvc(j1)
	 	   do i=1,ncal(j2)
			ycal(i,j2)=sy*ycal(i,j2) + off
          	   enddo
		enddo
	   endif
	endif
c Scale x values
4	if(.not.xdone) goto 5
	xoff=off
	if(j2.gt.0) then
	   if(ncurvd.gt.0) then
		do i=1,ndat(j2)
		   xval(i,j2)=sx*xval(i,j2) + off
      	enddo
	   endif
	   if(ncurvc.gt.0) then
		j2=icurvc(j1)
		do i=1,ncal(j2)
		   xcal(i,j2)=sx*xcal(i,j2) + off
      	enddo
	   endif
	else
	   if(ncurvd.gt.0) then
		do j1=1,ncurvd
		   j=icurvd(j1)
		   do i=1,ndat(j)
			xval(i,j)=sx*xval(i,j) + off
      	   enddo
		enddo
	   endif
	   if(ncurvc.gt.0) then
		do j1=1,ncurvc
	 	   j2=icurvc(j1)
	 	   do i=1,ncal(j2)
			xcal(i,j2)=sx*xcal(i,j2) + off
          	   enddo
		enddo
	   endif
	endif
c Reset xmin1 etc
5	continue
	if(ydone) then
	   ymax1=sy*ymax1 + off
	   ymin1=sy*ymin1 + off
	endif
	if(xdone) then
	   xmax1=sx*xmax1 + off
	   xmin1=sx*xmin1 + off
	endif
c
	if(ymin1.gt.ymax1) then
	   x=ymax1
	   ymax1=ymin1
	   ymin1=x
	endif
	if(xmin1.gt.xmax1) then
	   x=xmax1
	   xmax1=xmin1
	   xmin1=x
	endif
c
99	continue
	RETURN
	end
