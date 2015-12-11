	subroutine EXPMODFT(ix,idata1,ndfit,if1,if2,ixcalc,iycalc1,ke,
     & iyd1,iyd2,xtrig,expfac,basevga,cfac1,opendown,finter,
     & dx,x0,dxf,iexp,ibk,ibk2,icc,icbl,icol1,icol2)
c
c To expand/shift display if final fit in FITSUB.
c All data in pixel units (ixmin=0 to ixmax=639)
c Data are in idata(1) to idata(ndfit)
c Curve is in iycalc1(1) to (ke), but ixcalc(1) generally>1 so curve
c dx, dxf are x increments for data and calc curve (not changed here)
c
c To expand data about the point with index=iexp say, need to recalc ix() as
c ix1(1)=iexp+(i-iexp)*dx1 where dx1=new increment bet points
c This will generally exten below 0 and above 638 so display from point i1 to i2
c where i1=iexp + (ixmin-ix(iexp))/dx1 (round up)
c   and i2=iexp + (ixmax-ix(iexp))/dx1 (round up)
c Then shift the shortest interval to centre of screen (ix=320)

	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
	integer*2 ix(2048),ix1(2048)
	integer*2 ixcalc(5120),iycalc1(5120)
	integer*2 ixcalc1(5120)
	integer iact(0:15)
	character ch*1
	logical opendown
c arrays to hold graphics overwritten by line
c	integer*1 isavbase(320),isavy1(320),isavy2(320)
C For DCMENU
	integer Lb(30)
	character*78 text(30)
c
	logical discprt
	common/ampmark/nampmark,iamark(10)
	common/dp/discprt
c
c Area for data
	ixd1=0
	ixd2=640
c iyd1,iyd2 are params
c Start:clear part of the screen:
c
	ndisp=ndfit		!local definition
c
	ixmin=0
	ixmax=639
	xmin=0.
	xmax=639.
c
c Use shift in pixel units here (rather than index).  Calculate
c initial display with shift=ixinc=0, and alter ixinc to shift display
	ixinc=0		!initially
	inc1=50
	efac=1.5		!factor for expand/contract after the first
	expf=expfac		!first time

c
c	CALCULATE THE FIRST EXPANSION (about trigger pos as in expmode)
c	by factor=expfac
2	continue
	call FILLWIN(ixd1,iyd1,ixd2,iyd2,ibk2)
c	ndisp1=ifixr(float(ndisp)/expf)	!initial expansion
c	dx1=(xmax-xmin)/float(ndisp1-1)
	dx1=dx*expf
c Define new ix()
c Shift expansion point to centre of screen by adding 320-ix(iexp) to all ix values
c But don't shift to centre if this means leaving blank screen at either end
c -if this would happen, change ixinc to prevent it
c    check to prevent gap at left edge
	ix1(1)=320 + ifixr(float(1-iexp)*dx1) +ixinc
	if(ix1(1).gt.ixmin) ixinc=ixinc - (ix1(1)-ixmin)
c    check to prevent gap at right edge
	ix1(ndisp)=320 + ifixr(float(ndisp-iexp)*dx1) +ixinc
	if(ix1(ndisp).lt.ixmax) ixinc=ixinc + (ixmax-ix1(ndisp))
	do i=1,ndisp
c	   ix1(i)=ix(iexp) + ifixr(float(i-iexp)*dx1)
	   ix1(i)=320 + ifixr(float(i-iexp)*dx1) +ixinc
	enddo
c when expanded the new ix() will generally extend below 0 and above 639
c so display only elements i1 to i2 which are in this range
c where i1=iexp + (ixmin-ix(iexp))/dx1 (round up)
c   and i2=iexp + (ixmax-ix(iexp))/dx1 (round down)
	i1=iexp + ifix(float(ixmin-ix1(iexp))/dx1)
	if(i1.lt.1) i1=1
	i2=iexp + ifix(float(ixmax-ix1(iexp))/dx1)
	if(i2.gt.639) i2=639
	ndisp2=i2-i1+1
c NB total shift (pixels) =320 + ifixr(float(1-iexp)*dx1) because this is
c value is ix1(1), and ix(1)=0
	xshift=320. + dx1*(1.0-float(iexp)) + ixinc
c	ixshift=ifixr(xshift)
c
c Now expand the calc curve similarly
c  NB in fitsub, calc ixcalc(i)=ifixr(x0 + float(i-1)*dxf), i=1,ke
c In expanded trace, want x0 to be the same distance (in time units) from
c start of data trace, at ix1(1).  If time is expanded then given time is
c more pixels, by a factor=expfac
c	ke1=ifixr(float(ke)/expfac)
	x01=x0*expf
	dxf1=dxf*expf	!new increment for calc curve
	do i=1,ke
	   ixcalc1(i)=ifixr(xshift + x01 + float(i-1)*dxf1)
	enddo
c
	i1f=1 + ifix((xmin-xshift-x01)/dxf1)
	if(i1f.lt.1) i1f=1
	i2f=1+ifix((xmax-xshift-x01)/dxf1)
c=	if(i2f.gt.639) i2f=639
	ke2=i2f-i1f+1
	if(ke2.gt.ke) ke2=ke	!only ke points defined!
c
c Use xshift to recalculate start and end of fit range which, in input,
c extends from ix(if1) to ix(if2)
	ixf1=ifixr(float(ix(if1))*expf+xshift)
	ixf2=ifixr(float(ix(if2))*expf+xshift)
c
c Draw expanded data and calc curve
	ibasevga=ifixr(basevga)
c	call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,-1)	!calibration
c	call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
	call HLINE(0,639,ibasevga,icbl)	   !show baseline
	if(nampmark.gt.0) then
	   do i=1,nampmark
	      iy=ibasevga+iamark(i)	!sign of iamark set in inscan
	      call DHLINE(0,639,iy,10,22)
	   enddo
      endif
c   Draw data
	call SETACT(iact,15)		!draw white whatever
	call IDCPIX(ix1(i1),idata1(i1),ndisp2,iact)
c Draw calc curve (+ flat bit at each end)
	call SETACT(iact,-1)		!no draw
	iact(ibk2)=icc			!unless background col, when draw red
	if(ke2.gt.0) then
	   call IDCPIX(ixcalc1(i1f),iycalc1(i1f),ke2,iact)	!draw resp function
	endif
c Now extend calc curve to cover whole fitted range from data(if1) to
c data(if2).  These regions are flat, so can draw with HLINE
c Use RDGWIN1 to keep graphical data overwritten by HLINE to allow restoration
c when red fitted line is deleted
c	call RDGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
c	call RDGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),isavy2)
	if(ixf1.lt.ixcalc1(1).and.ixcalc1(1).ge.ixmin) then	!end of line on screen
	   ixh1=ixf1
	   if(ixh1.lt.ixmin) ixh1=ixmin
	   call HLINE(ixh1,ixcalc1(1),iycalc1(1),icc)	!before start of resp func
	endif
	if(ixcalc1(ke).lt.ixf2.and.ixcalc1(ke).le.ixmax) then
	   ixh2=ixf2
	   if(ixh2.gt.ixmax) ixh2=ixmax
	   call HLINE(ixcalc1(ke),ixh2,iycalc1(ke),icc)   !after end of resp func
	endif
c
	call DCMENU(0,4,Lb,text,ibk,ibk)  !delete all (to non-data background col)
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	Lb(4)=0
	Lb(5)=0
	Lb(6)=0
	Lb(7)=0
151	continue
	text(1)='1: FINISH EXPAND'
	text(2)='2: CONTRACT'
	text(3)='3: EXPAND  '
	text(4)='4: '
	text(5)='5: '
	text(6)='6: '
	text(7)='7: '
	text(8)='8:SHIFT LEFT'
	text(9)='9:SHIFT RIGHT'
c	text(10)='10: QUEUE PLOT '
	text(10)='0: FINISH EXPAND'
	text(11)='+:BIGGER SHIFT'
	text(12)='-:SMALLER SHIFT'
	text(13)='x:BIGGER EXPAND'
	text(14)='/:SMALLER EXPAND'
	text(15)='.:HELP (or F1)'
c
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.eq.75) ikey=8	!left arrow (added to ckey 06/13/98 09:40pm)
	if(ikey.eq.77) ikey=9	!right arrow
	SELECT CASE(ikey)
		CASE(1)
		   goto 9

		CASE(2)	!contract
		   expf=expf/efac
		   if(expf.lt.1.0) expf=1.0
		   goto 2

		CASE(3)	!expand
		   expf=expf*efac
		   if(expf.gt.20.) expf=20.
		   goto 2

		CASE(8)		!shift left
		   ixinc=ixinc + inc1
		   goto 2

		CASE(9)		!shift right
		   ixinc=ixinc - inc1
		   goto 2

		CASE(10)
		   goto 9

		case(11)	!increase shift increment
		   if(inc1.le.320) inc1=inc1*2
		   goto 2

		case(12)	!decrease shift increment
		   if(inc1.ge.2) inc1=inc1/2
		   goto 2

		CASE(13)		!increase expand
		   if(efac.lt.15.) efac=efac*1.5
		   goto 150

		CASE(14)		!decrease expand
		   if(efac.gt.1.1) efac=1.0 + (efac-1.0)/1.5	!also expand/contract
		   goto 150

	END SELECT
	goto 150
c redraw original fit after leaving
9	continue
	RETURN
	end


