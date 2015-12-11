	program TPLOT
c To test hard copy with new Hgraph
	integer*2 lth(10),lth(10)
c
	LTH(1)=int2(ifixr(thbig*float(int4(lth(1)))))	!sets line thickness
c
	lth(1)=20
	lth(2)=10
	thbig=1
	print 1
1	format(' idev = ')
	read 2,idev
2	format(i8)
	ivxlo=0
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	call INIPLT(idev,.false.,1.0)
	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
	ixlo=1400
	ixhi=9500
	iylo=1600
	iyhi=6500
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
	call COLTYP(4)
	call SETFONT(2,.false.)
	call SCALE(0.,100.,0.,100.)
c
c	if(idev.ge.5) call LOADVTB('2'//'W'//char(lth(1)))
	call LOADVTB('2'//'W'//char(lth(1)))
c	call LOADVTB('2'//'W'//char(20))
c	call LINWID(20)		!works!!
c
	ixlo=1000
	ixhi=4750
	iylo=800+3400
	iyhi=3250+3400
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)	!top left
	call MOVETO(0.,0.,0,0)
	call MOVETO(0.,100.,1,0)
	call MOVETO(100.,100.,1,0)
	call MOVETO(100.,0.,1,0)
	call MOVETO(0.,0.,0,0)
	call LOADVTB('2'//'W'//char(lth(2)))
	call DXAXIS(0.,100.,10.,0.,.false.,5,200,1)
	call LOADVTB('2'//'W'//char(lth(1)))
c
	call COLTYP(0)
	call MOVETO(70.,70.,0,0)
	ix=iscrx(75.)
	iy=iscry(75.)
	call JUSTIFYSTRING(ix,iy,'test',0,3,1,1)
	call LOADVTB('2'//'W'//char(lth(2)))
c
	call MOVETO(50.,50.,0,0)
	call MOVETO(25.,25.,1,0)
c
	call ENDPLT
	if(idev.ge.5) then
	   pause ' next call vtrans'
	   call VTPLOT(idev,0,4)
	endif
	end

	subroutine DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,itlen,itx)
	logical logx,equal
	logical debug,caplock
c
c Define function 'equal'
	EQUAL(x,y)=abs(x-y).lt.0.00001
	debug()=caplock()
c
c Define tic lengths in device coord (itlen=major tic length in
c device coord)
	itmaj=itlen
	itmin=itlen/2
	xticm=xtic/float(nx1)   !distance between minor tics
c draw the axis
	if(debug()) print 201,-2,xmin,xmax,ycros1,ix,iy,ioff
201	format(' pos#',i2,' xmin,xmax,ycros1,ix,iy,ioff=',3g13.6,3i8)
	call MOVETO(xmin,ycros1,0,0)
	if(debug()) print 201,-1,xmin,xmax,ycros1,ix,iy,ioff
	call MOVETO(xmax,ycros1,1,0)
	if(debug()) print 200,0,x,ycros1,ix,iy,ioff
c draw tics    - first major tic is at x1 for non-log scale (=xmin  at
c present but might want to make this adjustable later), but for
c log scale first major tic is at first integer decade
	k=0
	if(logx) then
	   x1=AFIXR(xmin)
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin)) goto 42
	   x=x1-1.0	!nearest integer BELOW xmin to start minor tics
	else
	   x1=xmin	 !make x1= position of 1st major tic
	   z=amod(xmin,xtic)
	   if(xmin.lt.0.) X1=xmin-z
	   if(xmin.gt.0.) X1=xmin-z+xtic
	   if(xmin.eq.0.) X1=xmin
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin)) goto 42
	   x=x1-xtic	!!to start minor tics preceding 1st major
	endif
c Now draw minor tics that precede 1st major tic
	ioff=(itx-1)*itmin/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	if(nx1.le.1) goto 42
	do 43 i=2,nx1
	if(logx) then
	   x2=x+alog10(float(i))
	else
	   x2=x+float(i-1)*xticm
	endif
	if(x2.gt.xmin.and.x2.le.xmax) then
	   call MOVETO(x2,ycros1,0,0)
	   call GETCURRPOS(ix,iy)
	   call PLOT(ix,iy+ioff,0)       !start pos to draw tic
	   call PLOT(ix,iy+ioff+itmin,1)	!draw tic
	endif
c	  if(debug()) print 200,2,x2,ycros1,ix,iy,ioff
43	continue
c
c set x for 1st major tic
	x=x1
c draw 1st major tic at x=X1
42	continue
	call MOVETO(x,ycros1,0,0)
	call GETCURRPOS(ix,iy)
	ioff=(itx-1)*itmaj/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	call PLOT(ix,iy+ioff,0)       !start pos to draw tic
	call PLOT(ix,iy+ioff+itmaj,1)	!draw tic
	if(debug()) print 200,1,x,ycros1,ix,iy,ioff
200	format(' pos#',i2,' x,ycros1,ix,iy,ioff=',2g13.6,3i8)
c first major tic drawn; now draw ntx-1 minor tics (or 8 log-spaced tics
c if log axis)
	ioff=(itx-1)*itmin/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	if(nx1.le.1) goto 471
	do 47 i=2,nx1
	if(logx) then
	   x2=x+alog10(float(i))
	else
	   x2=x+float(i-1)*xticm
	endif
	if(x2.gt.xmax) goto 48	!x axis finished
	call MOVETO(x2,ycros1,0,0)
	call GETCURRPOS(ix,iy)
	call PLOT(ix,iy+ioff,0)       !start pos to draw tic
	call PLOT(ix,iy+ioff+itmin,1)	!draw tic
c	if(debug()) print 200,2,x2,ycros1,ix,iy,ioff
47	continue
471	continue
	k=k+1
c Move up to posn for next major tic; recalc postition from the posn
c of the 1st major tic=x1 each time to reduce rounding errors
	if(logx) then
	   x=x1+float(k)
	else
	   x=x1+float(k)*xtic
	endif
	if(x.le.xmax) goto 42	!draw next cycle
c
48	continue
	RETURN
	end


