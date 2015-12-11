	subroutine PLOTAUT(draft,screen,quarter,ifont,
     & csize,thick,idev,mpos,ip,iptype)
c
c For automatic plots from queue in AUTPLT, to set viewport etc
c NEW VERSION THAT CHANGES GRAPHBOUNDARY RATHER THAN VIEWPORT
c 02/15/93 08:00pm ivxlo etc now in common/hgv/
c 05/07/91 07:31pm Surely for whole page plots should use queued values
c of ixlo,..,iyhi (e.g. if graph shape changed for I-V plots) so resetting
c removed here
c
c Modified 05/29/92 12:45pm for non-consecutive and/or rearrange plots
c and mpos=5 introduced to denote whole page plots.  Possibility of
c rearrangement means that, for quarter page plots,  mpos=1 no longer
c necessarilly signals first plot on new page, and mpos=4 may not signal
c that page is full. But for quarter page plots still always have 4 plots
c per page, and kplot=total number if plots to be done, in the loop
c do 100 ip=1,kplot. Thus mod(ip,4)=1, i.e. ip=1,5,9,... always indicates
c the first plot on a new page, and ip=4,8,12,... (or ip=kplot) always
c indicates the last plot on a page, i.e. mod(ip,4)=0 or ip=kplot.
c
	logical draft,screen,quarter,landplot
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi


	if(.not.quarter.or.mod(ip,4).eq.1) then
	   call devend
	   if(screen) then
            call gino
		call VGA
		call errswi(-1)
		call brkswi(1)
		call gsetcols(0)
		call chaswi(1)
		call grfmod (1)
		call mode(18)
	   	call papenq(xp,yp,itype)
	   	vxlo=0
	   	vxhi=xp
	   	vylo=0
	   	vyhi=yp
	   	idev=0		!screen, hi-res
	   else
		call gino
		if(ip.eq.1) then
		   if(mpos.gt.6) goto 1000
		   iprinter=1
	   	   print 2
c     & 	   ' (1) Postscript Laserjet printer (big files!)',/,    3
c     & 	   ' (2) Laserjet printer',/,                            1
c     & 	   ' (3) Colour Deskjet',/,                              2
c2	  	   format(
c     & 	   ' (1) Laserjet printer',/,
c     & 	   ' (2) Colour Deskjet',/,
c     & 	   ' (3) Postscript Laserjet printer (big files!)',/,
c     & 	   ' Option number [1] = ')
2	  	   format(
     & 	   ' (1) Colour Deskjet',/,
     & 	   ' (2) Laserjet printer (black & white)',/,
     & 	   ' (3) Postscript Laserjet printer (big files!)',/,
     & 	   ' Option number [1] = ')
		   call INPUTi(iprinter)
		   print 3
3		   format(
     & 	   ' (1) Landscape',/,
     & 	   ' (2) Portrait',/,
     & 	   ' Option number [1] = ')
	   	   iopt=1
		   landplot=.true.
		   call INPUTi(iopt)
		   if(iopt.eq.2) landplot=.false.
c idev=0: screen
c idev=1: .wmf
c idev=2: .cgm
c idev=3,4: postscript printer
c idev=5  : laserjet
c idev=6  : color deskjet
	         if(iprinter.eq.3) then
			if(landplot) then
			   idev=4
			else
			   idev=3
			endif
	   	   else if(iprinter.eq.2) then
		   	idev=5
	   	   else if(iprinter.eq.1) then
		   	idev=6
		   endif
		endif
1000		if(idev.eq.3) then     	! portrait postscript
	   	   call eps(1,5.,5.,210.,297.,210.,297.)
		else if(idev.eq.4) then ! landscape postscript
	   	   call eps(1,5.,5.,297.,210.,297.,210.)
		else if(idev.eq.5) then
	   	   call hpljr           ! laserjet
		else  if(idev.eq.6) then
	   	   call DJ500C       ! color deskjet->dj500c.out
c	   	   call dj550c          ! produces dj550c.out
		endif
	   	call errswi(-1)
	   	call brkswi(1)
	   	call papenq(xp,yp,itype)
	      if(iprinter.eq.2.and.(.not.landplot)) then
		   xp=196.7
		   yp=280.5
		   call devpap(196.7,280.5,itype)
		endif
	      if(iprinter.eq.1.and.landplot) then
		   xp=266.7
		   yp=203.5
		   call devpap(266.7,203.5,itype)
		endif
	   	vxlo=0
	   	vxhi=xp
	   	vylo=0
	   	vyhi=yp
	   endif
	endif
	if(draft) then
	   ifont=0			!draft font
	endif
c
c Call INIPLT after page filled and plotted (by call to ENDPLT)
c (have one plot per page if not auto (ie posh), or if auto and
c not quarter, ie automatic whole page plots)
c
c NOTE: params of GRAPHBOUNDARY are defined relative to the lower
c left hand corner of the VIEWPORT so if former params refer to
c 'lower left' posn, then graph can be moved about simply by changing
c viewport. Values of ix,iy specified in JUSTIFYSTRING must be in the
c viewport- will be OK if they are based on x,y values that are converted
c to device coords with ISCRX(x) etc- they will then move with rest
c of graph.
c ALSO: scalefactor in INIPLT keeps bottom left corner in same posn when
c graph is shrunk.
c ###NB HALF-PAGE SINGLE CHANNEL PLOTS MAY NEED TO BE MIXED WITH
C QUARTER PAGE HISTOS (DONE HERE) OR GRAPHS

c At present, if not quarter then must be whole page (mpos=5)
c  For quarter page plots, try just resetting viewport, as in manual (p15)
c In this version mpos is already set, from jpos() specified in PLOTOPT
	if(.not.quarter) then
	 csize=2.5	!ignore queued value
	 thick=1.0	!ignored if idev<5
	 if(mpos.ge.6) then
	   xlo=0.05*xp
	   xhi=0.95*xp
	   ylo=0.
	   yhi=0.9*yp
	 else
	   xlo=0.2*xp
	   xhi=0.8*xp
	   ylo=0.2*yp
	   yhi=0.8*yp
	 endif
	 call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	else
	   csize=2.5
	   thick=0.6	!ignored if idev<5
	   if (mpos.eq.1) then
	      xlo=0.12*xp
	      xhi=0.44*xp
	      ylo=0.55*yp
	      yhi=0.87*yp
	   if(iptype.ne.40) call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)	!top left
	   else if(mpos.eq.2) then
c		xlo=0.62*xp
c		xhi=0.94*xp
		xlo=0.56*xp
		xhi=0.88*xp
		ylo=0.55*yp
		yhi=0.87*yp
	    if(iptype.ne.40) call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)	!top right
	   else if(mpos.eq.3) then
c bottom left
		xlo=0.12*xp
		xhi=0.44*xp
		ylo=0.1*yp
		yhi=0.42*yp
		if(iptype.ne.40) call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)	!same for all
	   else if(mpos.eq.4) then
c bottom right
c		xlo=0.62*xp
c		xhi=0.94*xp
		xlo=0.56*xp
		xhi=0.88*xp
		ylo=0.1*yp
		yhi=0.42*yp
		if(iptype.ne.40) call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)	!same for all
	   endif
	endif
	if(.not.screen) then
	   print 1553
1553	   format(' Assembling the plot...')
	endif
c
	RETURN
	end

