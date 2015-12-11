subroutine zoom_edges(iform,iplot,jplot,graph1_1,GraphMainPanel1_1,&
     graphics1_1,xval,yval,ndat,pixbuf,x0,wxmin,wxmax,wymin,wymax,&
	 xmin,xmax,ymin,ymax,ivalpz,yopen,pixbufz,ivalpz1)
	    
	    use gino_f90
	    use menu_f90
		integer Graph1_1(100)
		integer pixbuf(10,800),pixbufz(800,10)
		integer GraphMainPanel1_1(100)
		integer Graphics1_1(100)
	  	real*4 XVAL(ndat),YVAL(ndat)
		character*60 gfile
		character*11 cnum
		logical plot,landscape,poplot
		
	    common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
        common/plotopen/icombo1,poplot,&
        itext1,itext2,itext_1,itext_2,itext_3,itext_4,ivalp1,ivalp2,ivalp3,ivalp4,&
        iradiop1,iradiop2,iradiop3,iradiop4
	    common/limit/wxminz,wxmaxz,wyminz,wymaxz,xmin1z,xmax1z,ymin1z,ymax1z,&
     	xminz,xmaxz,yminz,ymaxz

  
    
    call gDefineRGB(20,0.5,0.,0.25)	!burgundy
    cbig=2.5
	   ifont=3
	   ifitype=0		!no display of fitted parameters
	   ntx=5		!if not logt set initial input values
	   nty=5
	   itx=1
	   ity=1
	   ilabel=1
	 
	   ilog=0
	   iscal=1		!scale internally
	   xlo=-1		!whole screen
	   
	   
	   inumx=-1		!don't allow scaling
	   inumy=-1

	
	   ymin1=1.e37
	xmin1=1.e37
	ymax1=-1.e37
	xmax1=-1.e37
	xcrit=1.e-37	!for first x value only
	ycrit=1.e-37

	   do i=1,ndat
		xv=xval(i)
		yv=yval(i)
		if(i.gt.1) then
		   xcrit=0.2*abs(xv-xlast)
		endif
	
		if(xv.lt.xmin1) xmin1=xv
		if(xv.gt.xmax1) xmax1=xv
		if(yv.lt.ymin1) ymin1=yv
		if(yv.gt.ymax1) ymax1=yv

		xlast=xv
	   enddo
	 if(itx.lt.-1.or.itx.gt.1) then
	   itx=1	!default axis tic orientation in LAXES
	   ity=1
	endif
	if(ntx.eq.0) ntx=5		!label every 5th tic
	if(nty.eq.0) nty=5
if(ymin1.ge.yopen) then 
		ymin1=yopen
	    ymin1=yopen-0.1*(ymax1-ymin1)
	endif
	!if(x0.le.0.000) x0=xval(int(0.5*ndat))
		if(x0.lt.0.000) x0=0.0
	call FIXAX(xmin1,xmax1,xmin2,xmax2,xtic,0)	!always non-log
	call FIXAX(ymin1,ymax1,ymin2,ymax2,ytic,0)
     xmax=xmax2
	 ymax=ymax2
	 xmin=xmin2
	 ymin=ymin2
	 disfacx=0.7
	 disfacy=0.7
	 xmax=xmax1
	 xmin=xmin1
	    xlth=xmax-xmin
		ylth=ymax-ymin
		deltax=xlth/disfacx
		deltay=ylth/disfacy
		dxg=(deltax-xlth)/2.
		dyg=(deltay-ylth)/2.

		wxmin=xmin-0.5*dxg
		wxmax=xmax+0.5*dxg
		wymin=ymin-0.5*dyg
		wymax=ymax+0.5*dyg

		dxsw=xlth*0.007
		dysw=ylth*0.010
	
		wid=0.5*dysw*1.000001
		ix=100
		iy=100
		wxminz=wxmin
		wyminz=wymin
		wxmaxz=wxmax
		wymaxz=wymax

	xmin0=xlth/2
	plot=.false.
	ipos=0
    call graph1(iplot,jplot,iform,ix,iy,graph1_1,GraphMainPanel1_1,&
      graphics1_1,wxminz,wxmaxz,wyminz,wymaxz,ipos,gfile,plot,-3,0,npagtot)
    	ivalp = gmCreatetextEntry(graph1_1(iplot), 9, 0, 2, 1,'x=', 20,6, Gdisplay, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	ivalpz = gmCreateValueEntry(graph1_1(iplot), 11, 0, 3, 1,x0, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	ivalp = gmCreatetextEntry(graph1_1(iplot), 15, 0, 2, 1,'y open=', 20,6, Gdisplay, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	ivalpz1 = gmCreateValueEntry(graph1_1(iplot), 17, 0, 3, 1,yopen, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	linetype=ijoinj
		ic=20
		icol=9
		idev=0
		call glincols(ic,idev)
		thj=0.0		!set line thickness=0 for screen (or too slow)
		call LINWID(thj)
		ip=0		!index for popen(ip), xpval(ip), ip=1,..., npops
	 xcross=xmin2		!crossing point for axes
	   ycross=ymin2
        linetype=0
        idev=0
		call DFRAME(xmin,xmax,ymin,ymax,linetype)
		call realtoch(xmin,cnum,11)
		call movto2(xmin,ymin-3*dysw)
		call gSetStrJustify(-1)
		call gSetCharSize(2*dxsw,2*dysw)
	    call gDisplayStr(cnum)
		call realtoch(xmax,cnum,11)
		call movto2(xmax,ymin-3*dysw)
		call gSetStrJustify(-1)
		call gSetCharSize(2*dxsw,2*dysw)
	    call gDisplayStr(cnum)

			call realtoch(ymin,cnum,11)
		call movto2(xmin,ymin)
		call gSetStrJustify(0)
		call gSetCharSize(2*dxsw,2*dysw)
	    call gDisplayStr(cnum)
		call realtoch(ymax,cnum,11)
		call movto2(xmin,ymax)
		call gSetStrJustify(0)
		call gSetCharSize(2*dxsw,2*dysw)
	    call gDisplayStr(cnum)
     !   call write_string(cnum,xmin,ymin-0.05*ylth,0,-1,ifont,4,0,dxsw,dysw)
    iflag=0
	do i=1,ndat
			xv=xval(i)
		
			yv=yval(i)     !NB j=index of loop!
			if(xv.ge.xmin.and.xv.le.xmax.and.yv.ge.ymin.and.yv.le.ymax)then
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv)   !join with line from last point
				
					endif
				jflag=1
			endif
	enddo
	nx=10
	ny=800
	call gmActivateGraphicsFrame(graphics1_1(51))
	call getpixelarray(x0,ymin,x0,ymax,pixbuf,nx,ny)
	ic=13
	call drawline(x0,ymin,x0,ymax,ic,idev)
	ic=12
	call getpixelarray(xmin,yopen,xmax,yopen,pixbufz,ny,nx)
	call drawline(xmin,yopen,xmax,yopen,ic,idev)
	
	
	call gflushgraphics()
	
end