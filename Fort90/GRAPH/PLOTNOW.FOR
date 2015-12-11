	subroutine plotnow()

1551	continue
	nxlo=8
	nylo=200
	nyhi=-1
c	ictm=15		!text white
c	ibkm=8		!background dark grey
c	icfm=1		!frame dark blue
c	icupm=12		!upper case red
	TITLEs     ='   PLOT OPTIONS    '
	strings(1)='1. Black&white postscript '
	strings(2)='2. Black&white laserjet '
	strings(3)='3. Colour deskjet'
	strings(4)='4. Metafile colour (*.cgm)'
	strings(5)='5. metafile Grey shades (*.cgm)'
	strings(6)='6. Ascii File   '
	strings(8)='8. Windows metafile (*.wmf)'
	strings(7)='7. bitmap Format (*.bmp) '
	strings(7)='7. End          '
c	nval=8
	nval=7

	TITLEs1    ='POSITION    '
	str2(1)='1. Whole page'
	str2(2)='2. Top left'
	str2(3)='3. Top right '
	str2(4)='4. Lower left'
	str2(5)='5. Lower right '
	str2(6)='6. Cancel '
	nval1=6

777	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,line1,charout,ival)
      if(line1.ge.1.and.line1.le.nval) then	!iline=0 for ESC=cancel
	   ihelp=line1
	else
	   ihelp=6
	endif

	if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	   plot=.true.
	   if(ihelp.eq.3) then
		idev=6
c     	call setcls(.false.,icol,autplt,plotcols,isetcol)
         else if(ihelp.eq.1.or.ihelp.eq.2) then
		call setcls(.true.,icol,autplt,plotcols,isetcol)
	   endif
	   call POPMENU(nxlo,nylo,nyhi,str2,nval1,ictm,icfm,
     &   icupm,ibkm,titles1,helps,nhelp,jline,charout,ival)
	   if(jline.ge.1.and.jline.le.nval) then	!jline=0 for ESC=cancel
		ih1=jline
	   else
		ih1=7
	   endif
	   select case(ih1)
	    case(1)
		  mpos=5
	        csize=cbig		!csize=default size for whole page
	        thick=thbig		!line thickness=default thickness for whole page plots
	        vxloq=0		! for VIEWPORT
	        vxhiq=xp
	        vyloq=0
	        vyhiq=yp
	        xloq=s1*xp		! set normal shape
	        xhiq=s2*xp		! display
	        yloq=s1*yp		! location
	        yhiq=s2*yp		! screen
	    case(2)
		  mpos=ih1-1
	        vxloq=0		! for VIEWPORT: top left
	        vxhiq=xp/2
	        vyloq=yp/2
	        vyhiq=yp
	    case(3)
		  mpos=ih1-1
	        vxloq=xp/2	!for VIEWPORT: top right
	        vxhiq=xp
	        vyloq=yp/2
	  	  vyhiq=yp
	    case(4)
		  mpos=ih1-1
	  	  vxloq=0		!for VIEWPORT: bottom left
	        vxhiq=xp/2
	        vyloq=0
	        vyhiq=yp/2
	    case(5)
		  mpos=ih1-1
	        vxloq=xp/2	!for VIEWPORT: bottom right
	        vxhiq=xp
	        vyloq=0
	        vyhiq=yp/2
	    case(6)
	        plot=.false.
	        meta=.false.
	        wmeta=.false.
	        wbmp=.false.
		  goto 777
	   end select
	   if(mpos.eq.1.or.mpos.eq.2.or.mpos.eq.3.or.mpos.eq.4) then
	      thick=0.5		!line thickness multiplier
	      csize=csize*cfacsml
	      thick=thick*thfacsml
	      xloq=vxloq+0.6*s1*xp	! for GRAPHBOUNDARY- same for all
	      xhiq=vxloq+0.55*s2*xp
	      yloq=vyloq+0.5*s1*yp
	      yhiq=vyloq+0.52*s2*yp
	   endif
	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
 	   if(ihelp.eq.4) then
c     	call setcls(.false.,icol,autplt,plotcols,0)
	   else
          	call setcls(.true.,icol,autplt,plotcols,0)
 	   endif
	   metafil='cvfit.cgm'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
765	   continue
	   nc=nblank1(metafil)
	   call qdialog(1,'Name for metafile ',
     &   ' ['//metafil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 654
	   nc1=nblank1(cans)
	   if(metafil(nc1-4:nc1).ne.'.cgm'.and.nc1.le.26) then
		metafil=cans(1:nc1)//'.cgm'
	   else if(metafil(nc1-4:nc1).ne.'.cgm'.and.nc1.gt.26) then
		call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',ict)
		goto 765
	   endif
654	   continue
	else if(ihelp.eq.8) then
	   call WDIALOG(1,
     &'Sorry this option is not available for computers with less then
     &32MB RAM',ict)
	   goto 777
c	     wmeta=.true.
c	     idev=1
c      else if(ihelp.eq.7) then
c	   call WDIALOG(1,
c     &'Sorry this option is not available for computers with less then
c     &32MB RAM',ict)
c	   goto 777
c	     wbmp=.true.
c	     idev=1
	else if(ihelp.eq.6) then
	   if(ncurvd.gt.0) then
	   call TDIALOG(1,'File name for ascii OUTPUT (+ path if nec)',
     &	filnam,33,.true.,ict)
	   INQUIRE(file=filnam,exist=present,flen=len)
	   if(present.and.len.gt.0) then
	      call BELL(2)
		ans='N'
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &    ' The file '//charnb(filnam)//' already exists: overwrite it'
     &        ,defolt,12,cans)
 		call GETINPa(cans,ans)
	   endif
	   OPEN(unit=12,file=filnam,status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
	   write(12,FMT=800) ncurvd
800	   FORMAT(I10)
801	   FORMAT(2F15.5)
	   j=1
802	   write(unit=12,fmt=800) ndat(j)
	   n=ndat(j)
	   i=1
8030	   write(unit=12,fmt=801) Xval(i,j),Yval(i,j)
	   i=i+1
	   if(i.lt.n) goto 8030
	   j=j+1
	   if(j.lt.ncurvd) goto 802
	   CLOSE(unit=12)
	   endif
	   goto 777
	else if(ihelp.eq.7) then
	   plot=.false.
	   meta=.false.
	   wmeta=.false.
	   wbmp=.false.
	   goto 1814
	else
	   goto 777
	endif

c=====================================================================
	jopt=1
	call DEFOLTi(jopt,defolt)
	call QDIALOG(1,'(1) Plot now, or (2) Cancel plot',
     &  defolt,ict,cans)
	call GETINPi(cans,jopt)
	if(jopt.eq.1) then
	   if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	     nopt=1
	     call DEFOLTi(nopt,defolt)
	     call QDIALOG(1,'(1) Landscape, or (2) Portrait',
     &     defolt,ict,cans)
	     call GETINPi(cans,nopt)
	     if (nopt.eq.1) then
		  landplot=.true.
		  if(ihelp.eq.1) then
			idev=4
		  else if(ihelp.eq.2) then
			idev=5
		  else if(ihelp.eq.3) then
			idev=6
		  endif
	     else if (nopt.eq.2) then
		  landplot=.false.
	        vxloq=0		! for VIEWPORT
	        vxhiq=yp
	        vyloq=0
	        vyhiq=xp
	        xloq=s1*yp		! set normal shape
	        xhiq=s2*yp		! display
	        yloq=s1*xp		! location
	        yhiq=s2*xp		! screen
		  if(ihelp.eq.1) then
		  	idev=3
	   	  else if(ihelp.eq.2) then
			idev=5
	   	  else if(ihelp.eq.3) then
			idev=6
		  endif
	     endif
	   endif
	else
	   plot=.false.
	   meta=.false.
	   wmeta=.false.
	   wbmp=.false.
	   goto 777
	endif

c=====================================================================
	if (meta) then
	   call WDIALOG(1,
     &   'Assembling the metafile. . .',ict)
	   call devend
	   META=.TRUE.
	   ICGMDV=14
	   OPEN(UNIT=ICGMDV,FILE=metafil,STATUS='UNKNOWN')
	   CALL CGMBi
	   CALL DEVICE(ICGMDV,0)
c	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	   call papenq(xpm,ypm,itype)
	   vxlo=0.
	   vxhi=xpm
	   vylo=0.
	   vyhi=ypm
	   xlo=s1*xpm	! for GRAPHBOUNDARY- same for all
	   xhi=s2*xpm
	   dym=((s2-s1)*xpm*yp)/(ypm*xp)
	   ylo=((1-dym)/2)*ypm
	   yhi=ylo+dym*ypm
	   call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	   if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	   if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	   call spagra(vxlo,vylo,wxmin,wymin)
	   call spagra(vxhi,vyhi,wxmax,wymax)
	   call mode(18)
	   call WDIALOG(1,'Metafile in progress . . .',12)
	else if(wmeta) then
	   call WDIALOG(1,
     &   'Assembling the wmetafile. . .',ict)
	   call devend
c	   call wmf(60,640,480)
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	else if(wbmp) then
	   call WDIALOG(1,
     &   'Assembling the bmp. . .',ict)
	   call devend
c	   call bmp
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	else if (plot) then
	   if(ihelp.eq.2) then
	   	oldfile='HPLJR.OUT'
		if(landplot) then
c			a=2.
			XPP=280.5
			YPP=196.7
		else
c			a=1.4
			yPP=280.5
			xPP=196.7
			b=xp
			xp=yp
			yp=b
		endif
	   else if(ihelp.eq.3) then
	   	oldfile='DJ500C.OUT'
		if(.not.landplot) then
c			a=1.4
			XPP=203.2
			YPP=266.7
			b=xp
			xp=yp
			yp=b
		else
c			a=2.
			yPP=203.5
			xPP=266.7
		endif
	   else if(ihelp.eq.1) then
	   	oldfile='EPS.OUT'
		if(idev.eq.3)then
			XPP=210.
			YPP=297.
			b=xp
			xp=yp
			yp=b
c			a=1.4
		else
c			a=2.
			XPP=297.
			YPP=210.
		endif
	   endif
	   xhi=xhiq*xpp/xp
	   yhi=yhiq*ypp/yp
	   xlo=xhi-(xhiq-xloq)*xpp/xp
	   ylo=yhi-(yhiq-yloq)*ypp/yp
	   vxhi=vxhiq*xpp/xp
	   vyhi=vyhiq*ypp/yp
	   vxlo=vxhi-(vxhiq-vxloq)*xpp/xp
	   vylo=vyhi-(vyhiq-vyloq)*ypp/yp
	   kopt=1
	   call defolti(kopt,defolt)
	   call QDIALOG(1,' (1) Specify scale factor (2) Fix with mouse' ,
     &      defolt,ict,cans)
	   call getinpi(cans,kopt)
	   if (kopt.lt.1.or.kopt.gt.2) kopt=1
	   scalf=-1.
	   if(kopt.eq.1) then
	      call DEFOLTr(scalfac,defolt)
	      call QDIALOG(1,'Scale up/down : scale factor [0.2 to 1.2]',
     &      defolt,ict,cans)
	      call GETINPr(cans,scalfac)
	      if(scalfac.lt.0.2) scalfac=0.2
	      if(scalfac.gt.1.2) scalfac=1.2
		scalf = scalfac
	   endif
	   call FILLWIN(0,0,639,479,0)		!graph area
         call printopt(xlo,xhi,ylo,yhi,xpp,ypp,scalf)
	   thick=0.5		!line thickness multiplier
	   csize=csize*scalfac
	   thick=thick*scalfac
	   INQUIRE(file=oldfile,exist=present,flen=len)

	   if(present) call ERASE(oldfile)
	   if(bigplot.and.(.not.allpnt)) then
	      call INTCONV(ndelt,cnum1)
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Every nth point displayed, n = '
     &   	  //CHARNB(cnum1)//'; plot ALL points',defolt,ict,cans)
	      call GETINPa(cans,ans)
	      if(ans.ne.'N') allpnt=.true.
	   endif
	   call FILLWIN(0,0,639,387,icol(71))		!graph area
	   call FILLWIN(0,388,639,479,1)			!dialog area
	   jopt=1
	   call DEFOLTi(jopt,defolt)
	   call QDIALOG(1,' (1) Plot now, or (2) Cancel plot',
     &   defolt,ict,cans)
	   call GETINPi(cans,jopt)
	   if(jopt.ne.1) then
	      plot=.false.
	      meta=.false.
	      wmeta=.false.
	      wbmp=.false.
	      goto 1814
	   endif
	   call WDIALOG(1,
     &      'Assembling the plot . . .',ict)
	   call devend
	   if(ihelp.eq.1) then
	      if(landplot) then
		   printer='postscript landscape'
	   	   call eps(1,5.,5.,297.,210.,297.,210.)
	      else
		   printer='postscript portrait'
	   	   call eps(1,5.,5.,210.,297.,210.,297.)
	      endif
	   else if(ihelp.eq.2) then
		   if(landplot) then
		   	printer='laserjet landscape'
		   else
		   	printer='laserjet portrait'
		   endif
	   	   call hpljr
	   else if(ihelp.eq.3) then
		   if(landplot) then
		   	printer='color landscape'
		   else
		   	printer='color portrait'
		   endif
	   	   call DJ500C
	   endif
	   call errswi(-1)
	   call brkswi(1)
1042	   continue
	   call papenq(xpp,ypp,itype)
	   if(ihelp.eq.2.and..not.landplot) call devpap(196.7,280.5,itype)
	   if(ihelp.eq.3.and.landplot) call devpap(266.7,203.5,itype)
	   call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	   if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	   if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	   call spagra(vxlo,vylo,wxmin,wymin)
	   call spagra(vxhi,vyhi,wxmax,wymax)
	   call WDIALOG(1,'Plotting in progress ..'//printer,12)
c	   need to redefine box positions here (at least for bits
c	   of text that have boxes on final graph) as done after
c	   'GRAPH SHAPE'
	   call mode(18)
	endif
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	call FILLWIN(0,388,639,479,1)			!dialog area

