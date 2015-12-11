c======================================================================
	subroutine gplot3d(xmid,ymid,z,nbinx,nbiny,xtitle,ytitle,ztitle,
     &			title1,idrawt,idrawx,idrawy,idrawz)
c======================================================================

	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG
	INTEGER*4 IFILLZ(:,:),IFILLG(:,:)
	integer Lb(30),icol(100)		!for DCMENU
	real*4 z(nbinx,nbiny)
	real*4 xmid(nbinx),ymid(nbiny)
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:)
	character*75 xtitle,ytitle,ztitle,str
	character*64 title1
	character*78 text(30)
	character defolt*30,cans*30,kpchar*5,ans,UC,getch		!to hold text & result of QDIALOG
	logical all
	logical mono,plot,fill,inter,meta,present,landplot,axes
	character*33 titles,titles1
	character*33 metafil,printfil,oldfile,printer,strings(10),str2(10)
      INTEGER ISTYLE(6),ISUP(6)
      DATA ISTYLE/3,3,3,3,3,3/
      DATA ISUP/0,0,0,0,0,0/
	common/DMENU/ifonb,csize,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/hlp/help		!for QDIALOG
	COMMON/cols/icol,mono
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls

	idev=0
	ifnt=4
	ifnl=4
	alfa=0.
	beta=0.
	gama=0.
	delta=90.
	ijust=0
	ijusx=-1
	ijusy=1
	ijusz=0
	idim=nbinx*nbiny
	allocate(az(idim),ax(idim),ay(idim))
	ALLOCATE(ifillZ(nBINx-1,nBINy-1))
	ang=5.
	deltax=10.
	deltay=10.
	zmin=1.e36
	zmax=0.
	k=1
	do i=1,nbinx
	   do j=1,nbiny
		az(k)=z(i,j)
		ax(k)=xmid(i)
		ay(k)=ymid(j)
		k=k+1
		if(z(i,j).gt.zmax) zmax=z(i,j)
		if(z(i,j).lt.zmin) zmin=z(i,j)
	   enddo
	enddo
c	zmax=zmax+0.1*zmax
	zmin1=zmin	!save value
	zmax1=zmax	!save value
	call FIXAX(zmin1,zmax1,zmin,zmax,ztic,0)  !new zmin etc
	numx=nbinx
	numy=nbiny

C
C  INITIALISE VARIABLES
C
      xlow=xmid(1)
      xhigh=xmid(nbinx)
	dx=xmid(2)-xmid(1)
      ylow=ymid(1)
      yhigh=ymid(nbiny)
	dy=ymid(2)-ymid(1)
	np=nbinx*nbiny
C
C  MINIMUM WORKSPACE SIZES ARE :
C
      nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
	allocate(w(nw))

	call VGA
	call gsetcols(0)
 	call errswi(-1)
 	call brkswi(1)
 	call chaswi(1)
 	call grfmod (1)
 	call harcha
 	call mode(18)
      CALL PICCLE
	vxlo=0.	! for VIEWPORT
	vxhi=1080.
	vylo=0.
	vyhi=810.
	vxlo1=vxlo
	vxhi1=vxhi
	vylo1=vylo
	vyhi1=vyhi
	xmin=xlow
	xmax=xhigh
	ymin=ylow
	ymax=yhigh
	xminz=xlow
	xmaxz=xhigh
	yminz=ylow
	ymaxz=yhigh
	icf=1
	icf2=1
	ict=11
	icb2=0
	ifont=2
	ifont2=0
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	icol0=7	!white text/border for altered boxes
	icb2=0	!background colour for dialog box 1,2
	icf=7		!frame colour for dialog box 1
	icf2=12	!frame colour for dialog box 2
	ict=11	!text colour for dialog box 1
	call DEFDIALOG(1,1,2,4,60,icb2)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,icb2)	!define dialog box #2
1000  continue


	call broken(0)
	RATIO=0.35
      RADIUS=500.0
      THETA=30.0
      PHI=30.0
	IFRAM=2
	XWMIN=100.
	XWMAX=1050.
	YWMIN=50.
	YWMAX=620.
	FILL=.FALSE.
	INTER=.FALSE.
	AXES=.TRUE.
c     Fill background colour
	 NDC = 1
       NCOL = NDC
       ISET=4
       NDC=MIN(NDC-ISET,64)
      XSOURC=-100000.0
      YSOURC=100000.0
      ZSOURC=150000.0
      DO 300 I=1,nBINX-1
        DO 310 J=1,nBINY-1
          XNORM=Z(I,J)-Z(I+1,J)
          YNORM=Z(I+1,J)-Z(I+1,J+1)
          ZNORM=1.0
          MAGNIT=SQRT(XNORM**2+YNORM**2+ZNORM**2)
          XNORM=XNORM/MAGNIT
          YNORM=YNORM/MAGNIT
          ZNORM=ZNORM/MAGNIT
          XLIGHT=XSOURC-float(I)
          YLIGHT=YSOURC-float(J)
          ZLIGHT=ZSOURC-Z(I,J)
          MAGNIT=SQRT(XLIGHT**2+YLIGHT**2+ZLIGHT**2)
          XLIGHT=XLIGHT/MAGNIT
          YLIGHT=YLIGHT/MAGNIT
          ZLIGHT=ZLIGHT/MAGNIT
          SFILL=(XNORM*XLIGHT+YNORM*YLIGHT+ZNORM*ZLIGHT)
          IF(SFILL.LT.0.0)SFILL=0.0
          IF(SFILL.GT.0.9)SFILL=0.9
          IFILLZ(I,J)=NINT(SFILL*float(NDC-1))+ISET
310     CONTINUE
300     CONTINUE
1002	if(.not.mono) then
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	endif
	if(.not.plot) then
	   call HLINE(0,639,54,15)		!line to mark bottom of data area
	   call HLINE(0,639,389,15)	!line to mark top of data area
	endif

c
c	PLOT GRAPH
c
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
1003  CALL SETVP2(vxlo,vxhi,vylo,vyhi,0.0,XPAP,0.0,YPAP)	!screen->grey
	ymenu=vyhi/480.*53.
	xp=xpap
	yp=ypap
	xlo=0.2*vxhi
	xhi=0.8*vxhi
	ylo=0.2*vyhi
	yhi=0.75*vyhi
	csize=vyhi/80. !copy for common/dmenu/
	sizet=csize*2.5
	sizel=csize*1.7
      CALL SURF		!Surf banner

	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
      call perspa(xmax,ymax,1.2*zmax,xst,yst)
c=====================================================================

100   continue
	if(all) then
		CALL PRJBND(XLO1,XHI1,YHI,YHI)
	else
		CALL PRJBND(XLO,XHI,YHI,YHI)
	endif
	all=.false.
      CALL PRJFRA(IFRAM)
      CALL WINDO2(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL HEIRAT(RATIO)
	call PRJLEV(zmin,zmax)

      if(plot) then
      	CALL PRJLIN(100,100,100)
	else
      	CALL PRJLIN(1000,100,100)
	endif
	call setfnt(ifnl)
      CALL CHASIZ(csize,csize)
	call perset(xmin,xmax,ymin,ymax,zmin,zmax,radius,theta,phi)
	IF(INTER) THEN
		if(fill) then
		   if(ifg.eq.1) deallocate(ifillg)
		   ALLOCATE(ifillg(numx-1,numy-1))
		   ifg=1
		   DO I=1,NUMX-1
		     DO J=1,NUMY-1
		       XNORM=GRIDZ(I,J)-GRIDZ(I+1,J)
		       YNORM=GRIDZ(I+1,J)-GRIDZ(I+1,J+1)
		       ZNORM=1.0
		       MAGNIT=SQRT(XNORM**2+YNORM**2+ZNORM**2)
		       XNORM=XNORM/MAGNIT
		       YNORM=YNORM/MAGNIT
		       ZNORM=ZNORM/MAGNIT
		       XLIGHT=XSOURC-float(I)
		       YLIGHT=YSOURC-float(J)
		       ZLIGHT=ZSOURC-GRIDZ(I,J)
		       MAGNIT=SQRT(XLIGHT**2+YLIGHT**2+ZLIGHT**2)
		       XLIGHT=XLIGHT/MAGNIT
		       YLIGHT=YLIGHT/MAGNIT
		       ZLIGHT=ZLIGHT/MAGNIT
		       SFILL=(XNORM*XLIGHT+YNORM*YLIGHT+ZNORM*ZLIGHT)
		       IF(SFILL.LT.0.0)SFILL=0.0
		       IF(SFILL.GT.0.9)SFILL=0.9
		       IFILLG(I,J)=NINT(SFILL*float(NDC-1))+ISET
		     enddo
		   enddo
      	   CALL PERFIL(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX ,GRIDZ,
     +             RADIUS,THETA,PHI,IFILLG,NW,W)	!grid displayed
		else
      	   CALL PERPRJ(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX ,GRIDZ,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed
		endif
	ELSE
		if(fill) then
   			CALL PERFIL(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,IFILLZ,NW,W)
		else
   			CALL PERPRJ(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,NW,W)
		endif
	ENDIF
C  	ADD 3D AXES
	if(axes) then
      	CALL WINDOW(2)
      	CALL PRJSCA(1.0,1.0,ztic)
      	CALL PERAXE(NW,W)		!axes displayed
C		if(perf.eq.0) then
		call perspa((xmin+xmax)/2,ymin-4*dy,zmin,xspx,yspx)
		call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
		call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
C		endif
		call lincols(icol(23),idev)
		call setfnt(ifnl)
	   	if(idrawx.eq.1) call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &	sizel,ijusx)
	   	if(idrawy.eq.1) call JUSTIFYSTRING(xspy,yspy,ytitle,gama,
     &	sizel,ijusy)
	   	if(idrawz.eq.1) call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	sizel,ijusz)
	endif
c	ADD TITLE
	call lincols(ICOL(25),idev)
	call setfnt(ifnt)
	if(perf.eq.0) call perspa(xmax,ymax,1.2*zmax,xst,yst)
	if(idrawt.eq.1) call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	sizet,ijust)
	perf=1
	if(meta) goto 5555
	if(plot) goto 1812		!return with plot=true?


c=====================================================================
c
c  	DRAW MENU
c

150	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
151	continue
	text(1) =' 1: RESCALE     '
	text(2) =' 2: ROTATE    '
	text(3) =' 3: ELEVATION'
	text(4) =' 4: FRAME'
	text(5) =' 5: INTERPOLATE'
	text(6) =' 6: GIVE TITLE'
	text(7) =' 7: PLOT NOW   '
	text(8) =' 8: AXES STYLE'
	text(9) =' 9: END DISPLAY'
	text(10)='10: REDRAW    '
	text(11)=' +: X AXIS LABEL'
	text(12)=' -: Y AXIS LABEL'
	text(13)=' x: Z AXIS LABEL'
	if(fill) then
	   text(14)='/: NO FILL'
	else
	   text(14)='/: FILL  '
	endif
	text(15)=' .: MORE OPTION  '
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)
	   goto 152		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 152		!another menu choice
	endif

	goto(1,2,3,4,5,6,7,8,9,10,11,11,11,14,15)ikey

c========================================================================
c                           RESCALE
c========================================================================
1	continue
	call DCMENU(-1,4,Lb,text,0,0)	!delete box 1 only
	Lb(1)=-1
	call DCMENU(-1,4,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	call DEFOLT2r(xminz,xmaxz,defolt)
	call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	call GETINP2r(cans,xminz,xmaxz)
	XMIN=XMINz
	XMAX=XMAXz

c=============================================================
c				' Ymin,Ymax  '
c=============================================================
	call DEFOLT2r(yminz,ymaxz,defolt)
	call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	call GETINP2r(cans,yminz,ymaxz)
	YMIN=YMINz
	YMAX=YMAXz
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                           ROTATE
c========================================================================
2	continue
	call DEFOLTr(phi,defolt)
	call QDIALOG(1,'Phi',defolt,ict,cans)
	call GETINPr(cans,phi)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                           ELEVATION
c========================================================================
3	continue

	call DEFOLTr(theta,defolt)
	call QDIALOG(1,'Theta',defolt,ict,cans)
	call GETINPr(cans,theta)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                          FRAME
c========================================================================
4	continue
	call DEFOLTI(IFRAM,defolt)
	call QDIALOG(1,'Frame',defolt,ict,cans)
	call GETINPi(cans,ifram)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                           INTERPOLATE
c========================================================================
5	continue
	call DEFOLT2i(numx,numy,defolt)
	call QDIALOG(1,'Numx,Numy',defolt,ict,cans)
	call GETINP2i(cans,numx,numy)
	if(numx.eq.nbinx.and.numy.eq.nbiny) goto 152
	if(numx.lt.nbinx.or.numy.lt.nbiny) goto 5
	if(ifl.eq.1) deallocate(gridz)
	allocate(gridz(numx,numy))
	ifl=1
	deallocate(w)
      nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
	allocate(w(nw))
      CALL RANGRD(NP,AX,AY,AZ,NUMX,XLOW,XHIGH,NUMY,YLOW,YHIGH,GRIDZ,
     +            NW,W)
	INTER=.TRUE.
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	GOTO 152

c========================================================================
c                           READ IN TITLE
c========================================================================
6	continue
	call DCMENU(-6,4,Lb,text,0,0)	!delete box 4 only
	Lb(6)=-1
	call DCMENU(-6,4,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	call setfnt(ifnt)
	call lincols(icol(71),0)
	if(idrawt.eq.1) then	!delete existing title first
	   call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	 sizet,ijust)
	endif
	call GSTRING(xst,yst,title1,ifnt,alfa,sizet,
     &   ijust,icol(25),icol(71),64,blank,nrealt)
	nlg=nblank1(title1)

	if (title1(nlg:nlg).eq.char(95)) title1(nlg:nlg)=char(32)
	if(nlg.gt.1) nlg=nlg-1
	if(idrawt.eq.0.and.nlg.gt.1) idrawt=1
	goto 100

c========================================================================
c                           PLOT NOW
c========================================================================
7	continue
c 	open a 2 row dialog box top=row 23; cols 2-58
	landplot=.true.		!landscape is default
c set whole page as default
c NB ixlo already set (and possibly reset by 'MOVE GRAPH') so do set here
	call SETLBOX(nbox,Lb,1)
	Lb(1)=-1		!whole page is default
	call DCMENU(0,4,Lb,text,0,0)		!delete all
1551	continue
	nxlo=8
	nylo=200
	nyhi=-1
	TITLEs     ='   PLOT OPTIONS    '
	strings(1)='1. Black&white postscript '
	strings(2)='2. Black&white laserjet '
	strings(3)='3. Color deskjet'
	strings(4)='4. Metafile color (*.cgm)'
	strings(5)='5. metafile Grey shades (*.cgm)'
	strings(6)='6. End          '
	nval=6

	ictm=15		!text white
	ibkm=1		!background daek blue
	icfm=11		!frame/title light blue
	icupm=12		!upper case red
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
	   endif

	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
	   metafil='cvfit.cgm'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
765	   continue
	   nc=nblank1(metafil)
	   call qdialog(1,'Name for metafile ',
     &   ' ['//metafil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 654
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		metafil=cans(1:nc1)//'.cgm'
		goto 654
	   endif
	   call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',ict)
	   goto 765
654      continue
	else if(ihelp.eq.6) then
	   plot=.false.
	   meta=.false.
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
	        vxlo=vylo		! for VIEWPORT
	        vxhi=vyhi
	        vylo=vxlo
	        vyhi=vxhi
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
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	   call mode(18)
	   call WDIALOG(1,'Metafile in progress . . .',12)
	else if (plot) then
	   if(ihelp.eq.2) then
	   	oldfile='HPLJR.OUT'
		if(landplot) then
			a=2.
			XPP=280.5
			YPP=196.7
		else
			a=1.4
			yPP=280.5
			xPP=196.7
			b=xp
			xp=yp
			yp=b
		endif
	   else if(ihelp.eq.3) then
	   	oldfile='DJ500C.OUT'
		if(.not.landplot) then
			a=1.4
			XPP=203.2
			YPP=266.7
			b=xp
			xp=yp
			yp=b
		else
			a=2.
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
			a=1.4
		else
			a=2.
			XPP=297.
			YPP=210.
		endif
	   endif
c	   xhi=xhiq*xpp/xp
c	   yhi=yhiq*ypp/yp
c	   xlo=xhi-(xhiq-xloq)*xpp/xp
c	   ylo=yhi-(yhiq-yloq)*ypp/yp
c	   vxhi=vxhiq*xpp/xp
c	   vyhi=vyhiq*ypp/yp
c	   vxlo=vxhi-(vxhiq-vxloq)*xpp/xp
c	   vylo=vyhi-(vyhiq-vyloq)*ypp/yp
c	   kopt=1
c	   call defolti(kopt,defolt)
c	   call QDIALOG(1,' (1) Specify scale factor (2) Fix with mouse' ,
c     &      defolt,ict,cans)
c	   call getinpi(cans,kopt)
c	   if (kopt.lt.1.or.kopt.gt.2) kopt=1
c	   scalf=-1.
c	   if(kopt.eq.1) then
c	      call DEFOLTr(scalfac,defolt)
c	      call QDIALOG(1,'Scale up/down : scale factor [0.8 to 1.1]',
c     &      defolt,ict,cans)
c	      call GETINPr(cans,scalfac)
c	      if(scalfac.lt.0.8) scalfac=0.8
c	      if(scalfac.gt.1.2) scalfac=1.1
c		scalf = scalfac
c	   endif
c	   call FILLWIN(0,0,639,479,0)		!graph area
c         call printopt(xlo,xhi,ylo,yhi,xpp,ypp,scalf)
	   INQUIRE(file=oldfile,exist=present,flen=len)
	   if(present) call ERASE(oldfile)
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
	   call WDIALOG(1,'Plotting in progress ..'//printer,12)
	   call mode(18)
	endif
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	call FILLWIN(0,388,639,479,1)			!dialog area
	goto 1002	!do plot and return to 1812

c=================================================================================7

5555  continue
	call WDIALOG(1,'Metafile done. . .',12)
	meta=.false.
	CLOSE(UNIT=ICGMDV)
	CALL DEVEND
c==========================================
9876	call VGA
	call gsetcols(0)
	call mode(18)
	call errswi(-1)
	call brkswi(1)
	idev=0
	scalfac=1.0
c	call papenq(xp,yp,itype)
c	xlo=xlo2		!restore screen GRAPHBOUNDARY
c	xhi=xhi2
c	ylo=ylo2
c	yhi=yhi2
c	vxlo=0		! for VIEWPORT
c	vxhi=xp
c	vylo=0
c	vyhi=yp
	goto 1002

c=================================================================================7
c 	Return to 1813 to plot via VTRANS
1813	continue
	plot=.false.
c	mono=.false.

c====================================================================================
c After plot finished:
1812	continue
	call DEVEND
	if(idev.eq.2) then
	   printfil='cvfit.out'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
7651	   continue
	   nc=nblank1(printfil)
	   call qdialog(1,'Name for printfile ',
     &   ' ['//printfil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 6541
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		printfil=cans(1:nc1)//'.out'
		goto 6541
	   endif
	   call wdialog(2,'YOU ENTERED AN INVALID FILE NAME',ict)
	   call COPY(oldfile,printfil)
	   goto 7651
6541	   continue
	endif
	if(idev.eq.4.or.idev.eq.3.or.idev.eq.5.or.idev.eq.6) then
	   kp=kp+1
	   call intconv(kp,kpchar)
	   nc1=nblank1(kpchar)
	   if(ihelp.eq.1) then
	   	printfil='efit'//kpchar(1:nc1)//'.out'
	   else if(ihelp.eq.2 ) then
	   	printfil='lfit'//kpchar(1:nc1)//'.out'
	   else if(ihelp.eq.3) then
	   	printfil='cfit'//kpchar(1:nc1)//'.out'
	   endif
c	   call COPY(oldfile,printfil)
c	   call COPY(printfil,'lpt1')
	   call COPY(oldfile,'lpt1')
	endif

	call VGA
	call errswi(-1)
	call brkswi(1)
	call gsetcols(0)
	call mode(18)
1814	continue
	idev=0
c	call papenq(xp,yp,itype)
	plot=.false.
c 	Restore idev=0 (and scalfac, if altered)
	scalfac=1.0
	call setfnt(ifont)
c	xlo=xlo2		!restore screen GRAPHBOUNDARY
c	xhi=xhi2
c	ylo=ylo2
c	yhi=yhi2
	vxlo=vxlo1		! for VIEWPORT
	vxhi=vxhi1
	vylo=vylo1
	vyhi=vyhi1
1043	continue
	if (ih1.eq.7.or.jopt.eq.2) then
		call WDIALOG(1,'Plot cancelled',12)
	else
		call WDIALOG(1,'Plot done. . .',12)
	endif
	goto 1002


c======================================================================
c				AXIS STYLE
c========================================================================
8	continue

	IF(axes) THEN
	   axes=.FALSE.
	ELSE
	   axes=.TRUE.
	ENDIF
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c     goto 152

c======================================================================
c				END DISPLAY
c========================================================================
9  	goto 999

c==================================================================
c			REDRAW
c==================================================================
10	CONTINUE
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100


c======================================================================
c				AXIS LABELS
c========================================================================

c Section to read in new axis labels
11	continue
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol1,icol2)	!draw box #ikey only (italic)
	call WDIALOG(1,'Now write text: hit F1 anytime for HELP  ',ict)
	call setfnt(ifnl)
	call lincols(icol(71),0)
	if(ikey.eq.11) then
		if(idrawx.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &	 sizel,ijusx)
		endif
		call GSTRING(xspx,yspx,xtitle,ifnl,beta,sizel,
     &   	ijusx,icol(23),icol(71),64,blank,nrealt)
		if(idrawx.eq.0) idrawx=1
	else if(ikey.eq.12) then
		if(idrawy.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspy,yspy,ytitle,gama,
     &	 sizel,ijusy)
		endif
		call GSTRING(xspy,yspy,ytitle,ifnl,gama,sizel,
     &   	ijusy,icol(23),icol(71),64,blank,nrealt)
		if(idrawy.eq.0) idrawy=1
	else if(ikey.eq.13) then
		if(idrawz.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	 sizel,ijusz)
		endif
		call GSTRING(xspz,yspz,ztitle,ifnl,delta,sizel,
     &   	ijusz,icol(23),icol(71),64,blank,nrealt)
		if(idrawz.eq.0) idrawz=1
	endif
	ilabel=1		!so internal default labels not used
	goto 152


c========================================================================
c                           FILL
c========================================================================
14	continue
	IF(FILL) THEN
	   FILL=.FALSE.
	   if(ifg.eq.1) DEALLOCATE(IFILLG)
 	   call FILLWIN(0,55,639,387,icol(71))	!graph area
	   ifg=0
	   goto 100
c	   GOTO 152
	ELSE
	   FILL=.TRUE.
	ENDIF
C  	CALCULATE IFILL ARRAY FROM POINT LIGHT SOURCE

 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
c	GOTO 152

c========================================================================
C		MORE OPTION
c========================================================================
15 	CONTINUE
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
251	continue
	text(1) =' 1: FIX TEXT'
	text(2) =' 2: VIEWPORT'
	text(3) =' 3: WINDOW'
	text(4) =' 4: RATIO'
	text(5) =' 5: Z LIMIT'
	text(10) ='10: REDRAW'
	DO I=6,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
252	call CKEY(ch,ikey)
	if(ikey.eq.10) goto 150
	if(ikey.lt.-2.or.ikey.gt.10) goto 251
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)
	   goto 252		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 252		!another menu choice
	endif
	GOTO(601,602,603,604,605)IKEY
c========================================================================
C		FIX TEXT
c========================================================================
601 	CONTINUE
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
161	continue
	text(1) =' 1: TITLE'
	text(2) =' 2: XLABEL'
	text(3) =' 3: YLABEL'
	text(4) =' 4: ZLABEL'
	text(10) =' 10: REDRAW'
	DO I=5,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
162	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.10) goto 161
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)
	   goto 162		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 162		!another menu choice
	endif
	if(ikey.eq.10) goto 15
	if(ikey.eq.1) then
		str   =title1
		angle	=alfa
		size	=sizet
		icolor	=icol(25)
		ifont	=ifnt
		xm	=xst
		ym	=yst
		ijus	=ijust
		idraw	=idrawt
	else if(ikey.eq.2) then
		str   =xtitle
		angle	=beta
		size	=sizel
		icolor	=icol(23)
		ifont	=ifnl
		xm	=xspx
		ym	=yspx
		ijus	=ijusx
		idraw	=idrawx
	else if(ikey.eq.3) then
		str   =ytitle
		angle	=gama
		size	=sizel
		icolor	=icol(23)
		ifont	=ifnl
		xm	=xspy
		ym	=yspy
		ijus	=ijusy
		idraw	=idrawy
	else if(ikey.eq.4) then
		str   =ztitle
		angle	=delta
		size	=sizel
		icolor	=icol(23)
		ifont	=ifnl
		xm	=xspz
		ym	=yspz
		ijus	=ijusz
		idraw	=idrawz
	endif
	ibk=icol(71)
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
171	continue
	text(1) =' <-: MOVE LEFT'
	text(2) =' ->: MOVE RIGHT'
	text(3) =' v : MOVE DOWN'
	text(4) =' ^ : MOVE UP'
	text(5) =' S : SIZE   '
	text(6) =' C : COLOR  '
	text(7) =' F : FONT   '
	text(8) =' + : INC ANGLE'
	text(9) =' - : DEC ANGLE'
	text(10) ='10: REDRAW'
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
33	ans=getch(ktype)
	ival=ichar(ans)
	if(ktype.eq.3.and.ival.eq.48) goto 400
	if(ktype.eq.8) then	!move menu with ARROW keys
	   if(ival.eq.77) then	!right
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(xm-deltax.lt.xwmax) then
	         xm=xm+deltax
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.75) then		!shift left
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(xm+deltax.gt.xwmin) then
	         xm=xm-deltax
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.72) then	!shift up
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(ym-deltay.lt.ywmax) then
	         ym=ym+deltay
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.80) then	!shift down
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(ym+deltay.gt.ywmin) then
	         ym=ym-deltay
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	else if(ktype.eq.3) then
	   if(ival.eq.43) then
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
	      angle=angle+ang
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.45) then
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(l)
	      call justifystring(xm,ym,str,angle,size,ijus)
	      angle=angle-ang
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	else if(ktype.eq.2) then
	   if(ival.eq.99) then
 	   	call DEFOLTi(icolor,defolt)
	      call QDIALOG(1,'Color ',defolt,11,cans)
	   	call GETINPi(cans,icolor)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.102) then
		ifont0=ifont
 	   	call DEFOLTr(size,defolt)
	      call QDIALOG(1,'Size ',defolt,11,cans)
	   	call GETINPr(cans,size)
		call setfnt(ifont0)
		call lincols(ibk,idev)
	      call justifystring(xm,ym,str,angle,size0,ijus)
		call setfnt(ifont)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.115) then
		size0=size
 	   	call DEFOLTr(size,defolt)
	      call QDIALOG(1,'Size ',defolt,11,cans)
	   	call GETINPr(cans,size)
		call lincols(ibk,idev)
	      call justifystring(xm,ym,str,angle,size0,ijus)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	endif
	if(ktype.eq.16.and.ival.eq.27) then
   		GOTO 400
	endif
400   continue
	if(ikey.eq.1) then
	     alfa    = angle
	     sizet   = size
	     icol(25)= icolor
	     ifnt    = ifont
	     xst     = xm
	     yst     = ym
	     ijust   = ijus
	     idrawt  = idraw
	else if(ikey.eq.2) then
	     beta    =angle
	     sizel   =size
	     icol(23)=icolor
	     ifnl    =ifont
	     xspx    =xm
	     yspx    =ym
	     ijusx   =ijus
	     idrawx  =idraw
	else if(ikey.eq.3) then
	     gama    =angle
	     sizel   =size
	     icol(23)=icolor
	     ifnl    =ifont
	     xspy    =xm
	     yspy    =ym
	     ijusy   =ijus
	     idrawy  =idraw
	else if(ikey.eq.4) then
	     delta    =angle
	     sizel    =size
	     icol(23) =icolor
	     ifnl     =ifont
	     xspz     =xm
	     yspz     =ym
	     ijusz    =ijus
	     idrawz   =idraw
	endif
   	GOTO 601

c========================================================================
C		VIEWPORT
c========================================================================
602 	CONTINUE
	call DEFOLT2r(vxlo,vxhi,defolt)
	call QDIALOG(1,'vxlo, vxhi',defolt,ict,cans)
	call GETINP2r(cans,vxlo,vxhi)
	call DEFOLT2r(vylo ,vyhi ,defolt)
	call QDIALOG(1,'vylo, vyhi',defolt,ict,cans)
	call GETINP2r(cans,vylo ,vyhi )
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	vxlo1=vxlo
	vxhi1=vxhi
	vylo1=vylo
	vyhi1=vyhi

	GOTO 1003
c========================================================================
C		WINDOW
c========================================================================
603	CONTINUE
	call DEFOLT2r(xwmin,xwmax,defolt)
	call QDIALOG(1,'Xwmin, Xwmax',defolt,ict,cans)
	call GETINP2r(cans,xwmin,xwmax)
	call DEFOLT2r(ywmin,ywmax,defolt)
	call QDIALOG(1,'Ywmin, Ywmax',defolt,ict,cans)
	call GETINP2r(cans,ywmin,ywmax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================
c========================================================================
C		RATIO
c========================================================================
604 	CONTINUE
	call DEFOLTr(ratio,defolt)
	call QDIALOG(1,'Ratio',defolt,ict,cans)
	call GETINPr(cans,ratio)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
C		Z
c========================================================================
605 	CONTINUE
	call DEFOLT2r(zmin,zmax,defolt)
	call QDIALOG(1,'Zmin, Zmax',defolt,ict,cans)
	call GETINP2r(cans,zmin,zmax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================

c TIDY UP AND EXIT
999	continue
	if(iabs(iask).eq.1) then
	   call BELL(1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'ARE YOU SURE',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(UC(ans).eq.'N') goto 151
	endif
	if(iask.gt.0) call DCMENU(0,5,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	deALLOCATE(ax,ay,az,ifillz)
	if(ifg.eq.1) deALLOCATE(ifillg)
	if(ifl.eq.1) deALLOCATE(gridz)
	vxlo=0.	! for VIEWPORT
	vxhi=xp
	vylo=0.
	vyhi=yp

	call mode(3)
	RETURN
	END

c========================================================================

      SUBROUTINE SCOL(NDC,ISET)
      DO 100 I=ISET,ISET+NDC
        CALL HLSDEF(I,80.0,
     +         float(I-ISET)/float(NDC*3)+0.2,0.8)
100   CONTINUE
      RETURN
      END
