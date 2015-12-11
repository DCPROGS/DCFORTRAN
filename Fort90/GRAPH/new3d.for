c======================================================================
	subroutine gplot3d(xmid,ymid,z,nbinx,nbiny,xtitle,ytitle,ztitle,
     &			title1,idrawt,idrawx,idrawy,idrawz)
c======================================================================
c	tic control : length only by CHASIZ
c			: number by PRJSCA
	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG,res,fes,resi,fesi
	allocatable::azR,axR,ayR
 	INTEGER*4 IFILLZ(:,:),IFILLG(:,:)
	integer Lb(30),icol(100)		!for DCMENU
	real*4 z(nbinx,nbiny),azr(:)
	real*4 xmid(nbinx),ymid(nbiny),res(:,:),fes(:,:),axr(:),ayr(:)
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:),resi(:,:),fesi(:,:)
	character*75 xtitle,ytitle,ztitle,str
	character*75 title1
	character*78 text(30),message
	character defolt*30,cans*30,kpchar*5,ans,UC,getch		!to hold text & result of QDIALOG
	logical all,cross,after,rescale,xda
	logical mono,plot,fill,inter,meta,present,landplot,axes,help
	character*33 titles
	character*33 metafil,printfil,oldfile,printer,strings(10)
      INTEGER ISTYLE(6),ISUP(6)
	common/DMENU/ifonb,csizeb,ifont2,nboxlast,nblast		!for DCMENU
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
	allocate(azr(5*idim),axr(5*idim),ayr(5*idim))
	ALLOCATE(ifillZ(nBINx,nBINy))
	ALLOCATE(fes(nBINx,nBINy))
	ALLOCATE(res(nBINx,nBINy))
	ifz=1
	ang=5.
	deltax=10.
	deltay=10.
	zmin=1.e36
	zmax=0.
	k=1
	do i=1,6
		istyle(i)=3
		isup(i)=0
	enddo
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
	numx=nbinx
	numy=nbiny
C
	   OPEN(unit=12,file='result',status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
801	   FORMAT(I5,I5,5F10.5)
	   j=1
802	   i=1
8030	   write(unit=12,fmt=801) j,i,(z(j,k),k=i,i+4)
	   i=i+5
	   if(i.le.nbiny) goto 8030
	   j=j+1
	   if(j.le.nbinx) goto 802
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
	ifw=1
	call VGA
	call gsetcols(0)
c 	call errswi(-1)
c 	call errmax(-1)
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
	xmin1=xmin	!save value
	xmax1=xmax	!save value
	ymin1=ymin	!save value
	ymax1=ymax	!save value
	zmin1=zmin	!save value
	zmax1=zmax	!save value
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)  !new zmin etc
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)  !new zmin etc
	call FIXAX(zmin1,zmax1,zmin,zmax,ztic,0)  !new zmin etc
	dxd=xmax-xmin
	dyd=ymax-ymin
	dzd=zmax-zmin
	dnbinx=nbinx/(xmax-xmin)
	dnbiny=nbiny/(ymax-ymin)
	dnumx=numx/(xmax-xmin)
	dnumy=numy/(ymax-ymin)
	qmin=xmin
	qmax=xmax
	rmin=ymin
	rmax=ymax
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

	cross=.false.
	all=.false.
	call broken(0)
	RATIO=0.35
	RAT=1.0
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
	NUMAXI=0
	NWIDTH=5
	NPOW=1
	NPLACE=1
	nxstep=1
	nystep=1
c     Fill background colour
	 NDC = 1
c==	NCOL = NDC
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
      CALL SURF		!Surf banner
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
1003  CALL SETVP2(vxlo,vxhi,vylo,vyhi,0.0,XPAP,0.0,YPAP)	!screen->grey
	ymenu=vyhi/480.*53.
	xp=xpap
	yp=ypap
	xlo=0.2*vxhi
	xhi=0.8*vxhi
	ylo=0.2*vyhi
	yhi=0.72*vyhi
	xlo1=xlo
	xhi1=xhi
	ylo1=ylo
	yhi1=yhi

	csize=vyhi/80. !copy for common/dmenu/
	csizeb=csize
	sizet=csize*2.5
	sizel=csize*1.7
c====      CALL SURF		!Surf banner

	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
c=====================================================================

100   continue
	call setfnt(ifnl)
      CALL CHASIZ(csize,csize)
      if(plot) then
      	CALL PRJLIN(100,100,100)
	else if(meta.and.cross) then
      	CALL PRJLIN(1000,100,100)
	else
      	CALL PRJLIN(1000,100,100)
	endif
	if((cross.and..not.meta.and..not.plot.and..not.after)) goto 444
	if(all) then
		CALL PRJBND(XLO1,XHI1,YLO1,YHI1)
	else
		CALL PRJBND(XLO,XHI,YLO,YHI)
	endif
	if(cross.and.meta.or.cross.and.plot.or.cross.and.after)
     &	call linvis(0)
c	call error(message)
      CALL PRJFRA(IFRAM)
	call prjgrd(nxstep,nystep)
      CALL WINDO2(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PRJSCA(xtic,ytic,ztic)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL HEIRAT(RATIO)
      CALL XYRAT(RAT)
	call PRJLEV(zmin,zmax)
	call perset(xmin,xmax,ymin,ymax,zmin,zmax,radius,theta,phi)
	IF(INTER) THEN
		if(fill) then
		   if(ifg.eq.1) deallocate(ifillg)
		   ALLOCATE(ifillg(numx-1,numy-1))
		   iff=1
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
		   IF(RESCALE) THEN
      	   CALL PERPRJ(NUMXR,XMIN,XMAX ,NUMYR,YMIN,YMAX ,RESI,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed
		   ELSE
      	   CALL PERPRJ(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX ,GRIDZ,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed
		   ENDIF
		endif
	ELSE
		if(fill) then
		   if(rescale) then
   			CALL PERFIL(NBINXR,XMIN,XMAX,NBINYR,YMIN,YMAX,
     &              RES(1:nbinxr,1:nbinyr),RADIUS,THETA,PHI,FES,NW,W)
		   else
   			CALL PERFIL(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,IFILLZ,NW,W)
		   endif
		else
		   if(rescale) then
   			CALL PERPRJ(NBINXR,XMIN,XMAX,NBINYR,YMIN,YMAX,RES,
     &              RADIUS,THETA,PHI,NW,W)
		   else
   			CALL PERPRJ(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,NW,W)
		   endif
		endif
	ENDIF
C  	ADD 3D AXES
      CALL WINDOW(2)
	if(cross) goto 444
	if(axes) then
		call ANNDIG(1,NWIDTH,NPLACE,NPOWER)
		call ANNDIG(2,NWIDTH,NPLACE,NPOWER)
		call ANNDIG(3,NWIDTH,NPLACE,NPOWER)
		call ANNSTR(1,'            ')
		call ANNSTR(2,'            ')
     		call ANNSTR(3,'            ')
      	CALL PERAXE(NW,W)		!axes displayed
		if(perf.ne.-100) then
		call perspa((xmin+xmax)/2,ymin-4*dy,zmin,xspx,yspx)
		call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
		call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
		endif
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
	if(perf.ne.-100) call perspa(xmax,ymax,1.2*zmax,xst,yst)
	if(idrawt.eq.1) call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	sizet,ijust)
444	continue
	call linvis(1)
	if(cross) then
		if(all) then
			call CRSBND(0.55*vxhi,0.9*vxhi,ylo,yhi)
			call CRSSEC(x_miny,y_miny,x_maxy,y_maxy,nw,w)
			call CRSBND(0.1*vxhi,0.45*vxhi,ylo,yhi)
		else
			call CRSBND(xlo,xhi,ylo,yhi)
		endif
		call CRSSEC(x_min,y_min,x_max,y_max,nw,w)
	endif
888	continue
	if(meta) goto 5555
	if(plot) goto 1812		!return with plot=true?
	after=.false.
858   format(7F10.5)
	i=1
708	write(unit=12,fmt=858) (w(k),k=i,i+6)
	i=i+7
	if(i.le.nw-7) goto 708

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
	if(cross) then
	   if(all) then
	   	text(1)=' 1: ONE VIEW'
	   else
	   	text(1)=' 1: ALL '
	   endif
	else
	   if(inter) then
		text(1) =' 1: ZMIN,ZMAX'
	   else
		text(1) =' 1: RESCALE     '
	   endif
	endif
	text(2)='  2: INTERPOLATE '
	text(3) =' 3: GRAPH SHAPE'
	text(4) =' 4: GRAPH AXES '
	text(5) =' 5: POSH OPTIONS'
	text(6) =' 6: GIVE TITLE '
	text(7) =' 7: PLOT NOW   '
	text(8) =' 8: QUEUE PLOT '
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
	if(cross) then
	   text(15)=' .: 3D PROJECTION'
	else
	   text(15)=' .: CROSS SECTION '
	endif

	do i=1,15
		Lb(i)=1
	enddo
	if(rescale) Lb(2)=0
	if(cross) then
		do i=2,6
			Lb(i)=0
		enddo
		do i=11,14
			Lb(i)=0
		enddo
	endif
	Lb(8)=0
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(15)
	   goto 152		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 152		!another menu choice
	endif

	goto(1,2,3,4,5,6,7,8,9,10,11,11,11,14,15)ikey


c========================================================================
C				RESCALE
c unfortunately this does not work properly.Gino does not have option
c to rescale on y and x.
c Y works, x not!!!!
c========================================================================
1 	CONTINUE
	if(cross) goto 15
	cross=.false.
	if(inter) goto 1111
	RAT=1.0
	rescale=.false.
	all=.false.
c	what about x , y and z??//
	call DCMENU(-1,4,Lb,text,0,0)	!delete box 1 only
	Lb(1)=-1
	call DCMENU(-1,4,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'xmin, xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin,xmax)
	call DEFOLT2r(ymin,ymax,defolt)
	call QDIALOG(1,'ymin, ymax',defolt,ict,cans)
	call GETINP2r(cans,ymin,ymax)
1111	call DEFOLT2r(zmin,zmax,defolt)
	call QDIALOG(1,'Zmin, Zmax',defolt,ict,cans)
	call GETINP2r(cans,zmin,zmax)
	if(xmin.ne.qmin.or.ymin.ne.rmin.or.xmax.ne.qmax.
     &	or.ymax.ne.rmax) then
     		 rescale=.true.
		nbinxl=nbinx-(qmax-xmin)*dnbinx
		nbinxh=nbinx-(qmax-xmax)*dnbinx
		nbinyl=nbiny-(rmax-ymin)*dnbiny
		nbinyh=nbiny-(rmax-ymax)*dnbiny
		if(nbinyl.le.0) nbinyl=1
		if(nbinxl.le.0) nbinxl=1
		if(nbinyh.ge.nbiny) nbinyh=nbiny
		if(nbinxh.ge.nbinx) nbinxh=nbinx
		m=0
		k=0
		do i=nbinxl,nbinxh
		   n=0
		   m=m+1
		   do j=nbinyl,nbinyh
			n=n+1
			res(m,n)=z(i,j)
			fes(m,n)=ifillz(i,j)
			k=k+1
			azr(k)=z(i,j)
			axr(k)=xmid(i)
			ayr(k)=ymid(j)
		   enddo
		enddo
		if(inter) then
		 	numxl= numx-(qmax-xmin)*dnumx
		 	numxh= numx-(qmax-xmax)*dnumx
		 	numyl= numy-(rmax-ymin)*dnumy
		 	numyh= numy-(rmax-ymax)*dnumy
		 	if(numyl.le.0) numyl=1
		 	if(numxl.le.0) numxl=1
		 	if(numyh.ge.numy) numyh=numy
		 	if(numxh.ge.numx) numxh=numx
			numxr=numxh-numxl+1
			numyr=numyh-numyl+1
			np1=nbinxr*nbinyr
      		nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
			if(ifw.eq.1) deallocate(w)
			allocate(w(nw))
			ifw=1
      		CALL RANGRD(NP1,AXR,AYR,AZR,NUMXR,XMIN,XMAX,NUMYR,
     &		YMIN,YMAX,RESI,NW,W)
		endif
		nbinxr=nbinxh-nbinxl+1
		nbinyr=nbinyh-nbinyl+1
c		if(xmin.ne.qmin.or.xmax.ne.qmax) nbinyr=nbinxr
	      rat=(xmax-xmin)*dyd/((ymax-ymin)*dxd)
	endif
805	format(2I5)
	write(unit=12,fmt=805) nbinxr,nbinyr
      j=1
8021	i=1
8031	write(unit=12,fmt=801) j,i,(res(j,k),k=i,i+4)
      i=i+5
      if(i.le.nbinyr) goto 8031
      j=j+1
	if(j.le.nbinxr) goto 8021
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================
c                           INTERPOLATE
c========================================================================
2	continue
	if(rescale) goto 152
	inter=.false.
	call DEFOLT2i(numx,numy,defolt)
	call QDIALOG(1,'Numx,Numy',defolt,ict,cans)
	call GETINP2i(cans,numx,numy)
	if(numx.eq.nbinx.and.numy.eq.nbiny) goto 222
	if(numx.lt.nbinx.or.numy.lt.nbiny) goto 2
	if(ifgz.eq.1) then
	   deALLOCATE(gridz)
	endif
	allocate(gridz(numx,numy))
	ifgz=1
	if(ifr.eq.1) then
	   deALLOCATE(resi)
	endif
	allocate(resi(numx,numy))
	ifr=1
	ifl=1
	if(ifw.eq.1) deallocate(w)
	if(rescale) then
		numxl= numx-(qmax-xmin)*dnumx
		numxh= numx-(qmax-xmax)*dnumx
		numyl= numy-(rmax-ymin)*dnumy
		numyh= numy-(rmax-ymax)*dnumy
		if(numyl.le.0) numyl=1
		if(numxl.le.0) numxl=1
		if(numyh.ge.numy) numyh=numy
		if(numxh.ge.numx) numxh=numx
		numxr=numxh-numxl+1
		numyr=numyh-numyl+1
		np1=nbinxr*nbinyr
      	nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
		allocate(w(nw))
 		CALL RANGRD(NP1,AXR,AYR,AZR,NUMXR,XMIN,XMAX,NUMYR,
     &		YMIN,YMAX,RESI,NW,W)
	else
      	nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
		allocate(w(nw))
      	CALL RANGRD(NP,AX,AY,AZ,NUMX,XLOW,XHIGH,NUMY,YLOW,YHIGH,
     +   	GRIDZ,NW,W)
	endif
	ifw=1
	INTER=.TRUE.
222	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	GOTO 152


c========================================================================
c                           GRAPH SHAPE
c========================================================================
3	continue
250	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
251	continue
	text(1) =' 1: ROTATE    '
	text(2)='  2: ELEVATION'
	text(3) =' 3: X/Y RATIO'
	text(4) =' 4: HEIGHT/BASE'
	text(5) =' 5: X,Y,Z'
	text(6) =' 6: VIEWPORT'
	text(7) =' 7: WINDOW'
	text(8) =' 8: GRID'
	text(9) =' 9: FRAME'
	text(10)='10: REDRAW    '
	text(11)=' +: CROSS SECTION'
	text(12)=' -: SETTINGS'
	DO I=11,15
		Lb(I)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
252	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.12) goto 251
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(16)
	   goto 252		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 252		!another menu choice
	endif

	goto(101,102,103,104,105,106,107,108,109,110,111,112)ikey
c========================================================================
c                           ROTATE
c========================================================================
101	continue
	call DEFOLTr(phi,defolt)
	call QDIALOG(1,'Phi',defolt,ict,cans)
	call GETINPr(cans,phi)
	perf=100
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                           ELEVATION
c========================================================================
102	continue

	call DEFOLTr(theta,defolt)
	call QDIALOG(1,'Theta',defolt,ict,cans)
	call GETINPr(cans,theta)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
C		X/W RATIO
c========================================================================
103 	CONTINUE
	call DEFOLTr(rat,defolt)
	call QDIALOG(1,'X/Y Ratio',defolt,ict,cans)
	call GETINPr(cans,rat)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100

c========================================================================
C		RATIO
c========================================================================
104 	CONTINUE
	call DEFOLTr(ratio,defolt)
	call QDIALOG(1,'HEIGHT/BASE Ratio',defolt,ict,cans)
	call GETINPr(cans,ratio)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
c                           X,Y,Z
c========================================================================
105	continue
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
c=============================================================
c				' zmin,zmax  '
c=============================================================
	call DEFOLT2r(zmin,zmax,defolt)
	call QDIALOG(1,'Zmin, Zmax',defolt,ict,cans)
	call GETINP2r(cans,zmin,zmax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152
c========================================================================
C		VIEWPORT
c========================================================================
106 	CONTINUE
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
107	CONTINUE
	call DEFOLT2r(xwmin,xwmax,defolt)
	call QDIALOG(1,'Xwmin, Xwmax',defolt,ict,cans)
	call GETINP2r(cans,xwmin,xwmax)
	call DEFOLT2r(ywmin,ywmax,defolt)
	call QDIALOG(1,'Ywmin, Ywmax',defolt,ict,cans)
	call GETINP2r(cans,ywmin,ywmax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================
C				  GRID
c========================================================================
108	CONTINUE
	call DEFOLT2i(nxstep,nystep,defolt)
	call QDIALOG(1,'nxstep, nystep',defolt,ict,cans)
	call GETINP2i(cans,nxstep,nystep)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100

c========================================================================
c                          FRAME
c========================================================================
109	continue
	call DEFOLTI(IFRAM,defolt)
	call QDIALOG(1,'Frame',defolt,ict,cans)
	call GETINPi(cans,ifram)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
C				  REDRAW
c========================================================================
110	CONTINUE
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100

c========================================================================
C				  CROSS SECTION
c========================================================================
15	continue
111	continue
	if(cross) then
	   if(ikey.eq.15) then
		cross=.false.
		goto 987
	   else
		if(all) then
		   all=.false.
		else
		   all=.true.
		endif
	   endif
	else
	   all=.false.
	   cross=.true.
	endif
	x_min=xmin
	x_max=xmax
	y_min=ymid(nbiny/2)
	y_max=y_min
	if(all) then
		x_miny=xmid(nbinx/2)
		x_maxy=x_miny
		y_miny=ymin
		y_maxy=ymax
		goto 987
	endif
88	call DEFOLT2r(x_min,y_min,defolt)
	call QDIALOG(1,'Draw from x_min, y_min',defolt,ict,cans)
	call GETINP2r(cans,x_min,y_min)
	call DEFOLT2r(x_max,y_max,defolt)
	call QDIALOG(1,' -to x_max, y_max',defolt,ict,cans)
	call GETINP2r(cans,x_max,y_max)
987	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
C				  SETTINGS
c========================================================================
112	continue
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'xmin, xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin,xmax)
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'ymin,ymax ',defolt,ict,cans)
	call GETINP2r(cans,ymin,ymax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
c                           GRAPH AXES
c========================================================================
4	continue
450	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
451	continue

	if(axes) then
	   text(1)='1: NO AXES'
	else
	   text(1)='1: AXES  '
	endif
	text(2)='2: DRAWING STYLE'
	text(3)='3: DATA SUPRESION'
	text(4)='4: ANNOTATION'
	text(5)='5: TICS NUMBER'
	text(6)='6: TICS SIZE'
	text(10)='10: REDRAW'
	DO I=7,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
452	call CKEY(ch,lkey)
	if(lkey.lt.-2.or.ikey.gt.10) goto 450
	if(lkey.eq.-1) then	!F1 key
	   call VHELP(17)
	   goto 452		!another menu choice
	else if(lkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 452		!another menu choice
	endif
	if(lkey.eq.1) then
		IF(axes) THEN
	   		axes=.false.
		ELSE
	   		axes=.true.
		ENDIF
	endif
	if(lkey.eq.2) then
		call DEFOLT2i(istyle(1),istyle(2),defolt)
		call QDIALOG(1,'Style Axis x1, y1',defolt,ict,cans)
		call Wdialog(1,'0:nothing ;1:axis; 2:grid; 3:axis+grid',ict)
		call GETINP2i(cans,istyle(1),istyle(2))
		call DEFOLT2i(istyle(3),istyle(4),defolt)
		call QDIALOG(1,'Style x2, y2',defolt,ict,cans)
		call GETINP2i(cans,istyle(3),istyle(4))
		call DEFOLT2i(istyle(5),istyle(6),defolt)
		call QDIALOG(1,'Style Axis x3, y3',defolt,ict,cans)
		call GETINP2i(cans,istyle(5),istyle(6))
	endif
	if(lkey.eq.3) then
		call DEFOLT2i(isup(1),isup(2),defolt)
		call QDIALOG(1,'supresion Axis x1, y1',defolt,ict,cans)
		call Wdialog(1,'0:all ;1:no lowest; 2:no highest; 3:none',ict)
		call GETINP2i(cans,isup(1),isup(2))
		call DEFOLT2i(isup(3),isup(4),defolt)
		call QDIALOG(1,'supresion  x2, y2',defolt,ict,cans)
		call GETINP2i(cans,isup(3),isup(4))
		call DEFOLT2i(isup(5),isup(6),defolt)
		call QDIALOG(1,'supresion  Axis x3, y3',defolt,ict,cans)
		call GETINP2i(cans,isup(5),isup(6))
	endif
	if(lkey.eq.4) then
		call DEFOLTi(numaxi,defolt)
		call QDIALOG(1,'annotation for axis',defolt,ict,cans)
		call GETINPi(cans,numaxi)
		call DEFOLTi(nwidth,defolt)
		call QDIALOG(1,'field width of the number',defolt,ict,cans)
		call GETINPi(cans,nwidth)
		call DEFOLTi(nplace,defolt)
		call QDIALOG(1,'decimal places',defolt,ict,cans)
		call GETINPi(cans,nplace)
		call DEFOLTi(npow,defolt)
		call QDIALOG(1,'power',defolt,ict,cans)
		call GETINPi(cans,npow)
	endif
	if(lkey.eq.5) then
		call DEFOLTR(XTIC,defolt)
		call QDIALOG(1,'X TIC',defolt,ict,cans)
		call GETINPR(cans,XTIC)
		call DEFOLTR(YTIC,defolt)
		call QDIALOG(1,'Y TIC',defolt,ict,cans)
		call GETINPR(cans,YTIC)
		call DEFOLTR(ZTIC  ,defolt)
		call QDIALOG(1,'Z TIC',defolt,ict,cans)
		call GETINPR(cans,ZTIC)
	endif
	if(lkey.eq.6) then
		call DEFOLTR(CSIZE,defolt)
		call QDIALOG(1,'SIZE ',defolt,ict,cans)
		call GETINPR(cans,CSIZE)
	endif
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c     goto 152

c========================================================================
c                           POSH OPTION
c========================================================================
5	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
351	continue
	text(1) =' 1 : FIX TEXT'
	text(10) ='10: REDRAW'
	DO I=2,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
352	call CKEY(ch,jkey)
	if(jkey.eq.10) then
	      call FILLWIN(0,55,639,387,icol(71))		!graph area
		goto 100
	endif
	if(jkey.lt.-2.or.jkey.gt.10) goto 351
	if(jkey.eq.-1) then	!F1 key
	   call VHELP(10)
	   goto 352		!another menu choice
	else if(jkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 352		!another menu choice
	endif
c========================================================================
C		FIX TEXT
c========================================================================
601 	CONTINUE
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
661	continue
	text(1) =' 1: TITLE'
	text(2) =' 2: XLABEL'
	text(3) =' 3: YLABEL'
	text(4) =' 4: ZLABEL'
	text(10) ='10: REDRAW'
	DO I=5,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
662	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.10) goto 661
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(10)
	   goto 662		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 662		!another menu choice
	endif
	if(ikey.eq.10) goto 5
	if(ikey.eq.1) then
		str   =title1
		angle	=alfa
		size	=sizet
		icolor=icol(25)
		ifont	=ifnt
		xm	=xst
		ym	=yst
		ijus	=ijust
		idraw	=idrawt
	else if(ikey.eq.2) then
		str   =xtitle
		angle	=beta
		size	=sizel
		icolor=icol(23)
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
	      call linwid(1.0)
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
	      call linwid(1.0)
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
	      call linwid(1.0)
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
	      call linwid(1.0)
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
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
	      angle=angle+ang
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.45) then
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
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
	perf=-100
   	GOTO 601

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
     &   ijust,icol(25),icol(71),75,blank,nrealt)
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
	after=.true.
	goto 1002

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
	   	call COPY(oldfile,'lpt1')
	   else if(ihelp.eq.2 ) then
	   	printfil='lfit'//kpchar(1:nc1)//'.out'
	   	call system('copy/b hpljr.out lpt1:')
	   else if(ihelp.eq.3) then
	   	call system('copy/b dj500c.out lpt1:')
	   	printfil='cfit'//kpchar(1:nc1)//'.out'
	   endif
	endif

	call VGA
	call errswi(-1)
	call brkswi(1)
	call gsetcols(0)
	call mode(18)
1814	continue
	idev=0
	plot=.false.
c 	Restore idev=0 (and scalfac, if altered)
c	scalfac=1.0
	call setfnt(ifont)
	vxlo=vxlo1		! for VIEWPORT
	vxhi=vxhi1
	vylo=vylo1
	vyhi=vyhi1
1043	continue
	after=.true.
	goto 1002


c======================================================================
c				queue plot
c========================================================================
8  	goto 152

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
     &   	ijusx,icol(23),icol(71),75,blank,nrealt)
		if(idrawx.eq.0) idrawx=1
	else if(ikey.eq.12) then
		if(idrawy.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspy,yspy,ytitle,gama,
     &	 sizel,ijusy)
		endif
		call GSTRING(xspy,yspy,ytitle,ifnl,gama,sizel,
     &   	ijusy,icol(23),icol(71),75,blank,nrealt)
		if(idrawy.eq.0) idrawy=1
	else if(ikey.eq.13) then
		if(idrawz.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	 sizel,ijusz)
		endif
		call GSTRING(xspz,yspz,ztitle,ifnl,delta,sizel,
     &   	ijusz,icol(23),icol(71),75,blank,nrealt)
		if(idrawz.eq.0) idrawz=1
	endif
c==	ilabel=1		!so internal default labels not used
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

c                           FILL
c========================================================================
C15	continue
C 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
C	   goto 100
c	GOTO 152


c========================================================================

c TIDY UP AND EXIT
999	continue
c	if(iabs(iask).eq.1) then
c	   call BELL(1)
c	   ans='Y'
c	   call DEFOLTa(ans,defolt)
c	   call QDIALOG(1,'ARE YOU SURE',defolt,ict,cans)
c	   call GETINPa(cans,ans)
c	   if(UC(ans).eq.'N') goto 151
c	endif
	iask=-1
	if(iask.gt.0) call DCMENU(0,5,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	deALLOCATE(ax,ay,az,ifillz)
	if(ifg.eq.1) deALLOCATE(ifillg)
	if(ifgz.eq.1) deALLOCATE(gridz)
	vxlo=0.	! for VIEWPORT
	vxhi=xp
	vylo=0.
	vyhi=yp

	CLOSE(unit=12)
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
