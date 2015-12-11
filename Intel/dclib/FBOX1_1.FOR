c======================================================================
      SUBROUTINE FBOX1_1(X,Y,STR,ANGLE,SIZE,IJUS,xbox,ybox,imode)
c======================================================================
c FBOX is subroutine based on Hgraph example FIG72.FOR that returns in
c IXBOX,IYBOX the coordinates of the corners of the box
c Called by TEXTBOX and by HGSTRING

	real xbox(4),ybox(4)
      CHARACTER*(*) STR
	character*100 newstr
	LOGICAL logx,logy,logity,sqrty,gino50
      REAL C, S, PI
      COMMON /HHHTTT/ C, S
	common/cars/dxsa,dysa,graph
	common/limit/wxmin,wxmax,wymin,wymax,gino50
      DATA PI /3.141592654/

c	xt=x-wxmin
c     yt=y-wymin
	xt=x
	yt=y
	left=-1
	centre=0
	right=1
	nl=NBLANK1(str)
	nrl=1
	m=1
	nstr0=1
	ya=yt
	do i=1,nl-1
	   j=m
	   k=i
	   if(str(i:i+1).eq.'*N') then
		nrl=nrl+1
		newstr=str(j:k-1)
		call realength(newstr,nstr)
		if(nstr.gt.nstr0) nstr0=nstr
		m=i+2
	   endif
	enddo
	if(size.eq.0.) size=2.5
	w=size*dxsa
	h=size*dysa
	if(angle.eq.90.) then
	   w=size*dysa
	   h=size*dxsa
	endif
      C = COS(PI * ANGLE / 180.0)
      S = SIN(PI * ANGLE / 180.0)
	call realength(str,nrw)
	XLEN = w*nrw
	if (nl.gt.4.and.str(1:4).eq.'10*E') xlen=xlen-2*w
	YLEN = h
	if(nrl.gt.1) then
		xlen=w*(nstr0+1)
		ylen=2*h*nrl
		ya=yt-ylen+h
	endif
      XB = -w / 2
      YB = -h / 2
      IF (IJUS .EQ. 0) THEN
        XB = XB - XLEN / 2
      ELSEIF (IJUS .EQ. 1) THEN
        XB = XB - XLEN
      ENDIF

      XLEN = XLEN + w
      YLEN = YLEN + h

      CALL iDRAWSIDE(Xt,Ya,XB,YB,xbox(1),ybox(1),0,imode)
      CALL iDRAWSIDE(Xt,Ya,XB+XLEN,YB,xbox(2),ybox(2),1,imode)
      CALL iDRAWSIDE(Xt,Ya,XB+XLEN,YB+YLEN, xbox(3), ybox(3),1,imode)
      CALL iDRAWSIDE(Xt,Ya,XB,YB+YLEN, xbox(4), ybox(4),1,imode)
	if(imode.eq.0) RETURN
      CALL iDRAWSIDE( Xt, Ya, XB, YB,r,p,1,imode)
	RETURN
      END


