      SUBROUTINE FBOX(IX,IY,STR,ANGLE,CHARWIDTH,CHARHEIGHT,IJUS,
     &ixbox,iybox,imode)
c FBOX is subroutine based on Hgraph example FIG72.FOR that returns in
c IXBOX,IYBOX the coordinates of the corners of the box
c Called by TEXTBOX and by HGSTRING
	integer ixbox(4),iybox(4)
      INTEGER IX, IY, IXB, IYB
      INTEGER XJUS, YJUS, CHARWIDTH, CHARHEIGHT
      CHARACTER*(*) STR
      INTEGER XMIN, YMIN, XLEN, YLEN, ANGLE, SIZE
      REAL C, S, PI
      COMMON /HHHTTT/ C, S
c	logical debug,caplock
      INCLUDE 'CONSTS.FOR'
      DATA PI /3.141592654/
c
c	debug()=caplock()
c
c	nl=NBLANK1(str)
c      CALL BOXSTRING(str, size, xmin, ymin, xlen, ylen)
c###      CALL JUSTIFYSTRING(ix, iy, str, angle, size, xjus, yjus)
      C = COS(PI * ANGLE / 180.0)
      S = SIN(PI * ANGLE / 180.0)
	XLEN = CHARWIDTH*nl
	YLEN = CHARHEIGHT
      IXB = -CHARWIDTH/ 2
      IYB = -CHARHEIGHT/ 2
      IF (IJUS .EQ. LEFT) THEN
        IXB = IXB + XMIN
        IYB = IYB + YMIN
      ELSEIF (IJUS .EQ. CENTER) THEN
        IXB = IXB - XLEN / 2
        IYB = IYB - (YLEN / 2)
      ELSEIF (IJUS .EQ. RIGHT) THEN
        IYB = IYB - YLEN
        IXB = IXB - XLEN
      ENDIF
      XLEN = XLEN + CHARWIDTH
      YLEN = YLEN + CHARHEIGHT
      CALL DRAWSIDE(IX,IY,IXB,IYB,ixbox(1),iybox(1),0,imode)
c      if(debug())CALL DRAWSIDE(IX,IY,IXB,IYB,ixbox(1),iybox(1),1,imode)
c	if(debug()) pause
      CALL DRAWSIDE(IX,IY,IXB+XLEN,IYB,ixbox(2),iybox(2),1,imode)
c	if(debug()) pause
      CALL DRAWSIDE(IX,IY,IXB+XLEN,IYB+YLEN,ixbox(3),iybox(3),1,imode)
c	if(debug()) pause
      CALL DRAWSIDE(IX,IY,IXB,IYB+YLEN,ixbox(4),iybox(4),1,imode)
c	if(debug()) pause
	if(imode.eq.0) RETURN
      CALL DRAWSIDE(IX,IY,IXB,IYB,i,j,1,imode)
	RETURN
      END


      SUBROUTINE DRAWSIDE(IX, IY, IXB, IYB, ix0,iy0,iud,imode)
      INTEGER IX, IY, IXB, IYB, IUD, IX0, IY0
      REAL C, S
      COMMON /HHHTTT/ C, S
c
      IX0 = IX + IXB * C - S * IYB + 0.5
      IY0 = IY + IXB * S + C * IYB + 0.5
c	print 1,ix0,iy0,imode,iud
c1	format(' ix0,iy0,imode,iud= ',4i8)
	if(imode.eq.0) RETURN
	if(iud.eq.0) then
         CALL MOVTO2(ix0, iy0)
	else
         CALL LINTO2(ix0, iy0)
	endif
C      CALL PLOT(ix0, iy0, iud)
	RETURN
      END

