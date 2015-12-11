      SUBROUTINE FBOX2(IX,IY,STR,ANGLE,SIZE,XJUS,YJUS,ixbox,iybox,imode)
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
	include 'g:\fortran\graph\consts.for'
c      INCLUDE 'CONSTS.FOR'
      DATA PI /3.141592654/
c
c	debug()=caplock()
c
	nl=NBLANK1(str)
      CALL BOXSTRING(str(1:nl), size, xmin, ymin, xlen, ylen)
      C = COS(PI * ANGLE / 180.0)
      S = SIN(PI * ANGLE / 180.0)
      IXB = -CHARWIDTH(size) / 2
      IF (XJUS .EQ. BASE) THEN
        IXB = IXB + XMIN
      ELSEIF (XJUS .EQ. CENTER) THEN
        IXB = IXB - XLEN / 2
      ELSEIF (XJUS .EQ. RIGHT) THEN
        IXB = IXB - XLEN
      ENDIF
      IYB = -CHARHEIGHT(size) / 2
      IF (YJUS .EQ. BASE) THEN
        IYB = IYB + YMIN
      ELSEIF (YJUS .EQ. CENTER) THEN
        IYB = IYB - (YLEN / 2)
      ELSEIF (YJUS .EQ. ABOVE) THEN
        IYB = IYB - YLEN
      ENDIF
      XLEN = XLEN + CHARWIDTH(size)
      YLEN = YLEN + CHARHEIGHT(size)
      CALL iDRAWSIDE(IX,IY,IXB,IYB,ixbox(1),iybox(1),0,imode)
      CALL iDRAWSIDE(IX,IY,IXB+XLEN,IYB,ixbox(2),iybox(2),1,imode)
      CALL iDRAWSIDE(IX,IY,IXB+XLEN,IYB+YLEN,ixbox(3),iybox(3),1,imode)
      CALL iDRAWSIDE(IX,IY,IXB,IYB+YLEN,ixbox(4),iybox(4),1,imode)
	if(imode.eq.0) RETURN
      CALL iDRAWSIDE(IX,IY,IXB,IYB,i,j,1,imode)
	RETURN
      END


      SUBROUTINE iDRAWSIDE(IX, IY, IXB, IYB, ix0,iy0,iud,imode)
      INTEGER IX, IY, IXB, IYB, IUD, IX0, IY0
      REAL C, S
      COMMON /HHHTTT/ C, S
c
      IX0 = IX + IXB * C - S * IYB + 0.5
      IY0 = IY + IXB * S + C * IYB + 0.5
c	print 1,ix0,iy0,imode,iud
c1	format(' ix0,iy0,imode,iud= ',4i8)
	if(imode.eq.0) RETURN
      CALL PLOT(ix0, iy0, iud)
	RETURN
      END

