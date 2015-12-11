subroutine string_box(str,x,y,angle,ijus,textsize,icol,imode,xbox,ybox,&
dxs,dys)

use gino_f90
use menu_f90
!integer pixbufv(40,800)
!integer pixbufh(800,40)
real xbox(4),ybox(4)
CHARACTER*(*) STR
character*100 newstr
REAL C, S, PI
COMMON /HHHTTT/ C, S
DATA PI /3.141592654/
logical bold,italik,underline
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax

w=textsize*dxs
h=textsize*dys
if(angle.eq.90.) then
	w=textsize*dys
	h=textsize*dxs

endif


xt=x
yt=y
nl=len_trim(str)
nrl=1
m=1
nstr0=1
ya=yt
nf=0
call realength(str,nstr)
do i=1,nl-1
	   j=m
	   k=i
	   if(str(i:i+1).eq.'£N') then
		nrl=nrl+1
		newstr=str(j:k-1)
        call realength(newstr,nstr)
		nc=nstr
		do ns=1,nc-1
		if(newstr(ns:ns+1).eq.'£F') then
		  nstr=nstr-4
		endif
		enddo
		if(nstr.gt.nstr0) nstr0=nstr
		m=i+2
	   else
	     
	   endif
enddo
if(nrl.le.1) then
do i=1,nl-1
if(str(i:i+1).eq.'£F') then
		  nstr=nstr-4
		  nstr0=nstr
		  nrl=nrl+1
endif
enddo
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
!	call gSetLineColour(icol)
	call glincols(icol,idev)
	if(imode.eq.-1) call glincols(0,idev)

    CALL DRAWSIDE(Xt,Ya,XB,YB,xbox(1),ybox(1),0,imode)
    
	CALL DRAWSIDE(Xt,Ya,XB+XLEN,YB,xbox(2),ybox(2),1,0)
	CALL DRAWSIDE(Xt,Ya,XB+XLEN,YB+YLEN, xbox(3), ybox(3),1,0)
	CALL DRAWSIDE(Xt,Ya,XB,YB+YLEN, xbox(4), ybox(4),1,0)
	if(angle.eq.0.) then
!	call getpixelarray(xbox(1),ybox(1),xbox(3),ybox(3),pixbufh,800,40)
	else
!call getpixelarray(xbox(1),ybox(1),xbox(3),ybox(3),pixbufv,40,800)
	endif
	CALL DRAWSIDE(Xt,Ya,XB+XLEN,YB,xbox(2),ybox(2),1,imode)
	if(imode.eq.2) call glincols(0,idev)
   
	
	CALL DRAWSIDE(Xt,Ya,XB+XLEN,YB+YLEN, xbox(3), ybox(3),1,imode)
   	
	CALL DRAWSIDE(Xt,Ya,XB,YB+YLEN, xbox(4), ybox(4),1,imode)
	
	if(imode.eq.0) RETURN
 

    CALL DRAWSIDE( Xt, Ya, XB, YB,r,p,1,imode)
	
    call gFlushGraphics()   
	RETURN  
	END

SUBROUTINE DRAWSIDE( X,  Y,  XB,  YB,  x0, y0,iud,imode)

use gino_f90
use menu_f90
      REAL C, S
      COMMON /HHHTTT/ C, S

       x0 =  X +  XB * C - S *  YB
       Y0 =  Y +  XB * S + C *  YB
	if(imode.eq.0) RETURN

	if(iud.eq.0) then
         CALL gmoveto2d( x0,  y0)
	else
         CALL gdrawLINeto2d( x0,  y0)
	endif
	RETURN
      END



