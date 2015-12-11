subroutine draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
		XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)

use gino_f90
DIMENSION ICOL(250),ITYPE(250),THICK(250),IDRAW(250)
DIMENSION XBEG(50),YBEG(50),XEND(50),YEND(50),C_THICK(250)
LOGICAL logx,logy,logity,sqrty,landscape
logical bold,italik,underline
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/logval/logx,logy,sqrty,logity
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
xlo=xmin1
xhi=xmax1
ylo=ymin1
yhi=ymax1

sx=0.7*dxs                                                     
sy=0.7*dys
if(nline.gt.0) then
do i=1,nline
	xb=xbeg(i+10)
	xe=xend(i+10)
	yb=ybeg(i+10)
	ye=yend(i+10)
	call clipping(xlo,ylo,xhi,yhi,xb,yb,xe,ye,iflag)
	if(iflag.ne.0) then
	if(logx) then
		xb=alog10(xbeg(i+10))
		xe=alog10(xend(i+10))
	endif
	if(logy) then
		yb=alog10(ybeg(i+10))
		ye=alog10(yend(i+10))
	endif
	if(sqrty) then
		yb=sqrt(ybeg(i+10))
		ye=sqrt(yend(i+10))
	endif
	if(logity) then
		yb=(yb-y0)/(yinf-yb)	! Hill scale
		ye=(ye-y0)/(yinf-ye)	! Hill scale
		if(yb.gt.x0) then
			yb=alog10(yb)
		endif
		if(ye.gt.x0) then
			ye=alog10(ye)
		endif
    endif
	call glincols(210+i,idev)
	call movto2(xb,yb)
	call broken(itype(210+i))
	call linto2(xe,ye)
	endif
enddo
endif
if(nhline.gt.0) then
do i=1,nhline
	xb=xbeg(i+20)
	xe=xend(i+20)
	yb=ybeg(i+20)
	ye=yend(i+20)
	call clipping(xlo,ylo,xhi,yhi,xb,yb,xe,ye,iflag)
	if(iflag.ne.0) then
	if(logx) then
		xb=alog10(xbeg(i+20))
		xe=alog10(xend(i+20))
	endif
	if(logy) then
		yb=alog10(ybeg(i+20))
		ye=alog10(yend(i+20))
	endif
	if(sqrty) then
		yb=sqrt(ybeg(i+20))
		ye=sqrt(yend(i+20))
	endif
	call glincols(220+i,idev)
	call movto2(xb,yb)
		call broken(itype(220+i))
	call linto2(xe,ye)
	endif
enddo
endif
if(nvline.gt.0) then
do i=1,nvline
	xb=xbeg(i+30)
	xe=xend(i+30)
	yb=ybeg(i+30)
	ye=yend(i+30)
	call clipping(xlo,ylo,xhi,yhi,xb,yb,xe,ye,iflag)
	if(iflag.ne.0) then
	if(logx) then
		xb=alog10(xbeg(i+30))
		xe=alog10(xend(i+30))
	endif
	if(logy) then
		yb=alog10(ybeg(i+30))
		ye=alog10(yend(i+30))
	endif
	if(sqrty) then
		yb=sqrt(ybeg(i+30))
		ye=sqrt(yend(i+30))
	endif
	call glincols(230+i,idev)
		call broken(itype(230+i))
	call movto2(xb,yb)
	call linto2(xe,ye)
	endif
enddo
endif
alpha=10.
if(narrow.gt.0) then
do i=1,narrow
	xb=xbeg(i)
	xe=xend(i)
	yb=ybeg(i)
	ye=yend(i)
	call clipping(xlo,ylo,xhi,yhi,xb,yb,xe,ye,iflag)
	if(iflag.ne.0) then
	if(logx) then
		xb=alog10(xbeg(i))
		xe=alog10(xend(i))
	endif
	if(logy) then
		yb=alog10(ybeg(i))
		ye=alog10(yend(i))
	endif
	if(sqrty) then
		yb=sqrt(ybeg(i))
		ye=sqrt(yend(i))
	endif
	thick(200+i)=0.
	!call gmoveto2d(xb,yb)
	!call gdrawarrow2d(xe,ye,0)
	call ARROW1(xb,yb,xe,ye,alpha,sx,sy,itype(i+200),&
     	    thick(200+i),icol(200+i),idev)
    endif
enddo
endif
call gFlushGraphics() 

end