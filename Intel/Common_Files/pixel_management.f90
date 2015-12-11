subroutine drawpixelarray(x1,y1,x2,y2,pixbuf,nx,ny)
use gino_f90
use menu_f90
integer pixbuf(nx,ny)

type (GPIXEL) :: pix1,pix2
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax

		x11=x1-wxmin
	  y11=y1-wymin
		x21=x2-wxmin
	  y21=y2-wymin
call gEnqPixelPos(x11,y11, pix1)
call gEnqPixelPos(x21,y21, pix2)
if(pix1%ix.le.pix2%ix) then
	if(pix1%iy.le.pix2%iy) then
	ix=pix1%ix
	iy=pix1%iy
	else
	ix=pix1%ix
	iy=pix2%iy
	endif
else
	if(pix1%iy.le.pix2%iy) then
	ix=pix2%ix
	iy=pix2%iy
	else
	ix=pix2%ix
	iy=pix1%iy
	endif
endif
isx=1
isy=1
idx=abs(pix1%ix-pix2%ix)
idy=abs(pix1%iy-pix2%iy)
if(idx.le.0) idx=1
if(idy.le.0) then 
    idy=3
    iy=iy-1
endif
npixx=idx
npixy=idy

call gdrawPixelArea(ix, iy, npixx, npixy, isx, isy, idx, idy, pixbuf)
!call gFlushGraphics() 
end


subroutine getpixelarray(x1,y1,x2,y2,pixbuf,nx,ny)
use gino_f90
use menu_f90
integer pixbuf(nx,ny)

type (GPIXEL) :: pix1,pix2


common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax


x11=x1-wxmin
	  y11=y1-wymin
		x21=x2-wxmin
	  y21=y2-wymin
call gEnqPixelPos(x11,y11, pix1)
call gEnqPixelPos(x21,y21, pix2)
if(pix1%ix.le.pix2%ix) then
	if(pix1%iy.le.pix2%iy) then
	ix=pix1%ix
	iy=pix1%iy
	else
	ix=pix1%ix
	iy=pix2%iy
	endif
else
	if(pix1%iy.le.pix2%iy) then
	ix=pix2%ix
	iy=pix2%iy
	else
	ix=pix2%ix
	iy=pix1%iy
	endif
endif
isx=1
isy=1
idx=abs(pix1%ix-pix2%ix)
idy=abs(pix1%iy-pix2%iy)
if(idx.le.0) idx=1
if(idy.le.0) then 
    idy=3
    iy=iy-1
endif
!if(idy.le.0) idy=1
npixx=idx
npixy=idy

call ggetPixelArea(ix, iy, npixx, npixy, isx, isy, idx, idy, pixbuf)
continue
end


subroutine drawpixel(x1,y1,ipix)
use gino_f90
use menu_f90


type (GPIXEL) :: pix1
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax
x11=x1-wxmin
	  y11=y1-wymin
	
call gEnqPixelPos(x11,y11, pix1)
	ix=pix1%ix
	iy=pix1%iy

call gdrawpixel(ix,iy,ipix)
call gFlushGraphics() 
end


subroutine getpixel(x1,y1,ipix)
use gino_f90
use menu_f90

type (GPIXEL) :: pix1
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax
x11=x1-wxmin
	  y11=y1-wymin
	
call gEnqPixelPos(x11,y11, pix1)
	ix=pix1%ix
	iy=pix1%iy
call ggetpixel(ix,iy,ipix)
end