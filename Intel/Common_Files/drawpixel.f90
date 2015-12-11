subroutine drawpixelarray(pixbuf,ix, iy, npixx, npixy, isx, isy, idx, idy)
use gino_f90
use menu_f90

integer pixbuf(800,600)

call gdrawPixelArea(ix, iy, npixx, npixy, isx, isy, idx, idy, pixbuf)
call gFlushGraphics() 
end