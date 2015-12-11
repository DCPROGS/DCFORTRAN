subroutine drawline(x1,y1,x2,y2,icol,idev)
use gino_f90
use menu_f90



call glincols(icol,idev)
call gmoveto2d(x1,y1)

call gdrawlineto2d(x2,y2)
call gFlushGraphics() 
end




