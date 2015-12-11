subroutine draw_par_mechanism(xinit,yinit,dx,dy)
use menu_f90
use gino_f90
type (gpoint) :: pts(4)

pts(1)%x=xinit
pts(1)%y=yinit
pts(2)%x=xinit+dx
pts(2)%y=yinit
pts(3)%x=xinit+dx
pts(3)%y=yinit-dy
pts(4)%x=xinit
pts(4)%y=yinit-dy
call LinCol(15)
call gMoveTo2D(pts(1)%x,pts(1)%y)
call gFillPolygonTo2D(gsolid,15,GAREA,4,pts)
call gFlushGraphics()

end