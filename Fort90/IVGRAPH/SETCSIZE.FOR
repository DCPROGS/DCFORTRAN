	subroutine SETCSIZE(csize,isize)
c Lahey/Hgraph subroutine to set character size
c Input=csize (0.5-18.0)
c Output=isize(1-9)
	if((csize.ge.1.0).and.(csize.le.9)) then
 	   isize=ifixr(csize)
	   cfac=csize/float(isize)
	else if(csize.lt.1.0) then
	   isize=1
	   cfac=csize
	else if(csize.gt.9.) then
	   isize=9
	   cfac=csize/float(isize)
	endif
c in case:
	if(isize.lt.1) isize=1
	if(isize.gt.9) isize=9
	if(cfac.lt.0.5) csize=0.5
	if(cfac.gt.2.0) csize=2.0
c
	call setcharaspect(cfac,cfac)	!width, height
c	print 1,isize,cfac
c1	format(' isize,cfac= ',i5,f9.3)
	RETURN
	end



