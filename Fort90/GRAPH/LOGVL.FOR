	subroutine logvl(xin,xv,xmin,xmax,log,off,bad,sqrty)
	logical log,sqrty,off,bad
c	integer*2 lrow
c
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
c NB should be OK with XIN as array element
	xv=xin
	bad=(log.and.xv.lt.1.e-37).or.(sqrty.and.xv.lt.0.)
	if(bad) then
	   call OPENDIALOG(2,12,.true.)	!draw dialog box #2
	   call WDIALOG(2,'Bad log/sqrt!',12)
         RETURN
	endif
c	if(.not.bad) goto 1
c	call bell(1)
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 21,xv
c21	format(' Bad log/sqrt in logvl= ',g13.6)
c	RETURN
c
	if(log) xv=alog10(xv)
	off=(xv.lt.xmin.or.xv.gt.xmax)	!off scale
c NB min, max are NOT sqrt values for square root scale
	if(sqrty) xv=sqrt(xv)
	if(.not.off) RETURN
	if(xv.lt.xmin) xv=xmin
	if(xv.gt.xmax) xv=xmax
	RETURN
	end


