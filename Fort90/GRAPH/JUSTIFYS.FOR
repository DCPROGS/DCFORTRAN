	subroutine JUSTIFYSTRING(xc,yc,str,ang,size,ijus)
c=========================================================================
c	simulates HGRAPH routine
c=========================================================================

	character*(*) str
	call getsize(size,w,h)
c	chw=w-ang*(w-h)/90.
c	chh=h+ang*(w-h)/90.
	chw=w
	chh=h
 	call CHABEG(xc,yc)
 	call CHAANG(ang)
  	call CHASIZ(chw,chh)
 	call CHAJUS(ijus)
	nl=nblank1(str)
	k=ichar(str(nl:nl))
	if(k.eq.0) nl=nl-1
	call CHASTR(str(1:nl))
1	continue
	end
