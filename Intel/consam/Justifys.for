	subroutine JUSTIFYSTRING(xc,yc,str,ang,size,ijust)
c=========================================================================
c	simulates HGRAPH routine
c========================================================================
	logical graph
	integer ijust
	common/cars/dxsa,dysa,graph

	character*(*) str
	chw=size*dxsa
	chh=size*dysa
	if(ang.eq.90.) then
	   chw=size*dysa
	   chh=size*dxsa
	endif
 	call CHABEG(xc,yc)
 	call CHAANG(ang)
  	call CHASIZ(chw,chh)
 	call CHAJUS(ijust)
	nl=nblank1(str)
c140202	nl=nblank(str)
	if(nl.eq.0) goto 1
	k=ichar(str(nl:nl))
	if(k.eq.0) nl=nl-1
	call CHASTR(str(1:nl))
1	continue
	end
