	subroutine JUSTIFYSTRING(xc,yc,str,ang,sizet,ijust)
c=========================================================================
c	simulates HGRAPH routine
c========================================================================
	logical graph
	integer ijust
	common/cars/dxsa,dysa,graph

	character*(*) str
	chw=sizet*dxsa
	chh=sizet*dysa
	if(ang.eq.90.) then
	   chw=sizet*dysa
	   chh=sizet*dxsa
	endif
 	call CHABEG(xc,yc)
 	call CHAANG(ang)
  	call CHASIZ(chw,chh)
 	call CHAJUS(ijust)
	nl=len_trim(str)
c140202	nl=nblank(str)
	if(nl.eq.0) goto 1
	k=ichar(str(nl:nl))
	if(k.eq.0) nl=nl-1
	call CHASTR(str(1:nl))
1	continue
	end
