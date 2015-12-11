	program TCUR
	integer movcur,ix,iy,key
	EXTERNAL DRWBOX
c
c	call INIPLT(0,.false.,1.0)
	call INIPLT(0,0,1.0)
	call VIEWPORT(0,10000,0,7000)
 	call GRAPHBOUNDARY(0,10000,0,7000)
	call PUTCUR(1000,1000)
	key=MOVCUR(ix,iy,1000,1000,DRWBOX,1000,9000,1000,6000,0)
	call ENDPLT
	end

	integer function DRWBOX(ix1,iy1)
c	subroutine DRWBOX(ix1,iy1)
	integer ix1,iy1,jx1,jx2,jy1,jy2
	include 'hgraph.cmn'
c ixx,iyy in CRESOL is current cursor pos in VDC
	ixx=ixxg
	iyy=iyyg
	call VDC2DSC(ix1,iy1,jx1,jy1)
	call VDC2DSC(ixx,iyy,jx2,jy2)
	call XPLOTTT(jx1,jy1,jx1,jy2)
	call XPLOTTT(jx1,jy2,jx2,jy2)
	call XPLOTTT(jx2,jy2,jx2,jy1)
	call XPLOTTT(jx2,jy1,jx1,jy1)
	RETURN
	end
