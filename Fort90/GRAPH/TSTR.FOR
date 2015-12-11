	program tstr
c To test DC's modif to ILVFONT, renamed HGSTRING
	character*60 text
	integer fndcur
	real RX(100),RY(100)
	character ch*1
	logical mouse_on
	common/mousval/mouse_on,nbutton
	include 'g:\fortran\graph\consts.for'
c
	nbutton=2
	mouse_on=.true.
	ndim=60
	call sethg()
	call INIPLT(0,.false.,1.0)
	call VIEWPORT(0,10000,0,7000)
c	call VIEWPORT(1000,10000,1000,7000)
c	call graphboundary(0,10000,0,7500)
	call graphboundary(3000,9000,2000,6500)
	call scale(0.,639.,0.,479.)
	ibk=3
	call clears(0,480,ibk)
c
c	for text:
c	ixjust=base
c	iyjust=base
c
	ixjust=center	!as for x axis label
	iyjust=above	!=1
	iangle=0
c
	ixjust=center	!as for y axis label
	iyjust=center
	iangle=90
c
	icolor=4
	k=1
c
	call PUTCUR(3000,3000)
      ch=char(FNDCUR(ix,iy))	!note new usage
	rx(1)=XWORLD(ix)
	ry(1)=YWORLD(iy)
c	ixjus(n)=base
c	iyjus(n)=base
c
c	subroutine SETCSIZE(csize,isize)
cc Lahey/Hgraph subroutine to set character size
cc Input=csize (0.5-18.0)
cc Output=isize(1-9)
	csize=5.
	call SETCSIZE(csize,isize)
c	call SETFONT1(4)
	ifont=4		!initial font; set in HGSTRING
	do i=1,60
c	   text(i:i)=char(0)
	   text(i:i)=char(32)
	enddo
	call HGSTRING(ix,iy,text,ifont,iangle,isize,
     & ixjust,iyjust,icolor,ibk,ndim,.true.)
	call ANYKEY
	call clears(0,480,ibk)	!delete string
	call ANYKEY
	call HGSTRING(ix,iy,text,ifont,iangle,isize,
     & ixjust,iyjust,icolor,ibk,ndim,.false.)
	call ANYKEY
c
	call ENDPLT
	n=nblank1(text)
	print 1,text
1	format(1x,a60)
	end

