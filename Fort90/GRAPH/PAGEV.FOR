	subroutine PAGEV(landscap,title1,ibk)
c For PAGE PREVIEW in VPLOT4
c
	integer*2 lth(100) 		!for line thickness
	character*64 TITLE1
	logical landscap
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
	real    angle(100)
      integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real size(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c
	common/ginov/vxlo,vxhi,vylo,vyhi,xlo,xhi,ylo,yhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,lth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & ntrace,ytsep
	COMMON/cols/icol,mono
c
c Clear graph area
	call FILLWIN(0,55,639,387,ibk)		!paper size
c
	if(.not.landscap) goto 100


c Fill A4 page area in white

c A4 = 210x297 mm: aspect ratio=1.414 (sqrt(2)!) -close to 326*461 pixels
c y=383-58+1=326, x=550-90+1=461
c i.e. 1.552 pixels per mm
	call lincols(15)		!bright white
	call FILLWIN(90,58,550,383,15)		!paper size
c Max fillable area.  Max plottable area on page in landscape is 10000*7484
c i.e. aspect ratio=1.336 which occupies 10 inches by 7.484 inches, i.e.
c 190*254 mm, i.e. 295*394 pixels (cf 326*461 pixels for A4 paper)
c Calculate ix1,iy1 =pixel coordinates corresponding to ivxlo=0, ivylo=0
c ie lower left usable point
	ivx1=90+33
	ivx2=ivx1+394-1
	ivy1=58+15
	ivy2=ivy1+295-1
	call HLINE(ivx1,ivx2,ivy1,12)
	call HLINE(ivx1,ivx2,ivy2,12)
	call VLINE(ivx1,ivy1,ivy2,12)
	call VLINE(ivx2,ivy1,ivy2,12)
c Repeat for ixlo,... ,iyhi
	dx=xhi-xlo
	dy=yhi-ylo
	x=float(xlo)*25.4/1000.	!in mm on paper
	y=float(ylo)*25.4/1000.	!in mm on paper
	dx=float(dx)*25.4/1000.	!in mm on paper
	dy=float(dy)*25.4/1000.	!in mm on paper
	ix1=ivx1 + ifixr(x*1.552)	!pixels relative to ivx1
	iy1=ivy1 + ifixr(y*1.552)	!pixels relative to ivy1
	ix2=ix1 + ifixr(dx*1.552)	!pixels relative to ivx1
	iy2=iy1 + ifixr(dy*1.552)	!pixels relative to ivx1
	call HLINE(ix1,ix2,iy1,9)
	call HLINE(ix1,ix2,iy2,9)
	call VLINE(ix1,iy1,iy2,9)
	call VLINE(ix2,iy1,iy2,9)
c
c Define
c scal1 such that HG units (0-10000 etc)*scalfac = pixels on this screen
c scal2=# of pixels on this screen/# on orig screen (10000 HG units=640 pixels)
c i.e. factor by which graph was scaled down for pageview screen
	scal1=(25.4*1.552)/1000.	!0.039421
	scal2=scal1*10000./640.       !same (nearly) as scal1*7484/480
c
	call lincols(0)		!all rest in black
c
c Draw title
	ix=iscrx(rx(10))        !HG units on orig screen
	iy=iscry(ry(10))
	ix=ivx1 + ifixr(float(ix)*scal1)		!pixels here
	iy=ivy1 + ifixr(float(iy)*scal1)
c	subroutine pixtohg(ixpix,iypix,ixhg,iyhg)
	call PIXTOHG(ix,iy,ixhg,iyhg)		!convert to HG units here
c	call SETCSIZE(size(10)*csize*scal2,isize)	!scale type size
	call gvalues(size(10)*csize*scal2,ixjus(10),iyjus(10),iw,ih,ijus)
	call LINWID(ifixr(scal2*float(lth(25))))
c	call COLTYP1(icol(25))
	nl=NBLANK1(title1)
	call JUSTIFYSTRING(ixhg,iyhg,title1(1:nl),iangle(10),size,
     & ijus(10))

	goto 99
c
c Now portrait mode
100	continue
c
99	continue
	call ANYKEY
c
	RETURN
	end

