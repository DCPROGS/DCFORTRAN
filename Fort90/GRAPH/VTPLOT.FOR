	subroutine VTPLOT(ifile,iview,idev)
c To plot PLOTnn.DAT (nn=ifile) with VTRANS (see TPLOT.FOR for tests)
c Modif 09/05/91 05:06pm so question re preview is in VPLOT/VHIST rather
c than here; iview=0 for no preview, =1 for preview
c
	integer*2 n1,n2,n3,n4,n5,n6
	character*77 command
	character*6 filnam
	character*11 ix1,iy1,ix2,iy2
	character*30 rx1,ry1
      character*1 ans,UC
	logical caplock
c
c     VTRANS files unit frame
c
cwhere
c
c     files - a list of archive files to be plotted. Multiple files can be
c             separated by a plus sign (+) or a comma (,). Files separated
c             by a plus sign will be graphed together. Files separated by a
c             comma will be graphed individually.
c
c     unit  - HGraph unit number, 0 & 1 Screen device, 2 Vector device, 3 &
c             4 Raster device.
c
c     frame - of the form
c
c               ixorg,iyorg,ixbond,iybond,sclfx,sclfy
c
c             where
c
c               ixorg  - horizontal displacement in mils (integer, default 0)
c               iyorg  - vertical displacement in mils (integer, default 0)
c               ixbond - horizontal bound in mils (integer, default 0)
c               iybond - vertical bound in mils (integer, default 0)
c               sclfx  - horizontal scale factor (real, default 1.0)
c               sclfy  - vertical scale factor (real, default 1.0)
c
cFor instance
c
c     VTRANS PLOT5+PLOT6,PLOT7 0 0
c
c will place draw the graphs contained in PLOT5.DAT and PLOT6.DAT on the
c screen, wait for a key to be pressed, then draw the graph in PLOT7.DAT.
c If unit 3 had been selected, graphs from PLOT5.DAT and PLOT6.DAT would be
c drawn on one sheet of paper, and PLOT7.DAT would be drawn on a second.
c
c	print 12
c12	format(' Type <ENTER> to continue')
101	format(a1)
c	call ENDPLT		!for some reason, enter not needed here!
	idevsav=idev	!input value=3 or 4
	if(iview.eq.1) idev=0
c
c	if(idev.le.0) idev=4
c               ixorg  - horizontal displacement in mils (integer, default 0)
c               iyorg  - vertical displacement in mils (integer, default 0)
c               ixbond - horizontal bound in mils (integer, default 0)
c               iybond - vertical bound in mils (integer, default 0)
c               sclfx  - horizontal scale factor (real, default 1.0)
c               sclfy  - vertical scale factor (real, default 1.0)
c	print 4
c4	format('&Horizontal, vertical displacement (integer mm) [0,0] = ')
c	read 5,ixorg,iyorg
c5	format(2i8)
c	print 6
c6	format('&Horiz, vert bound (integer mm) [10000,10000] = ')
c	read 5,ixbond,iybond
c	fac=1000./25.4	!converts mm to thousandths of an inch
c	ixorg=ifixr(float(ixorg)*fac)
c	iyorg=ifixr(float(iyorg)*fac)
c	ixbond=ifixr(float(ixbond)*fac)
c	iybond=ifixr(float(iybond)*fac)
c	if(ixbond.le.0) then
c	 ixbond=10000
c	 iybond=10000
c	endif
c	print 7
c7	format('&Horizontal, vertical scale factor [1.0, 1.0] = ')
c	read 8,sclfx,sclfy
c8	format(2g13.6)
c	if(sclfx.le.0.0) then
c	 sclfx=1.0
c	 sclfy=1.0
c	endif
c
c Set default values
	ixorg=0		!no offset
	iyorg=0
	ixbond=10000
	iybond=10000
	sclfx=1.0
	sclfy=1.0
c
	nd1=ifile/10		!first digit
	nd2=ifile-10*nd1	!2nd digit
	filnam='PLOT'//char(nd1+48)//char(nd2+48)
c	filnam='PLOT'//'05'
c problem to represent large integers and floating point numbers in ascii-
c solved in conversion routines INTCONV,REALCONV
c NB for NBLANK to work correctly must initialise strings (in these routines)
c with spaces (char(32)), not NUL (char(0))
	call INTCONV(ixorg,ix1)
	n1=nblank(ix1)
	call INTCONV(iyorg,iy1)
	n2=nblank(iy1)
	call INTCONV(ixbond,ix2)
	n3=nblank(ix2)
	call INTCONV(iybond,iy2)
	n4=nblank(iy2)
	call REALCONV(sclfx,rx1)
	n5=nblank(rx1)
	call REALCONV(sclfy,ry1)
	n6=nblank(ry1)
c	print 11,n1,n2,n3,n4,n5,n6
c11	format(' n1-n6= ',6i5)
c
32	continue		!return here with new idev
	command='VTRANS'//' '//filnam//' '//char(idev+48)//' '//
     &ix1(1:n1)//','//iy1(1:n2)//','//ix2(1:n3)//','//iy2(1:n4)//
     &','//rx1(1:n5)//','//ry1(1:n6)
	i=NBLANK1(command)	!terminate with ASCII 0
c
	if(caplock()) then
	   print 700,idev,idevsav,ifile,command
700	   format(' idev,idevsav,ifile = ',3i5,/,1x,a77)
	   pause
	endif
c
c Do the plot
	call SYSTEM(command)
c     call system ('print hpljr.out')
c     call system ('erase hpljr.out')
c
	if(idev.eq.0) then
	  print 30
30	  format(' Plot on Laserjet/Deskjet now [Y] ? ')
	  read 101,ans
	  if(UC(ans).eq.'N') goto 90
	  idev=idevsav
	  goto 32
	endif
c
90	iview=0
c
	RETURN
	end

