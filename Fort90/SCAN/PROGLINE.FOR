	subroutine PROGLINE(iupdate,istart,iend,ibit,isfst,ndisp,ipsec,
     & npp,npmax,xdline,ilcol,icol1,icol2,ip1s,ip2s)
	integer istart(512),iend(512)
	integer ilcol(npmax)
	real xdline(npmax)
c	logical debug,caplock
	common/lgrf/xpos,ypos,ipen		!ditto
c
c Draw 'progress line' at top of screen, to (a) show position of current
c frame in whole data set of NINT integers (per channel) and
c (b) to show which bits are bad/good. Must  be updated (a) each time frame
c is altered (ie change in isfst,isec,ndisp) and (b) each time a marked section
c is created or cancelled.
c IUPDATE=0 to redraw entire line; =1 to update only the box marking current
c position; =2 to update only the colours showing marked bits- change to icol2;
c =3 like 2 but change to icol1
c ILCOL(i)=colour of ith pixel
c
c	debug()=caplock()
c
c Index in all data of 1st point on display=
	if(iupdate.le.1) then
	   in1 = isfst + ipsec		!ipsec=(isec-1)*npsec calc in main prog
c and of last point on display
	   in2 = isfst+ndisp-1 + ipsec
c One pixel represents npp=1+nint/npixx actual data points, so point #in in all
c data corresponds to pixel # 1 + (in-1)/npp
c	   npp=nint/npixx	!calc in main prog
	   ip1=1 + (in1-1)/npp
	   ip2=1 + (in2-1)/npp
c	   if(debug()) print 31,in1,in2,ip1,ip2,npmax
c31	   format(' in1,in2,ip1,ip2,npmax = ',5i8)
	   if(ip1.lt.1) ip1=1
	   if(ip2.gt.npmax) ip2=npmax
c	   if(ip1.gt.npmax) print 700,in1,isfst,ipsec,npp,npmax,ip1
c700	   format(' in1,isfst,ipsec,npp,npmax,ip1 =',i10,5i6)
	endif
c Draw the line
	if(iupdate.eq.0) then
	   do 22 i=1,npmax
22	   call SETPIX(xdline(i),7.9,ilcol(i))
	endif
c Mark present pos with white box
	if(iupdate.le.1) then
c	First delete old box
	   ipen=0
	   if(ip1s.ge.1) then		!so skipped for 1st display
		call MOVEL1(xdline(ip1s),7.8)
		call DRAWL1(xdline(ip1s),8.0)
		call DRAWL1(xdline(ip2s),8.0)
		call DRAWL1(xdline(ip2s),7.8)
		call DRAWL1(xdline(ip1s),7.8)
c  and replace deleted pixels on the line
		call SETPIX(xdline(ip1s),7.9,ilcol(ip1s))
		call SETPIX(xdline(ip2s),7.9,ilcol(ip2s))
	   endif
c and save positions for next delete
	   ip1s=ip1
	   ip2s=ip2
c draw new box
	   ipen=14
c	   if(debug()) print 30,ip1,ip2,npmax
c30	   format(' ip1,ip2,npmax = ',3i8)
	   call MOVEL1(xdline(ip1),7.8)
	   call DRAWL1(xdline(ip1),8.0)
	   call DRAWL1(xdline(ip2),8.0)
	   call DRAWL1(xdline(ip2),7.8)
	   call DRAWL1(xdline(ip1),7.8)
	endif
c
c Update colours for marked bit #ibit; istart(),iend() already contain index
c of points in whole data set
	if(iupdate.ge.2) then
	   ip1=1 + (istart(ibit)-1)/npp
	   ip2=1 + (iend(ibit)-1)/npp
	   if(ip1.lt.1) ip1=1
	   if(ip2.gt.npmax) ip2=npmax
	   ic=icol2		 !'marked' colour
	   if(iupdate.eq.3) ic=icol1		!'unmarked' colour
	   do 23 i=ip1,ip2
	   call SETPIX(xdline(i),7.9,ic)
	   ilcol(i)=ic		!update ilcol()
23	   continue
	endif
	RETURN
	end
