	subroutine LOCTEXT(n,nt,angle,rxbox,rybox,idraw)
c	subroutine LOCTEXT(n,nt,iangle,ixlo,iyl,ixhi,iyh,iylo,ixl,iyhi)
c Puts up crosshairs, and returns n, the number of the text string
c which is located by the crosshairs. If crosshair intersection
c is not within any of the NT rectangles defined
c by ixlo(),...,iyhi(), then n=0 is returned
c Modif 07/07/93 09:45am to use dialog box
c Modif 01/03/90 02:36pm to go through ALL boxes rather than skip out when
c one found, in case the point is in more than one box. If it is then all
c those boxes which enclose the point are drawn on screen and cursors put
c up again for another try. If point is in none of the boxes then all boxes
c are drawn on screen.
c
c First test if ix,iy is within box (parallel to axes) that circumscribes
c the text box.  Actually the next two tests are together sufficient, but
c this prelim test is simpler and will usually reject an incorrect box
c with minimum of calculation
c	Next start at bottom-most corner of box (coord=iylo,ixl). From
c To the right of this point rises one box-side, with slope=tan(iangle)
c To the left of this point rises another box-side, with slope=-tan(90-iangle)
c Expt shows that TEXTBOX always draws box clockwise, starting at the
c corner nearest to the bottom left hand corner (as viewed upright) of the
c text, so if we start at bottom corner (that with lowest Y) then moving
c up and right takes us to the next higher numbered corner; moving
c up and left goes to the next lowest-numbered corner
c
c Difficult bit is to deal with sloping strings
c      character*1 ch, fndcur
	character defolt*30,cans*30		!to hold text & result of QDIALOG
      character cnum1*11
c	integer fndcur
	real rxbox(4,nt),rybox(4,nt)
	real	 angle(nt)
      integer	idraw(nt)
	integer ifind(10)		!to hold numbers of all boxes that enclose point
      character*1 ch,ans,UC
c
101   format(a1)
c
c	do 21 i=1,4
c	do 21 j=1,nt
c	ixbox(i,j)=ISCRX(rxbox(i,j))
c	iybox(i,j)=ISCRY(rybox(i,j))
c21	continue
c
	ifail=0
31	continue
c
c	call PUTCUR(5000,3500)		!start with cursor central
c      ch=FNDCUR(ix,iy)
c      idum=FNDCUR(ix,iy)
	 call CURPOS(0.,0.)
	 call CURSOR(x,y,itype)
c
c Define slopes
	radfac=3.141592654/180.		!conversion to radians
c
	n=0
	nfind=0	!number found
c Start the loop
	do 1 j=1,nt
	   if(idraw(j).eq.-2) goto 1	!skip undefined strings
	   n=j
c define circumscribing box
	   xlo=10000	!min
	   xhi=0	!max
	   ylo=10000	!min
	   yhi=0	!max
	   do 5 i=1,4
	     if(rxbox(i,j).lt.xlo) xlo=rxbox(i,j)
	     if(rxbox(i,j).gt.xhi) xhi=rxbox(i,j)
	     if(rybox(i,j).lt.ylo) then
	        ylo=rybox(i,j)
	        ilo=i
	     endif
	     if(rybox(i,j).gt.yhi) yhi=rybox(i,j)
5	   continue
c test 1:
	   if(xlo.le.x.and.x.le.xhi.and.
     &      ylo.le.y.and.y.le.yhi) goto 2	!ix,iy in circumscribed box
	   goto 1	!ix,iy not in current box- try next box
c
2	   continue    !test 2 now
c Test 2 now (only if box is sloping, otherwise test 1 suffices)
	   if(mod(angle(i),90.).eq.0) then
		nfind=nfind+1
		ifind(nfind)=n
		goto 1	!next box
	   endif
c test 2: for the specified IX, is IY between the pair of parallel
c with pos slope. The lower one goes through the lowermost point
c (index=ilo). The vertical at IX crosses this line at
c iy1=iybox(ilo) + b*(ix - ixbox(ilo))
c First define slopes for current box
	   angle(i)=mod(angle(i),360.)		!ensure between 0-360
	   if(angle(i).lt.0.) angle(i)=360.+angle(i)
c Slope from bottom-most corner will always have angle between 0-90 so
	   ang=mod(angle(i),90.)	!so between 0-90
	   b=TAN(radfac*ang)
c	   b1=-atan(90.-ang)
	   b1=-1.0/b
	   y1=rybox(ilo,j) + b*(x - rxbox(ilo,j))
c The side parallel with, and above (on screen), this goes through
c the next lowest number box, ie index=ihi say, and Y at IX is thus
	   ihi=ilo-1
	   if(ihi.eq.0) ihi=4
	   y2=rybox(ihi,j) + b*(x - rxbox(ihi,j))
	   if(y1.lt.y.and.y.lt.y2) goto 3   !ix,iy between lines so try test 3
	   goto 1	!ix,iy not in current box- try next box
3	   continue
c Test 3: is ix,iy also between the pair of parallel lines that
c go through the other two sides of the box? That arising from lowest
c point gives
c	print 51,b1
	   y1=rybox(ilo,j) + b1*(x - rxbox(ilo,j))
c The higher line, parallel with it goes through the next highest-numbered
c corner so
	   ihi=ilo+1
	   if(ihi.eq.5) ihi=1
	   y2=rybox(ihi,j) + b1*(x - rxbox(ihi,j))
	   if(y1.lt.y.and.y.lt.y2)  then
		nfind=nfind+1
		ifind(nfind)=n
		goto 1	!next box
	   endif
1	continue
c
	if(nfind.eq.1) then
		n=ifind(1)
		RETURN
	else if(nfind.gt.1) then
c		Draw the boxes that contain point
	   call BELL(1)
	   do 33 i=1,nfind
		j=ifind(i)
		if(idraw(j).eq.-2) goto 33
		call IFRAME1(rxbox(1,j),rybox(1,j))
33	   continue
	   call INTCONV(nfind,cnum1)
	   call WDIALOG(1,CHARNB(cnum1)
     &	//' BOXES FOUND: TRY AGAIN',12)
	   goto 31
	else if(nfind.eq.0) then
c		Draw ALL boxes
	   call BELL(2)
	   ifail=ifail+1
c - draw all the current boxes
	   do 30 i=1,nt
		if(idraw(i).eq.-2) goto 30
		call IFRAME1(rxbox(1,i),rybox(1,i))
30	   continue
	   if(ifail.le.2) goto 31		!try again
	   call QDIALOG(1,'Text box not found- try again'
     &	,' ',11,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') goto 31
	   n=0
	   RETURN		!give up- return with n=0
	endif
c	n=0		!ix,iy was in none of the NT boxes
c	RETURN
c10	continue
c	RETURN		!ix,iy was in box #n
	end







