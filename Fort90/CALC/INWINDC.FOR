	subroutine INWINDc(cdata,ni,nj,nidisp,title,heading,
     & nr1,nc1,iflag,iconst,nd1,nd2,ic)
c INWINDc is version of inwindv, in which input and output are ascii,
c cdata(), rather than data() so can be used for input of parameter
c names in qdefine. No constraints or multipliers so ICONST=0 always
c
c INWINDv is version of INWIND1 that has data() declared as data(nd1,nd2)
c for use when data() array has been ALLOCATED these dimensions in calling
c prog (tests in TWIND2)
c
c Modif 01/29/02 12:42pm
c Add colour of window as a parameter (ic=0 for usual blue;
c    ic=1 for green)
c
c Modif 08/31/96 08:01am to (1) add discprt, so multipliers printed
c to disc if used, and (2) when iconst>0, Y multiplier also multiplies
c SD (col 3) as well as Y (col 2)
c
c Modif 04/19/95 05:14pm by changing names of getrc and getij to GETRC2
c and GETIJ3, to prevent duplicate names with QWIND and MODWIND
c
c Modif 07/09/90 05:12pm, so number of rows (values) need not be
c specified in advance: call with ni=-1 and type # after last
c number. This is detected by ENDCHAR routine so returns with ni
c set to number of obs.
c
c To make a window for input/correction of numerical values
c (e.g. for CVFIT). Version in which window can be moved round screen
c by CTRL-arrow. Top left corner of the window in row=nr1 (0-24) and
c column nc1 (0-79). If nr1 or nc1 is neg on entry use values defined below.
c (ctrl-up,ctrl-down do not work, though initial tests with TKEY indicated
c that they would; only ctrl-left,ctrl-right seem to work now)
c
c ICONST=0 No link between columns
c ICONST=n is special version of INWIND for CVFIT in which cols n and
c 	n+1, if both present, are linked by (n+1)=1/[(n)*(n)] so if one
c 	is altered, so is other
c ICONST=-1 is special version for MECHFIT in which col 2 contains
c	areas for pdf, and last area is inserted automatically.
c
c Make NJ columns of data values (eg x,y,sd),
c and NI rows of data values (of which NIDISP are on display at a time).
c TITLE is overall title of window
c HEADING is column headings
C
c INPUT:
c IFLAG is set=1-5 then input values in cDATA are displayed in the first
c	IFLAG columns are filled from data, and the remaining cols (if any)
c	are left blank
c IFLAG is set=0 then display on entry is all blank
C
c OUTPUT:
c IFLAG is set=1 if any value of the DATA array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=0 if NO value of the DATA array has been changed (including
c	no change in either x or y multiplier). (Internally sep flag (Iflagx,y)
c	used to indicate reset of multiliers)
c IFLAG=-1 if F5 is hit (eg to abort further display)
c
c Xmult,Ymult are internal variables; ignored unless they are changed
c  in which case DATA(i,1)=DATA(i,1)*Xmult, and  DATA(i,2)=DATA(i,2)*Ymult.
c  (##would be more general to describe these as multipliers for col1,col2,
c	rather than for X,Y)
c
c Numbers entered are up to 11 characters long (now- but can alter size
c in declaration of CDATA and in definition of NCN).
c Modified 08/07/01 11:48am so ESC or ENTER causes exit
c ENTER or TAB: Move to next number (scrolls when nec)
c INS or SHIFT-TAB: Move to previous number (scrolls when nec)
c ARROWS: move round in current window to correct values (no scroll)
c PAGE-DOWN: moves down nidisp values (or to last nidisp values)
c PAGE-DOWN: moves up nidisp values (or to first nidisp values)
c HOME: moves up to first nidisp values
c END: moves down to last nidisp values
c ESC: finished.
c When all entered hit ESC.
c The string of values is converted
c to real numbers with CHTOREAL (note that dec point NOT necessary!)
c (See tests in TWIND2)
c Based on earlier version (INWINDOW.FOR,TWIND3) which filled whole window
c before decoding its contents from one long string. This version keeps
c separate character representation of  each number in CDATA.
c	real data(ni,nj)	!does not work (for j>1; see TAR1.FOR)
c=	real data(100,nj)
c	real data(nd1,nd2)
c	ALLOCATABLE::cdata
	character*11 cdata(nd1,nd2)
	character*11 Cmult,cnum
	integer scrnbuf1(1024),scrnbuf2(1024)
	integer*2 ktype,mr,mc,iw2,jw2
	character*3 crow(99)		!for row numbers up to 99
	character*1 ch1,getch
c##	character*80 windchar(20)
	character*79 heading,title,blank
	logical digchar,caplock,bad,colour,valnum,newobs
	INCLUDE 'c:\lf90\lib\SPINATTI.HDR'		!modif version of SPINATTR.HDR
cc	INCLUDE 'c:\hgraph\CONSTS.FOR'		!only for Hgraph -not here!
c	INCLUDE 'g:\f77l3\lib\SPINATT.HDR		!modif version of SPINATTR.HDR
	common/gij/njm,ncn,ni1,nj1,nscrol,jcol
	logical discprt,end
	common/dp/discprt
c
	iconst=0 	!in INWINDc
	end=.false.
	colour=.true.	!for Dell 425
c	colour=.false.	!for Tosh
	if(ni.lt.0) then
	   ni=99
	   nidisp=20
	endif
c Keep original screen
	call CAPTSCN(scrnbuf1)
c
c	call CLS
	call CLRAREA(24,0,24,80)  !clear bottom row only (in case model on display)
	call locate(24,5)
	call locate(24,1)
	if(iconst.le.0) then
c	   print *,
c     &    ' F1=HELP; F2=X MULTIPLIER; F3=Y MULTIPLIER;',
c     &    '  F4=Add a value; #=PREV LINE WAS LAST; F5= abort; ESC=exit'
c	   else
c	      print *,
c     &    ' F1=HELP; F2=X MULTIPLIER; F3=Y and SD MULTIPLIER;',
c     &    '  F4=Add a value; #=PREV LINE WAS LAST; F5= abort; ESC=exit'
	   print *,
     &    ' F1=HELP;',
     & '  F4=Add a value; #=PREV LINE = LAST; F5= abort; ESC/ENTER=exit'
	   else
	      print *,
     &    ' F1=HELP;',
     & '  F4=Add a value; #=PREV LINE = LAST; F5= abort; ESC/ENTER=exit'
	endif
c
c Start of initialisation
	if(nidisp.gt.21) nidisp=21	!correct input value if nec
c Define row numbers
	do 8 i=1,99
	 call INTCONV(i,cnum)
	 crow(i)=charnb(cnum)//': '
8	continue
c Initialise CDATA
c Input:
c IFLAG is set=1-5 then input values in DATA are displayed in the first
c	IFLAG columns are filled from data, and the remaining cols (if any)
c	are left blank
c IFLAG is set=0 then display on entry is all blank
c Output:
c IFLAG is set=1 if any value of the DATA array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=0 if NO value of the DATA array has been changed (including
c	no change in either x or y multiplier
c	Cmult='           '
cc Make all blank initially (eg in case more data added) -do before calling now
c	nd11=nd1
c	if(nidisp.gt.nd1) nd11=nidisp+5
c	nd12=5
c	if(nj.gt.nd12) nd12=nj
c	do 4 i=1,nd11
c	do 4 j=1,nd12
c         CDATA(i,j)='           '
c4	continue
c	if(iflag.eq.0) then
c	   do 4 i=1,ni
c	   do 4 j=1,nj
c	else
	if(iflag.ne.0) then
	   do 5 i=1,ni
c	   do 5 j=1,nj
		do 51 j=1,iflag
c==		   call REALTOCH(data(i,j),CDATA(i,j),11)
c		   if(caplock()) print 60,i,j,data(i,j),cdata(i,j)
c60		   format(' i,j,data,cdata(i,j)= ',2i4,2x,g13.6,2x,a11)
51		continue
c		if(iflag.lt.nj) then
c		   do 52 j=iflag+1,nj
c52		   CDATA(i,j)='           '
c		endif
5	   continue
c	   if(caplock()) pause
	endif
	IFLAG=0	!set for output (=no value changed)
	newobs=.false.
c
c ncn=11 char per number with 2 blank cols between numbers and 1 blank
c col at left and right edges so total width of window =
c (max=5 cols of numbers for 70 col window)
c + 3 cols at left edge for row number
	ncn=11
	njm=3		!number of cols on left for display of row number
	ni1=ni	!copy for common
	nj1=nj	!copy for common
c END of initialisation
c
c Define help window (40 col,10 row), in case needed
c -attributes definedin spinatt.hdr
	if(colour) then
	   ihatt=whitechr+redbkg
	else
	   ihatt=inverse
	endif
	call DEFWIND(2,5,30,19,71,dblbdr+wrap+scrol,ihatt)
c
c Define multiplier window
	call DEFWIND(3,5,30,7,71,dblbdr+wrap+scrol,inverse)
c
131	continue	!return here to redraw window if obs added
c Total number of cols needed=
	nc=njm + nj*(ncn+2) + 1		!1 more than above calc actually needed
	do 61 i=1,nc-1
61	blank(i:i)=' '
c Total number of rows needed=number for data entries=ni + 2 more for
c title and column headings  (+ 1 more, as for cols-room for border presumably)
c	nrtot=ni + 3	!total no of rows (needed??)
	nr=nidisp+3		!no of rows on display at a time
c Define corners of the window
c If nr1 or nc1 is neg on entry use values defined here
	if(nr1.lt.0.or.nc1.lt.0) then
	   nc1=(79-nc)/2	!start col number (whole screen numbering)
	   if(nc1.lt.0) nc1=0
c	   nr1=(24-nr)/2	!start row number (whole screen numbering)
	   nr1=0
	   if(nr1.lt.0) nr1=0
	endif
c
130	continue	!return here to redraw window if moved
	nr2=nr1+nr  	!end row number (whole screen numbering)
	nc2=nc1+nc        !end col number (whole screen numbering)
c One row for titles, up to 24 at a time for numbers
c	normatt=bright+dblubkg		!'normal' attribute
	if(ic.eq.0) then
	   normatt=bright+bluebkg		!'normal' attribute
	else if(ic.eq.1) then
	   normatt=bright+greenbkg		!'normal' attribute
	endif
	call DEFWIND(1,nr1,nc1,nr2,nc2,singlbd+wrap+scrol,normatt)
	call FILLB(1,' ')
c Draw the window
c	call OPENWIND(1)
      call CLRB(1)
	nscrol=0	!initialise number of lines scrolled
	ilast=1			!record posn of last inverse-contrast box
	jlast=1
11	continue
	call BLDB(1,0,0,title(1:nc-1),normatt)
	call BLDB(1,1,0,heading(1:nc-1),normatt)
	ir=2
	do 40 i=1,nidisp
	call BLDB(1,ir,0,crow(i),normatt)	!='n:'
	ir=ir+1
	do 40 j=1,nj
	call GETRC2(i,j,nidisp,iw,jw,jw2,bad)  !get row, and 1st col for cdata(i,j)
	call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
40	continue
c	nlt=nblank(title)
c	call BLDB(1,0,0,title(1:nlt),normatt)
c	nlh=nblank(heading)
c	call BLDB(1,1,0,heading(1:nlh),normatt)
c  define row numbers
c	do 40 i=1,nidisp
c40 	call BLDB(1,i+1,0,crow(i),normatt)	!='n:'
c Note that cdata(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
c Start with cursor in 1st place in first number,CDATA(1,1), and wait for
c something to be typed
c NB in,jn used for index of a particular number CDAT(in,jn)
c and mr,mc (or int*4 versions mr1,mc1) used as index of the row
c and column in the displayed window
	in=1		!number is cdata(1,1)
	jn=1
	call GETRC2(in,jn,nidisp,iw,jw,jw2,bad)
c	iw=2		!row number (window numbering; starts at 0)
c	jw=njm+1	!col number (window numbering; starts at 0)
c	j1max=njm + 1 + (nj-1)*(ncn+2)	!col # for start of nj'th box
c	iwmax=nj+2	    !##?
c	if(iwmax.gt.24) iwmax=24	!max row number
c Mark position to be filled in by 11 spaces in reverse contrast
c	call BLDB(1,iw,jw,'           ',inverse)
c	call GETIJ3(iw,jw,in,jn,k,bad)	!get (i,j)
	call BLDB(1,iw,jw,CDATA(in,jn),inverse)
	iw2=iw
	jw2=jw
	call LOCATEW(1,iw2,jw2)	!put cursor there
	iw=int4(iw2)
	jw=int4(jw2)
c	ilast=in			!record posn of last inverse-contrast box
c	jlast=jn
	call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
	mr1=int4(mr)			!integer*4
	mc1=int4(mc)			!integer*4
	call GETIJ3(mr1,mc1,in,jn,k,bad)	!get position in CDATA(i,j)
c
c NOW GET CHARACTERS
10	if(newobs) then	!redraw ready to enter new data
	   ktype=8
	   ival=79		!simulate END key
	   goto 132
	endif
c
	ch1=GETCH(ktype)
C	call movcurw(1)	!better pick up arrow keys via GETCH
	ival=ichar(ch1)
c If the value typed is a 0-9 or - + . e or E then type it in
c the window at the current cursor position, then check the cursor position
c (it may have been moved eg with arrow keys) before inserting the character
c into the correct position in CDATA(i,j) . If TAB is typed move on to next value or if arrow
c key typed move round to correct values already typed.
c
	end=.false.
	if((ktype.eq.16).or.		!ESC to end
     &	(ktype.eq.4.and.ival.eq.13)) then		!ESC or ENTER key
	   end=.true.
	   goto 15				!make sure constraints set before leaving
c	   goto 99				!ESC to end
	endif
	if(ktype.eq.2.and.ival.eq.35) then		!# hit to end series
	   call GETIJ3(mr1,mc1,in,jn,k,bad)	!get in=current row number
c	   if(VALNUM(cdata(in,jn))) then
		ni=in		!if # is at end of valid number, no of obs=in
c	   else
c	      ni=in-1	!no of obs if # is on line AFTER the last number
c	   endif
	   ni1=ni	!copy for common
	   nidisp=ni
	   if(nidisp.gt.21) nidisp=21
	   call CLS
c	   call LOCATE(24,5)
	   call locate(24,1)
	   if(iconst.le.0) then
	      print *,
     & ' F1=HELP;',
     & '  F4=Add a value; #=PREV LINE = LAST; F5= abort; ESC/ENTER=exit'
	   else
	      print *,
     & ' F1=HELP;',
     & '  F4=Add a value; #=PREV LINE = LAST; F5= abort; ESC/ENTER=exit'
	   endif
	   goto 131		!redraw window
c	   goto 99		!out
c Do not leave straight away in case F2,F3 needed now ni is defined -use ESC
c to leave window in usual way
	endif
	if(ktype.eq.0.and.ival.eq.59) goto 90		! F1 key=help
c	if(ktype.eq.0.and.(ival.eq.60.or.ival.eq.61)) goto 91		!F2 or F3
	if(ktype.eq.0.and.ival.eq.63) then
	   iflag=-1				!F5
	   goto 99				!end
	endif
	if(ktype.eq.0.and.ival.eq.62.and.ni.lt.99) then			!F4
	   ni=ni+1
	   ni1=ni	!copy for common
	   if(nidisp.lt.20) nidisp=nidisp+1
	   newobs=.true.
	   goto 131		!redraw the window
	endif
c Section to move window by CTRL-arrow
	if(ktype.eq.8.and.(ival.eq.116.or.ival.eq.145.or.
     & ival.eq.115.or.ival.eq.141)) then
	   call DISPSCN(scrnbuf1)     !restore orig screen
	   if(ival.eq.116) nc1=nc1+1	!CTRL-right arrow
	   if(ival.eq.115) nc1=nc1-1	!CTRL-left arrow
	   if(ival.eq.141) nr1=nr1+1	!CTRL-up arrow
	   if(ival.eq.145) nr1=nr1-1	!CTRL-down arrow
	   if(nr1.lt.0) nr1=0
	   if(nc1.lt.0) nc1=0
	   goto 130		!redraw window
	endif
c Now take action according to what key hit, and then return to 10 for
c another character
c
c VALID CHARACTER
c In this version, any character is valid, so put character input at the
c end, after the final 'else'
132	continue
c	if(digchar(ival).or.ival.eq.32) then		!allow spaces?
c	   if(.not.bad) then		!char is in box so put in cdata and screen
c		CDATA(in,jn)(k:k)=ch1     !put character in CDATA
c		call BLDB(1,mr1,mc1,ch1,inverse)	!put character in window
c		iflag=1		!data altered
cc can insert constraint here but if so it is recalc as each digit is changed
cc (but not recalc when altered with backspace)
c	   endif
c	   call LOCATEW(1,mr,mc+1)    !move cursor on(not needed for PRINTW)
c
c ENTER or TAB
c	else if((ktype.eq.8.and.ival.eq.9).or.
c	if((ktype.eq.8.and.ival.eq.9).or.
c     &	(ktype.eq.4.and.ival.eq.13)) then		!TAB or ENTER key
c TAB only now
	if(ktype.eq.8.and.ival.eq.9) then		!TAB
c TAB ONLY now for next cell
c ENTER or TAB key- move to start of next box (next number=next cdata(i,j)
c Note that cdata(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
		jn=jn+1
		if(jn.gt.nj) then
		   jn=1
		   in=in+1
		   if(in.gt.ni) in=1	!back to 1st row
		   jw2=4
		endif
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(in.ge.1+nscrol.and.in.le.nidisp+nscrol) then  !i already on display
c		  Code if i already in display (if it is not on display then, since
c		  TAB key moves on one box at a time must either scroll up one row,
c		  or, if end reached, go back to start with i=1 to nidisp on display
		  call GETRC2(in,jn,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
		  call BLDB(1,iw,jw,CDATA(in,jn),inverse)
c		  ilast=in		!update posn of last inverse-contrast box
c		  jlast=jn
		  iw2=int2(iw)
		  if(jn.eq.1) jw2=4	!move to start of 1st col
		  call LOCATEW(1,iw2,jw2)
		  iw=int4(iw2)
		  jw=int4(jw2)
		else if(in.eq.nidisp+nscrol+1) then	!scroll up one line
		   call SCROLLW(1,1)	!scroll up one line
		   nscrol=nscrol+1
		   call BLDB(1,0,0,blank(1:nc-1),normatt)	!delete row 0
		   call BLDB(1,0,0,title(1:nc-1),normatt)	!rewrite headings
		   call BLDB(1,1,0,blank(1:nc-1),normatt)	!delete row 1
		   call BLDB(1,1,0,heading(1:nc-1),normatt)
		   call GETRC2(in,jn,nidisp,iw,jw,jw2,bad)	!row,1st col for cdata(i,j)
 		   call BLDB(1,iw,0,crow(in),normatt)	!='n:'
		   call BLDB(1,iw,jw,CDATA(in,jn),inverse)  !put in existing data
c		   ilast=in		!update posn of last inverse-contrast box
c		   jlast=jn
		   call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		  iw=int4(iw2)
		  jw=int4(jw2)
		else if (in.eq.1) then	!return to initial display-redraw all!
		   nscrol=0			!reset (eg for getrc() below)
		   goto 11		!must redraw entire display
		endif
c INS or SHIFT-TAB
	else if((ktype.eq.8.and.ival.eq.15).or.
     &	(ktype.eq.8.and.ival.eq.82)) then		!SHFT-TAB or INS key
c INS or SHFT-TAB key- move to start of prev box (prev number=prev cdata(i,j)
c Note that cdata(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
		jn=jn-1
		if(jn.lt.1) then
		   jn=nj
		   in=in-1
		   if(in.lt.1) in=ni	!back to last row
		endif
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(in.ge.1+nscrol.and.in.le.nidisp+nscrol) then  !i already on display
c	    Code if i already in display (if it is not on display then, since
c	    SHFT-TAB key moves back one box at a time must either scroll down one row,
c	    or, if start reached, go back to end with i=ni-nidisp+1 to ni on display
		  call GETRC2(in,jn,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
		  call BLDB(1,iw,jw,CDATA(in,jn),inverse)
c		  ilast=in		!update posn of last inverse-contrast box
c		  jlast=jn
		  call LOCATEW(1,iw2,jw2)
		  iw=int4(iw2)
		  jw=int4(jw2)
		else if(in.eq.nscrol) then	!scroll down one line
c	  NB for some reason the scroll down, with -1 as arg, does not
c		seem to work like scroll up, but just deletes the whole
c		window. Therefore rewrite whole window for now
cIn general i=1+nscrol to i=nidisp+nscrol are in display, so move back 1 row
c i=nscrol to nidisp+nscrol-1; but as nscrol decremented first use former
c		   call SCROLLW(1,-1)	!scroll down one line
c		   call BLDB(1,0,0,blank(1:nc-1),normatt)	!delete row 0
c		   call BLDB(1,0,0,title(1:nc-1),normatt)	!rewrite headings
c		   call BLDB(1,1,0,blank(1:nc-1),normatt)	!delete row 1
c		   call BLDB(1,1,0,heading(1:nc-1),normatt)
		   nscrol=nscrol-1
c	draw boxes i=nscrol+1 to nidisp+nscrol (nscrol already decremented)
		   call CLRW(1)
	 	   call BLDB(1,0,0,title(1:nc-1),normatt)
	 	   call BLDB(1,1,0,heading(1:nc-1),normatt)
		   ir=2
	 	   do 43 i=nscrol+1,nidisp+nscrol
	 	   call BLDB(1,ir,0,crow(i),normatt)	!='n:'-start row 2
		   ir=ir+1
	 	   do 43 j=1,nj
	 	   call GETRC2(i,j,nidisp,iw,jw,jw2,bad)  !get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
43	 	   continue
c put cursor at start of last box on new (top) line
		   in=nscrol+1
		   jn=nj
	 	   call GETRC2(in,jn,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
c		   ilast=in		!update posn of last inverse-contrast box
c		   jlast=jn
		   call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		   iw=int4(iw2)
		   jw=int4(jw2)
c		   call GETRC(in,jn,nidisp,iw,jw,jw2,bad) !get row, and col for cdata(i,j)
c 		   call BLDB(1,iw,0,crow(in),normatt)	!='n:'
c		   call BLDB(1,iw,jw,CDATA(in,jn),inverse)  !put in existing data
c		   call LOCATEW(1,iw2,jw2)	!put cursor at start of box
c		  iw=int4(iw2)
c		  jw=int4(jw2)
		else if (in.eq.ni) then
c display the last nidisp values-redraw all!
		   nscrol=ni-nidisp			!set (eg for getrc() below)
c	draw boxes i=ni-nidisp+1 to ni
		   call CLRW(1)
	 	   call BLDB(1,0,0,title(1:nc-1),normatt)
	 	   call BLDB(1,1,0,heading(1:nc-1),normatt)
		   ir=2
	 	   do 41 i=ni-nidisp+1,ni
	 	   call BLDB(1,ir,0,crow(i),normatt)	!='n:'-start row 2
		   ir=ir+1
	 	   do 41 j=1,nj
		   call GETRC2(i,j,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
41	 	   continue
c put cursor at start of last box
		   in=ni
		   jn=nj
		   call GETRC2(in,jn,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
c		   ilast=in		!update posn of last inverse-contrast box
c		   jlast=jn
		   call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		   iw=int4(iw2)
		   jw=int4(jw2)
		endif
c RIGHT ARROW
	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
c	  At present right arrow will not cause scroll when end of display
c	  reached
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=int4(mr)			!integer*4
		mc1=int4(mc)			!integer*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc+1			!move cursor right
		if(mc.gt.nc-2) then	!reached end of line
		   mc=nc-2			!stay in last col
		   if(mr.lt.nr-2) then     !unless there is another row in disp
			mr=mr+1		   !in which case move to start of it
                  mc=njm+1
		   endif
		endif
		call LOCATEW(1,mr,mc)    !move cursor on
	else if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
c	  At present left arrow will not cause scroll when start of display
c	  reached
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=int4(mr)			!integer*4
		mc1=int4(mc)			!integer*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc-1			!move cursor left
		if(mc.lt.njm+1) then	!reached start of line
		   mc=njm+1			!stay in first col
		   if(mr.gt.2) then     !unless there is another row in disp
			mr=mr-1		   !in which case move to end of prev line
                  mc=nc-2
		   endif
		endif
		call LOCATEW(1,mr,mc)    !move cursor back
c DOWN ARROW
	else if(ktype.eq.8.and.ival.eq.80) then		!DOWN arrow key
c	  At present down arrow will not cause scroll when end of display
c	  reached
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=int4(mr)			!integer*4
		mc1=int4(mc)			!integer*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.lt.nr-2) mr=mr+1			!move cursor down
		call LOCATEW(1,mr,mc)    !move cursor down, same col
	else if(ktype.eq.8.and.ival.eq.72) then		!UP arrow key
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=int4(mr)			!integer*4
		mc1=int4(mc)			!integer*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.gt.2) mr=mr-1
		call LOCATEW(1,mr,mc)    !move cursor up, same col
	else if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=int4(mr)			!integer*4
		mc1=int4(mc)			!integer*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc-1			!move cursor left
		if(mc.lt.njm+1) then	!reached start of line
		   mc=njm+1			!stay in first col
		   if(mr.gt.2) then     !unless there is another row in disp
			mr=mr-1		   !in which case move to end of prev line
                  mc=nc-2
		   endif
		endif
		mr1=mr		!int*4
		mc1=mc		!int*4
		call LOCATEW(1,mr,mc)    !move cursor back
		call GETIJ3(mr1,mc1,in,jn,k,bad)	!get position in CDATA(i,j)
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(.not.bad) then		!char is in box so put in cdata and screen
		   CDATA(in,jn)(k:k)=' '     !put blank in CDATA
		   call BLDB(1,mr1,mc1,' ',inverse)	!put blank in window
		   iflag=1		!data altered
c		   ilast=in		!update posn of last inverse-contrast box
c		   jlast=jn
		endif
c HOME key: Draw boxes 1 to nidisp
	else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
		call CLRW(1)
		nscrol=0
		goto 11
c END key: Draw the last nidisp rows; #ni-nidisp+1 to #ni
	else if(ktype.eq.8.and.ival.eq.79) then		!END key
		newobs=.false.
		nscrol=ni-nidisp			!set (eg for getrc() below)
		call CLRW(1)
	 	call BLDB(1,0,0,title(1:nc-1),normatt)
	 	call BLDB(1,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 42 i=ni-nidisp+1,ni
	 	   call BLDB(1,ir,0,crow(i),normatt)	!='n:'
		   ir=ir+1
	 	do 42 j=1,nj
	 	   call GETRC2(i,j,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
42	 	continue
		in=ni-nidisp+1	!first row on display
		jn=1			!first col
	 	call GETRC2(in,jn,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
c      and put cursor at start of first box on new (bottom) line
		in=nidisp+nscrol	!last row
		jn=1			!first col
	 	call GETRC2(in,jn,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
	 	call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		iw=int4(iw2)
		jw=int4(jw2)
c PgUp KEY. Move up nidisp rows (or to start there are fewer than nidisp to go)
c and leave cursor in bottom right box
	else if(ktype.eq.8.and.ival.eq.73) then		!PG-UP key
c In general i=1+nscrol to i=nidisp+nscrol are in display so NSCROL must be
c in range 0 to (NI-NIDISP). So decrement nscrol by nidisp
		if(nscrol.ge.nidisp) then
		   nscrol=nscrol-nidisp
		else
		   nscrol=0
		endif
		call CLRW(1)
	 	call BLDB(1,0,0,title(1:nc-1),normatt)
	 	call BLDB(1,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 44 i=1+nscrol,nidisp+nscrol
	 	   call BLDB(1,ir,0,crow(i),normatt)	!='n:'
		   ir=ir+1
	 	do 44 j=1,nj
	 	   call GETRC2(i,j,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
44	 	   continue
c       put cursor at start of last box on new (bottom) line
		in=nidisp+nscrol	!last row
		jn=nj			!last col
	 	call GETRC2(in,jn,nidisp,iw,jw,jw2,bad) !get row,col for cdata(i,j)
	 	call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
c		ilast=in		!update posn of last inverse-contrast box
c		jlast=jn
		call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		iw=int4(iw2)
		jw=int4(jw2)
c PgDn KEY. Move down nidisp rows (or to end there are fewer than nidisp to go)
c and leave cursor in top left box
	else if(ktype.eq.8.and.ival.eq.81) then		!PG-DN key
c In general i=1+nscrol to i=nidisp+nscrol are in display so NSCROL must be
c in range 0 to (NI-NIDISP). So increment nscrol by nidisp
		if(nscrol+nidisp.le.ni-nidisp) then
		   nscrol=nscrol+nidisp
		else
  		   nscrol=ni-nidisp			!set for last nidisp values
		endif
c		if(caplock()) call ERRMES(nscrol,nidisp,i,j,k,mr,mc)
		call CLRW(1)
	 	call BLDB(1,0,0,title(1:nc-1),normatt)
	 	call BLDB(1,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 45 i=1+nscrol,nidisp+nscrol
		   if(i.gt.99) print 451,i,nscrol,ni,nidisp
451		   format(' i,nscrol,ni,nidisp = ',4i8)
	 	   call BLDB(1,ir,0,crow(i),normatt)	!='n:'
		   ir=ir+1
	 	do 45 j=1,nj
	 	   call GETRC2(i,j,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	   call BLDB(1,iw,jw,CDATA(i,j),normatt)	!put in existing data
45	 	continue
c       put cursor at start of first box on new (top) line
		in=1+nscrol		!first row
		jn=1			!first col
	 	call GETRC2(in,jn,nidisp,iw,jw,jw2,bad)	!get row,col for cdata(i,j)
	 	call BLDB(1,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		call LOCATEW(1,iw2,jw2)	!put cursor at start of box
		iw=int4(iw2)
		jw=int4(jw2)
	else		!the character input at last
	   if(.not.bad) then		!char is in box so put in cdata and screen
		CDATA(in,jn)(k:k)=ch1     !put character in CDATA
		call BLDB(1,mr1,mc1,ch1,inverse)	!put character in window
		iflag=1		!data altered
c can insert constraint here but if so it is recalc as each digit is changed
c (but not recalc when altered with backspace)
	   endif
	   call LOCATEW(1,mr,mc+1)    !move cursor on(not needed for PRINTW)
	endif
c
c End of key options
c After any key hit, check if cursor is in a different number box from
c that last highlighted, and if so remove highlight from the last one
c IF it is in the window
	call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
	mr1=int4(mr)			!integer*4
	mc1=int4(mc)			!integer*4
	call GETIJ3(mr1,mc1,in,jn,k,bad)	!get i,j in CDATA(i,j)
15	continue  	!jump here when ESC hit (end=true) so constraint set
	if(in.ne.ilast.or.jn.ne.jlast.or.end) then
	   call GETRC2(ilast,jlast,nidisp,iw,jw,jw2,bad)
	   if(.not.bad) then
		call BLDB(1,iw,jw,blank(1:ncn),normatt)
		call BLDB(1,iw,jw,CDATA(ilast,jlast),normatt)
	   endif
c Start of CONSTRAINT: incorporate constrained relationship between
c col 3 and 4 here, after one of them completed (its box has been left)
	   if(iconst.eq.-1) then
c========
c now stuff for iconst>0
	   else if(iconst.gt.0) then
c=======
	   endif		!End of constraint #1
c
c     Reset ilast,jlast
	   ilast=in			!record posn of last inverse-contrast box
	   jlast=jn
	endif
	if(end) goto 99
	GOTO 10	!get another character
c
c EXIT SECTION
99	continue
c now decode CDATA(i,j) into reals in DATA(i,j)
	call CLS
	call LOCATE(0,0)
c output as ascii CDATA in this version
c	do i=1,ni
c	   do j=1,nj
c		call CHTOREAL(cdata(i,j),data(i,j))
c	   enddo
c	enddo
c15	data(i,j)=CHTOREAL(cdata(i,j))
c	if(caplock()) then
c	   do 6 i=1,ni
c	   print 7,i,(cdata(i,j),j=1,nj)
c6	   print 71,i,(data(i,j),j=1,nj)
c7 	   format(i4,2x,5a11)
c71	   format(i4,2x,5g13.6)
c	   pause
c	endif
c
c	if(iflag.eq.1.or.iflagx.eq.1.or.iflagy.eq.1) iflag=1	!set output flag
c
	RETURN		!from INWIND
c
c SEPARATE SECTION FOR F1,F2,F3,F4
c jump to 90 for help screen
90	continue
	call CAPTSCN(scrnbuf2)
c	call OPENWIND(2)
	call CLRB(2)
	call BLDB(2,0,0,'TAB: Move to next number',ihatt)
	call BLDB(2,1,0,'INS or SHFT-TAB: Back to previous number',ihatt)
	call BLDB(2,2,0,'ARROWS:    move round in current window',ihatt)
	call BLDB(2,3,0,'PAGE-DOWN: moves down one screenful',ihatt)
	call BLDB(2,4,0,'PAGE-UP:   moves up one screenful',ihatt)
	call BLDB(2,5,0,'HOME:      moves up to start',ihatt)
	call BLDB(2,6,0,'END:       moves down to end',ihatt)
	call BLDB(2,7,0,'Type # on line after last number to end',ihatt)
	call BLDB(2,8,0,'ENTER or ESC: finished (and leave HELP)',ihatt)
	call BLDB(2,9,0,'F4: Add another observation (row)',ihatt)
	call BLDB(2,10,0,'F5: Abort display',ihatt)
	call BLDB(2,11,0,'CTRL-ARROW: moves whole window left/right',
     &	ihatt)
	call ANYKEY
c	ch1=GETCH(ktype)
c	ival=ichar(ch1)
c	if(ktype.eq.0.and.(ival.eq.60.or.ival.eq.61)) goto 91		!F2 or F3
	call DISPSCN(scrnbuf2)
	GOTO 10
c
c
	END		!end if INWIND



c	subroutine ERRMES(n1,n2,n3,n4,n5,mr,mc)
cc To print debug message outside window
c	integer*2 mr,mc
cc
c	call locate(20,0)
c	print 1,n1,n2,n3,n4,n5
c1	format('&n1,n2,m3,n4,n5= ',5i6)
c	call LOCATEW(1,mr,mc)	!restore position of cursor
c	RETURN
c	end

	subroutine GETRC2(i,j,nidisp,mr,mc1,mc2,bad)
c Given i,j, which define number CDATA(i,j), returns the row number,mr,
c in the current window where this number is displayed, and the first
c and last columns in which digits can be written, mc1 and mc2
c BAD set true if the row number is illegal (eg in row 0,1 which
c are used for title and col headings)
	logical bad
	common/gij/njm,ncn,ni,nj,nscrol,jcol
c
	mr=i+1-nscrol
	bad=mr.le.1.or.mr.gt.nidisp+1
	mc1=njm + 1 + (j-1)*(ncn+2)
	mc2=mc1 + ncn - 1
	RETURN
	end

	subroutine GETIJ3(mr,mc,i,j,k,bad)
c Input cursor position (row,col=mr,mc; int*2), and calculate
c that cursor is in kth character position in CDATA(i,j).
c If cursor is in some part of the window that is not occupied
c by a data value (eg headings or row numbers) then returns
c with bad=true, and i,j unchanged
c	integer*2 mr,mc	!no- convert to int*4 before call if nec
	logical bad
	common/gij/njm,ncn,ni,nj,nscrol,jcol
c
	bad=.false.
c rows 0,1 are title & headings so data(1,j) is on row 2
	if(mr.lt.2.or.mr.gt.24) then
		bad=.true.
		RETURN
	endif
c cols 0,..,njm-1 are occupied by row number (and next col is blank) so
	if(mc.lt.njm+1.or.mc.gt.79) then
		bad=.true.
		RETURN
	endif
c
	itry=mr-1+nscrol
	if(itry.lt.1.or.itry.gt.ni) then
		bad=.true.
	else
		i=itry
	endif
c
c On each row the jth 'box' starts at col # j1=njm + 1 + (j-1)*(ncn+2)
c and ends at col # j2=j1 + ncn - 1. Check all nj boxes
	do 1 jc=1,nj
	jcol=jc
	j1=njm + 1 + (jc-1)*(ncn+2)
	j2=j1 + ncn - 1
	if(mc.ge.j1.and.mc.le.j2) goto 2	!mc is within jth box
1	continue
c If reach here then mc is not within any box
	bad=.true.
	RETURN
c
2	continue	!in jcol
	j=jcol	!position mr,mc is within box for data(i,j)
c position within this box is
	k=mc-j1+1
c
	RETURN
	end


