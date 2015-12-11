	subroutine QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,irate,jrate,itry)
c 06/04/90 10:45pm QWIND2 is routine for QGEN2 that displays window
c with rows labelled 'q(1,2)' etc for all connections, plus two
c columns viz (1) rate constant names (2) numerical values for rates
c
c Modif 01/24/02 07:57am so that now possible to write to first
c character in col 1 (names column) -see getij2
c
c Modif 11/23/01 02:11pm by addition of obeymr(50) in common
c
c Modif 05/19/00 06:46am by adding common/lig/ so that rates marked
c as conc dep by '*' are now, if more than one ligand present,
c marked '*1', '*2' etc.
c Width of first column increased form 8 to 12 to accommodate, at max,
c something like 'ô*1 q(11,12)'.  So make crow*12 and njm=13
c
c New Col numbers are thus:
c 0-12='q(i,j)' label=0 to njm-1
c 13=blank =njm
c 14-24=name (11 char) icol2=njm+1 to (njm+1)+ncn-1
c 25-26=blank
c 27-37=value (11 char) icol3=icol1+13
c 38=blank

c Old Col numbers are thus:
c 0-8='q(i,j)' label=0 to njm-1
c 9=blank
c 10-20=name (11 char)=njm+1 to (njm+1)+ncn-1
c 21-22=blank
c 23-33=value (11 char)
c 34=blank
c
c INPUT:
c When iflag=3 input values of both rate constant names and values shown
c      iflag=2 rate constant names  initially blank, values shown
c      iflag=1 rate constant values initially blank, names shown
c      iflag=0 both cols initially blank.
c OUTPUT:
c IFLAG is set=1 if any value of the DATA array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=0 if NO value of the DATA array has been changed (including
c	no change in either x or y multiplier
c 	The array IQ defines correspondence between param # (index of TITLEP)
c and elements of Q: IQ(i,j)=m where theta(m) is the parameter that goes
c in QT(i,j) (though may be multiplied by a statistical factor in QT)
c 	Use F2 as toggle to swap from column with q(i,j) values (numbers) and
c the column with rate constant names.
c
c ITRY need not be defined before entry. On exit set to zero unless exit
c takes place by hitting F3, in which case set to 1
c
c Problem with inverse contrast boxes:
c	 The call 'BLDB(3,iw,jw,CDATA(in,jn),inverse)' seems to use NBLANK
c	call effectively, so inverse contrast does not cover all 11 characters
c	but only up to the last non-blank character. The only way round this
c	that I have found so far is to set last element, if it is a blank,
c	to nul (=0, rather than blank=32) eg
c	if(ichar(CDATA(in,jn)(11:11)).eq.32) CDATA(in,jn)(11:11)=char(0)
c	but note that nul must be removed again before converting
c	to a number because CHTOREAL uses NBLANK call to decide how long
c	the character is.
c
c Special purpose version of INWIND1 for input of q(i,j) values in QGEN.
c Ask for all values specified by the connections in IC(), except
c for micro rev values are so labelled and values not inserted here
c (they are calc in QSETD but need V-dep etc so better not calc here)
c Windows 1,2 called 3,4 here in case Q matrix already displayed in
c  window 1 on entry
c
c NOTES FOR INWIND1:
c To make a window for input/correction of numerical values
c (e.g. for CVFIT). Version in which window can be moved round screen
c by CTRL-arrow. Top left corner of the window in row=nr1 (0-24) and
c column nc1 (0-79). If nr1 or nc1 is neg on entry use values defined below.
c (ctrl-up,ctrl-down do not work, though initial tests with TKEY indicated
c that they would; only ctrl-left,ctrl-right seem to work now)
c
c Make NJ columns of data values (eg x,y,sd),
c and NI rows of data values (of which NIDISP are on display at a time).
c TITLE is overall title of window
c HEADING is column headings
c Input:
c IFLAG is set=1-5 then input values in DATA are displayed in the first
c	IFLAG columns are filled from data, and the remaining cols (if any)
c	are left blank
c IFLAG is set=0 then display on entry is all blank
c Output:
c IFLAG is set=1 if any value of the DATA array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=0 if NO value of the DATA array has been changed
c
c Numbers entered are up to 11 characters long (now- but can alter size
c in declaration of CDATA and in definition of NCN).
c TAB: Move to next number (scrolls when nec)
c INS or SHIFT-TAB: Move to previous number (scrolls when nec)
c ARROWS: move round in current window to correct values (no scroll)
c PAGE-DOWN: moves down nidisp values (or to last nidisp values)
c PAGE-DOWN: moves up nidisp values (or to first nidisp values)
c HOME: moves up to first nidisp values
c END: moves down to last nidisp values
c ESC or ENTER: finished.
c When all entered hit ESC/ENTER
c The string of values is converted
c to real numbers with CHTOREAL (note that dec point NOT necessary!)
c (See tests in TWIND2)
c Based on earlier version (INWINDOW.FOR,TWIND3) which filled whole window
c before decoding its contents from one long string. This version keeps
c separate character representation of  each number in CDATA.
c	real data(100,nj)
	real data(200)
	real*8 QT(100,100)
	character*11 cdata(200,2),istr,jstr
	integer scrnbuf1(1024),scrnbuf2(1024)
	integer*2 ktype,mr,mc,iw,jw,jw2
c	character*3 crow(99)		!for row numbers up to 99
c	character*9 crow(99)		!for row labels
	character*12 crow(199)		!for row labels
	character*10 titlep(200)		!for parameter names
c	character*1 ch1,getch,pre1,pre2
	character*1 ch1,getch
	character*4 pre1,pre2
	character*79 heading,title,blank
c declarations for qwind2:
	integer IQ(100,100)
	integer irate(200),jrate(200)
	integer IC(2,200)
	integer IM(50,100),JM(50,100),ix(100),jx(100)
	integer imr(50)	!record row #s for micro rev routes
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c
	logical digchar,caplock,bad,bad1,colour
	COMMON/LIG/nlig,IL(100)
	common/gij/njm,ncn,ni1,nj1,nscrol,jcol
      INCLUDE '\lf90\lib\SPINATTI.HDR'		!modif version of SPINATTR.HDR
c      INCLUDE '\hgraph\CONSTS.FOR'	!no Hgraph here!
c
c Some fixed values
	colour=.true.	!for Dell 425
c	colour=.false.	!for Tosh
	itry=0	!unless F3 hit
	title=' Enter rates [1/(Ms) for assoc rates]'
	heading='      i,j    Rate const     Value'
c	heading='01234567890123456789012345678901234'	!col numbers
c
c Keep original screen
	call CAPTSCN(scrnbuf1)
c
c	call CLS(24,0,24,80)	!clear bottom row only (in case model on display)
	call CLRAREA(24,0,24,80)	!clear bottom row only (in case model on display)
	call LOCATE(24,5)
	nj=2		!two columns of alterable characters
	print *,
     & ' F1: HELP   F2: TOGGLE 2nd AND 3rd COLUMNS    F3: REREAD RATES'
c
c Start of initialisation
C Blanks in CDATA initially
	do 4 i=1,ni
	do 4 j=1,nj
          CDATA(i,j)='           '
4	continue
	if(nidisp.gt.21) nidisp=21	!correct input value if nec
c NOW transfer data from QT() to DATA; also define row labels accordingly
	i1=0
	do 8 m=1,ncon
	pre1='    '		!prefix for 'q(i,j)' label
	pre2='    '
	i=IC(1,m)
	j=IC(2,m)
c append * for conc dep param (unless they are also micro rev in which
c case they are so labelled- done below so * overwritten)
	if(ncdep.gt.0) then
	   do n=1,ncdep
	    if(ix(n).eq.i.and.jx(n).eq.j) then
		if(il(n).eq.1) then
		   pre1(2:3)='*1'		!asterisk indicates rate to be mult by conc
		else if(il(n).eq.2) then
		   pre1(2:3)='*2'		!asterisk indicates rate to be mult by conc
		endif
	    else if(ix(n).eq.j.and.jx(n).eq.i) then
		if(il(n).eq.1) then
		   pre2(2:3)='*1'		!asterisk indicates rate to be mult by conc
		else if(il(n).eq.2) then
		   pre2(2:3)='*2'		!asterisk indicates rate to be mult by conc
		endif
	    endif
	   enddo
	endif
c check if either if i,j or j,i is a micro rev route
	if(ncyc.gt.0) then
	   do n1=1,ncyc
		n=isetmr(n1)	!actual cycle number
		if(obeymr(n)) then
		   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre1(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
			imr(n)=i1         !record row # for m.r. route
		   else if(im(n,1).eq.j.and.jm(n,1).eq.i) then
			pre2(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
			imr(n)=i1+1         !record row # for m.r. route
	         endif
		endif
	   enddo
	endif
	call INTCONV(i,istr)
	call INTCONV(j,jstr)
	nis=nblank(istr)
	njs=nblank(jstr)
	i1=i1+1
	DATA(i1)=sngl(QT(i,j))
	crow(i1)=pre1//'q('//istr(1:nis)//','//jstr(1:njs)//')'     !q(i,j)
c param names in cdata(i,1) if req, and QT() values in DATA(), which
c is converted to character form and put in CDATA(i,2) below
c When iflag=3 input values of both rate constant names and values shown
c      iflag=2 rate constant names  initially blank, values shown
c      iflag=1 rate constant values initially blank, names shown
c      iflag=0 both cols initially blank.
c Now omit prefix from parameter name
c	if(iflag.eq.1.or.iflag.eq.3) CDATA(i1,1)=pre1//titlep(i1)
	if(iflag.eq.1.or.iflag.eq.3) CDATA(i1,1)=titlep(i1)
	i1=i1+1
	DATA(i1)=sngl(QT(j,i))
	crow(i1)=pre2//'q('//jstr(1:njs)//','//istr(1:nis)//')'     !q(j,i)
c	if(iflag.eq.1.or.iflag.eq.3) CDATA(i1,1)=pre2//titlep(i1)
	if(iflag.eq.1.or.iflag.eq.3) CDATA(i1,1)=titlep(i1)
8	continue
c Initialise CDATA
c OUTPUT:
c IFLAG is set=1 if any value of the DATA array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=0 if NO value of the DATA array has been changed (including
c	no change in either x or y multiplier
	if(iflag.eq.2.or.iflag.eq.3) then
	   do 5 i=1,ni
c#	      do 5 j=1,nj
c		   call REALTOCH(data(i,j),CDATA(i,j))
		   call REALTOCH(data(i),CDATA(i,2),11)
		   if(caplock()) print 60,i,j,data(i),cdata(i,2),
     &	(ichar(cdata(i,2)(m:m)),m=1,11)
60		   format(' i,j,data,cdata(i,2)= ',2i4,2x,g13.7,2x,a11,/,
     &	' ichar=',11i5)
5	   continue
c	   if(caplock()) pause
	endif
c Set last character of CDATA to nul (if it is blank) so inverse contrast
c fills all 11 characters of cell
	do 62 i=1,ni
	do 62 j=1,2
62	if(ichar(CDATA(i,j)(11:11)).eq.32) CDATA(i,j)(11:11)=char(0)
c
c
	IFLAG=0	!set for output (=no value changed)
c Cursor initially restricted to col 2=numerical values (jcol=2), until
c toggled to col 1 (names) with F2
	JCOL=2
c
c ncn=11 char per number with 2 blank cols between numbers and 1 blank
c col at left and right edges so total width of window =
c (max=5 cols of numbers for 70 col window)
c + 9 cols at left edge for '=q(10,10)'
cc + 3 cols at left edge for row number
	ncn=11
c	njm=3		!number of cols on left for display of row number
c	njm=9		!number of cols on left for display of 'q(i,j)' label
	njm=13	!number of cols on left for display of 'q(i,j)' label
	icol2=njm+1 	!start col for name
	icol3=icol2+13	!start col for value
	ni1=ni	!copy for common
	nj1=nj	!copy for common
c END of initialisation
c
c Define help window (41 col, 14 row), in case needed
c -attributes definedin spinatt.hdr
	if(colour) then
	   ihatt=whitechr+redbkg
	else
	   ihatt=inverse
	endif
c===	call DEFWIND(1,nr1,nc1,nr2,nc2,singlbd,normatt)
	call DEFWIND(4,5,30,21,72,dblbdr+scrol,ihatt)
c Total number of cols needed=
	nc=njm + nj*(ncn+2) + 1		!1 more than above calc actually needed
	do 61 i=1,nc-1
61	blank(i:i)=' '
c Total number of rows needed=number for data entries=ni + 2 more for
c title and column headings  (+ 1 more, as for cols-room for border presumably)
	nr=nidisp+3		!no of rows on display at a time
c Define corners of the window
c If nr1 or nc1 is neg on entry use values defined here
	if(nr1.lt.0.or.nc1.lt.0) then
	   nc1=(79-nc)/2	!start col number (whole screen numbering)
	   if(nc1.lt.0) nc1=0
c#	   nr1=(24-nr)/2	!start row number (whole screen numbering)
	   nr1=0
	   if(nr1.lt.0) nr1=0
	endif
c
130	continue	!return here to redraw window if moved
c
	nr2=nr1+nr  	!end row number (whole screen numbering)
	nc2=nc1+nc        !end col number (whole screen numbering)
c One row for titles, up to 24 at a time for numbers
c	normatt=bright+dblubkg		!'normal' attribute
	normatt=bright+bluebkg		!'normal' attribute
c===	call DEFWIND(1,nr1,nc1,nr2,nc2,singlbd,normatt)
	call DEFWIND(3,nr1,nc1,nr2,nc2,singlbd+scrol,normatt)
	call FILLB(3,' ',current)
c Draw the window
c	call OPENWIND(3)
	nscrol=0	!initialise number of lines scrolled
11	continue
	call BLDB(3,0,0,title(1:nc-1),normatt)
	call BLDB(3,1,0,heading(1:nc-1),normatt)
	ir=2
	do 40 i=1,nidisp
	   call BLDB(3,ir,0,CROW(i),normatt)	!='q(i,j):'
c	   call BLDB(3,ir,10,CDATA(i,1),normatt)	!name of rate constant
c	   call BLDB(3,ir,23,CDATA(i,2),normatt)	!put in existing data
	   call BLDB(3,ir,icol2,CDATA(i,1),normatt)	!name of rate constant
	   call BLDB(3,ir,icol3,CDATA(i,2),normatt)	!put in existing data
	   ir=ir+1
40	continue
c Note that cdata(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
c Start with cursor in 1st place in first number,CDATA(1,1), and wait for
c something to be typed
c NB in,jn used for index of a particular number CDAT(in,jn)
c and mr,mc (or int*4 versions mr1,mc1) used as index of the row
c and column in the displayed window
c Fixed so cursor initially in the numbers column, and stays there unless
c F2 hit, by modif. of GETRC and GETIJ2
	in=1
	jn=jcol		!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
	ilast=in		!record posn of last inverse-contrast box
	jlast=jn
	call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
c Mark position to be filled in by 11 spaces in reverse contrast
	call BLDB(3,iw,jw,CDATA(in,jn),inverse)
	call LOCATEW(3,iw,jw)	!put cursor there
	call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2       why?.........
	mr1=mr			!integer*4
	mc1=mc			!integer*4
	call GETIJ2(mr1,mc1,in,jn,k,bad)	!get position in CDATA(i,j)
c
c NOW GET CHARACTERS
10	ch1=GETCH(ktype)
	ival=ichar(ch1)
c If the value typed is a 0-9 or - + . e or E then type it in
c the window at the current cursor position, then check the cursor position
c (it may have been moved eg with arrow keys) before inserting the character
c into the correct position in CDATA(i,j) . If TAB is typed move on to next
c value or if arrow key typed move round to correct values already typed.
c
	if((ktype.eq.16.and.ival.eq.27).or.
     &  (ktype.eq.4.and.ival.eq.13)) goto 99		!ESC or ENTER to finish
	if(ktype.eq.0.and.ival.eq.59) goto 90	! F1 key=help
	if(ktype.eq.0.and.ival.eq.60) then	   !F2 key=toggle to/from par name col
	   if(jcol.eq.2) then
		jcol=1		!move to 'names' column
	   else
		jcol=2		!move to values column
	   endif
c redraw and start with cursor in 'home' pos
	   call CLRW(3)
	   nscrol=0
	   goto 11
	endif
c
	if(ktype.eq.0.and.ival.eq.61) then ! F3 key=exit to reread QT
	   itry=1
	   goto 99
	endif
c
c Section to move window by CTRL-arrow
	if(ktype.eq.8.and.(ival.eq.116.or.ival.eq.145.or.
     & ival.eq.115.or.ival.eq.141)) then
c##?	   call DISPSCN(scrnbuf1)     !restore orig screen ##
c	   call CLS(nr1,nc1,nr2,nc2)
	   call CLRAREA(nr1,nc1,nr2,nc2)
	   if(iv al.eq.116) nc1=nc1+1	!CTRL-right arrow
	   if(ival.eq.115) nc1=nc1-1	!CTRL-left arrow
	   if(ival.eq.141) nr1=nr1-1	!CTRL-up arrow
	   if(ival.eq.145) nr1=nr1+1	!CTRL-down arrow
c	   if(nr1.lt.0) nr1=0
c	   if(nc1.lt.0) nc1=0
	   goto 130		!redraw window
	endif
c Now take action according to what key hit, and then return to 10 for
c another character
c
c VALID CHARACTER
c	if(digchar(ival)) then
c For names (jcol=1) allow ktype=2 (letters, +,-,* ,etc and also
c blank which has ktype=2,ival=32) or ktype=3 (numbers)
	if(jcol.eq.2.and.digchar(ival)) then
	   if(.not.bad) then		!char is in box so put in cdata and screen
		CDATA(in,2)(k:k)=ch1     !put character in CDATA(i,2)=num values
		call BLDB(3,mr1,mc1,ch1,inverse)	!put character in window
		iflag=1		!data altered
	   endif
	   call LOCATEW(3,mr,mc+1)    !move cursor on(not needed for PRINTW)
      else if(jcol.eq.1.and.(ktype.eq.2.or.ktype.eq.3)) then
	   if(.not.bad) then		!char is in box so put in cdata and screen
		CDATA(in,1)(k:k)=ch1     !put character in CDATA(i,1)=names
		call BLDB(3,mr1,mc1,ch1,inverse)	!put character in window
		iflag=1		!data altered
	   endif
	   call LOCATEW(3,mr,mc+1)    !move cursor on(not needed for PRINTW)
c
c ENTER or TAB
	else if((ktype.eq.8.and.ival.eq.9)) then	!TAB key
c     & (ktype.eq.4.and.ival.eq.13)) then		!TAB or ENTER key
c ENTER or TAB key- move to start of next box (next number=next cdata(i,j)
c Note that cdata(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
		in=in+1
		if(in.gt.ni) in=1	!back to 1st row
		jn=jcol		!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(in.ge.1+nscrol.and.in.le.nidisp+nscrol) then  !i already on display
c		  Code if i already in display (if it is not on display then, since
c		  TAB key moves on one box at a time must either scroll up one row,
c		  or, if end reached, go back to start with i=1 to nidisp on display
		  call GETRC(in,jn,nidisp,iw,jw,jw2,bad1) !get row,col for cdata(i,j)
		  call BLDB(3,iw,jw,CDATA(in,jn),inverse)
		  call LOCATEW(3,iw,jw)
		else if(in.eq.nidisp+nscrol+1) then	!scroll up one line
		   call SCROLLW(3,1)	!scroll up one line
		   nscrol=nscrol+1
		   call BLDB(3,0,0,blank(1:nc-1),normatt)	!delete row 0
		   call BLDB(3,0,0,title(1:nc-1),normatt)	!rewrite headings
		   call BLDB(3,1,0,blank(1:nc-1),normatt)	!delete row 1
		   call BLDB(3,1,0,heading(1:nc-1),normatt)
		   call GETRC(in,jn,nidisp,iw,jw,jw2,bad1) !row,1st col for cdata(i,j)
 		   call BLDB(3,iw,0,crow(in),normatt)	!label
c	   	   call BLDB(3,iw,10,CDATA(in,1),normatt)	!name of rate constant
c	   	   call BLDB(3,iw,23,CDATA(in,2),normatt)	!put in existing data
	   	   call BLDB(3,iw,icol2,CDATA(in,1),normatt)	!name of rate constant
	   	   call BLDB(3,iw,icol3,CDATA(in,2),normatt)	!put in existing data
		   call BLDB(3,iw,jw,CDATA(in,jn),inverse)  !current box=inverse
		   call LOCATEW(3,iw,jw)	!put cursor at start of box
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
		in=in-1
		if(in.lt.1) in=ni	!back to last row
		jn=jcol		!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(in.ge.1+nscrol.and.in.le.nidisp+nscrol) then  !i already on display
c	    Code if i already in display (if it is not on display then, since
c	    SHFT-TAB key moves back one box at a time must either scroll down one row,
c	    or, if start reached, go back to end with i=ni-nidisp+1 to ni on display
		  call GETRC(in,jn,nidisp,iw,jw,jw2,bad1) !get row,col for cdata(i,j)
		  call BLDB(3,iw,jw,CDATA(in,jn),inverse)
		  call LOCATEW(3,iw,jw)
		else if(in.eq.nscrol) then	!scroll down one line
c	  NB for some reason the scroll down, with -1 as arg, does not
c		seem to work like scroll up, but just deletes the whole
c		window. Therefore rewrite whole window for now
cIn general i=1+nscrol to i=nidisp+nscrol are in display, so move back 1 row
c i=nscrol to nidisp+nscrol-1; but as nscrol decremented first use former
c		   call SCROLLW(3,-1)	!scroll down one line
c		   call BLDB(3,0,0,blank(1:nc-1),normatt)	!delete row 0
c		   call BLDB(3,0,0,title(1:nc-1),normatt)	!rewrite headings
c		   call BLDB(3,1,0,blank(1:nc-1),normatt)	!delete row 1
c		   call BLDB(3,1,0,heading(1:nc-1),normatt)
		   nscrol=nscrol-1
c	draw boxes i=nscrol+1 to nidisp+nscrol (nscrol already decremented)
		   call CLRW(3)
	 	   call BLDB(3,0,0,title(1:nc-1),normatt)
	 	   call BLDB(3,1,0,heading(1:nc-1),normatt)
		   ir=2
	 	   do 43 i=nscrol+1,nidisp+nscrol
	 	   call BLDB(3,ir,0,crow(i),normatt)	!='q(i,j)'-start row 2
		   ir=ir+1
	 	   do 43 j=1,nj
	 	   call GETRC(i,j,nidisp,iw,jw,jw2,bad1)  !get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(i,j),normatt)	!put in existing data
43	 	   continue
c put cursor at start of last box on new (top) line
		   in=nscrol+1
		   jn=jcol		!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
	 	   call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		   call LOCATEW(3,iw,jw)	!put cursor at start of box
		else if (in.eq.ni) then
c display the last nidisp values-redraw all!
		   nscrol=ni-nidisp			!set (eg for getrc() below)
c	draw boxes i=ni-nidisp+1 to ni
		   call CLRW(3)
	 	   call BLDB(3,0,0,title(1:nc-1),normatt)
	 	   call BLDB(3,1,0,heading(1:nc-1),normatt)
		   ir=2
	 	   do 41 i=ni-nidisp+1,ni
	   	   call BLDB(3,ir,0,crow(i),normatt)	!='q(i,j):'
	   	   call BLDB(3,ir,9,cdata(i,1),normatt)  !name of rate constant
	 	   call BLDB(3,ir,21,CDATA(i,2),normatt)	!put in existing data
		   ir=ir+1
41	 	   continue
c put cursor at start of last box
		   in=ni
		   jn=jcol		!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
		   call GETRC(in,jn,nidisp,iw,jw,jw2,bad1) !get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		   call LOCATEW(3,iw,jw)	!put cursor at start of box
		endif
	else if(ktype.eq.8.and.(ival.eq.77.or.ival.eq.75.or.
     & ival.eq.80.or.ival.eq.72.or.ival.eq.8)) then
c For arrows( right,left,up,down), and backspace, fix so that if cursor
c enters a new box then box entered is rewritten in inverse contrast; if a box
c is left (ie cursor is in a new box OR no box =bad) then the last inverse
c contrast box is rewritten in normal contrast.
	   call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
	   mr1=mr		!int*4
	   mc1=mc		!int*4
c======
c	   call GETIJ2(mr1,mc1,in,jn,k,bad)	!get i,j in CDATA(i,j)
c	   if(in.ne.ilast.or.jn.ne.jlast.or.bad) then !in another cell, or no cell
c	      call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
c	      if(.not.bad1) then
c		   call BLDB(3,iw,jw,blank(1:ncn),normatt)
c		   call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
c	      endif
c	      ilast=in			!record posn of last inverse-contrast box
c	      jlast=jn
c	   else if((in.ne.ilast.or.jn.ne.jlast).and.(.not.bad)) then
c	      call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
c	      if(.not.bad1) then
c		   call BLDB(3,iw,jw,blank(1:ncn),inverse)
c		   call BLDB(3,iw,jw,CDATA(in,jn),inverse)
c	      endif
c	      ilast=in			!record posn of last inverse-contrast box
c	      jlast=jn
c	   endif
c===
c	   mr1=mr		!int*4
c	   mc1=mc		!int*4
c	   call GETIJ2(mr1,mc1,in,jn,k,bad)	!get i,j in CDATA(i,j)
c	   if(.not.bad.and.(in.ne.ilast.or.jn.ne.jlast)) then	  !in another cell
c	      call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
c	      if(.not.bad1) then
c		   call BLDB(3,iw,jw,blank(1:ncn),normatt)
c		   call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
c	         call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
c		   call BLDB(3,iw,jw,blank(1:ncn),inverse)
c		   call BLDB(3,iw,jw,CDATA(in,jn),inverse)
c	         ilast=in			!record posn of last inverse-contrast box
c	         jlast=jn
c	      endif
c	   endif
cc======
c RIGHT ARROW
c====	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
	   if(ival.eq.77) then		!RIGHT arrow key
c	   At present right arrow will not cause scroll when end of display
c	   reached
c#		call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
c#		mr1=mr		!int*4
c#		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc+1			!move cursor right
		if(mc.gt.nc-2) then	!reached end of line
		   mc=nc-2			!stay in last col
		   if(mr.lt.nr-2) then     !unless there is another row in disp
			mr=mr+1		   !in which case move to start of it
                  mc=njm+1
		   endif
		endif
		call LOCATEW(3,mr,mc)    !move cursor on
		goto 64	!check if new box entered
c
c	else if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
	   else if(ival.eq.75) then		!LEFT arrow key
c	  At present left arrow will not cause scroll when start of display
c	  reached
c#		call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
c#		mr1=mr		!int*4
c#		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc-1			!move cursor left
		if(mc.lt.njm+1) then	!reached start of line
		   mc=njm+1			!stay in first col
		   if(mr.gt.2) then     !unless there is another row in disp
			mr=mr-1		   !in which case move to end of prev line
                  mc=nc-2
		   endif
		endif
		call LOCATEW(3,mr,mc)    !move cursor back
		goto 64	!check if new box entered
c DOWN ARROW
c	else if(ktype.eq.8.and.ival.eq.80) then		!DOWN arrow key
	   else if(ival.eq.80) then		!DOWN arrow key
c	  At present down arrow will not cause scroll when end of display
c	  reached
c#		call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
c#		mr1=mr		!int*4
c#		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.lt.nr-2) mr=mr+1			!move cursor down
		call LOCATEW(3,mr,mc)    !move cursor down, same col
		goto 64	!check if new box entered
c UP ARROW
c	else if(ktype.eq.8.and.ival.eq.72) then		!UP arrow key
	   else if(ival.eq.72) then			!UP arrow key
c#		call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
c#		mr1=mr		!int*4
c#		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.gt.2) mr=mr-1
		call LOCATEW(3,mr,mc)    !move cursor up, same col
		goto 64	!check if new box entered
c BACKSPACE
c	else if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
	   else if(ival.eq.8) then			!BACKSPACE key
c#		call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
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
		call LOCATEW(3,mr,mc)    !move cursor back
		call GETIJ2(mr1,mc1,in,jn,k,bad)	!get position in CDATA(i,j)
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		if(.not.bad) then		!char is in box so put in cdata and screen
		   CDATA(in,jn)(k:k)=' '     !put blank in CDATA
		   call BLDB(3,mr1,mc1,' ',inverse)	!put blank in window
		   iflag=1		!data altered
		endif
	   endif	!end of left,right,down,up,backspace options
c===
c Section to follow arrow and backspace keys to check if box (i,j) has
c changed; if so remove inverse contrast from box last left and make that
c just entered into inverse contrast
64	   continue
	   mr1=mr		!int*4
	   mc1=mc		!int*4
	   call GETIJ2(mr1,mc1,in,jn,k,bad)	!get i,j in CDATA(i,j)
	   if(.not.bad.and.(in.ne.ilast.or.jn.ne.jlast)) then	  !in another cell
	      call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
	      if(.not.bad1) then
		   call BLDB(3,iw,jw,blank(1:ncn),normatt)
		   call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
	         call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
		   call BLDB(3,iw,jw,blank(1:ncn),inverse)
		   call BLDB(3,iw,jw,CDATA(in,jn),inverse)
	         ilast=in			!record posn of last inverse-contrast box
	         jlast=jn
	      endif
	   endif
c HOME key: Draw boxes 1 to nidisp
	else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
		call CLRW(3)
		nscrol=0
		goto 11
c END key: Draw the last nidisp rows; #ni-nidisp+1 to #ni
	else if(ktype.eq.8.and.ival.eq.79) then		!END key
c
		nscrol=ni-nidisp			!set (eg for getrc() below)
		call CLRW(3)
	 	call BLDB(3,0,0,title(1:nc-1),normatt)
	 	call BLDB(3,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 42 i=ni-nidisp+1,ni
	   	   call BLDB(3,ir,0,crow(i),normatt)	!='q(i,j):'
		   ir=ir+1
	 	do 42 j=1,nj
	 	   call GETRC(i,j,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(i,j),normatt)	!put in existing data
42	 	continue
		in=ni-nidisp+1	!first row on display
	 	call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	call BLDB(3,iw,jw,CDATA(in,jn),inverse)	!put in existing data
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
		call CLRW(3)
	 	call BLDB(3,0,0,title(1:nc-1),normatt)
	 	call BLDB(3,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 44 i=1+nscrol,nidisp+nscrol
	   	   call BLDB(3,ir,0,crow(i),normatt)	!='q(i,j):'
c#	   	   call BLDB(3,ir,9,cdata(i,2),normatt)  !name of rate constant
		   ir=ir+1
	 	do 44 j=1,nj
	 	   call GETRC(i,j,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(i,j),normatt)	!put in existing data
44	 	   continue
c       put cursor at start of last box on new (bottom) line
		in=nidisp+nscrol	!last row
		jn=jcol	!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
	 	call GETRC(in,jn,nidisp,iw,jw,jw2,bad1) !get row,col for cdata(i,j)
	 	call BLDB(3,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		call LOCATEW(3,iw,jw)	!put cursor at start of box
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
		call CLRW(3)
	 	call BLDB(3,0,0,title(1:nc-1),normatt)
	 	call BLDB(3,1,0,heading(1:nc-1),normatt)
		ir=2
	 	do 45 i=1+nscrol,nidisp+nscrol
	   	   call BLDB(3,ir,0,crow(i),normatt)	!='q(i,j):'
		   ir=ir+1
	 	do 45 j=1,nj
	 	   call GETRC(i,j,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	   call BLDB(3,iw,jw,CDATA(i,j),normatt)	!put in existing data
45	 	continue
c       put cursor at start of first box on new (top) line
		in=1+nscrol		!first row
		jn=jcol	!=1 for name in cdata(1,1); =2 for number is cdata(1,2)
	 	call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)	!get row,col for cdata(i,j)
	 	call BLDB(3,iw,jw,CDATA(in,jn),inverse)	!put in existing data
		call LOCATEW(3,iw,jw)	!put cursor at start of box
	endif
c
c End of key options
c After any key hit, check if cursor is in a different number box from
c that last highlighted, and if so remove highlight from the last one
c IF it is in the window
	call GETPOSW(3,mr,mc)	!NB mr,mc must be int*2
	mr1=mr			!integer*4
	mc1=mc			!integer*4
	call GETIJ2(mr1,mc1,in,jn,k,bad)	!get i,j in CDATA(i,j)
c	if(.not.bad.and.(in.ne.ilast.or.jn.ne.jlast)) then
	if(in.ne.ilast.or.jn.ne.jlast) then
	   call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
	   if(.not.bad) then
		call BLDB(3,iw,jw,blank(1:ncn),normatt)
		call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
	   endif
c Start of CONSTRAINT: incorporate constrained relationship here
c          End of constraint #1
c     Reset ilast,jlast
	   ilast=in			!record posn of last inverse-contrast box
	   jlast=jn
	endif
	GOTO 10	!get another character
c
c EXIT SECTION
99	continue
c now decode CDATA(i,j) into reals in DATA(i,j); but first remove any
c terminal null characters (see above) to blanks before calling CHTOREAL
	do 63 i=1,ni
	do 63 j=1,2
63	if(ichar(CDATA(i,j)(11:11)).eq.0) CDATA(i,j)(11:11)=char(32)
c
	call CLS
	call LOCATE(0,0)
	do 15 i=1,ni
15	call CHTOREAL(cdata(i,2),data(i))
c	if(caplock()) then
c	   do 6 i=1,ni
c	   print 7,i,(cdata(i,j),j=1,nj)
c6	   print 71,i,(data(i,j),j=1,nj)
c7 	   format(i4,2x,5a11)
c71	   format(i4,2x,5g13.6)
c	   pause
c	endif
c
c Finally put DATA() values back into QT; also put param names in CDATA(i,2),
c which may have been altered back into TITLEP, and define IQ too.
c 	The array IQ defines correspondence between param # (index of TITLEP)
c and elements of Q: IQ(i,j)=m where theta(m) is the parameter that goes
c in QT(i,j) (though may be multiplied by a statistical factor in QT)
	call QZERO(QT,100)		!zero 1st in case
	do i=1,100		!ditto
	   irate(i)=0
	   jrate(i)=0
	   do j=1,100
		IQ(i,j)=0
	   enddo
	enddo
c
	i1=0
	do m=1,ncon
	   i=IC(1,m)
	   j=IC(2,m)
	   i1=i1+1
	   irate(i1)=i
	   jrate(i1)=j
	   QT(i,j)=dble(DATA(i1))
	   IQ(i,j)=i1
c=	   titlep(i1)=CDATA(i1,1)(2:11)
	   titlep(i1)=CDATA(i1,1)(1:11)
	   i1=i1+1
	   irate(i1)=j
	   jrate(i1)=i
	   QT(j,i)=dble(DATA(i1))
	   IQ(j,i)=i1
c	   titlep(i1)=CDATA(i1,1)(2:11)
	   titlep(i1)=CDATA(i1,1)(1:11)
	enddo
c
	RETURN		!from QWIND2
c
c SEPARATE SECTION for F1
c jump to 90 for help screen
90	continue
	call CAPTSCN(scrnbuf2)
c	call OPENWIND(4)
	call CLRB(4)
	call BLDB(4,0,0,'TAB:          move to next number',current)
	call BLDB(4,1,0,'INS/SHFT-TAB  back to previous number',current)
	call BLDB(4,2,0,'ARROWS:       move in current window',
     &	current)
	call BLDB(4,3,0,'PAGE-DOWN:    moves down one screenful',current)
	call BLDB(4,4,0,'PAGE-UP:      moves up one screenful',current)
	call BLDB(4,5,0,'HOME:         moves up to start',current)
	call BLDB(4,6,0,'END:          moves down to end',current)
	call BLDB(4,7,0,'BACKSPACE:    erase',current)
	call BLDB(4,8,0,'ESC or ENTER: finished (and leave HELP)',current)
	call BLDB(4,10,0,'F2: move to/from rate constant name column',
     &	current)
	call BLDB(4,11,0,'F3: exit to read rate constants again     ',
     &	current)
	call BLDB(4,12,0,'CTRL-ARROW: moves whole window left/right',
     &	current)
	call BLDB(4,13,0,'*n = rate multiplied by conc of ligand #n',
     &  current)
	call BLDB(4,14,0,
     & char(244)//' = rate det. by microscopic reversibility',
     & current)
	call ANYKEY
c	ch1=GETCH(ktype)
c	ival=ichar(ch1)
	call DISPSCN(scrnbuf2)
	GOTO 10
c
c
	END		!end if INWIND



	subroutine ERRMES(n1,n2,n3,n4,n5,mr,mc)
c To print debug message outside window
	integer*2 mr,mc
c
	call locate(20,0)
	print 1,n1,n2,n3,n4,n5
1	format('&n1,n2,m3,n4,n5= ',5i6)
	call LOCATEW(3,mr,mc)	!restore position of cursor
	RETURN
	end

	subroutine GETRC(i,j,nidisp,mr,mc1,mc2,bad1)
c Given i,j, which define number CDATA(i,j), returns the row number,mr,
c in the current window where this number is displayed, and the first
c and last columns in which digits can be written, mc1 and mc2
c BAD set true if the row number is illegal (eg in row 0,1 which
c are used for title and col headings)
c Cursor initially restricted to col 2=numerical values (jcol=2), until
c toggled to col 1 (names) with F2. To restrict to 2nd col pretend that
c nj=1 and njm=21 (ie only one col with margin of 21 characters). To restrict
c to col 1 pretend nj=1 and njm=9
c New col numbers are thus:
c 0-12='q(i,j)' label=0 to njm-1
c 13=blank =njm
c 14-24=name (11 char) icol2=njm+1 to (njm+1)+ncn-1
c 25-26=blank
c 27-37=value (11 char) icol3=icol1+13
c 38=blank
c
	logical bad1
	integer*2 mr,mc1,mc2
	common/gij/njm,ncn,ni,nj,nscrol,jcol
c
	mr=int2(i+1-nscrol)
	bad1=mr.le.1.or.mr.gt.nidisp+1
c
	mc1=int2(njm + 1 + (j-1)*(ncn+2))   !=col 23-33 for j=2, 10-20 for j=1
	mc2=mc1 + int2(ncn - 1)
	RETURN
	end

	subroutine GETIJ2(mr,mc,i,j,k,bad)
c Input cursor position (row,col=mr,mc; int*2), and calculate
c that cursor is in kth character position in CDATA(i,j).
c If cursor is in some part of the window that is not occupied
c by a data value (eg headings or row numbers) then returns
c with bad=true, and i,j unchanged
c   Cursor initially restricted to col 2=numerical values (jcol=2), until
c toggled to col 1 (names) with F2. To restrict to 2nd col pretend that
c nj=1 and njm=21 (ie only one col with margin of 21 characters). To restrict
c to col 1 pretend nj=1 and njm=9
c	integer*2 mr,mc	!no- convert to int*4 before call if nec
	logical bad
	common/gij/njm,ncn,ni,nj,nscrol,jcol
c
c
	bad=.false.
c rows 0,1 are title & headings so data(1,j) is on row 2
	if(mr.lt.2.or.mr.gt.24) then
		bad=.true.
		RETURN
	endif
c Cols 0,..,njm-1 are occupied by row number (and next col is blank) so
c set bad if outside  this region. When 'jcol=1' also set bad for the
c the first character of the param name which is reserved for *,= etc
c Modif 01/24/02 07:57am so that now possible to write to first
c character in col 1 (names column)
c	if((jcol.eq.2.and.mc.lt.njm+1).or.
c     & (jcol.eq.1.and.mc.lt.njm+2).or.
c     & mc.gt.79) then
	if((jcol.eq.2.and.mc.lt.njm+1).or.
     & (jcol.eq.1.and.mc.lt.njm+1).or.
     & mc.gt.79) then
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


