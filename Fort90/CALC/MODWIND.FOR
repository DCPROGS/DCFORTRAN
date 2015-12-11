c========================================================================
	subroutine MODWIND(charmod,nrow,ncol,iflag,ilast,jlast,ic)
c========================================================================
c Subroutine to input a mechanism in character form (rather like INWIND1)
c Each row has char*2 cells.
c
c Modif 01/29/02 12:42pm
c Add colour of window as a parameter (ic=0 for usual blue;
c    ic=1 for green)
c
c Each cell on 1st,3rd,...row contains either (a) 'Cn' or 'On'
c where C=shut state, O=open state, n=0,1,...,9 (where 0 is interp as 10)
c denotes state number
c OR
c (b) a ' -', or '- ', to denote connection of states horizontally
c The 2nd,4th,... rows contain cells that are blank, or contain
c '| ' for vertical connection of states in cells above and below
c or '/ ', or '\ ' for diagonal connections
c Input:
c IFLAG is set=1 then input values in CHARMOD are displayed
c IFLAG is set=2 then input values in CHARMOD are displayed but routine
c  left immediately leaving display (so can be seen while entering q(i,j))
c IFLAG is set=< 0 then display on entry is all blank
c Output:
c IFLAG is set=0 if any value of the CHARMOD array has been changed (though
c	changes that restore orig number will set iflag
c IFLAG is set=1 if NO value of the CHARMOD array has been changed
c
c  j=column number;  j1=(j+1)/2 = char col number (each char takes 2 cols)
c
c=	character*2 charmod(20,30),ch2
	character*2 charmod(25,40),ch2
	integer scrnbuf1(1024),scrnbuf2(1024)
	character*1 UC,ch1,getch
	logical valchar,state,caplock,bad,colour
	integer*2 ktype,mr,mc,iw,jw
	common/grc/nr1,nc1,nr2,nc2
      INCLUDE 'c:\lf90\lib\SPINATTi.HDR'		!modif version of SPINATTR.HDR

	colour=.true.	!for Dell 425
c 	Keep original screen
	call CAPTSCN(scrnbuf1)
	call CLS
c	call CLRAREA(24,0,24,80)  !clear bottom row only (in case model on display)
c	call locate(24,5)
	if(iflag.ne.2) print *,' F1=help  ESC=exit'
c 	Define help window (41 col, 12 row), in case needed
c 	-attributes definedin spinatt.hdr
	if(colour) then
	   ihatt=whitechr+redbkg
	else
	   ihatt=inverse
	endif
	call DEFWIND(2,5,30,22,72,dblbdr+scrol,ihatt)
c
	if(iflag.le.0) then
	   do 4 i=1,nrow
	   do 4 j=1,ncol
          charmod(i,j)='  '
4	   continue
	endif
c ncn=2 char per cell
	ncn=2
	nidisp=nrow
	nr=nrow
	nc=ncol
	ncmax=ncn*(ncol-1)
	ni=nrow
	nj=ncol
c END of initialisation
c
	if(ic.eq.0) then
	   light=bright+bluebkg
	else if(ic.eq.1) then
	   light=bright+greenbkg
	endif
	idark=bright+blackbkg
c	normatt=bright+bluebkg		!'normal' attribute
	if(ic.eq.0) then
	   normatt=bright+bluebkg		!'normal' attribute
	else if(ic.eq.1) then
	   normatt=bright+greenbkg		!'normal' attribute
	endif
c==	nr1=1
c==	nc1=1
	nr1=1
	nc1=0
	nr2=nrow
	nc2=ncn*ncol
	call DEFWIND(1,nr1,nc1,nr2,nc2,singlbd,normatt)
	call FILLB(1,' ',current)		!needed for border to appear?
      call CLRB(1)       !diff
c Initially make chessboard pattern- states in light squares, connections
c in dark (colour will deviate from chessboard later if connections span
c more than one cell)
	do jn=1,ncol
	do in=1,nrow-2
	if((iflag.le.0).or.in.gt.ilast.or.jn.gt.jlast) then
	   call CELLCOL(in,jn,iw,jw,iattrib,light,idark)
	   call BLDB(1,iw,jw,'  ',iattrib)
	else
c	   ival=ichar(charmod(in,jn)(1:1))
c	   call CHARVAL(ival,ktype,valchar,state)
	   ch1=UC(charmod(in,jn)(1:1))
	   iattrib=idark
         if(ch1.eq.'C'.or.ch1.eq.'O') iattrib=light
	   if(state) iattrib=light
	   call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
	   call BLDB(1,iw,jw,charmod(in,jn),iattrib)
	endif
c60	continue
	enddo
	enddo
c
c
11	continue
	in=1		!number is charmod(1,1)
	jn=1
	call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
	call LOCATEW(1,iw,jw)	!put cursor there
	call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
	iw4=int4(iw)
	jw4=int4(jw)
	mr1=int4(mr)			!integer*4
	mc1=int4(mc)			!integer*4
	call GETIJ1(mr,mc,in,jn,k,bad)	!get position in CHARMOD(i,j)
c
c NOW GET CHARACTERS
	if(iflag.eq.2) goto 999		!return leaving display
	iflag=1		!to indicate NO changes to the input CHARMOD
10	ch1=GETCH(ktype)
	ival=ichar(ch1)
c If the value typed is a c,C,o,O,0-9, or -,|,/,\ or blank (space)then type it
c in the window at the current cursor position, then check the cursor position
c (it may have been moved eg with arrow keys) before inserting the character
c into the correct position in CHARMOD(i,j) . If TAB is typed move on to next cell
c or if arrow key typed move round to correct values already typed.
c
	if(ktype.eq.16) goto 99				!ESC to end
	if(ktype.eq.0.and.ival.eq.59) goto 90	! F1 key=help
c
	call CHARVAL(ival,ktype,valchar,state)
c VALID CHARACTER
	if(valchar) then
	   if(.not.bad) then	!char is in box so put in CHARMOD and screen
		if(state) then
		   CHARMOD(in,jn)(k:k)=ch1     !put character in CHARMOD
		   call BLDB(1,mr1,mc1,ch1,light)	!put character in window
		   call LOCATEW(1,mr,mc+1)    !move cursor on(not needed for PRINTW)
		else
		   call SETCHAR(ktype,ival,ch2)
		   CHARMOD(in,jn)=ch2     !put character in CHARMOD
c==
c Fixed so 2-char value inserted in cell properly whether cursor was
c at the 1st OR 2nd postion in the cell
		   call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
		   iw4=int4(iw)
		   jw4=int4(jw)
		   call BLDB(1,iw4,jw4,ch2,idark)	!put character in window
		   mr=iw		!int*2
		   mc=jw
		   call LOCATEW(1,mr,mc+2)    !move cursor on(not needed for PRINTW)
c==
c		   call BLDB(1,mr1,mc1,ch2,idark)	!put character in window
c		   call LOCATEW(1,mr,mc+2)    !move cursor on(not needed for PRINTW)
c==
		endif
		iflag=0		!data altered
	   endif
c
c ENTER or TAB
	else if((ktype.eq.8.and.ival.eq.9).or.
     &	(ktype.eq.4.and.ival.eq.13)) then		!TAB or ENTER key
c ENTER or TAB key- move to start of next box (next number=next CHARMOD(i,j)
c Note that CHARMOD(i,j) for i=1 to nidisp are on display initially, but
c after cursor reaches last value display scrolls up one place (and nscrol
c set=1) so i=2 to nidisp+1 on display. In general i=1+nscrol to i=nidisp+nscrol
c are in display so NSCROL must be in range 0 to (NI-NIDISP)
		jn=jn+1
		if(jn.gt.nj) then
		   jn=1
		   in=in+1
		   if(in.gt.ni) in=1	!back to 1st row
		endif
c		if(caplock()) call ERRMES(in,jn,ilast,jlast,k,mr,mc)
		call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
	     iw4=int4(iw)
	     jw4=int4(jw)
c		call BLDB(1,iw,jw,CHARMOD(in,jn),inverse)
		call LOCATEW(1,iw,jw)	!just move cursor
c		iw=int4(iw2)
c		jw=int4(jw2)
c
c INS or SHIFT-TAB
	else if((ktype.eq.8.and.ival.eq.15).or.
     &	(ktype.eq.8.and.ival.eq.82)) then		!SHFT-TAB or INS key
c INS or SHFT-TAB key- move to start of prev box (prev number=prev CHARMOD(i,j)
c Note that CHARMOD(i,j) for i=1 to nidisp are on display initially, but
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
		call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
c		call BLDB(1,iw,jw,CHARMOD(in,jn),inverse)
		call LOCATEW(1,iw,jw)
c
c RIGHT ARROW
	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
c	  At present right arrow will not cause scroll when end of display
c	  reached
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=mr		!int*4
		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc+1			!move cursor right
c		if(mc.gt.nc) then		!reached end of line
		if(mc.gt.ncmax) then		!reached end of line
c		   mc=nc			!stay in last col
		   mc=ncmax			!stay in last col
		   if(mr.lt.nr) then     !unless there is another row in disp
			mr=mr+1		   !in which case move to start of it
                  mc=0
		   endif
		endif
		call LOCATEW(1,mr,mc)    !move cursor on
	else if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=mr		!int*4
		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		mc=mc-1			!move cursor left
		if(mc.lt.0) then	!reached start of line
		   mc=0			!stay in first col
		   if(mr.gt.0) then     !unless there is another row in disp
			mr=mr-1		   !in which case move to end of prev line
                  mc=nc
		   endif
		endif
		call LOCATEW(1,mr,mc)    !move cursor back
c DOWN ARROW
	else if(ktype.eq.8.and.ival.eq.80) then		!DOWN arrow key
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=mr		!int*4
		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.lt.nr) mr=mr+1			!move cursor down
		call LOCATEW(1,mr,mc)    !move cursor down, same col
	else if(ktype.eq.8.and.ival.eq.72) then		!UP arrow key
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=mr		!int*4
		mc1=mc		!int*4
c		if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
		if(mr.ge.1) mr=mr-1
		call LOCATEW(1,mr,mc)    !move cursor up, same col
	else if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
c when backspace hit, move to start of current cell and fill both
c characters with blanks
		call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
		mr1=mr			!integer*4
		mc1=mc			!integer*4
		call GETIJ1(mr,mc,in,jn,k,bad) !get position in CHARMOD(i,j)
		call GETRC1(in,jn,iw,jw,jw2,bad) !get row,col for CHARMOD(i,j)
		call LOCATEW(1,iw,jw)	!put cursor there
		call CELLCOL(in,jn,iw,jw,iattrib,light,idark)
		call BLDB(1,iw,jw,'  ',iattrib)		!blank the display
		CHARMOD(in,jn)='  '     !put blank in CHARMOD (both char)
c	   After erasing current cell, move cursor back to prev cell,so
c	   can erase continuously
		jw=jw-1			!move cursor left
		if(jw.lt.0) then	!reached start of line
		   jw=0			!stay in first col
		   if(iw.gt.0) then     !unless there is another row in disp
			iw=iw-1		   !in which case move to end of prev line
                  jw=nc
		   endif
		endif
		mr=iw		!int*2
		mc=jw
		call LOCATEW(1,mr,mc)    !move cursor back
	endif
c
c End of key options
cc After any key hit, check if cursor is in a different number box from
cc that last highlighted, and if so remove highlight from the last one
cc IF it is in the window
	call GETPOSW(1,mr,mc)	!NB mr,mc must be int*2
	mr1=mr			!integer*4
	mc1=mc			!integer*4
	call GETIJ1(mr,mc,in,jn,k,bad)	!get i,j in CHARMOD(i,j)
	GOTO 10	!get another character
c
c EXIT SECTION
99	continue
c	pause
	call CLS
999	continue	!exit leaving model on display
	call LOCATE(0,0)
c
	RETURN	!from MODWIND
c
c SEPARATE SECTION for F1
c jump to 90 for help screen
90	continue
	call CAPTSCN(scrnbuf2)
c	call OPENWIND(2)
	call CLRB(2)

	call BLDB(2,0,0,'States are numbered 1-99 ',current)
	call BLDB(2,1,0,'Enter open states as o1,o2,. . .,o9'
     &	,current)
	call BLDB(2,2,0,'    (must have lowest numbers)'
     &	,current)
	call BLDB(2,3,0,'Enter shut states as c3,c4,...,c9,',current)
	call BLDB(2,4,0,'  then as 10, 11, . . .99',current)
	call BLDB(2,5,0,'Enter horizontal connections as - (minus)',
     &	current)
	call BLDB(2,6,0,'Enter vertical as |(or ! for Win95)',current)
	call BLDB(2,7,0,'Enter corners with Home,End,PgUp,PgDn',current)
	call BLDB(2,8,0,' NB corners must join lines not states',current)
	call BLDB(2,9,0,'ENTER or TAB: Move to next cell',current)
	call BLDB(2,10,0,'INS or SHFT-TAB: Back to previous cell',current)
	call BLDB(2,11,0,'ARROWS:   move round in current window',current)
	call BLDB(2,12,0,'BACKSPACE:  erase cells',current)
	call BLDB(2,13,0,'ESC:      finished (and leave HELP)',current)
	call BLDB(2,14,0,'CTRL-ARROW: moves whole window left/right',
     &	current)
	call ANYKEY
	call DISPSCN(scrnbuf2)
	GOTO 10
c
	END         !of MODWIND

	subroutine GETRC1(in,jn,iw,jw,jw1,bad)
c Given in,jn, which define number CHARMOD(in,jn), returns the row number,iw,
c in the current window where this value is displayed, and the first
c and last columns in which char can be written, jw and jw1
c BAD set true if the row number is illegal
	integer*2 iw,jw
	logical bad
	common/grc/nr1,nc1,nr2,nc2
c
	iw=in-1		!numbering for window rows
	jw=2*(jn-1)		!numbering for window cols=0,2,...,78
	jw1=jw+1		!for 2 char per cell
	bad=iw.lt.nr1.or.jw.lt.nc1.or.iw.gt.nr2.or.jw1.gt.nc2
	RETURN
	end


	subroutine GETIJ1(iw,jw,in,jn,k,bad)
c Input cursor position (row,col=iw,jw; int*2), and calculate
c that cursor is in kth character position in CHARMOD(i,j).
	integer*2 iw,jw
	logical bad
c
	bad=.false.
	in=iw+1
	jn=1+jw/2
c cells start in cols 0,2,4,..., ie even values of jw imply k=1; odd imply k=2
	jw4=int4(jw)
	k=1+mod(jw4,2)
c
	RETURN
	end

	subroutine CHARVAL(ival,ktype,valchar,state)
c Input is ival=ichar(ch) found in call to GETCH
c If the value typed is a c,C,o,O,0-9, or -,|,/,\ or blank (space)then
c VALCHAR=true
	integer*2 ktype
	logical state,valchar
c
	valchar=.false.
	state=.false.
	if((ktype.eq.2.or.ktype.eq.3).and.
     &   ((ival.ge.48.and.ival.le.57).or.
     &   (ival.eq.67.or.ival.eq.99).or.
     &   (ival.eq.79.or.ival.eq.111))) then
	   valchar=.true.    !0-9,  D,c  or O,o resp
	   state=.true.      !characters specify a 'state' rather than a 'connection'
	endif
c
      if((ktype.eq.3.and.ival.eq.45).or.
     &   (ktype.eq.2.and.ival.eq.124).or.
     &   (ktype.eq.2.and.ival.eq.33).or.
     &   (ktype.eq.8.and.
     &   (ival.eq.71.or.ival.eq.73.or.ival.eq.79.or.ival.eq.81)).or.
     &   (ival.eq.32)) then
	   valchar=.true.       !- or | resp (or ! for vertical in Win95)
	   state=.false.
	endif
	RETURN
	end

	subroutine SETCHAR(ktype,ival,ch2)
	integer*2 ktype
	character*2 ch2
c To set characters for connections
	if(ktype.eq.3.and.ival.eq.45) ch2=char(196)//char(196)      !horizontal
	if(ktype.eq.2.and.ival.eq.124) ch2=char(32)//char(179)      !vertical
	if(ktype.eq.2.and.ival.eq.33) ch2=char(32)//char(179)      !vertical (=! for Win95)
	if(ktype.eq.8.and.ival.eq.71) ch2=char(32)//char(218) !home=top left
	if(ktype.eq.8.and.ival.eq.73) ch2=char(196)//char(191) !pgUp=top right
	if(ktype.eq.8.and.ival.eq.79) ch2=char(32)//char(192) !end=bottom left
	if(ktype.eq.8.and.ival.eq.81) ch2=char(196)//char(217) !pgDn=bottom right
	RETURN
	end


	subroutine CELLCOL(in,jn,iw,jw,iattrib,light,idark)
	integer*2 iw,jw
c To get attribute (colour) for cell containing CHARMOD(in,jn)
	jw=2*(jn-1)		!numbering for window cols=0,2,...,78
	iw=in-1		!numbering for window rows
c For 1st,3rd,... rows start with light box
	if(mod(in,2).eq.1) then
	   if(mod(jn,2).eq.1) then
		iattrib=light
	   else
		iattrib=idark
	   endif
	else			!rows 2,4,.... start with dark cell
	   if(mod(jn,2).eq.1) then
		iattrib=idark
	   else
		iattrib=light
	   endif
	endif
	RETURN
	end

