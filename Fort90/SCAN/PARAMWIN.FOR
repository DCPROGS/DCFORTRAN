	subroutine PARAMWIN(nvals,names,values,helpstr,
     & title1,ialter,nalter,icolor,jfix)
c To make alphanumeric mode window (WIND3 library) for checking, and altering,
c default values of a series of named parameters (based in INWIND1)
c NB THIS VERSION NEEDS KTYPE TO BE DECLARED AS INTEGER*4 -THIS PROBLEM
c SHOULD HAVE BEEN FIXED!!
c
c Modif 04/08/98 03:22pm -extra parameter jfix(nvals) added -as before if
c jfix(i)=0, but if jfix(i)=1 then paramater values can't be changed
c Modif 08/01/94 02:42pm by replacing ni in call with ndisp=number of rows
c on display at a time (max=22) and nvals=total number of values;
c If nrow>22:
c nrfst=row number at top of initial display
c pgup/pgdn moves to next page.
c Also icolor in call controls colour thus:
c  icolor=1:   normatt=brite+yellochr+dblubkg
c  icolor=2:   normatt=brite+dbluchr+lblubkg
c
c Two columns: on left have names of parameters, e.g.
c     'Number of points above critical level'
c: on right have current values (which are defaults) e.g. '2'
c (all ASCII -right hand column only can be edited)
c character*n1 NAMES(nvals) contains names for right column
c character*i1 VALUES(nvals) contains values for left column
c title*i3 contains title for the window (set i3=0 for no title)
	character*60 names(nvals)
	character*11 values(nvals),cnum
	character*165 helpstr(nvals)
	integer jfix(nvals)
	character title1*(*),title2*70
c	character names*(*)
c	character values*(ni)
c	character title*(*)
	integer*4 ktype
	integer*2 irow,jcol,jcolr1,jcolr2,jmax1,jmax2,ilast
	character*1 ch1,getch,ch2,UC
	logical digchar,valid,yesno
	integer*2 ichange(100),istpos(100),ialter(nvals)
	integer scrnbuf1(1024)			!make allocatable?
      INCLUDE 'c:\lf90\lib\SPINATTi.HDR'		!modif version of SPINATTR.HDR
	common/rj/nj	!common for RJUSTIF
c
	nalter=0		!no input values altered
c Keep original screen
	call CAPTSCN(scrnbuf1)
	call SETATTR(0)	!util3 library -to ensure black screen
	call CLS
c If nvals=22 or more then nidisp=22, ndisp=23
	if(nvals.lt.23) then
	   ndisp=nvals+1	!+1 for help line at bottom
	   nidisp=nvals	!excluding bottom line
	else
	   ndisp=23
	   nidisp=22
	endif
c
c Number of pages needed
	npage=1 + (nvals-1)/22
	if(npage.gt.1) then
	   nlast=nvals-(npage-1)*nidisp	!# of values on last page
	else
	   nlast=nidisp
	endif
	ipage=1		!current page on display
	nidisp1=nidisp	!number of values on current page
c
c Get max width of left column
	nmax=0
	do i=1,nvals
	   ichange(i)=0
	   n=NBLANK(names(i))
	   if(n.gt.nmax) nmax=n
	enddo
	jmax1=int2(nmax)
c Get max width of right column
c -no -make R column 11 characters wide always!
	jmax2=11
	jcolr1=jmax1+2	!1st col # for right column
	jcolr2=jcolr1+jmax2-1	!last col # for right column
c Right-justify all values (so 'inverse' spans all 11 cols -otherwise
c inverse attribute goes only up to last character -does not extend to
c trailing blanks))
	do i=1,nvals
	   call RJUSTIF(values(i),11)
c	   istpos(i)=nj	!posn of 1st char after right justif, from COMMON
	   istpos(i)=int2(nj)+jcolr1-1 !posn of 1st char in R column, from COMMON
	enddo
c
c Total width =jmax1+2(for gap)+jmax2+1(for gap)
	jwid=int4(jmax1+2+jmax2+1)
c cols=0 to 79 -centre the window
	jleft=(80-jwid)/2
	jright=jleft+jwid
	if(jleft.lt.0.or.jright.gt.79) then
	   print*,'WIDTH TOO GREAT'
	   call BELL(2)
	   pause
	endif
	if(jleft.lt.0) jleft=0
	if(jright.gt.79) jright=79
	if(icolor.eq.1) then
	   normatt=bright+bluebkg
	else
	   normatt=bright+whitebkg
	endif

c	if(icolor.eq.1) then
c	   normatt=brite+yellowchr+bluebkg
c	else
c	   normatt=brite+bluechr+whitebkg
c	endif
	ihatt=whitechr+redbkg		!for help menu
	call DEFWIND(1,0,jleft,ndisp+1,jright,
     &	singlbd+wrap+scrol,normatt)
	call DEFWIND(2,6,30,18,72,wrap+dblbdr+scrol,ihatt)
	n=NBLANK(title1)
	title2=title1(1:n)//': page 1'
	nt=NBLANK(title2)
	call TITLE(1,title2(1:nt),top,center1,brite+lbluchr+dblubkg)
	call CLRB(1)	!draws empty window with title
c Show the first ndisp values (page 1)
	do i=1,nidisp
	   call BLDB(1,i-1,0,names(i)(1:jmax1),normatt)		!names in L column
	   call BLDB(1,i-1,jcolr1,values(i)(1:jmax2),normatt)	!values in R column
	enddo
c Call to LOCATE seems to upset positioning of inverse contrast area
c so better put this text into window 1?
c	call LOCATE(24,5)
c	print *,
c     & ' F1=HELP; ESC or ENTER to exit;'
	call BLDB(1,ndisp-1,0,
     & 'F1=HELP; PgUp/PgDn=page; ENTER or ESC=quit',ihatt)
c Define irow=row on displayed page (1-ndisp for each page)
c and jrow=index in names() etc (1-nvals).
	irow=0	!irow=current column on display (1st row=1 whatever page #)
c	jrow=1	!index in names() or values().
c
c	jcol=jcolr2	 !jcol=current cursor pos (col #) -at R end of field
	jcol=istpos(1)	!cursor on 1st character
	call LOCATEW(1,irow,jcol)
	ilast=-1	!last row not defined first time
	iplast=-1	!last page not defined first time
c===nvals or ndisp or nidisp
	call SETINVERS(irow,ilast,values,normatt,inverse,jcolr1,jmax2,
     &	istpos,nvals,ipage,iplast)
c
c Now get character
10	continue
	ch1=GETCH(ktype)
	ival=ICHAR(ch1)
	jn=int4(jcolr2-jcolr1+1)	!position of rightmost character in values(ir)
	j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
	ch2=values(j)(jn:jn)		!rightmost char
	yesno=UC(ch2).eq.'Y'.or.UC(ch2).eq.'N'
c If the value typed is a 0-9 or - + . e or E then type it in
c the window at the current cursor position, then check the cursor position
c (it may have been moved eg with arrow keys) before inserting the character
c into the correct position in CDATA(i,j) . If TAB is typed move on to next value or if arrow
c key typed move round to correct values already typed.
c
cc If the value typed is a 0-9 or - + . e or E then type it in
cc the window at the current cursor position, then check the cursor position
cc (it may have been moved eg with arrow keys) before inserting the character
cc into the correct position in CDATA(i,j) . If TAB is typed move on to next value or if arrow
cc key typed move round to correct values already typed.
c  Any character except following is put in R column, and into 'values'
c  TAB moves to next 'value' (next row)/down arrow
c  SHIFT-TAB moves to previous row/up arrow
c
	if(ktype.eq.16.or.ival.eq.13) goto 99		!ESC or ENTER to end
	if(ktype.eq.0.and.ival.eq.59) then		!F1 key=help
c	   call HELPMEN(helpstr(irow+1),ihatt)
	   call HELPMEN(helpstr(j),ihatt)
	   goto 10
	else if((ktype.eq.8.and.ival.eq.9).or.	!TAB
     &     (ktype.eq.8.and.ival.eq.80)) then	!down arrow
	   irow=irow+1
cc	   if(irow.gt.ndisp-1) irow=0	!back to top
c	   if(irow.gt.ndisp-2) irow=0	!exc bottom line=help
	   if(irow.gt.nidisp1-1) irow=0	!exc bottom line=help
	   j=irow+1 + nidisp*(ipage-1)
c	   jcol=istpos(irow+1)	!cursor on 1st character
	   jcol=istpos(j)		!cursor on 1st character
	   call LOCATEW(1,irow,jcol)
	   call SETINVERS(irow,ilast,values,normatt,inverse,jcolr1,
     &	jmax2,istpos,nvals,ipage,iplast)
	else if((ktype.eq.8.and.ival.eq.15).or.	!SHIFT-TAB
     &     (ktype.eq.8.and.ival.eq.72)) then	!up arrow
	   irow=irow-1	!========check when scroll needed!!
cc	   if(irow.lt.0) irow=ndisp-1	!back to bottom (row numbered from 0)
c	   if(irow.lt.0) irow=ndisp-2	!exclude bottom line=help
	   if(irow.lt.0) irow=nidisp1-1	!exclude bottom line=help
	   j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
c	   jcol=istpos(irow+1)	!cursor on 1st character
	   jcol=istpos(j)		!cursor on 1st character
	   call LOCATEW(1,irow,jcol)
	   call SETINVERS(irow,ilast,values,normatt,inverse,jcolr1,
     &	jmax2,istpos,nvals,ipage,iplast)
c ARROW (left/right only) move cursor within field, ie cols jcolr1 to jcolr2
	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
	   jcol=jcol+1	!move cursor right
	   if(jcol.gt.jcolr2) jcol=jcolr1	!back to 1st col
	   call LOCATEW(1,irow,jcol)
	else if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
	   jcol=jcol-1	!move cursor right
	   if(jcol.lt.jcolr1) jcol=jcolr2 	!go to end of field
	   call LOCATEW(1,irow,jcol)
	else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
	   jcol=jcolr1		!current cursor pos (col #)
	   call LOCATEW(1,irow,jcol)
	else if(ktype.eq.8.and.ival.eq.79) then		!END key
	   jcol=jcolr2		!current cursor pos (col #)
	   call LOCATEW(1,irow,jcol)
	else if(ktype.eq.8.and.(ival.eq.73.or.ival.eq.81)) then	!PG-UP/DN key
	   if(ival.eq.73) ipage=ipage-1		!PgUp
	   if(ival.eq.81) ipage=ipage+1		!PgDn
	   if(ipage.lt.1) ipage=npage
	   if(ipage.gt.npage) ipage=1
	   jrfst=1 + nidisp*(ipage-1)        !index in names(), values()
	   if(ipage.eq.npage) then
		jrlast=nvals
		nidisp1=nlast	!# of values on last page
	   else
		jrlast=jrfst+nidisp-1			!last value displayed
		nidisp1=nidisp	!# of values on current page
	   endif
         call INTCONV(ipage,cnum)
	   title2(nt:nt)=cnum(1:1)		!new page number
	   call TITLE(1,title2(1:nt),top,center1,brite+lbluchr+dblubkg)
	   call CLRB(1)	!draws empty window with title
c Show the first ndisp values (page #=ipage)
	   do j=jrfst,jrlast
		i=j-jrfst+1		!starts at 1 (i-1 starts at 0)
		call BLDB(1,i-1,0,names(j)(1:jmax1),normatt)	!names in L column
		call BLDB(1,i-1,jcolr1,values(j)(1:jmax2),normatt)	!values in R column
	   enddo
c    redraw help
	   call BLDB(1,ndisp-1,0,
     &	 'F1=HELP; PgUp/PgDn=page; ENTER or ESC=quit',ihatt)
c
	   irow=0
	   j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
c	   jcol=istpos(irow+1)	!cursor on 1st character
	   jcol=istpos(j)		!cursor on 1st character
	   call LOCATEW(1,irow,jcol)
	   call SETINVERS(irow,ilast,values,normatt,inverse,jcolr1,
     &	 jmax2,istpos,nvals,ipage,iplast)
c	else if(ktype.eq.8.and.ival.eq.81) then		!PG-DN key
c	   irow=int2(ndisp-2)	!last line=help so goto next-last
c	   jcol=istpos(irow+1)	!cursor on 1st character
c	   call LOCATEW(1,irow,jcol)
c	   call SETINVERS(irow,ilast,values,normatt,inverse,jcolr1,
c     &	jmax2,ni,istpos)
	else if(ktype.eq.2.and.ival.eq.32.and.yesno) then  !SPACE=toggle Y/N
	   jn=int4(jcolr2-jcolr1+1)	!position of rightmost character in values(ir)
	   j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
	   if(jfix(j).ne.0) then	!CAN'T CHANGE THIS VALUE
		call BELL(2)
		goto 10	!next character
	   endif
c	   ch2=values(irow+1)(jn:jn)		!rightmost char
	   ch2=values(j)(jn:jn)		!rightmost char
	   if(UC(ch2).eq.'Y') then
		ch2='N'
	   else if(UC(ch2).eq.'N') then
		ch2='Y'
	   endif
c	   values(irow+1)(jn:jn)=ch2
c	   ichange(irow+1)=1
	   values(j)(jn:jn)=ch2
	   ichange(j)=1
	   call BLDB(1,irow,jcolr2,ch2,inverse)
	else if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE
	   jn=int4(jcol-jcolr1+1)	!position of character in values(ir)
	   j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
	   values(j)(jn:jn)=char(32)
	   ichange(j)=1
	   call BLDB(1,irow,jcol,char(32),inverse)
	   if(jcol.gt.jcolr1) then
		jcol=jcol-1
	      call LOCATEW(1,irow,jcol)
	   endif
	else		!any other character goes into 'values'
c Check that a number is not replaced by a letter (except e,E) ie digchar)
	   jn=int4(jcol-jcolr1+1)	!position of character in values(ir)
	   j=irow+1 + nidisp*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
	   if(jfix(j).ne.0) then	!CAN'T CHANGE THIS VALUE
		call BELL(2)
		goto 10	!next character
	   endif
	   ival1=ICHAR(values(j)(jn:jn))	!ival for char being replaced
	   valid=((digchar(ival1).and.digchar(ival)).or. !replace with number only
     &	(.not.digchar(ival1)))	!not number -replace with anything
c check if value=Y or N -if so allow replacement only with Y/N
	   jn1=int4(jcolr2-jcolr1+1)	!position of rightmost character in values(ir)
	   ch2=values(j)(jn1:jn1)		!rightmost char
	   if(UC(ch2).eq.'Y'.or.UC(ch2).eq.'N') then
	      valid=(UC(ch1).eq.'Y'.or.UC(ch1).eq.'N')
	   endif
c If value being replaced is a space, then have no check on what is valid
c so allow anything!
	   if(ival1.eq.32) valid=.true.
	   if(valid) then
	      values(j)(jn:jn)=ch1
	      ichange(j)=1
	      call BLDB(1,irow,jcol,ch1,inverse)
	      if(jcol.lt.jcolr2) then
		   jcol=jcol+1
	         call LOCATEW(1,irow,jcol)
		endif
	   endif
	endif
	goto 10	!next character
c
99	continue
	call DISPSCN(scrnbuf1)     !restore orig screen
c
c Convert all values to left-justified before return
	do i=1,nvals
	   call LJUSTIF(values(i),11)
	enddo
c Count number of altered values, and set output array, ialter()
	nalter=0
	do i=1,nvals
	   if(ichange(i).eq.1) then
		nalter=nalter+1
		ialter(nalter)=i
	   endif
	enddo
c
	RETURN
c
	end


	subroutine SETINVERS(irow,ilast,values,normatt,inverse,j1,j2,
     &	istpos,nvals,ipage,iplast)
c To set inverse contrast for number in R column for row where cursor is located
c (returns this row # in IROW)
c and also to remove inverse contrast for same area on row #ilast
c Contrast is set in j2 columns, starting at col j1, within window 1
c NB rows are numbered starting at zero but values(i) start at 1
c NB if backspace is used so value is no longer right-justif, then
c write with normatt extends only to rightmost character and leaves
c blanks at right end highlighted when field is left; therefore
c call RJUSTIF again before removing highlight
c iplast=last page (we are still in same box only if irow=ilast AND
c ipage=iplast)
	character*11 values(nvals)
	integer*2 irow,j1,j2,ilast,istpos(100)
	common/rj/nj	!common for RJUSTIF
c
c	call GETPOSW(1,irow,jc)
	if(irow.eq.ilast.and.ipage.eq.iplast) then
	   RETURN	!no action needed
	else
c restore normal contrast for last box drawn
	   if((ilast.ge.0.and.ilast.le.24).and.(ipage.eq.iplast)) then
	   	j=ilast+1 + 22*(iplast-1)        !=ilast+1 for page 1 (22=nidisp)
c		call RJUSTIF(values(ilast+1),11)
		call RJUSTIF(values(j),11)
c	      istpos(ilast+1)=int2(nj)+j1-1 !update posn of 1st character
	      istpos(j)=int2(nj)+j1-1		!update posn of 1st character
		call BLDB(1,ilast,j1,values(j)(1:j2),normatt)
	   endif
c draw ne box in inverse contrast
	   j=irow+1 + 22*(ipage-1)        !=irow+1 for page 1 (22=nidisp)
c	   call BLDB(1,irow,j1,values(irow+1)(1:j2),inverse)
	   call BLDB(1,irow,j1,values(j)(1:j2),inverse)
	   ilast=irow
	   iplast=ipage
	endif
c
	RETURN
	end

	subroutine HELPMEN(helpstr,ihatt)
	character*165 helpstr
	integer scrnbuf2(1024)	!make allocatable?
c
	call CAPTSCN(scrnbuf2)
c	call OPENWIND(2)
	call CLRB(2)
	n=NBLANK(helpstr)
	call BLDB(2,0,0,helpstr(1:41),ihatt)
	call BLDB(2,1,0,helpstr(42:82),ihatt)
	call BLDB(2,2,0,helpstr(83:123),ihatt)
	call BLDB(2,3,0,helpstr(124:164),ihatt)
	call BLDB(2,6,0,'TAB/SHFT-TAB or UP/DN: move between lines',ihatt)
	call BLDB(2,7,0,'PgUP/PgDN: move up/down one page',ihatt)
	call BLDB(2,8,0,'ARROWS/BACKSP/HOME/END: to edit entries',ihatt)
	call BLDB(2,9,0,'SPACE BAR: toggles Y/N',ihatt)
	call BLDB(2,10,0,'Hit ANY KEY to leave HELP',ihatt)
	call ANYKEY
	call DISPSCN(scrnbuf2)
	RETURN
	end

