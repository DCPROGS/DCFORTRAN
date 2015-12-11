c*****Contains routines for scrolling dialogue box:
c Modif 01/12/95 02:56pm
c (a) to check validity of drive in DISCDIALOG
c (b) increase max rows=60 boxes=10 (previously 30,2)
c QDIALOG modified 01/22/95 07:01pm to switch on NUMLOCK (as in INPUTi etc)
c
c Define a box:
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c Draw the box defined by DEFDIALOG
c	subroutine OPENDIALOG(ibox,icf,shad)
c Write next line of text to box
c	subroutine WDIALOG(ibox,text,ict)
c Write question to box -ASCII result in CANS (see DEFOLT, GETINP)
c	subroutine QDIALOG(ibox,text,defolt,ict,cans) (cans must be at least *25)
c	subroutine QDIALOG1(ibox,text,defolt,nchar,ict,cans)
c QDIALOG1 is version if QDIALOG that accepts exactly nchar characters
c and then leaves without needing ENTER
c Get a title (like TITENT1)
c	subroutine TDIALOG(ibox,query,title,nlen,blank,ict)
c Get a disc (like DISCNUM1)
c	subroutine DISCDIALOG(ibox,query,idisc,ndev,iask,idef,ict)
c Clear the box and stored text lines
c	subroutine CLRDIALOG(ibox,icb)
c Scroll up and down contents of box #ibox
c	subroutine SCROLDIAL(ibox)
c Close and erase the box
c	subroutine ENDDIALOG(ibox,icb)
c
c SIMPLE NON-SCROLLING TEXT BOXES (defined by DEFBOX, OPENBOX etc
c but can have many such boxes on screen because previously-written text
c not saved)
c	subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
c Draw the box defined by DEFBOX
c	subroutine OPENBOX(ibox,icf,shad)
c Write text to box
c	subroutine WRITBOX(ibox,text,nrow,ict)
c Clear the box
c	subroutine CLRBOX(ibox,icb)
c Close and erase the box
c	subroutine ENDBOX(ibox,icb)

	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c To define (but not yet draw) a window for text dialogue in graphics mode
c Box has top left corner at row, column=irhi,iclo (irhi=0,29; iclo=0,79)
c nrow, ncol=number of rows and cols of text to fit in box
c Modified 06/15/93 10:29am so all ndmax (=60 at present) rows of text written
c in WDIALOG or QDIALOG are stored in dtext before any are overwritten; this
c allows ability to scroll back in these two subroutines. Number of rows
c stored=ndtext(ibox); element of dtext currently on 1st line of box=ifirst
c ibox=box number (1 - 10 at present)
c Frame colour=icf
c Background colour inside box=icb
c nrow=number of rows of text to be held in box
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c If irhi=1 and nrow=4, bottom line of frame is at
c  iylo-1=396 pixels (396/480=82.5%)
c
c	In VGA each text line occupies 16 pixels (letters occupy 11 pixels
c with space of 5 pixels between lines).  The top line (row 0) has
c letters with bases at iy=467 pixels, and the bottom line (row 29) has
c letters with bases at iy=3 pixels
c Thus letters in row #irow have bases at iy=3+(29-irow)*16
c	There are 80 columns of characters (icol=0,..,79) each occupying 8 pixels
c with col zero starting at ix=0.  In general ix for start (left edge) of
c character in column icol is at ix=8*icol (ie last col starts at ix=632)
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
c NB this common need never appear in calling progs (ixlo is nothing to
c do with Hgraph viewport!).
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c (ir1=last screen row written to)
c
	j=ibox
	irh(j)=irhi		!copy for COMMON
	icl(j)=iclo
	nro(j)=nrow
	nco(j)=ncol
	ib(j)=icb
c Initialise idrow for use in WDIALOG
	do i=1,60
	   idrow(i,j)=-1
	enddo
c Initialise
	ndmax=60		!can keep/display 60 rows at present
	ndtext(j)=0		!none kept yet
c
	RETURN
	end


	subroutine OPENDIALOG(ibox,icf,shad)
c To DRAW a window (prev defined by DEFDIALOG) for text dialogue in graphics mode
c Box has top left corner at row, column=irhi,iclo (irhi=0,29; iclo=0,79)
c nrow, ncol=number of rows and cols of text to fit in box
c Modified 06/15/93 10:29am so all ndmax (=60 at present) rows of text written
c in WDIALOG or QDIALOG are stored in dtext before any are overwritten; this
c allows ability to scroll back in these two subroutines. Number of rows
c stored=ndtext(ibox); element of dtext currently on 1st line of box=ifirst
c ibox=box number (1-10 at present)
c Frame colour=icf
c Background colour inside box=icb
c nrow=number of rows of text to be held in box
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c If irhi=1 and nrow=4, bottom line of frame is at
c  iylo-1=396 pixels (396/480=82.5%)
c
c	In VGA each text line occupies 16 pixels (letters occupy 11 pixels
c with space of 5 pixels between lines).  The top line (row 0) has
c letters with bases at iy=467 pixels, and the bottom line (row 29) has
c letters with bases at iy=3 pixels
c Thus letters in row #irow have bases at iy=3+(29-irow)*16
c	There are 80 columns of characters (icol=0,..,79) each occupying 8 pixels
c with col zero starting at ix=0.  In general ix for start (left edge) of
c character in column icol is at ix=8*icol (ie last col starts at ix=632)
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	logical shad
c NB this common need never appear in calling progs (ixlo is nothing to
c do with Hgraph viewport!).
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c (ir1=last screen row written to)
c
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	ncol=nco(j)
	icb=ib(j)
c
	margin=5	!5 pixels
	ixlo(j)=iclo*8 - margin
	ixhi(j)=ixlo(j) + ncol*8 + margin
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo(j)=8*(ixlo(j)/8)		!round down
	ixhi(j)=8*(ixhi(j)/8)		!round down
c add another 16 to iyhi because 3+(29-irhi) is posn for BASE of text
c -actually adding 10 better
	iyhi(j)=3 + (29-irhi)*16 +10 + margin
	iylo(j)=iyhi(j) - nrow*16 - margin
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
	if(shad) then
c	   iwid=ifix(abs(ixhi(j)-ixlo(j))/30)
	   iwid=ifixr((abs(ixhi(j)-ixlo(j))+abs(iyhi(j)-iylo(j)))/100.)
	   if(iwid.lt.2) iwid=2
	   if(iwid.gt.4) iwid=4
	   call SHADOW(ixlo(j),ixhi(j),iylo(j),iyhi(j),iwid,icf) !draw shadow border
	else
	   iwid=0		!signal to ENDDIALOG that frame drawn
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iylo(j)-1,icf)	!draw frame
	   call VLINE(ixhi(j)+1,iylo(j)-1,iyhi(j)+1,icf)
c	   call HLINE(ixhi(j),ixlo(j),iyhi(j),icf)	!hline draws 'backwards' OK
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iyhi(j)+1,icf)
c	   call VLINE(ixlo(j),iyhi(j),iylo(j),icf)	!but vline does not
	   call VLINE(ixlo(j)-1,iylo(j)-1,iyhi(j)+1,icf)
	endif
c
c If there was already text in this box, re-write it
	if(ndtext(j).gt.0) then
	   do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ic=(idrow(ir,j))
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	endif
	RETURN
	end

	subroutine CLRDIALOG(ibox,icb)
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c To clear a dialog box by filling with background colour=icb
c and reset so next write to box is on top line (but no simple way
c to keep prev dtext so this is lost, and cannot still scroll back
c in QDIALOG, TDIALOG in this version)
c NB usually call with icb=0 to get black background
	j=ibox
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
c Start dtext again (if next bit omitted, then box reappears as if no
c CLRDIALOG done the next time something is written to it!)
	do i=1,60
	   idrow(i,j)=-1
	enddo
	ndtext(j)=0	!none kept yet
	RETURN
	end


	recursive subroutine WDIALOG(ibox,text,ict)
c To write scrolling text in a dialogue box opened with OPENDIALOG
c Modif 02/07/94 07:58am so that if text has zero length, returns doing nothing
c Modif 11/28/93 09:32pm so that if ict is negative, the line is
c initially typed as normal, but is removed from DTEXT (ie NOT stored)
c before leaving, but are overwritten by the next call to WDIALOG (QDIALOG)
c (problem with this if ict=0 so sign not available -overcome by
c also removing any line of text written in background colour, i.e. erased)
c
c Keeps text already written to row i in DTEXT(i)
c Sets idrow(i)=ict of row i has already been written (in colour=ict)
c When WDIALOG is called it writes TEXT to the first available blank
c row.  If all rows already full it writes to the bottom row, and scrolls
c other rows up.
	character*(*) text
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c
	j=ibox
	ncol=nco(j)
	n=NBLANK(text)
	if(n.eq.0) RETURN
	nsav=n
	if(n.gt.ncol) n=ncol
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	icb=ib(j)
	if(ndtext(j).lt.ndmax) then
	   ndtext(j)=ndtext(j) + 1
	   irow=ndtext(j)
	   idrow(irow,j)=int2(ict)			!this row now used
	   dtext(irow,j)=text(1:n)			!keep the text in it
	else
	   do i=1,ndmax-1
		idrow(i,j)=idrow(i+1,j)
		dtext(i,j)=dtext(i+1,j)		!scroll up, losing dtext(1,j)
	   enddo
	   idrow(ndmax,j)=int2(ict)
	   dtext(ndmax,j)=text(1:n)			!save new one in last element
	   irow=ndmax
	endif
c
c Display in the box the last nrow (or fewer) dtext lines
	if(ndtext(j).lt.nrow) then	!irow=first unused row in box, from above
	   ir1=irow+irhi-1 			!last screen row written to
	   call SET_CURSOR(iclo,ir1)          !args = col,row
	   ic=iabs(ict)
	   call SET_STRING(dtext(irow,j)(1:n),ic)
c	   call SET_STRING(text(1:n),ict)
	   ifirst(j)=1				!first row displayed is dtext(1,j)
	else
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear box
	   ifirst(j)=ndtext(j)-nrow+1
	   do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
c		ic=iabs(idrow(ir,j))		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	endif
c If text was too long for box, write 2nd line!
	if(nsav.gt.ncol) then
	   icul=iabs(ict)
	   call WDIALOG(ibox,text(ncol:nsav),icul)
	endif
c Before leaving, check for, and remove, any lines ic=idrow negative,
c or for which it is same as background colour;
c adjust ndtext accordingly
	in=0
	do i=1,ndtext(j)
	   ic=idrow(i,j)
	   if(ic.ge.0.and.ic.ne.icb) then
		in=in+1
		idrow(in,j)=ic
		dtext(in,j)=dtext(i,j)
	   endif
	enddo
	ndtext(j)=in
c
	RETURN
	end

	subroutine QDIALOG(ibox,text,defolt,ict,cans)
c To write scrolling text in a dialogue box opened with OPENDIALOG.
c
c Modified 11/10/97 08:38am by adding common/def/usedef where usedef is
c set true if defaults used
c Modified 01/22/95 07:01pm to switch on NUMLOCK (as in INPUTi etc)
c Modif 02/10/94 06:23pm so that in case where defolt=Y or N, then enter
c  returns defolt as before, but 0/INS returns (and writes to screen)
c  the opposite response. (NB assumes defolt was set by DEFOLTa
c  so default= ' [Y] ?')
c Modif 02/07/94 07:58am so that if text has zero length, returns at once
c   Modif 01/08/94 10:50am so that if text colour (ict)=background colour
c (to erase message) then does not wait for input, just writes question
c and returns straight away.
c   Modif 11/28/93 09:32pm so that if ict is negative, the line is
c initially typed as normal, but is removed from DTEXT (ie NOT stored)
c before leaving, but are overwritten by the next call to WDIALOG (QDIALOG)
c (problem with this if ict=0 so sign not available -overcome by
c also removing any line of text written in background colour, i.e. erased)
c   Modif 12/01/93 12:36pm so that if <enter> hit straight away then
c default answer is put on screen.
c As WDIALOG but asks a question, and waits for answer. The answer
c is written to screen in same colour as question.
c     CANS= answer in ASCII form (character*12)
c If UP arrow is hit while waiting for display, box scrolls up
c If DOWN arrow is hit while waiting for display, box scrolls down
c Modif 10/19/93 09:30am to include common/hlp/help so that sets
c  help=true if F1 hit, otherwise false.
c Any other key returns display to how it was when called, and is
c treated as the answer
	character*(*) text,defolt,cans
	character*1 getch,ch,def,UC
	character*78 dtx,dtext(60,10)  !up to 60 rows
	character*30 defolt1
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	logical help,noenter,erase,yesno,isnum,numon,usedef
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
	common/hlp/help
	common/def/usedef
c
	numon=isnum()	!num lock is on already on
	if(.not.numon) call NUMSET	!Spindrift library
c
	help=.false.
	usedef=.false.	!set true if nothing altered -default used (in common)
	noenter=.true.	!before any response typed in
c Is it a yes/no question?
	n=NBLANK(defolt)
	yesno=n.ge.3.and.(defolt(1:3).eq.' [Y'.or.defolt(1:3).eq.' [N')
	if(yesno) then
	   def=UC(defolt(3:3))		!='Y' or 'N'
	else
	   def=' '
	endif
	erase=iabs(ict).eq.iabs(icb)	!text in background colour=erase
	do i=1,LEN(cans)
	  cans(i:i)=char(32)
	enddo
c
	n=NBLANK(text)
	if(n.eq.0) goto 999		!RETURN
	n1=NBLANK(defolt)
	if(n1.eq.0) then
	   dtx=text(1:n)//' = '
	else
	   dtx=text(1:n)//defolt(1:n1)//' '	!leave space after '=' or '?'
	endif
c Make copy of defolt with brackets, = and ? removed to write on screen
c if RETURN hit
	j=0
	defolt1='                             '
	do i=1,n1
	   iv=ichar(defolt(i:i))
	   if(iv.ne.61.and.iv.ne.63.and.iv.ne.91.and.iv.ne.93) then
		j=j+1
		defolt1(j:j)=defolt(i:i)
	   endif
	enddo
	nd=j
c
	n=NBLANK(dtx)
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	icb=ib(j)
c First store and write TEXT as in WDIALOG
	if(ndtext(j).lt.ndmax) then
	   ndtext(j)=ndtext(j) + 1
	   irow=ndtext(j)
	   idrow(irow,j)=int2(ict)			!this row now used
	   dtext(irow,j)=dtx			!keep the text in it
	else
	   do i=1,ndmax-1
		idrow(i,j)=idrow(i+1,j)
		dtext(i,j)=dtext(i+1,j)		!scroll up, losing dtext(1,j)
	   enddo
	   idrow(ndmax,j)=int2(ict)
	   dtext(ndmax,j)=dtx			!save new one in last element
	   irow=ndmax
	endif
c Display in the box the last nrow (or fewer) dtext lines
	if(ndtext(j).lt.nrow) then	!irow=first unused row in box, from above
	   call SET_CURSOR(iclo,irow+irhi-1)          !args = col,row
	   ic=iabs(ict)
	   n5=NBLANK(dtx)
	   call SET_STRING(dtx(1:n5),ic)
c===	   call SET_STRING(text,ic)
	   ifirst(j)=1				!first row displayed is dtext(1,j)
	   ilast=ndtext(j)
	   ir1=irow+irhi-1	!for use below
	else
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear box
	   ifirst(j)=ndtext(j)-nrow+1
	   ilast=ndtext(j)
	   do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
c		ic=iabs(idrow(ir,j))		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	endif
c
c Next read the answer to the question into CANS
	ifst=ifirst(j)
	i=0
3	continue	!return for next character
	if(erase) then	!ie text=background col=erase
	   ival=13		! so write (erase) defolt and leave
	   ktype=4
	else
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	endif
	if(ival.eq.3.and.ktype.eq.32) STOP	!^c
	if(ival.eq.13.and.ktype.eq.4) then	!finished (write defolt to screen
	   if(noenter.and.nd.gt.0) then
	     usedef=.true.	!nothing altered -default used (in common)
	     n=NBLANK(dtext(irow,j))	!before leaving if nothing else entered)
	     call SET_CURSOR(iclo+n,ir1)  !args = col,row
	     icul=iabs(ict)
	     call SET_STRING(defolt1(1:nd),icul)	!write defolt to screen
	     dtext(irow,j)=dtext(irow,j)(1:n)//defolt1(1:nd) !and add to dtext
	   endif
	   goto 99
	else if(yesno.and.((ktype.eq.3.and.ival.eq.48).or.
     &	(ktype.eq.8.and.ival.eq.82))) then		!zero/INS hit
	   if(def.eq.'Y') then
		cans(1:1)='N'			!so take non-default answer
	   else if(def.eq.'N') then
		cans(1:1)='Y'
	   endif
	   if(noenter) then
	     usedef=.true.	!nothing altered -default used (in common)
	     n=NBLANK(dtext(irow,j))	!before leaving if nothing else entered)
	     call SET_CURSOR(iclo+n,ir1)  !args = col,row
	     icul=iabs(ict)
	     call SET_STRING(' '//cans(1:1),icul)	!write defolt to screen
	     dtext(irow,j)=dtext(irow,j)(1:n)//' '//cans(1:1) !and add to dtext
	   endif
	   goto 99
	else if(ival.eq.59.and.ktype.eq.0) then	!F1=help
	   help=.true.
	   goto 99
	else if(ival.eq.72.and.ktype.eq.8) then	!scroll up
c 	   decrement rows of dtext to be displayed, until 1st is
c	   at top of the box
	   if(ifst.eq.1) goto 3		!can't scroll up any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst-1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else if(ival.eq.80.and.ktype.eq.8) then	!scroll down
	   if(ilst.ge.ilast) goto 3	!can't scroll down any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst+1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else		!input answer to question
	   if(ifst.ne.ifirst(j)) then	!text has been scrolled -restore it
		do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		   n=NBLANK(dtext(ir,j))
		   ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	         call SET_CURSOR(iclo,ir1)          !args = col,row
		   ndro=int4(idrow(ir,j))
		   ic=iabs(ndro)		!======iabs()!
		   call SET_STRING(dtext(ir,j)(1:n),ic)
		enddo
	   endif
	   if((ival.eq.8.or.ival.eq.83).and.ktype.eq.8) then  !backspace/del
		if(i.gt.0) then
c		   ir1=
		   n=NBLANK(dtext(irow,j))
      	   call SET_CURSOR(iclo+n-1,ir1)          !args = col,row
		   icul=iabs(ict)
		   call SET_STRING(' ',icul)		!write blank to screen
		   cans(i:i)=char(32)	!replace current character with blank
		   i=i-1
		   dtext(irow,j)(n:n)=' '	!and blank last char of dtext
		endif
	   else
c		if(i.ge.12) goto 99
		noenter=.false.	!response typed in -do not use default
		i=i+1
		cans(i:i)=ch	!accumulate result
		n=NBLANK(dtext(irow,j))
	      call SET_CURSOR(iclo+n,ir1)  !args = col,row
		icul=iabs(ict)
		call SET_STRING(ch,icul)			!write character to screen
		dtext(irow,j)=dtext(irow,j)(1:n)//ch	!and add to dtext
	   endif
	endif
	goto 3	!get next character
c
99	continue
c
c Before leaving, check for, and remove, any lines ic=idrow negative,
c or for which it is same as background colour;
c adjust ndtext accordingly
	in=0
	do i=1,ndtext(j)
	   ic=int4(idrow(i,j))
	   if(ic.ge.0.and.ic.ne.icb) then
		in=in+1
		idrow(in,j)=int2(ic)
		dtext(in,j)=dtext(i,j)
	   endif
	enddo
	ndtext(j)=in
c
999	if(.not.numon) call NUMCLR	!Spindrift library
	RETURN
	end

	subroutine QDIALOG1(ibox,text,defolt,nchar,ict,cans)
c To write scrolling text in a dialogue box opened with OPENDIALOG.
c QDIALOG1 is version if QDIALOG that accepts exactly nchar characters
c and then leaves without needing ENTER
c
c Modified 11/10/97 08:38am by adding common/def/usedef where usedef is
c set true if defaults used
c Modified 01/22/95 07:01pm to switch on NUMLOCK (as in INPUTi etc)
c Modif 02/10/94 06:23pm so that in case where defolt=Y or N, then enter
c  returns defolt as before, but 0/INS returns (and writes to screen)
c  the opposite response. (NB assumes defolt was set by DEFOLTa
c  so default= ' [Y] ?')
c Modif 02/07/94 07:58am so that if text has zero length, returns at once
c   Modif 01/08/94 10:50am so that if text colour (ict)=background colour
c (to erase message) then does not wait for input, just writes question
c and returns straight away.
c   Modif 11/28/93 09:32pm so that if ict is negative, the line is
c initially typed as normal, but is removed from DTEXT (ie NOT stored)
c before leaving, but are overwritten by the next call to WDIALOG (QDIALOG)
c (problem with this if ict=0 so sign not available -overcome by
c also removing any line of text written in background colour, i.e. erased)
c   Modif 12/01/93 12:36pm so that if <enter> hit straight away then
c default answer is put on screen.
c As WDIALOG but asks a question, and waits for answer. The answer
c is written to screen in same colour as question.
c     CANS= answer in ASCII form (character*12)
c If UP arrow is hit while waiting for display, box scrolls up
c If DOWN arrow is hit while waiting for display, box scrolls down
c Modif 10/19/93 09:30am to include common/hlp/help so that sets
c  help=true if F1 hit, otherwise false.
c Any other key returns display to how it was when called, and is
c treated as the answer
	character*(*) text,defolt,cans
	character*1 getch,ch,def,UC
	character*78 dtx,dtext(60,10)  !up to 60 rows
	character*30 defolt1
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	logical help,noenter,erase,yesno,isnum,numon,usedef
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
	common/hlp/help
	common/def/usedef
c
	numon=isnum()	!num lock is on already on
	if(.not.numon) call NUMSET	!Spindrift library
c
	help=.false.
	usedef=.false.	!set true if nothing altered -default used (in common)
	noenter=.true.	!before any response typed in
c Is it a yes/no question?
	n=NBLANK(defolt)
	yesno=n.ge.3.and.(defolt(1:3).eq.' [Y'.or.defolt(1:3).eq.' [N')
	if(yesno) then
	   def=UC(defolt(3:3))		!='Y' or 'N'
	else
	   def=' '
	endif
	erase=iabs(ict).eq.iabs(icb)	!text in background colour=erase
	do i=1,LEN(cans)
	  cans(i:i)=char(32)
	enddo
c
	n=NBLANK(text)
	if(n.eq.0) goto 999		!RETURN
	n1=NBLANK(defolt)
	if(n1.eq.0) then
	   dtx=text(1:n)//' = '
	else
	   dtx=text(1:n)//defolt(1:n1)//' '	!leave space after '=' or '?'
	endif
c Make copy of defolt with brackets, = and ? removed to write on screen
c if RETURN hit
	j=0
	defolt1='                             '
	do i=1,n1
	   iv=ichar(defolt(i:i))
	   if(iv.ne.61.and.iv.ne.63.and.iv.ne.91.and.iv.ne.93) then
		j=j+1
		defolt1(j:j)=defolt(i:i)
	   endif
	enddo
	nd=j
c
	n=NBLANK(dtx)
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	icb=ib(j)
c First store and write TEXT as in WDIALOG
	if(ndtext(j).lt.ndmax) then
	   ndtext(j)=ndtext(j) + 1
	   irow=ndtext(j)
	   idrow(irow,j)=int2(ict)			!this row now used
	   dtext(irow,j)=dtx			!keep the text in it
	else
	   do i=1,ndmax-1
		idrow(i,j)=idrow(i+1,j)
		dtext(i,j)=dtext(i+1,j)		!scroll up, losing dtext(1,j)
	   enddo
	   idrow(ndmax,j)=int2(ict)
	   dtext(ndmax,j)=dtx			!save new one in last element
	   irow=ndmax
	endif
c Display in the box the last nrow (or fewer) dtext lines
	if(ndtext(j).lt.nrow) then	!irow=first unused row in box, from above
	   call SET_CURSOR(iclo,irow+irhi-1)          !args = col,row
	   ic=iabs(ict)
	   n5=NBLANK(dtx)
	   call SET_STRING(dtx(1:n5),ic)
c===	   call SET_STRING(text,ic)
	   ifirst(j)=1				!first row displayed is dtext(1,j)
	   ilast=ndtext(j)
	   ir1=irow+irhi-1	!for use below
	else
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear box
	   ifirst(j)=ndtext(j)-nrow+1
	   ilast=ndtext(j)
	   do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
c		ic=iabs(idrow(ir,j))		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	endif
c
c Next read the answer to the question into CANS
	ifst=ifirst(j)
	i=0
3	continue	!return for next character
c EXTRA LINE FOR QDIALOG1
	if(i.eq.nchar) goto 99
c
	if(erase) then	!ie text=background col=erase
	   ival=13		! so write (erase) defolt and leave
	   ktype=4
	else
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	endif
	if(ival.eq.3.and.ktype.eq.32) STOP	!^c
	if(ival.eq.13.and.ktype.eq.4) then	!finished (write defolt to screen
	   if(noenter.and.nd.gt.0) then
	     usedef=.true.	!nothing altered -default used (in common)
	     n=NBLANK(dtext(irow,j))	!before leaving if nothing else entered)
	     call SET_CURSOR(iclo+n,ir1)  !args = col,row
	     icul=iabs(ict)
	     call SET_STRING(defolt1(1:nd),icul)	!write defolt to screen
	     dtext(irow,j)=dtext(irow,j)(1:n)//defolt1(1:nd) !and add to dtext
	   endif
	   goto 99
	else if(yesno.and.((ktype.eq.3.and.ival.eq.48).or.
     &	(ktype.eq.8.and.ival.eq.82))) then		!zero/INS hit
	   if(def.eq.'Y') then
		cans(1:1)='N'			!so take non-default answer
	   else if(def.eq.'N') then
		cans(1:1)='Y'
	   endif
	   if(noenter) then
	     usedef=.true.	!nothing altered -default used (in common)
	     n=NBLANK(dtext(irow,j))	!before leaving if nothing else entered)
	     call SET_CURSOR(iclo+n,ir1)  !args = col,row
	     icul=iabs(ict)
	     call SET_STRING(' '//cans(1:1),icul)	!write defolt to screen
	     dtext(irow,j)=dtext(irow,j)(1:n)//' '//cans(1:1) !and add to dtext
	   endif
	   goto 99
	else if(ival.eq.59.and.ktype.eq.0) then	!F1=help
	   help=.true.
	   goto 99
	else if(ival.eq.72.and.ktype.eq.8) then	!scroll up
c 	   decrement rows of dtext to be displayed, until 1st is
c	   at top of the box
	   if(ifst.eq.1) goto 3		!can't scroll up any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst-1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else if(ival.eq.80.and.ktype.eq.8) then	!scroll down
	   if(ilst.ge.ilast) goto 3	!can't scroll down any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst+1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ndro=int4(idrow(ir,j))
		ic=iabs(ndro)		!======iabs()!
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else		!input answer to question
	   if(ifst.ne.ifirst(j)) then	!text has been scrolled -restore it
		do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		   n=NBLANK(dtext(ir,j))
		   ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	         call SET_CURSOR(iclo,ir1)          !args = col,row
		   ndro=int4(idrow(ir,j))
		   ic=iabs(ndro)		!======iabs()!
		   call SET_STRING(dtext(ir,j)(1:n),ic)
		enddo
	   endif
	   if((ival.eq.8.or.ival.eq.83).and.ktype.eq.8) then  !backspace/del
		if(i.gt.0) then
c		   ir1=
		   n=NBLANK(dtext(irow,j))
      	   call SET_CURSOR(iclo+n-1,ir1)          !args = col,row
		   icul=iabs(ict)
		   call SET_STRING(' ',icul)		!write blank to screen
		   cans(i:i)=char(32)	!replace current character with blank
		   i=i-1
		   dtext(irow,j)(n:n)=' '	!and blank last char of dtext
		endif
	   else
c		if(i.ge.12) goto 99
		noenter=.false.	!response typed in -do not use default
		i=i+1
		cans(i:i)=ch	!accumulate result
		n=NBLANK(dtext(irow,j))
	      call SET_CURSOR(iclo+n,ir1)  !args = col,row
		icul=iabs(ict)
		call SET_STRING(ch,icul)			!write character to screen
		dtext(irow,j)=dtext(irow,j)(1:n)//ch	!and add to dtext
	   endif
	endif
	goto 3	!get next character
c
99	continue
c
c Before leaving, check for, and remove, any lines ic=idrow negative,
c or for which it is same as background colour;
c adjust ndtext accordingly
	in=0
	do i=1,ndtext(j)
	   ic=int4(idrow(i,j))
	   if(ic.ge.0.and.ic.ne.icb) then
		in=in+1
		idrow(in,j)=int2(ic)
		dtext(in,j)=dtext(i,j)
	   endif
	enddo
	ndtext(j)=in
c
999	if(.not.numon) call NUMCLR	!Spindrift library
	RETURN
	end


	subroutine TDIALOG(ibox,query,title,nlen,blank,ict)
c	subroutine TITENT1(query,title,nlen,blank)
c To write scrolling text in a dialogue box opened with OPENDIALOG.
c This is analogue of TITENT1 for use in dialog box
c Query is written on a separate row, then title entered.
c If UP arrow is hit while waiting for display, box scrolls up
c If DOWN arrow is hit while waiting for display, box scrolls down
c Any other key returns display to how it was when called, and is
c treated as the answer
c Modif 09/24/94 10:28pm to allow insert/overtype
	character query*(*),title*(*)
	character text1*78
	character getch,ch,ch1
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
c	integer*2 ktype
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	logical blank,overtype
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	icb=ib(j)
c
	overtype=.false.		!default on entry
	ic=11                   !colour for insert mode
c
	text1=CHARNB(query)//'  (use home/end/arrows/bs/ins/del)'
	call WDIALOG(ibox,CHARNB(text1),ict)
	if(blank) then
	   do i=1,nlen
	      title(i:i)=' '
	   enddo
	   n2=0
	else
	   call GBLANK(title,nlen,n1,n2)
	endif
c Write the dots -put them in text1 and use WDIALOG again
	do i=1,78
	   if(i.ge.n1.and.i.le.n2) then
		text1(i:i)=title(i:i)
	   else if(i.gt.nlen) then
		text1(i:i)=' '
	   else
		text1(i:i)='.'
	   endif
	enddo
	call WDIALOG(ibox,CHARNB(text1),ict)
c
c Next read the answer to the question into TITLE (as for CANS? but
c need DEL HOME etc and cursor)
c Put cursor at start of current line (last one written by WDIALOG above)
	if(ndtext(j).lt.ndmax) then
	   irow=ndtext(j)
	else
	   irow=ndmax
	endif
	ilast=ndtext(j)	!last of dtext written
	ifst=ifirst(j)
	i=0		!# of characters read in
	call DRCURS(iclo,ir1,ic)	!draw cursor at this pos
c
3	continue	!return for next character
	call GBLANK(title,nlen,n1,n2)	!check where current 'title' starts/ends
	ch=GETCH(ktype)
	ival=ichar(ch)
	if(ival.eq.13.and.ktype.eq.4) then	!finished
	   goto 99
	else if(ival.eq.72.and.ktype.eq.8) then	!scroll up
c 	   decrement rows of dtext to be displayed, until 1st is
c	   at top of the box
	   if(ifst.eq.1) goto 3		!can't scroll up any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst-1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ic=int4(idrow(ir,j))
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else if(ival.eq.80.and.ktype.eq.8) then	!scroll down
	   if(ilst.ge.ilast) goto 3	!can't scroll down any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst+1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ic=int4(idrow(ir,j))
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
c Now options that involve cursor (must check that text has not been scrolled
c up before doing these)
	else		!input answer to question
	   if(ifst.ne.ifirst(j)) then	!text has been scrolled -restore it
		do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		   n=NBLANK(dtext(ir,j))
		   ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	         call SET_CURSOR(iclo,ir1)          !args = col,row
		   ic=int4(idrow(ir,j))
		   call SET_STRING(dtext(ir,j)(1:n),ic)
		enddo
	   endif
c
c Home/End/Arrow keys -move cursor only
	   if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
		if(i.gt.0) then
		   call DRCURS(iclo+i,ir1,icb)	!erase prev cursor
		   i=i-1
		   call DRCURS(iclo+i,ir1,ic)	!draw cursor at this pos
		endif
	   else if(ktype.eq.8.and.ival.eq.77) then	!RIGHT arrow key
		if(i.lt.nlen) then
		   call DRCURS(iclo+i,ir1,icb)	!erase prev cursor
		   i=i+1
		   call DRCURS(iclo+i,ir1,ic)		!draw cursor at this pos
		endif
	   else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
		   call DRCURS(iclo+i,ir1,icb)	!erase prev cursor
		   i=0
		   call DRCURS(iclo,ir1,ic)	!draw cursor at this pos
         else if(ktype.eq.8.and.ival.eq.79) then		!END key
		   call DRCURS(iclo+i,ir1,icb)	!erase prev cursor
		   i=nlen-1
		   call DRCURS(iclo+i,ir1,ic)		!draw cursor at this pos
	   else if(ktype.eq.8.and.ival.eq.82) then		!INS key
	      overtype=.not.overtype		!can also change cursor shape?
		if(overtype) then
		   ic=12
		else
		   ic=11
		endif
		call DRCURS(iclo+i,ir1,ic)	!redraw in new colour
	   else if(ktype.eq.8.and.ival.eq.83) then		!DEL key
c for DEL, cursor not moved; character just deleted
		  if(i.gt.0.and.n2.ge.1) then
		   do m=i+1,n2-1
			ch=title(m+1:m+1)
      		call SET_CURSOR(iclo+m-1,ir1)    !args = col,row
			call SET_STRING(ch,ict)		!write blank to screen
			title(m:m)=ch
			dtext(irow,j)(m:m)=ch
		   enddo
      	   call SET_CURSOR(iclo+n2-1,ir1)    !args = col,row
		   call SET_STRING('.',ict)	     !replace last '.' on screen
		   title(n2:n2)=' '			!and as blank in string
		   dtext(irow,j)(n2:n2)=' '
		   call DRCURS(iclo+i,ir1,ic)	!redraw cursor (bit is erased by prev line)
		   endif
	   else if(ival.eq.8.and.ktype.eq.8) then  	!BACKSPACE
c		if(i.gt.0) then
		   do m=i+1,nlen-1
			if(m.ge.n2) then
			  ch='.'
			else
			  ch=title(m+1:m+1)
			endif
      		call SET_CURSOR(iclo+m-1,ir1)    !args = col,row
			call SET_STRING(ch,ict)		!write char one place earlier
			if(ch.eq.'.') then
			  title(m:m)=' '
			else
			  title(m:m)=ch
			endif
			dtext(irow,j)(m:m)=ch
		   enddo
		   if(i.gt.0) then
      	      call SET_CURSOR(iclo+i-1,ir1)    !args = col,row
		      call SET_STRING('.',ict)		!write '.' at end
		      call DRCURS(iclo+i,ir1,icb)	!erase prev cursor at this pos
                  i=i-1
		   endif
		   call DRCURS(iclo+i,ir1,ic)	!draw cursor at this pos
c		endif
c for any other key, now input the title
	   else
		if(overtype) then
		   i=i+1
		   if(i.gt.nlen) goto 3
		   title(i:i)=ch	!accumulate result
	         call SET_CURSOR(iclo+i-1,ir1)  !args = col,row
c=		   if(i.ne.0) call DRCURS(iclo+i-2,ir1,icb)	!erase prev cursor
		   if(i.ge.0) call DRCURS(iclo+i-1,ir1,icb)	!erase prev cursor
c=		   call DRCURS(iclo+i-1,ir1,ic)	!draw cursor at this pos
		   call DRCURS(iclo+i,ir1,ic)		!draw cursor at this pos
		   call SET_STRING(ch,ict)			!write character to screen
		   dtext(irow,j)(i:i)=ch			!and add to dtext
		else			!insert mode
		   if(n2+1.gt.nlen) goto 3		!full, so skip
c          Move existing characters to right
		   do m=n2,i+1,-1
			ch1=title(m:m)
			title(m+1:m+1)=ch1
			dtext(irow,j)(m+1:m+1)=ch1
	      	call SET_CURSOR(iclo+m,ir1)   !args = col,row
			call SET_STRING(ch1,ict)	!write to screen
		   enddo
c         Insert new char
c=		   call DRCURS(iclo+i-1,ir1,icb)	!delete prev cursor
		   call DRCURS(iclo+i,ir1,icb)	!delete prev cursor
		   i=i+1
		   title(i:i)=ch		!put new character into 'title' (mc=0-n so mc+1)
		   dtext(irow,j)(i:i)=ch
	         call SET_CURSOR(iclo+i-1,ir1)   !args = col,row
		   call SET_STRING(ch,ict)			!write character to screen
c=		   call DRCURS(iclo+i-1,ir1,ic)	!draw cursor at this pos
		   call DRCURS(iclo+i,ir1,ic)		!draw cursor at this pos
		endif
	   endif
	endif
	goto 3	!get next character
c
99	continue
c
	RETURN
	end

	subroutine DRCURS(icol,irow,icolor)
c	In VGA each text line occupies 16 pixels (letters occupy 11 pixels
c with space of 5 pixels between lines).  The top line (row 0) has
c letters with bases at iy=467 pixels, and the bottom line (row 29) has
c letters with bases at iy=3 pixels
c Thus letters in row #irow have bases at iy=3+(29-irow)*16
c	There are 80 columns of characters (icol=0,..,79) each occupying 8 pixels
c with col zero starting at ix=0.  In general ix for start (left edge) of
c character in column icol is at ix=8*icol (ie last col starts at ix=632)
	ixlo=icol*8 -2
	ixhi=ixlo + 10
	iylo=3 + (29-irow)*16
	iyhi=iylo+13
	call HLINE(ixlo,ixhi,iylo,icolor)
	call VLINE(ixhi,iylo,iyhi,icolor)
	call HLINE(ixlo,ixhi,iyhi,icolor)
	call VLINE(ixlo,iylo,iyhi,icolor)
	RETURN
	end

	subroutine DISCDIALOG(ibox,query,idisc,ndev,iask,idef,ict)
c For input of disc partition in dialog box (based on DISCNUM1)
c but 'query' added to params (eg ' Now specify disc for output') -this
c is omitted if blank. Also need ibox=1-10 (box number) and ict=text colour.
c If IDEF > 0 it is specified as the default device
c Modif 01/12/95 02:24pm so that when idef>0 the default is checked for validity
c
c INPUT
c IDEF=0: No default device specified
c IDEF=1: Default device name specified in NDEV input
c   (in this case must not overwrite default by value from disc)
c IDEF=2: Default device name specified in IDISC input
c
c IASK=0 asks for hard disk # only
c IASK=-1 asks for hard disk # or floppy #
c IASK=-2 asks for floppy # only
c
c OUTPUT
c IDISC=1,2,3,4,5,6,7,8,9,10,11,12  for A:,B:,C:,D:,E:,F:,G:,H:,I:,J:,K:,L:
c IDISC=-1 if ESC typed (and NDEV undefined)
c NDEV='C:' etc
c
	character ndev*2
	character ch,getch,UC,lastdev
	character query*(*),text*78
	character*32 filnam	!for COMMON only
	character*40 mtitle	!machine name
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
c	integer*2 ktype
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	logical good,good1
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	j=ibox
c	irhi=irh(j)
	iclo=icl(j)
c	nrow=nro(j)
c	icb=ib(j)
c
	if(ndisc.le.0.or.ndisc.gt.24) ndisc=24   !in case .INI not defined yet
c
	if(iask.eq.0.and.ndisc.eq.1) then	!no choice!
	   ndev='C:'
	   idisc=3
	   RETURN
	endif
c
	if(idef.eq.2) call CDISC1(ndev,idisc,1) !define ndev from idisc (see below)
	lastdev=char(66+ndisc)	!=D for ndisc=2,...,L for ndisc=10
c If idef not zero, expect a default -check that default is valid
	good=.false.		!default valid?
	if(idef.ne.0) then
	   ival=ichar(UC(ndev(1:1)))
	   ilast=66+ndisc
	   if(iask.eq.0) then
		good=ival.ge.67.and.ival.le.ilast	!67=C
	   else if(iask.eq.-1) then
		good=ival.ge.65.and.ival.le.ilast	!65=A
	   else if(iask.eq.-2) then
		good=ival.ge.65.and.ival.le.66	!65=A, 66=B
	   endif
	endif
c
	n=NBLANK(query)
	if(n.gt.0) then
	   call WDIALOG(ibox,query,ict)
	endif
c
	if(iask.eq.0) then
	   text=
     &    'Disc (type a character, C to '//lastdev//': ESC to abort)'
	else if(iask.eq.-1) then
	   text=
     &    'Disc (type a character, A to '//lastdev//': ESC to abort)'
	else if(iask.eq.-2) then
	   text=
     &    'Floppy disc (type a character, A or B: ESC to abort)'
	endif
c
	if(idef.gt.0) then	!add default
	   text=CHARNB(text)//' ['//ndev//'] = '
	else
	   text=CHARNB(text)//' = '
	endif
	call WDIALOG(ibox,text,ict)
c No default specified
	if(ndtext(j).lt.ndmax) then
	   irow=ndtext(j)
	else
	   irow=ndmax
	endif
c	ir1=irow+irhi-1 			!last screen row written to (in common)
1	continue
	ch=GETCH(ktype)
	ch=UC(ch)			!convert to upper case
	ival=ichar(ch)
	good1=.false.		!default valid?
	ilast=66+ndisc
	if(iask.eq.0) then
	   good1=ival.ge.67.and.ival.le.ilast	!67=C
	else if(iask.eq.-1) then
	   good1=ival.ge.65.and.ival.le.ilast	!65=A
	else if(iask.eq.-2) then
	   good1=ival.ge.65.and.ival.le.66		!65=A, 66=B
	endif
	if(ktype.eq.16.and.ichar(ch).eq.27) then	!ESC typed
	   idisc=-1
	   goto 99
	endif
	if(ival.eq.13.and.ktype.eq.4.and.idef.gt.0) then
	   if(.not.good) then
		call BELL(1)		!default was invalid
		goto 1
	   endif
	else if(good1) then		!input was valid drive
	   ndev=ch//':'	    		!converted to upper case above
	else
	   call BELL(1)		!default was invalid
	   goto 1		!any other key (inc 'enter', as their is no default)
	endif
c	print 25,ndev
	n=NBLANK(text)
	call SET_CURSOR(iclo+n+1,ir1)  !args = col,row
	call SET_STRING(ndev,ict)			!write character to screen
	dtext(irow,j)=CHARNB(dtext(irow,j))//ndev	!and add to dtext
	call ANYKEY		!so needs carriage return
	call CDISC1(ndev,idisc,0)	!define idisc from ndev (see below)
c	if(idisc.lt.1.or.idisc.gt.ndisc+2) goto 1
c	if(iask.eq.-2.and.(.NOT.(idisc.eq.1.or.idisc.eq.2))) goto 1
99	continue
	RETURN
	end

	subroutine SCROLDIAL(ibox)
c To scroll up and down the existing text (stored in dtext) in dialog
c box #ibox.
c ESC or ENTER returns text to original position and then exits
	character getch,ch
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
c	integer*2 ktype
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	integer*1 array1(:)
	allocatable :: array1
	character*46 strings(1)
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c
c	call WDIALOG(ibox,'Arrows to scroll: then ESC/ENTER',15)
c===fix WDIALOG so this line not stored (eg call with -ibox)
c Write instructions in pop-up box, as in POPUPS etc
	ix1=70
	iy2=250
	strings(1)='Up/down arrows scroll dialogue: then ESC/ENTER'
c	n=1
c	call WINDIM(ix1,iy1,ix2,iy2,idimen,strings,n,ixc0,iyc0)
	call WINDIM(ix1,iy1,ix2,iy2,idimen,strings,1,ixc0,iyc0)
	ALLOCATE(array1(idimen))
	call RDGWIN1(ix1,iy1,ix2,iy2,array1)  !Store orig in array1
c	call draw_with_frame(ix1,iy1,ix2,iy2,1,14)
	call FRAMEFILL(ix1,iy1,ix2,iy2,1,14)
	call WRSTRING18(ixc0,iyc0,strings(1),1,14)
c	iyc=iyc0
c	do i=1,n
c		call WRSTRING18(ixc0,iyc,strings(i),1,14)
c		iyc=iyc-16
c	enddo
c end of POPUP
c
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	icb=ib(j)
c Now scroll
	ifst=ifirst(j)
	ilast=ndtext(j)
3	continue	!return for next character
	ch=GETCH(ktype)
	ival=ichar(ch)
	if(ival.eq.13.or.ival.eq.27) then	!finished (ENTER or ESC)
c Restore orig text before leaving
c=======restoration not quite right!
	   if(ifst.ne.ifirst(j)) then	!text has been scrolled -restore it
		do ir=ifirst(j),ndtext(j)	!write the last nrow lines to box
		   n=NBLANK(dtext(ir,j))
		   ir1=ir-ifirst(j)+irhi		!row number on screen to write to
	         call SET_CURSOR(iclo,ir1)          !args = col,row
		   ic=int4(idrow(ir,j))
		   call SET_STRING(dtext(ir,j)(1:n),ic)
		enddo
	   endif
	   goto 99
	else if(ival.eq.72.and.ktype.eq.8) then	!scroll up
c 	   decrement rows of dtext to be displayed, until 1st is
c	   at top of the box
	   if(ifst.eq.1) goto 3		!can't scroll up any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst-1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ic=int4(idrow(ir,j))
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	else if(ival.eq.80.and.ktype.eq.8) then	!scroll down
	   if(ilst.ge.ilast) goto 3	!can't scroll down any more
	   call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)	!clear window
	   ifst=ifst+1	!don't change ifirst(j)!
	   ilst=ifst+nrow-1
	   if(ilst.gt.ndtext(j)) ilst=ndtext(j)
	   do ir=ifst,ilst	!write the lines to box
		n=NBLANK(dtext(ir,j))
		ir1=ir-ifst+irhi		!row number on screen to write to
	      call SET_CURSOR(iclo,ir1)          !args = col,row
		ic=int4(idrow(ir,j))
		call SET_STRING(dtext(ir,j)(1:n),ic)
	   enddo
	endif
	goto 3	!get next character
c
99	continue
c Remove POPUP and redraw orig screen
	call WRGWIN1(ix1,iy1,ix2,iy2,array1)
	deallocate(array1)
c
	RETURN
	end



	subroutine ENDDIALOG(ibox,icb)
	character*78 dtext(60,10)  !up to 60 rows
	integer*2 idrow(60,10)
	integer irh(10),icl(10),nro(10),ib(10)
	integer ixlo(10),ixhi(10),iylo(10),iyhi(10)
	integer nco(10),ndtext(10),ifirst(10),ndmax
	common/dial/dtext,idrow,irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,
     & ndtext,ndmax,ifirst,ir1,iwid
c To delete a dialog box by filling with screen background colour=icb
	j=ibox
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
c so need to delete frame separately (can not just increase ix,iy by 1)
	if(iwid.gt.0) then
	   do i=1,iwid
	     call HLINE(ixlo(j)-i,ixhi(j)+i,iylo(j)-i,icb) !delete shadow border
	     call VLINE(ixhi(j)+i,iylo(j)-i,iyhi(j)+i,icb)
	     call HLINE(ixlo(j)-i,ixhi(j)+i,iyhi(j)+i,icb)
	     call VLINE(ixlo(j)-i,iylo(j)-i,iyhi(j)+i,icb)
	   enddo
	else
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iylo(j)-1,icb)	!delete frame
	   call VLINE(ixhi(j)+1,iylo(j)-1,iyhi(j)+1,icb)
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iyhi(j)+1,icb)
	   call VLINE(ixlo(j)-1,iylo(j)-1,iyhi(j)+1,icb)
	endif
	RETURN
	end


	subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
c To define (but not yet draw) a simple non-scrolling for text in graphics mode
c ibox=1-10 for DEFBOX
c Box has top left corner at row, column=irhi,iclo (irhi=0,29; iclo=0,79)
c nrow, ncol=number of rows and cols of text to fit in box
c ibox=box number (1-10 at present)
c Frame colour=icf
c Background colour inside box=icb
c nrow=number of rows of text to be held in box
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c If irhi=1 and nrow=4, bottom line of frame is at
c  iylo-1=396 pixels (396/480=82.5%)
c
c	In VGA each text line occupies 16 pixels (letters occupy 11 pixels
c with space of 5 pixels between lines).  The top line (row 0) has
c letters with bases at iy=467 pixels, and the bottom line (row 29) has
c letters with bases at iy=3 pixels
c Thus letters in row #irow have bases at iy=3+(29-irow)*16
c	There are 80 columns of characters (icol=0,..,79) each occupying 8 pixels
c with col zero starting at ix=0.  In general ix for start (left edge) of
c character in column icol is at ix=8*icol (ie last col starts at ix=632)
	integer irh(10),icl(10),nro(10),ib(10)
	integer nco(10),ixlo(10),ixhi(10),iylo(10),iyhi(10)
c NB this common need never appear in calling progs (ixlo is nothing to
c do with Hgraph viewport!).
	common/tbox/irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,iwid
c
	j=ibox
	irh(j)=irhi		!copy for COMMON
	icl(j)=iclo
	nro(j)=nrow
	nco(j)=ncol
	ib(j)=icb
	RETURN
	end


	subroutine OPENBOX(ibox,icf,shad)
c To DRAW a simple non-scrolling for text in graphics mode (as previously
c defined by OPENBOX)
c ibox=1-10 for DEFBOX
c Frame colour=icf
c Background colour inside box=icb
c nrow=number of rows of text to be held in box
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	integer irh(10),icl(10),nro(10),ib(10)
	integer nco(10),ixlo(10),ixhi(10),iylo(10),iyhi(10)
c NB this common need never appear in calling progs (ixlo is nothing to
c do with Hgraph viewport!).
	logical shad
c NB this common need never appear in calling progs (ixlo is nothing to
c do with Hgraph viewport!).
	common/tbox/irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,iwid
c
	j=ibox
	irhi=irh(j)
	iclo=icl(j)
	nrow=nro(j)
	ncol=nco(j)
	icb=ib(j)
c
	margin=5	!5 pixels
	ixlo(j)=iclo*8 - margin
	ixhi(j)=ixlo(j) + ncol*8 + margin
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo(j)=8*(ixlo(j)/8)		!round down
	ixhi(j)=8*(ixhi(j)/8)		!round down
c add another 16 to iyhi because 3+(29-irhi) is posn for BASE of text
c -actually adding 10 better
	iyhi(j)=3 + (29-irhi)*16 +10 + margin
	iylo(j)=iyhi(j) - nrow*16 - margin
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
	if(shad) then
c	   iwid=ifix(abs(ixhi(j)-ixlo(j))/30)
	   iwid=ifixr((abs(ixhi(j)-ixlo(j))+abs(iyhi(j)-iylo(j)))/100.)
	   if(iwid.lt.2) iwid=2
	   if(iwid.gt.5) iwid=5
	   call SHADOW(ixlo(j),ixhi(j),iylo(j),iyhi(j),iwid,icf) !draw shadow border
	else
	   iwid=0		!signal to ENDBOX that frame drawn
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iylo(j)-1,icf)	!draw frame
	   call VLINE(ixhi(j)+1,iylo(j)-1,iyhi(j)+1,icf)
c	   call HLINE(ixhi(j),ixlo(j),iyhi(j),icf)	!hline draws 'backwards' OK
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iyhi(j)+1,icf)
c	   call VLINE(ixlo(j),iyhi(j),iylo(j),icf)	!but vline does not
	   call VLINE(ixlo(j)-1,iylo(j)-1,iyhi(j)+1,icf)
	endif
c
	RETURN
	end


	subroutine WRITBOX(ibox,text,n,ict)
c To write to simple non-scrolling text box opened with OPENBOX
c Note: extra param (compared with WDIALOG): n=number of rows to be written
	character*(*) text(n)
	integer irh(10),icl(10),nro(10),ib(10)
	integer nco(10),ixlo(10),ixhi(10),iylo(10),iyhi(10)
	common/tbox/irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,iwid
c To clear a non-scrolling text box by filling with background colour=icb
c
	n1=NBLANK(text(1))
	j=ibox
	ncol=nco(j)
	if(n1.gt.ncol) n1=ncol
	irhi=irh(j)
	iclo=icl(j)
c	nrow=nro(j)
c	icb=ib(j)
c Display text in the box starting at row 1
	irow=1
	ir1=irow+irhi-1 			!last screen row written to
	call SET_CURSOR(iclo,ir1)          !args = col,row
	call SET_STRING(text(1)(1:n1),ict)
c Write 2nd, 3rd ... rows if present
	if(n.gt.1) then
	   do j=2,n
		n1=NBLANK(text(j))
		ir1=ir1+1
		call SET_CURSOR(iclo,ir1)          !args = col,row
		call SET_STRING(text(j)(1:n1),ict)
	   end do
	endif
	RETURN
	end


	subroutine CLRBOX(ibox,icb)
	integer irh(10),icl(10),nro(10),ib(10)
	integer nco(10),ixlo(10),ixhi(10),iylo(10),iyhi(10)
	common/tbox/irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,iwid
c To clear a non-scrolling text box by filling with background colour=icb
c
	j=ibox
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
	RETURN
	end

	subroutine ENDBOX(ibox,icb)
	integer irh(10),icl(10),nro(10),ib(10)
	integer nco(10),ixlo(10),ixhi(10),iylo(10),iyhi(10)
	common/tbox/irh,icl,nro,nco,ib,ixlo,ixhi,iylo,iyhi,iwid
c To delete a text box by filling with screen background colour=icb
	j=ibox
	call FILLWIN(ixlo(j),iylo(j),ixhi(j),iyhi(j),icb)
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
c so need to delete frame separately (can not just increase ix,iy by 1)
	if(iwid.gt.0) then
	   do i=1,iwid
	     call HLINE(ixlo(j)-i,ixhi(j)+i,iylo(j)-i,icb) !delete shadow border
	     call VLINE(ixhi(j)+i,iylo(j)-i,iyhi(j)+i,icb)
	     call HLINE(ixlo(j)-i,ixhi(j)+i,iyhi(j)+i,icb)
	     call VLINE(ixlo(j)-i,iylo(j)-i,iyhi(j)+i,icb)
	   enddo
	else
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iylo(j)-1,icb)	!delete frame
	   call VLINE(ixhi(j)+1,iylo(j)-1,iyhi(j)+1,icb)
	   call HLINE(ixlo(j)-1,ixhi(j)+1,iyhi(j)+1,icb)
	   call VLINE(ixlo(j)-1,iylo(j)-1,iyhi(j)+1,icb)
	endif
	RETURN
	end


