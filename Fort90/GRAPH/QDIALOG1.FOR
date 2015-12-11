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
