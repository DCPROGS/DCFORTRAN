c========================================================================
	subroutine GSTRING(x,y,text,ifont,angle,size,ijust,
     &			icolor,ibk,ndim,blank,nreal)

c========================================================================
c Gino version
c DC's modification of ILVFONT -improved version of Hgraph's READSTRING
c ifont=default font
c Version in which cursor is underline character written in Justifystring,
c but removed before exit
c This version has essentially 3 fonts (1) as input
c (2) italic version of input (3) greek.  When reverting from Greek, should
c now go back to italic if prev in italic and conversely.
c
c Modif 09/26/94 10:21pm:
c  (1) to add parameter BLANK -if false, input text is displayed
c  (2) Set blanks as ' '=char(32) NOT char(0), since latter causes NBLANK
c	to give wrong answer
c Modif 07/18/94 08:52am to add parameter ndim=declared dimension of TEXT
c so TEXT can be set to char(0) internally, and can check when array is full.
c
c TO DO
c (1) Editing of existing title
c
c NB string nust be redrawn each time a visible character is added, because
c there is no way to choose accurately the position to write a new single
c character. If any of the preceding characters change then existing string
c must be deleted first -otherwise just overwrite it. -true only for
c ixjust=0 or 3 (left or base); but for ixjust=1,2 (left, centre) earlier
c characters move so must predelete.
c
c	Subroutine to simulate READSTRING(IX,IY,TEXT,ANGLE,SIZE)
c     x, y:     position to start (pixel units from FNDCUR)
c	text:     text string
c	angle:    angle
c     size:     size
c     ijust:    horizontal justification
c
c	icolor:	color string
c	ibk:        color background
c
c NB i=index in TEXT for next character to be typed
c    n=length of string (inc control char) after it has been typed
c (n now superfluous -i=n always in this version)
c************************************************************************

	character*1 char1,char2,getch
	character text*(*),oldtext*80
	integer*1 marray(:)
	logical greek,ital,blank,under,math,table
	allocatable :: marray

c
c
c NB no use calling SETFONT here; when string redrawn it changes initial
c font, but as soon as the first ^Fn is encountered that prevents the
c SETFONT having any further effect.
c
c Present FIXTEXT uses:
c move=arrows/F2
c size=+,-
c font=/,*
c rotate=R,r
c box on/off = b
c del/undel = DEL
c high/low sens =INS
c
	icf=14
	ixlo=0
	iyhi=478
	call pixpos(ixlo,iyhi,xlo,yhi)
	yhi=159.7-yhi
	ylo=yhi-110
	xhi=xlo+25
	call pospix(xhi,ylo,ixhi,iylo)
	iylo=480-iylo
	idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
	allocate(marray(idim))

	   do i=1,80
		oldtext(i:i)=char(32)
	   enddo
	n1=NBLANK1(text)
	if(blank) then
	   do i=1,ndim
		text(i:i)=char(32)
	   enddo
	else
	   if(n1.lt.ndim) then
		do i=n1+1,ndim
		   text(i:i)=char(32)
		enddo
	   endif
	endif
c
	under=.false.
	math=.false.
	table=.false.
	greek=ifont.eq.10	!set from input font
c	ital=mod(ifont,2).eq.1 	!odd, so italic
	ifont1=ifont		!current font
	ifont0=ifont1
	if(blank) then
	   i=1
	   ireal=1
	else
	   i=n1+1
	   if(i.gt.ndim) i=ndim
	   ireal=nreal
	endif
	call lincols(icolor)
	call SETSIZE(size)
	call setFNT(ifont)
	if(blank) then
	   text(1:1)=char(95)		!underline=cursor
         call JUSTIFYSTRING(x,y,text(1:1),
     &     angle,size,ijust) !draw initial cursor
	else
	   if(n1.lt.ndim) then
	      text(n1+1:n1+1)=char(95)		!underline=cursor
            call JUSTIFYSTRING(x,y,text(1:n1+1),
     &       angle,size,ijust) !draw text+cursor
	   else
           call JUSTIFYSTRING(x,y,text(n1:n1),
     &      angle,size,ijust) !draw initial cursor
	   endif
	endif
	oldtext=text
888	continue
	char1=getch(ktype1)
	ival1=ichar(char1)
    	if(ival1.eq.13.or.ival1.eq.27) then		!esc/enter
	   goto 777
	else if(ktype1.eq.0.and.ival1.eq.59) then	!F1=help
	   call VHELP(9)
	   goto 888
	else if(ktype1.eq.0.and.ival1.eq.60) then	!F2=help
	   call vhelp(-2)
	   goto 888
	else
	   if(ktype1.eq.32) then
	      call SETSIZE(size)
	      call setFNT(ifont)
		select case(ival1)
		   case(1)        !^A align  -> *A align
			text(i:i+1)='*A' !
			i=i+2
			call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)
		   case(4)        !^D index  -> *I
			if((i+2).gt.ndim) goto 888
			text(i:i+1)='*I' !
			i=i+2
			call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)
		   case(5)		!^E =terminate
			text(i:i+1)='*.' !
			i=i+2
			call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)
		   case(6)        !^F 0/1/2/3/4/5/6/7/8/9/a/*  -> *Fnnn
			char2=getch(ktype2)     !change font
			ival2=ichar(char2)
			if(ival2.ge.48.and.ival2.le.57) then
c			  ifont0=ifont1
			  if((i+5).gt.ndim) goto 888
			  select case(ival2)
			   case(48)
			      text(i:i+4)='*F'//'100'   !0 courier
  				ifont1=0
  			   case(49)
  			      text(i:i+4)='*F'//'101'   !1 helvetica
  				ifont1=1
  			   case(50)
  			      text(i:i+4)='*F'//'102'   !2 times
  				ifont1=2
  			   case(51)
  			      text(i:i+4)='*F'//'103'   !3 avantg
  				ifont1=3
  			   case(52)
  			      text(i:i+4)='*F'//'104'   !4 lublin
  				ifont1=4
  			   case(53)
  				ital=.true.
  			      text(i:i+4)='*F'//'105'   !5 newcent
				ifont1=5
			   case(54)
			      text(i:i+4)='*F'//'106'   !6 souvenir
				ifont1=6
			   case(55)
			      text(i:i+4)='*F'//'107'   !7 palatino
				ifont1=7
			   case(56)
			      text(i:i+4)='*F'//'008'   !8 script
				ifont1=8
			   case(57)
				ital=.true.
			      text(i:i+4)='*F'//'006'   !9 italic
				ifont1=9
			  end select
			  i=i+5
			else if(ival2.eq.42) then
			   if((i+3).gt.ndim) goto 888
			   text(i:i+2)='*FR'            !previous font
			   i=i+3
			   ifont1=ifont
			else
			   goto 888
			endif
		   case(7)           ! ^G change font to greek ->*F010
			if(i+5.gt.ndim) goto 888
			if(.not.greek) then
			   text(i:i+4)='*F010'
			   greek=.true.
			   i=i+5
			   ifont1=10
			else
			   text(i:i+2)='*FR'
			   greek=.false.
			   i=i+3
			   ifont1=ifont
			endif
		   case(12)        !^L underline  -> *S underline
			if((i+2).gt.ndim) goto 888
		      if(.not.under) then
				text(i:i+1)='*S' !
		      else
				text(i:i+1)='*A' !
		      endif
			i=i+2
c			call WHGSTRING(i,x,y,text,oldtext,angle,size,
c     &    		 ijust,char1,ibk,icolor,ndim,0)
		   case(14)       ! ^N new line ->*N
			text(i:i+1)='*N' !char1
			i=i+2
			irows=irows+1
			call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)
		   case(18)		!^R =revert to default font *FR
			if((i+3).gt.ndim) goto 888
			text(i:i+2)='*FR' !
			i=i+3
			ifont1=ifont
		   case(19)        !^S symbol/maths->*F070
			if((i+5).gt.ndim) goto 888
			if(.not.math) then
			   math=.true.
			   text(i:i+4)='*F'//'070'   !11/m math
			   ifont1=70
			   i=i+5
			else
			   text(i:i+2)='*FR'            !previous font
			   i=i+3
			   ifont1=ifont
			   math=.false.
			endif
		   case(20)		! ^T = table
			if(.not.table) then
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,marray)
			   call FRAMEFILL(ixlo,iylo,ixhi,iyhi,icf,0)
			   call chajus(-1)
			   call mathtable(xlo,yhi,icf)
			   call chajus(ijust)
			   table=.true.
			else
			   call wrgwin1(ixlo,iylo,ixhi,iyhi,marray)
			   table=.false.
			endif
		   case(21)       ! ^U up   *E
			if((i+2).gt.ndim) goto 888
			text(i:i+1)='*E' ! exponent
			i=i+2
		      call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)
		end select
	   else if(ktype1.eq.8) then
	      if((i+2).gt.ndim) goto 888
		if(ival1.eq.9) then	!^I
		   if(i+1.gt.ndim) goto 888
		   if(.not.ital) then		!
			text(i:i+1)='*/' !
			i=i+2
			ital=.true.
		   else					!odd, so italic
			ital=.false.
			if (ifont1.eq.9) then
				call setfnt(1)
				ifont1=1
c			else if (ifont1.eq.6) then
c				call setfnt(4)
c				ifont1=4
			else
				text(i:i+1)='*|' !
			      i=i+2
			endif
		   endif
		else if(ival1.eq.8.or.ival1.eq.83) then	!backspace and delete
		   if(i.ge.2) then
c Decide how many char to remove by looking backwards to find any control
c characters (ichar=1 to 26). If the character to be deleted is not visible
c going back until first visible character is found and delete up to and
c including the visible char. Character is visible if it is not a control
c character and is not preceded by a control char (ichar=1-26)
			n1=1
			if(i.eq.2) then	!only one char so far, so delete it
			   text(1:1)=char(32)
			   i=1
			   ireal=1
			else
			   text(i:i)=char(32)
			   n=i
			   do j=n-1,2,-1
			      iv1=ichar(text(j:j))
			      iv2=ichar(text(j-1:j-1))
			      if(iv1.ne.42.and.iv2.ne.42) then	!found vis char, #n1
				   if(iv2.eq.70.and.(iv1.eq.82.or.iv1.ge.48.
     &                        and.iv1.le.57)) then
			            iv3=ichar(text(j-2:j-2))
					if(iv3.ne.42) then
				         n1=j
				         goto 1
				      endif
				   else if (iv1.le.57.and.iv1.ge.48.and.
     &                             iv2.le.57.and.iv2.ge.48) then
				      if(j.ge.5) then
			               iv4=ichar(text(j-3:j-3))
			               iv5=ichar(text(j-4:j-4))
			                  if(iv4.ne.42.and.iv5.ne.42) then	!found vis char, #n1
				               n1=j
				               goto 1
						endif
				      endif
				   else
				      n1=j
				      goto 1
				   endif
				endif
			   enddo
1			   continue
			   do j=n1,n-1
				text(j:j)=char(32)
				i=i-1
				ireal=ireal-1
			   enddo
			endif
	      call SETSIZE(size)
	      call setFNT(ifont)
		      call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    		 ijust,char1,ibk,icolor,ndim,0)	!write cursor
		   endif
		endif
	   else                    !write character
	      if(i.gt.ndim) goto 888
	      call SETSIZE(size)
	      call setFNT(ifont)
		call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    	 ijust,char1,ibk,icolor,ndim,1)
		 ireal=ireal+1
	   endif ! end ktype1
	   goto 888
	endif !end else)
777	continue
c     Remove 'cursor' before leaving
	if(table) call wrgwin1(ixlo,iylo,ixhi,iyhi,marray)
	deallocate(marray)
	if(text(i:i).eq.'_') text(i:i)=char(32)
	      call SETSIZE(size)
	      call setFNT(ifont)
	call WHGSTRING(i,x,y,text,oldtext,angle,size,
     &    	 ijust,char1,ibk,icolor,ndim,2)
	nreal=ireal
	end




	subroutine WHGSTRING(i,x,y,text,oldtext,angle,size,
     &	ijust,char1,ibk,icolor,ndim,iadd)
	character text*(*),oldtext*80,char1*1
c	character text*80,oldtext*80,char1*1
c For HGSTRING
c IADD=0 delete oldtext, add cursor and write text
c IADD=1 delete oldtext, add char1 and cursor to text, and write text
c IADD=2 delete oldtext, write text without the cursor
c
	n1=NBLANK1(oldtext)
	if(n1.gt.0) then
	   call lincols(ibk)
	   call JUSTIFYSTRING(x,y,oldtext(1:n1),angle,size,ijust)
	endif
 	call lincols(icolor)
	if(iadd.eq.1.and.i.le.ndim) then
	   text(i:i)=char1
	   i=i+1
	endif
	if(iadd.ne.2) then
	   if(i.le.ndim) then
		text(i:i)=char(95)		!underline=cursor
	   else
		i=ndim
	   endif
	endif
	call JUSTIFYSTRING(x,y,text(1:i),angle,size,ijust)
	oldtext=text
	RETURN
	end

