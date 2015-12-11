	subroutine CKEY(ch,ikey)
c Generalization of NKEY that waits for a keyboard input and
c returns the character that was typed, and integer*2 ktype, if the
c character was
c Modified 06/13/98 09:28pm so arrow keys also returned
c
c  (1) a number (0-9); the number is returned in IKEY, ikey=10 if 0 hit;
c	 also returns ch=' '.
c  (2) a function key (ktype=0); returns ikey=-1 to -10 as in NKEY (and ch=' ')
c  (3) ENTER key; returns ikey=16 as in NKEY  (and ch=' ')
c  (4) ESC key; returns ikey=-16 as in NKEY  (and ch=' ')
c  (5) any letter (A-X,a-z) (ktype=3); always returns, in CH, the UPPER case
c	 letter, and ikey=-100,
c  (6) +,-,*,/,. return ikey=11,12,13,14,15 (and ch=' ') for use in DCMENU
c	 menus with 16 boxes.
c Any other character is invalid -waits for a valid character
c
	logical numlock,kbhit
	character*1 ch,getch
c	integer*2 ktype
	integer*2 videotyp
	if(.not.numlock()) call NUMSET
3	continue
	ch=GETCH(ktype)	!wait for character
c	if(kbhit()) then
c	   ch=GETCH(ktype)	!get waiting character and carry on
c	else
c	endif
	ival=ichar(ch)
	if(ktype.eq.3) then
		if(ival.ge.48.and.ival.le.57) then	! a digit 0-9
			ikey=ival-48
			ch=' '
      	else if(ival.eq.43) then	!+ key
			ikey=11
			ch=' '
      	else if(ival.eq.45) then	!- key
			ikey=12
			ch=' '
		else if(ival.eq.46) then	! . key
			ikey=15
			ch=' '
		endif
	else if(ktype.eq.8) then
		if(ival.eq.83) then	! DEL key
		   ikey=15
		   ch=' '
		else if(ival.eq.77) then	!right arrow
		   ikey=77
		   ch=' '
		else if(ival.eq.75) then	!left arrow
		   ikey=75
		   ch=' '
		else if(ival.eq.72) then	!up arrow
		   ikey=72
		   ch=' '
		else if(ival.eq.80) then	!down arrow
		   ikey=80
		   ch=' '
		endif
      else if(ktype.eq.4.and.ival.eq.13) then	!ENTER key
		ikey=16
		ch=' '
	else if(ktype.eq.0) then	!function key
		ikey=-(ival-58)		!=-n when Fn hit (n=1,..,10)
		ch=' '
	else if(ktype.eq.16) then
		ikey=-16					!ESC key
		ch=' '
	else if(ktype.eq.2) then
		if((ch.ge.'a').and.(ch.le.'z').or.
     &    	 	(ch.ge.'A').and.(ch.le.'Z')) then
			if(ival.gt.90) ch = char(ichar(ch)-32)	!make UC
			ikey=-100
      	else if(ival.eq.42) then	!* key
			ikey=13
			ch=' '
      	else if(ival.eq.47) then	!/ key
			ikey=14
			ch=' '
		endif
	else
	   call BELL(1)
	   if(videotyp().eq.3) then
		print *,' Invalid key'
	   else
c if in graphics, pop-up warning for 1 second
c	subroutine POPTIME(ixlo,iylo,strings,n,ic,ibk,icf,secs)
		call POPTIME(250,130,'INVALID KEY',1,14,12,14,1.0)
	   endif
	   goto 3			!try again
	endif
c
	if(ikey.eq.0) ikey=10
	call NUMCLR
	return
	end


