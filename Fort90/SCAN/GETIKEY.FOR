	subroutine GETIKEY(ikey)
c To return ikey =-1 etc, or ikey=1-15, as in CKEY
c Version of CKEY designed to be called in loop if kbhit()=true
c Must set Num Lock on outside the loop if req (call NUMSET) and
c switch it off again when loop is left (call NUMCLR) -not done here
c Returns
c  (1) a number (0-9); the number is returned in IKEY, ikey=10 if 0 hit
c  (2) a function key (ktype=0); returns ikey=-1 to -10 as in NKEY
c  (3) ENTER key; returns ikey=16 as in NKEY
c  (4) ESC key; returns ikey=-16 as in NKEY
c  (5) +,-,*,/,. return ikey=11,12,13,14,15 for use in DCMENU
c	 menus with 16 boxes.
c Any other character is invalid -returns ikey=-99
	character*1 ch,getch
	integer*2 ktype
c
	ch=GETCH(ktype)	!get the character
c
	ival=ichar(ch)
	if(ktype.eq.3) then
		if(ival.ge.48.and.ival.le.57) then	! a digit 0-9
			ikey=ival-48
      	else if(ival.eq.43) then	!+ key
			ikey=11
      	else if(ival.eq.45) then	!- key
			ikey=12
		else if(ival.eq.46) then	! . key
			ikey=15
		endif
	else if(ktype.eq.8.and.ival.eq.83) then	! DEL key
		ikey=15
      else if(ktype.eq.4.and.ival.eq.13) then	!ENTER key
		ikey=16
	else if(ktype.eq.0) then	!function key
		ikey=-(ival-58)		!=-n when Fn hit (n=1,..,10)
	else if(ktype.eq.16) then
		ikey=-16					!ESC key
	else if(ktype.eq.2) then
      	if(ival.eq.42) then	!* key
			ikey=13
      	else if(ival.eq.47) then	!/ key
			ikey=14
		endif
	else
		call BELL(1)
		ikey=-99		!invalid character
	endif
c
	if(ikey.eq.0) ikey=10
c
	RETURN
	end


