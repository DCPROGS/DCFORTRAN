	function NKEY()
	logical numlock,numon,kbhit
	character ch,getch
	integer*2 ktype,videotyp
c	character*1 ittinr,ch
c Modif to use
c To input a character without <enter> using UTILITY.LIB routine GETCH
c (waits for input).  This version accepts only
c (1) digits 0-9, for which NKEY=1-9, and 10 if zero hit
c (2) F1-F10, for which NKEY= -1 to -10
c (3) ESC key, for which NKEY=-16,
c (4) ENTER key, for which NKEY=16
c otherwise waits for another character
c	numon=NUMLOCK()		!record input state of numlock
c	call NUMSET			!set num lock on (UTILITY library)
c Modif so NUMSET can be called before nkey(), so response can be entered
c and waiting for nkey() call. NUMCLR always called before leaving now.
	if(.not.numlock()) call NUMSET
3	continue
	if(kbhit()) then
	   ch=GETCH(ktype)	!get waiting character and carry on
	else
	   ch=GETCH(ktype)	!wait for character
	endif
	ival=ichar(ch)
	if(ktype.eq.3) then	! a digit (or +,-,.)
		i=ichar(ch)-48		! ASCII version of character input
	else if(ktype.eq.0) then	!function key
		i=-(ichar(ch)-58)
c		i=ichar(ch)-58
	else if(ktype.eq.16) then
		i=-16
      else if(ktype.eq.4.and.ival.eq.13) then		!ENTER key
		i=16
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
	if(i.eq.0) i=10
	NKEY=i
c	if(.not.numon) call NUMCLR	!restore input state
	call NUMCLR
	return
	end

