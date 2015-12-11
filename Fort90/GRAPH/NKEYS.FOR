	subroutine NKEYS(ikey)
c Subroutine version of NKEY to test problems on Dell 425
	logical numlock,numon,kbhit,debug,caplock
	character ch,getch
	integer*2 ktype
c	character*1 ittinr,ch
c
	debug()=caplock()
c
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
c
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
	if(debug()) print 100,ch,ival,ktype
100	format('& In NKEYS ',2x,a1,3x,2i8)
	if(ktype.eq.3) then	! a digit (or +,-,.)
		i=ival-48		! ASCII version of character input
	else if(ktype.eq.0) then	!function key
		i=-(ival-58)
c		i=ival-58
	else if(ktype.eq.16) then
		i=-16
      else if(ktype.eq.4.and.ival.eq.13) then		!ENTER key
		i=16
	else
		call BELL(1)
		print *,' Invalid key'
		goto 3			!try again
	endif
c
	if(i.eq.0) i=10
	IKEY=i
c	if(.not.numon) call NUMCLR	!restore input state
	call NUMCLR
	return
	end


