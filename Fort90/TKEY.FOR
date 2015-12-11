	program TKEY
	character ch,getch
c	integer*2 ktype
c
7	print 5
5	format(/' Type a character (ESC to quit):')
	ch=GETCH(ktype)
c	call CTOA(ch,ia)		!same as ICHAR(ch)
	print 6,ktype,ichar(ch),ichar(ch),ch
6	format(
     & ' k type= ',i4/
     & ' value= ',i4/
     & ' value (hex)= ',z2/
     & ' character= ',a1)
	if(ktype.eq.16) goto 99
	goto 7
c
99	continue
	end


