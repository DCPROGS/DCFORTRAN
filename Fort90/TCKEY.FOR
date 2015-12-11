	program TCKEY
	character ch
c
99	print 9
9	format(/' Type a key (ESC to end):')
	call CKEY(ch,ikey)
c	i=nkey()
	print 6,ichar(ch),ichar(ch),ch
6	format(
     & ' value= ',i4/
     & ' value (hex)= ',z2/
     & ' character= ',a1)
	print 8,ch,ikey
8	format(' ch = ',a1,3x,'    ikey = ',i6)
	if(iabs(ikey).ne.16) goto 99
c
	end


