	program TKT
	character ch,ch2,getch
	integer*4 ktype
	integer*2 ktype2
c
1	ch=GETCH(ktype)
	ch2=GETCH(ktype2)
	if(ktype.eq.0.and.ichar(ch).eq.60) goto 99	!abort
	goto 1
99	END



