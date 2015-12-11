	logical function DIGCHAR(ival)
c Input is ival=ichar(ch) found in call to GETCH
c 06/08/90 07:49am- altered to allow SPACE (ival=32) also
c If the value typed is a 0-9 or - + . e or E then DIGCHAR=true
	digchar=.false.
	if((ival.ge.48.and.ival.le.57).or.
     & (ival.eq.43.or.ival.eq.45.or.ival.eq.46).or.
     & (ival.eq.69.or.ival.eq.101).or.ival.eq.32) digchar=.true.
	RETURN
	end



