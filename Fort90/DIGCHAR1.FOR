	logical function DIGCHAR1(ival)
c Input is ival=ichar(ch) found in call to GETCH
c Version of digchar in which space not allowed
c If the value typed is a 0-9 or - + . e or E then DIGCHAR=true
	digchar1=.false.
	if((ival.ge.48.and.ival.le.57).or.
     & (ival.eq.43.or.ival.eq.45.or.ival.eq.46).or.
     & (ival.eq.69.or.ival.eq.101)) digchar1=.true.
	RETURN
	end
