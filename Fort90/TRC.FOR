c To test REALCONV
	character string*12
c
4	print 1
1	format(' x = ')
	read 2,x
2	format(g13.6)
c Check that there are not too many sig figs.  Have 12 characters and
c need 4 for 'E-04', + one for dec point + 1 for sign if neg, so can
c have m=7 sig figs (or m=6 if neg)
	m=7
	if(x.lt.0.) m=6
	call REALASC(ROUND1(x,m),string)
	print 5,string
5	format(' From REALASC :',3x,a12)
	call REALCONV(ROUND1(x,m),string)
	print 3,string
3	format(' From REALCONV:',3x,a12)
	goto 4
	end



