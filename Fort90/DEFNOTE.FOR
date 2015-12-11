	subroutine DEFNOTE()
c To define equally tempered scale in common/note/
c
	integer notes(12)
	integer C,DF,D,EF,E,F,GF,G,AF,A,BF,B
	common/note/C,DF,D,EF,E,F,GF,G,AF,A,BF,B
c
	fac=10.**(alog10(2.)/12)	!factorebetween semitones (12/octave)
	notes(1)=264
	do i=2,12
	   notes(i)=ifixr(264.*(fac**(i-1)))
	enddo
	C=notes(1)
	DF=notes(2)
	D=notes(3)
	EF=notes(4)
	E=notes(5)
	F=notes(6)
	GF=notes(7)
	G=notes(8)
	AF=notes(9)
	A=notes(10)
	BF=notes(11)
	B=notes(12)
c
	RETURN
	end

