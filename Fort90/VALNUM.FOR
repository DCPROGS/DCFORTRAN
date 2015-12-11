	logical function VALNUM(ch)
c To test whether character CH contains a valid number representation
c ie all its characters satisfy DIGCHAR a valid number elements AND
c it is not all blanks (digchar allows blanks a valid)
	character*(*) CH
	logical digchar
c
	nl=NBLANK(ch)
	if(nl.eq.0) then
	   VALNUM=.false.
	   RETURN
	endif
	do 1 i=1,nl
	   ival=ichar(ch(i:i))
	   if(.not.DIGCHAR(ival)) goto 2
1	continue
	VALNUM=.true.
	RETURN
2	continue
	VALNUM=.false.
	RETURN
	end



