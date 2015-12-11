	subroutine SETCKEY(ktype,ival,ikey)
c To translate ktype, and ival=ichar(ch), into value of ikey that would
c be returned from CKEY when character ch=GETCH(ktype) is entered
c except does not count letters as valid characters
c Returns ikey=99 if no options valid
	integer*2 ktype
c
	m=ival
	ikey=99
	if(ktype.eq.3) then
		if(m.ge.48.and.m.le.57) then	! a digit 0-9
			ikey=m-48
      	else if(m.eq.43) then	!+ key
			ikey=11
      	else if(m.eq.45) then	!- key
			ikey=12
		else if(m.eq.46) then	! . key
			ikey=15
		endif
	else if(ktype.eq.8.and.m.eq.83) then	! DEL key
		ikey=15
      else if(ktype.eq.4.and.m.eq.13) then	!ENTER key
		ikey=16
	else if(ktype.eq.0) then	!function key
		ikey=-(m-58)		!=-n when Fn hit (n=1,..,10)
	else if(ktype.eq.16) then
		ikey=-16					!ESC key
	else if(ktype.eq.2) then
      	if(m.eq.42) then	!* key
			ikey=13
      	else if(m.eq.47) then	!/ key
			ikey=14
		endif
	endif
	RETURN
	end

