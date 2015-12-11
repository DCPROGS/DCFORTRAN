	subroutine TFORMAT(t,cnum)
c To format durations in SCAN
	character cnum*11
c
	if(t.lt.1.0) then
         call DCFORMAT(t,6,3,cnum)		!duration
	else if((t.ge.1.0).and.(t.lt.10.)) then
         call DCFORMAT(t,6,2,cnum)		!duration
	else if((t.ge.10.).and.(t.lt.100.)) then
         call DCFORMAT(t,6,1,cnum)		!duration
	else
         call DCFORMAT(t,6,0,cnum)		!duration
	endif
	RETURN
	end

