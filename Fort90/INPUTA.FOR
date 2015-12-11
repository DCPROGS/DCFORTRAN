	subroutine INPUTa(ans)
c To input yes/no.  If enter is hit, the input value of ans is returned
	character*1 ans,ans1,UC
c
	ans1=ans	!save input
	read 101,ans
101	format(a1)
	if(UC(ans).ne.'Y'.and.UC(ans).ne.'N') then
	   ans=UC(ans1)
	else
	   ans=UC(ans)
	endif
c
	RETURN
	end
