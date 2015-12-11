	subroutine SHOWSTR(n,text)
	character text*(*)
	character*3 show(60)
c To show the control characters in TEXT string (e.g under debugger)
c
	do i=1,n
	  ic=ichar(text(i:i))
	  if(ic.gt.33) then
		show(i)=text(i:i)
	  else if(ic.eq.0) then
		show(i)='nul'
	  else if(ic.eq.4) then
		show(i)='^D '
	  else if(ic.eq.6) then
		show(i)='^F '
	  else if(ic.eq.9) then
		show(i)='^I '
	  else if(ic.eq.18) then
		show(i)='^R '
	  else if(ic.eq.19) then
		show(i)='^S '
	  else if(ic.eq.21) then
		show(i)='^U '
	  endif
	enddo
	RETURN
	end

