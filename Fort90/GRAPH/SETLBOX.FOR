	subroutine SETLBOX(nbox,Lbox,j)
c sets elements i=1,2,...,nbox of Lbox equal to j, remainder to zero
c which is the most commonly required input for DRAWBOX
	integer Lbox(30)
c
	do 1 i=1,30
1	Lbox(i)=0
	if(nbox.gt.0) then
	   do 2 i=1,nbox
2	   Lbox(i)=j
	endif
	RETURN
	end

