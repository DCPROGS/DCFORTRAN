	subroutine REVCOL(icol)
c This subroutine interchanges black and white so they plot as required 
c on HP550C (see SETCOLS.FOR) (values here are all LAHEY numbering)
	integer icol(100)
c
	do i=1,100
	   if(icol(i).eq.7.or.icol(i).eq.15) then
		icol(i)=0
	   else if(icol(i).eq.0) then
		icol(i)=15
	   endif
	enddo
	RETURN
	end

