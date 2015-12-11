	subroutine MOVEPROG(indfst,backward)
c To update the progress line in SCAN, so it extends up to the input value
c of indfst=absolute value in CONSAM of the first point on display
c Entered with previous positions for upper and lower lines in iprogx1,
c iprogx2 (in common), and left with these replaced by new values.
c If moving backwards then iprogx2 is not changed, iprogx1 is reduced.
c When moving forwards, iprogx2 not changed as long as it is greater
c than iprogx1, but otherwise they are equal
	logical backward
	common/prog/ixbox,iprogx1,iprogx2,bdpp,iyframe,icprog,ibk
c
	ix10=iprogx1		!current position of upper line
	ix20=iprogx2		!current position of lower line
c
c calc  X pixel # for 1st point on display, from indfst
	iprogx1=ifixr(1. + float(indfst)/bdpp)
	if(.not.backward) then
	   if(ix10.eq.ix20) iprogx2=iprogx1
	endif		!otherwise iprogx2 unchanged
c
	if(backward) then
		do i=iprogx1,ix10  !erase upper LINE only, backwards
			i1=i+ixbox
			call rpixel(i1,iyframe-1,icp)
			if (icp.eq.icprog) then
			   call wpixel(i1,iyframe-1,ibk)
			   call wpixel(i1,iyframe-2,ibk)
			endif
		end do
      else		!not backward
		do i=ix10,iprogx1	  	!draw UPPER LINE
			i1=i+ixbox
			call rpixel(i1,iyFRAME-1,icp)
			if (icp.eq.ibk) then
				call wpixel(i1,iyframe-1,icprog)
				call wpixel(i1,iyframe-2,icprog)
			endif
		end do
		if(iprogx2.gt.ix20) then
		   do i=ix20,iprogx2  !draw LOWER LINE
			call rpixel(i,iyframe-3,icp)
			if (icp.eq.ibk) then
				call wpixel(i,iyframe-3,icprog)
				call wpixel(i,iyframe-4,icprog)
			endif
		   end do
		endif
	endif
c
	RETURN
	end

