	subroutine move_red_cursor(ixcr,iycr,iccr,ixframe,ibk)

	character*1	      ch,getch,LC

     	ifram1=iccr-8
	ch=getch(ktype)
	if(ktype.eq.2) ch=LC(ch)	!convert to lower case
	ival=ichar(ch)
	do while (ival.ne.13)
		if (ktype.eq.8) then
				if(ival.eq.77) then
					call draw_cursor(ixcr,iycr,ibk,ibk)
					if (ixcr.lt.ixframe) ixcr=ixcr+5
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.75) then
					call draw_cursor(ixcr,iycr,ibk,ibk)
					if (ixcr.gt.5) ixcr=ixcr-5
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.71) then		!HOME key
					call draw_cursor(ixcr,iycr,ibk,ibk)
					ixcr=0
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.79) then		!END key
					call draw_cursor(ixcr,iycr,ibk,ibk)
					ixcr=ixframe-4
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				endif
		endif
		ch=getch(ktype)
		if(ktype.eq.2) ch=LC(ch)	!convert to lower case
		ival=ichar(ch)
	end do
	end
