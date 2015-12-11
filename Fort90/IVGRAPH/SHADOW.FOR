	subroutine SHADOW(ixlo,ixhi,iylo,iyhi,iwid,ic)
c To draw shadow border (width= iwid pixels) round box)
c iwid=0 means draw a single pixel line, with 1 pixel gap (as in Windows?)
c ic=7 for standard gray/white
c ic=1 dark blue/blue
c ic=2 dark green/green
c ic=3 dark cyan/cyan
c ic=4 dark red/red
c ic=5 dark purple/purple
c ic=6 brown/yellow (!!)
	if(iwid.eq.0) then
	   call HLINE(ixlo-2,ixhi+2,iylo-2,ic+8)
	   call VLINE(ixhi+2,iylo-2,iyhi+2,ic+8)
c	   ic=15
	   call HLINE(ixlo-2,ixhi+2,iyhi+2,ic)
	   call VLINE(ixlo-2,iylo-2,iyhi+2,ic)
	else
	  do i=1,iwid
c	   ic=7
	   call HLINE(ixlo-i,ixhi+i,iylo-i,ic+8)
	   call VLINE(ixhi+i,iylo-i,iyhi+i,ic+8)
c	   ic=15
	   call HLINE(ixlo-i,ixhi+i,iyhi+i,ic)
	   call VLINE(ixlo-i,iylo-i,iyhi+i,ic)
	  enddo
	endif
	RETURN
	end

