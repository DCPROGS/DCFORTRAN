	subroutine RANSAMP(nr,ntot,iran)
c To select NR digits out of NTOT without repeats. Assume RANDSK already
c called.
c Input NR,NTOT
c Output=array IRAN the LAST NR elements of which contain a random
c	selection of the digits 1 to NTOT with no repeats (see TRAN for test)
	integer IRAN(ntot),temp
	logical debug,caplock
	common/rand/ix,iy,iz
c
	debug()=caplock()
c
c
c initialise IRAN
	do i=1,ntot
	   iran(i)=i
	enddo
c
c First choose a random value from the 1st ntot elements. Swap the chosen
c one to end of array, and choose 1 randomly from the 1st (ntot-1) elements
c etc
	do ir=1,nr
	   jr=ntot-ir+1   !jr=ntot,ntot-1,...,ntot-nr+1; eg 30,29,...,16 for nr=15
4	   u=random()			!uniform(0,1)
	   j=1 + ifix(float(jr)*u)	!random 1,...,jr
c If u=1.0 exactly this could (rarely) give jr+1
	   if(j.gt.JR) then
c		nbad=nbad+1
		call BELL(1)
		goto 4	!skip bad ones
	   endif
c Swap element #j with the last (#jr)
c	   if(debug()) print 21,U,jr,j
c21	   format(' u,jr,j= ',f9.6,2i8)
	   temp=iran(j)
	   iran(j)=iran(jr)
	   iran(jr)=temp
	enddo
c This leaves the random selection in the LAST nr elements of IRAN
c so now rotate?#######
	RETURN
	end


