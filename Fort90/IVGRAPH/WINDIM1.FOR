	subroutine WINDIM1(ixlo,iylo,ixhi,iyhi,idimen,n,
     & lenmax,ixc0,iyc0)
c	character*(*) strings(n)
c WINDIM1 is version of windim in which max length of a string to be displayed
c is specified as input parameter, lenmax.  Thus the 'strings' arguement
c is no longer needed.  Used eg for POPTABLE in which only a few of many
c values are displayed initially, and these may shorter than strings
c that appear when table is scrolled.
c NB do not call with n+1 because then would attempt to look at strings(n+1)
c  which may not exist (call with ixc0=-1 instead; DC 11/20/93 10:47pm)
c  or ixc0=-2 for 2 extra lines etc
c
c INPUT=ixlo/iyhi, iyhi/iylo, strings,n: rest = output.
c	 n=number of strings to be displayed
c OUTPUT=idimen (dimension for allocating integer*1 arrays)
c       ixc0,iyc0=position to start writing text
c
c  Standard call   -> input ixlo,iyhi : left upper :
c  call WINDIM(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
c
c  When called with iyhi=-1 -> input ixlo,iylo : left bottom
c  call WINDIM(ixlo,iylo,ixhi,-1,idimen,strings,n,ixc0,iyc0)
c
c  When called with ixlo=-1 -> input ixhi,iyhi : right upper
c  call WINDIM(-1,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
c
c  When called with ixlo=-1 and iyhi=-1 -> input ixhi,iylo : right bottom
c  call WINDIM(-1,iylo,ixhi,-1,idimen,strings,n,ixc0,iyc0)
c
c  When called with iyc0=-1 -> leave room for title on box
c
c  When called with ixc0=-1 -> leaves room extra line (eg help text) at bottom
c  When called with ixc0=-n -> leaves room n extra lines (eg for POPTABLE)
c
c Modif 09/28/93 08:13am so also returns start point for writing text
c Modif 11/02/93 04:33am with the input options

	if(ixc0.lt.0) then
	   n1=n+iabs(ixc0)
	else
	   n1=n
	endif
c
c Recalculate ixlo to be divisible by 8
	if(ixlo.eq.-1) then
	   ixhi=ixhi-mod(ixhi,8)-1
	else
	   ixlo=ixlo-mod(ixlo,8)
	endif
c Recalculate iyhi/iylo to be divisible by 16
	if(iyhi.eq.-1) then
	   iylo=iylo-mod(iylo,16)
	else
	   iyhi=iyhi-mod(iyhi,16)
	endif
c Calculate the maximum length of strings(i) =input in WINDIM1
c	lenmax=NBLANK(strings(1))
c	do i=2,n				!NB n (not n1)
c	   nl=NBLANK(strings(i))
c	   if(nl.ge.lenmax) then
c	      lenmax=nl
c	   endif
c	enddo
c	calculate ixhi of window;
	if(ixlo.eq.-1) then
	   ixlo=ixhi-(lenmax+2)*8+1 !here you can make it bigger
	else
	   ixhi=ixlo+(lenmax+2)*8-1 !here you can make it bigger
	endif
c	calculate iylo of window

	if(iyhi.eq.-1) then
	   iyhi=iylo+(n1+2)*16
	   if(iyc0.eq.-1) iyhi=iyhi+16
	else
	   iylo=iyhi-(n1+2)*16
	   if(iyc0.eq.-1) iylo=iylo-16
	endif
	idimen=(iyhi-iylo+9)*(ixhi-ixlo+9)+1
c Define start point for writing text
	ixc0=ixlo+8
	if(iyc0.eq.-1) then
	   iyc0=iyhi-48
	else
	   iyc0=iyhi-32
	endif
	RETURN
	end
