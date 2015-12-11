	integer function NBLANK2(text,n)
c	Version of NBLANK1 that includes as a parameter n=declared dimension
c of 'text' in the calling program.  This allows unambigous decision as
c to whether the is room to add ASCII 0 at end of the text.
c	NBLANK1 is Replacement for Lahey NBLANK function that is appropriate
c for zero-delimited strings, as used in Hgraph.  Returns the length of
c the string as the position of the first ASCII 0 encountered (so the
c length INCLUDES the 0).  If no 0 found, then looks for last non-blank
c (ascii 32) character, and appends ASCII 0 terminator.  Thus an empty
c string returns nblank1=1 (not 0) and text(1:1) is set to char(0).
c (Nblank seems to count ascii 32 only)
c Modif 02/17/93 10:20am because if last character of input array
c contains a valid character there is no room for a zero terminator, but
c I think it is prob not needed in this case, so returns NBLANK2=n and
c does not add ASCII 0 in this case
	character text*(*)
c
c Version that returns string that is terminated by ASCII 0, and length
c includes this and so is always => 1
1	continue
	do i=1,n
	   ival=ichar(text(i:i))
	   if(ival.eq.0) then	!end (zero delimited) found
		nblank2=i
		RETURN
	   endif
	enddo
c If zero terminator NOT found, look again for characters, starting
c at end
c First check whether last character is blank -if not no ASCII 0 added
	if(n.eq.0) then
	   nblank2=1      !all blank
	   RETURN
	else
	   ival=ichar(text(n:n))
	   if(ival.ne.0.and.ival.ne.32) then	!last char is non-blank
	      nblank2=n
	      RETURN
	   endif
	endif
c now check the rest
	do i=n-1,1,-1
	   ival=ichar(text(i:i))
	   if(ival.ne.0.and.ival.ne.32) then	!non-blank character found
		text(i+1:i+1)=char(0)		!ensure zero delimiter
		nblank2=i+1				!in zero
		RETURN
	   endif
	enddo
	text(1:1)=char(0)
	nblank2=1	!all blank
	RETURN
	end
