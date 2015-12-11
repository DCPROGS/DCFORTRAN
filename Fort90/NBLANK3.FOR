	integer function NBLANK3(text,ndim)
c	Version of NBLANK1 that includes as a parameter ndim=declared dimension
c of 'text' in the calling program.  This allows unambigous decision as
c to whether the is room to add ASCII 0 at end of the text
c   This version is useful when concatenating strings that may contain some
c ASCII zero characters.  It counts back from the end of the string and looks
c for the first character that is not either ASCII zero OR ASCII 32. The
c position of this character is returned as nblank3.  If it is not the last
c character an ascii zero is inserted in position n+1, but concatenating
c text(1:n) with another will exclude the ascii 0 terminator.
c
c Modif 07/04/02 07:21am so that, after finding end of string, it searches
c backwards through the string and if any more ASCII 0 found they are
c replaced by ASCII 32.
c
	character text*(*)
c
1	continue
	do i=ndim,1,-1
	   ival=ichar(text(i:i))
	   if(ival.ne.0.and.ival.ne.32) then
		n=i
		goto 2
	   endif
	enddo
c If string contains only 0 and 32 then return n=1 rather than n=0
c (n=0 may give errors in subsequent operations)
	text(1:1)=char(0)
	nblank3=1
	RETURN
c
2	continue
	if(n.lt.ndim) then
	   text(n+1:n+1)=char(0)
	endif
c If earlier ascii 0 found, replace with ascii 32
	do i=n,1,-1
	   ival=ichar(text(i:i))
	   if(ival.eq.0) then
		text(i:i)=char(32)
	   endif
	enddo
c
	nblank3=n
	RETURN
	end
