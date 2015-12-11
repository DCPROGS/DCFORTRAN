	subroutine SWAP(titled,titleds,nset,njset,icop)
c To copy titled into titleds (icop=0) or vice versa (icop=1)
c Titled=60 character set titles used in iver=1001
c Titleds=40 character set titles used in iver=1002
	character*60 titled(njset)
	character*40 titleds(10)
c
	if(icop.eq.0) then
	   do 1 i=1,nset
1	   titleds(i)=titled(i)(1:40)
	else
	   do 2 i=1,nset
	   titled(i)(1:40)=titleds(i)
2	   titled(i)(41:60)='                   '
	endif
	RETURN
	end
