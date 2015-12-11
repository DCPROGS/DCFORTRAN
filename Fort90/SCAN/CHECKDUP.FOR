	subroutine CHECKDUP(name,idisc,nfirst,nfile,jstrec,nextrec,
     &	name1,idisc1,nfirst1,nfile1,jstrec1,nextrec1,same)
	integer*4 jstrec(200),jstrec1(200)
	character name*12,name1*12
	logical same
c
	same=.true.
	if(name.ne.name1) same=.false.
	if(idisc.ne.idisc1) same=.false.
	if(nfirst.ne.nfirst1) same=.false.
	if(nfile.ne.nfile1) same=.false.
	if(nextrec.ne.nextrec1) same=.false.
	do i=1,200
	   if(jstrec(i).ne.jstrec1(i)) then
		same=.false.
		RETURN
	   endif
	enddo
	RETURN
	end

