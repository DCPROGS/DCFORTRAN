	subroutine CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)
c To calculate various indices for section #isec
c Input:
c   isec=section number
c   nbuf,nrlast,novlap,nsec
c Output:
c  Absolute indices:
c   in1=1 + (isec-1)*nbuf = abs index of 1st point in section (exc overlap)
c   in2=isec*nbuf = abs index of last point in section (exc overlap)
c   jn1=in1 - novlap = abs index of 1st point actually read in (inc overlap)
c   jn2=in2 + novlap = abs index of last point actually read in (inc overlap)
c  Indices in idata (within section)
c   is1=1 = index of 1st point in section (exc overlap)
c   is2=nbuf (or nrlast for isec=nsec)= index of last point (exc overlap)
c   js1=is1 - novlap = index of 1st point (inc overlap)
c   js2=is2 + novlap = index of last point (inc overlap)
c  also outputs
c   nread=total number of values read in (inc overlaps)
c   jn1next=jn1 for next section (undefined if isec=nsec in call)
c
c Point #1, of section #isec, has index in whole data set of
	in1=1 + (isec-1)*nbuf	!always
	is1=1				!always
	jn1=in1-novlap		!actual index that includes extension
	js1=is1-novlap		!index in section of 1st point (inc overlap)
	if(isec.eq.1) then
	   js1=1
	   jn1=1
	endif
c
c Define number of values to read
	nread=nbuf+2*novlap			!extension at both ends
	is2=nbuf
c modify when nec, and define jn2,js2
	if(nsec.gt.1) then
	   if(isec.eq.1) then
		nread=nbuf+novlap
		in2=in1-1+nbuf
	   else if(isec.eq.nsec-1) then
		if(nrlast.lt.novlap) then	!not enough points in last bit for ovlap
		   nread=nbuf+novlap+nrlast
		   in2=in1-1+nbuf
		endif
	   else if(isec.eq.nsec) then
		nread=novlap+nrlast	!last sec
		in2=in1-1+nrlast
	      is2=nrlast
	   endif
	else if(nsec.eq.1) then
	   nread=nrlast			!only one section!
	   in2=in1-1+nrlast
	   is2=nrlast
	endif
	jn2=jn1-1+nread
	js2=js1-1+nread
c
	if(isec.lt.nsec) then
	   jn1next=1 + isec*nbuf - novlap
	endif
c
	RETURN
	end


