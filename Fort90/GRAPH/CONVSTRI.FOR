	subroutine convstring(old,ndim)
c To convert strings with Hgraph formatting characters (eg from old plot queues)
c to Gino.  NB New string may be longer than old, so ndim added to arguments
c
	character*(ndim)  old
	character*150 new
	do i=1,150
	   new(i:i)=char(32)
	enddo

	nold=nblank1(old)
	k=1
	l=1
	i=1
c	do while(i.le.(nold-1))
	do while(i.lt.(nold))
		j=k
		i=l
		if (old(i:i).eq.char(6)) then
		   if(old(i+1:i+1).eq.'a') new(j:j+4)='*F010'
		   if(old(i+1:i+1).eq.'A') new(j:j+4)='*F010'
		   if((old(i+1:i+2).eq.'a;').or.(old(i+1:i+2).eq.'A;'))
     &         new(j:j+5)='*F070U'
		   if(old(i+1:i+1).eq.'b') new(j:j+4)='*F070'
		   if(old(i+1:i+1).eq.'B') new(j:j+4)='*F070'
		   if(old(i+1:i+1).eq.'0') new(j:j+4)='*F100'
		   if(old(i+1:i+1).eq.'1') new(j:j+4)='*F101'
		   if(old(i+1:i+1).eq.'2') new(j:j+4)='*F102'
		   if(old(i+1:i+1).eq.'3') new(j:j+4)='*F103'
		   if(old(i+1:i+1).eq.'4') new(j:j+4)='*F104'
c		   if(old(i+1:i+1).eq.'5') new(j:j+4)='*F105'
		   if(old(i+1:i+1).eq.'5') new(j:j+4)='*F006'
		   if(old(i+1:i+1).eq.'6') new(j:j+4)='*F106'
		   if(old(i+1:i+1).eq.'7') new(j:j+4)='*F107'
		   if(old(i+1:i+1).eq.'8') new(j:j+4)='*F007'
		   if(old(i+1:i+1).eq.'9') new(j:j+4)='*F008'
		   if(old(i+1:i+1).eq.'*') then
			new(j:j+2)='*FR'
			k=k+3
			l=l+2
		   else
		      k=k+5
			l=l+2
		   endif
		else if (old(i:i).eq.char(14).or.old(i:i).eq.char(13)) then
		   new(j:j+1)='*N'
		   k=k+2
		   l=l+1
		else if (old(i:i).eq.char(21)) then
		   new(j:j+1)='*E'
		   k=k+2
		   l=l+1
		else if (old(i:i).eq.char(4)) then
		   new(j:j+1)='*I'
		   k=k+2
		   l=l+1
		else if (old(i:i).eq.char(9)) then
		   new(j:j+1)='*F006'
		   k=k+5
		   l=l+1
		else
		   new(j:j)=old(i:i)
		   l=l+1
		   k=k+1
		endif
	enddo

	nnew=nblank1(new)
	do i=1,nnew
	   if(i.le.ndim) then
		old(i:i)=new(i:i)
c		it=ichar(old(i:i))
	   endif
	enddo
	if(nold.gt.nnew) then
	do i=nnew+1,nold
	   old(i:i)=char(32)
	enddo
	endif
c	it=ichar(old(nnew:nnew))
	if(nnew.le.ndim) then
	   n=nnew
	else
	   n=ndim
	endif
	if(old(n:n).eq.char(8).or.old(n:n).eq.char(9).or.
     & old(n:n).eq.char(10).or.old(n:n).eq.char(13)) then
	   old(n:n)=char(32)
	endif

	end
