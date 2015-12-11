	subroutine NEWMOD(charmod,inew,ilast,jlast,pon)
c To print model with new state numbering, without changing original
c specification in charmod()
c Modif 03/19/95 04:16pm by adding common/nmod so can use altered
c charmod in calling program with altering charmod (e.g. in GETQD)
c	character*2 charmod(20,30),cmodnew(20,30)		!to print model
	character*2 charmod(25,40),cmodnew(25,40)		!to print model
	character ch*1
	integer inew(100)
	logical discprt,pon
	common/nmodel/cmodnew
	common/dp/discprt
c
	do i=1,ilast
	   do j=1,jlast
		cmodnew(i,j)=charmod(i,j)
		ival=ichar(charmod(i,j)(2:2))-48
		if(ival.ge.0.and.ival.le.9) then	!digit 0-9
		   if(ival.eq.0) ival=10
		   in=inew(ival)
		   if(in.eq.10) in=0
		   ch=char(in+48)
		   cmodnew(i,j)(2:2)=ch
		endif
	   enddo
	enddo
c Print modified version
	do i=1,ilast
	   print 671,(cmodnew(i,j),j=1,jlast)
	   if(pon) write(7,671) (cmodnew(i,j),j=1,jlast)
	   if(discprt) write(8,671) (cmodnew(i,j),j=1,jlast)
671	   format(4x,10a2)
	enddo
	RETURN
	end

