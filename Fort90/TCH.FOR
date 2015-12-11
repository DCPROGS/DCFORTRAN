	character title*10
	integer ival(10)
c
	title='title'
	do i=1,10
	  ival(i)=ichar(title(i:i))
	enddo
	nl=NBLANK(title)
	call TSUB(title(1:nl),nl)
c
	title='title     '
	do i=1,10
	  ival(i)=ichar(title(i:i))
	enddo
	nl=NBLANK(title)
	call TSUB(title(1:nl),nl)
c
	title='title     '
	nl1=NBLANK1(title)
	do i=1,10
	  ival(i)=ichar(title(i:i))
	enddo
	call TSUB(title(1:nl1),nl)
c
	end

	subroutine tsub(string,nl)
	character*(*) string
	integer ival(10)
c
c	do i=1,10
c	  ival(i)=ichar(string(i:i))
c	enddo
	nl=NBLANK(string)
	return
	end


