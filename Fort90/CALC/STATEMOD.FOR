	subroutine STATEMOD(ilast,jlast,jlasts,nmax,charmod,
     &	statname,statmod)

c To make state diagram with AR, A2R etc rather than o1,c3 etc
c       Calc length of row needed for statmod array, and jlasts for this array
c       In charmod(i,j), each state occupies 2 characters, in statmod(i,j) each
c	  state occupies nmax characters, and there are nsrow(i) states in
c       row(i), maximum=nsmax, found in CHARQ. 
c	   ALLOCATE(statmod(ilast,jlasts))
c	nmax=number of bytes for longest statname
c
	character*10 statname(100)
	character*1 statmod(ilast,jlasts)
	character*2 charmod(25,40)	!to print model
	character*1 s,ch
	ALLOCATABLE statmod1
	logical blank
	character*1 statmod1(:,:)
c
	   do i=1,ilast
		do j=1,jlasts
		   statmod(i,j)=' '
		enddo
	   enddo
c
	   do i=1,ilast
		js=1		!index for cols of statmod
		do j=1,jlast
		   s=charmod(i,j)(1:1)
		   if(s.eq.'o'.or.s.eq.'O') then			!an open state
			m=ichar(charmod(i,j)(2:2))-48		!state number=2nd char
			if(m.eq.0) m=10
			do n=1,nmax
			   statmod(i,js)=statname(m)(n:n)
			   js=js+1
			enddo
		   else if(s.eq.'c'.or.s.eq.'C'.or.
     &	    (ichar(s).ge.48.and.ichar(s).le.57)) then  !a shut state
			if(s.eq.'c'.or.s.eq.'C') then
		   	   m=ichar(charmod(i,j)(2:2))-48		!state number=2nd char
			   if(m.eq.0) m=10
			else	!2 digit number
		   	   call CHTOREAL(charmod(i,j)(1:2),real)
			   m=ifixr(real)
			endif
			do n=1,nmax
			   statmod(i,js)=statname(m)(n:n)
			   js=js+1
			enddo
		   else							!a connector
c if there is a connector this col must be widened by same amount as others
			do n=1,nmax
			   if(n.eq.1) then
				statmod(i,js)=charmod(i,j)(1:1)
			   else if(n.eq.2) then
				statmod(i,js)=charmod(i,j)(2:2)
			   else
				statmod(i,js)='  '	!fill with spaces
			   endif
			   js=js+1
			enddo
		   endif
		enddo
	   enddo
c
c Code above is imperfect! Now look at each column in turn, and remove all
c columns that consist entirely of spaces
	ALLOCATE(statmod1(ilast,jlasts))
c
	j1=0
	do j=1,jlasts
	   blank=.true.	!column j in statmod(i,j) is blank
	   do i=1,ilast	!check each row of col j
		statmod1(i,j)=' '
		ch=statmod(i,j)
		if(ch.ne.char(0).and.ch.ne.' ') blank=.false.	!not blank
	   enddo
c       reach here if col j is entirely spaces
	   if(.not.blank) then
		j1=j1+1
		do i=1,ilast
		   statmod1(i,j1)=statmod(i,j)
		enddo
	   endif
	enddo
c Copy result back from statmod1 to statmod
	do i=1,ilast
	   do j=1,jlasts
		statmod(i,j)='  '
	   enddo
	enddo
	do i=1,ilast
	   do j=1,jlasts
		statmod(i,j)=statmod1(i,j)
	   enddo
	enddo
	DEALLOCATE(statmod1)
c
	RETURN
	end

