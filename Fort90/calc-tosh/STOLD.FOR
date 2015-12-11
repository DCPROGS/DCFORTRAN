	subroutine STATEMOD(ilast,jlast,jlasts,nm2,charmod,
     &	statname,statmod)

c To make state diagram with AR, A2R etc rather than o1,c3 etc
c       Calc length of row needed for statmod array, and jlasts for this array
c       In charmod(i,j), each state occupies 2 characters, in statmod(i,j) each
c	  state occupies nmax characters, and there are nsrow(i) states in
c       row(i), maximum=nsmax, found in CHARQ.  Each state occupies nmax-2 more
c       characters,(nmax-2)/2 more elements, than in charmod(i,j)
c	   jlasts=jlast+nsmax*(nmax-2)/2
c	   jlasts=2+jlast+nsmax*nmax/2
c	   ALLOCATE(statmod(ilast,jlasts))
c	nm2=number of 2-byte chunks for longest statname
c
	character*10 statname(100)
	character*2 statmod(ilast,jlasts)
	character*2 charmod(25,40)	!to print model
	character*1 s
	character*2 e32,e0,ch		!for 'empty'
	ALLOCATABLE jblank,statmod1
	character*2 statmod1(:,:)
	integer jblank(:)
c
	   do i=1,ilast
		do j=1,jlasts
		   statmod(i,j)='  '
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
			n1=1
			do n=1,nm2
			   statmod(i,js)=statname(m)(n1:n1+1)
			   n1=n1+2
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
			n1=1
			do n=1,nm2
			   statmod(i,js)=statname(m)(n1:n1+1)
			   n1=n1+2
			   js=js+1
			enddo
		   else							!a connector
c if there is a connector this col must be widened by same amount as others
			if(nm2.eq.1) then
			   statmod(i,js)=charmod(i,j)
			   js=js+1
			else
			   do n=1,nm2
				if(n.eq.1) then
				   statmod(i,js)=charmod(i,j)
				else
				   statmod(i,js)='  '	!fill with spaces
				endif
				js=js+1
			   enddo
			endif
		   endif
		enddo
	   enddo
c
c Code above is imperfect! Now look at each column in turn, and remove all
c columns that consist entirely of spaces
	ALLOCATE(statmod1(ilast,jlasts),jblank(jlasts))
	e32='  '		!=char(32)//char(32)
	e0=char(0)//char(0)
c
	do j=1,jlasts
	   jblank(j)=0
	enddo
	do j=1,jlasts
	   do i=1,ilast	!check each row of col j
		ch=statmod(i,j)
		if(ch.ne.e32.and.ch.ne.e0) goto 2	!jump out when non-empty element found
	   enddo
c       reach here if col j is entirely spaces
	   jblank(i)=1
2	   continue
	enddo
c now remove the blank columns
	j1=0
	do j=1,jlasts
	   if(jblank(j).eq.0) then 	!column j is not blank
		j1=j1+1
		do i=1,ilast
		   statmod1(i,j1)=statmod(i,j)
		enddo
	   endif
	enddo
c
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
	DEALLOCATE(statmod1,jblank)
c
	RETURN
	end

