C**************************************************************************
	SUBROUTINE EXSTR(chary,stry,n,k)
C**************************************************************************

c     Subroutine to separate into strings an array of characters; 
c	character(i)=' ' signifies the end of a string and the beggining of 
c			 another string
c     chary(0:n)	: array of character*1
c	stry(n)	: array of strings
c	k		: nr of resulting strings
c
c     Example:
c	chary : array of characters (GetString return from C a string
c	containing the code errors, ie : 255 0 ;this is returned by
c	Fortran interface like a character array, ie : chary(0)=2,
c	chary(1)=5, etc; we have to separate each number in substrings
c	form: stry(1)=255,stry(2)=0,etc
c

	character*1 chary(0:n)
	character*(*)  stry(n)
	k=1
	l=0
	m=1
2	continue
	j=1
	stry(k)(1:1)=chary(l)
	do i=m,n
		if (chary(i).eq.' ') then
			stry(k)(1:j)=stry(k)(1:j)
			goto 1
		else
			stry(k)(1:j+1)=stry(k)(1:j)//chary(j)
		endif
	j=j+1
	enddo
1     continue
	if (i.lt.n) then
		k=k+1
		l=i+1
		m=i+2
		goto 2
	endif
	end
