	subroutine TITENT(QUERY,TITLE,NLEN)
	character*1 TITLE(NLEN)
	character QUERY*(*)
c Version for MICRO
C Typical call:
c CALL TITENT('Enter title for directory:',TITLE,36)
c
	print *,charnb(QUERY)
c1	FORMAT(1x,50A1)
	print 1
1	format(' .')	!first one
	DO 2 I=1,NLEN-1
2	print 3
3	FORMAT('&.')
	print 4
4	FORMAT('+')
	read 5,TITLE
5	FORMAT(80A1)
c
	RETURN
	END


