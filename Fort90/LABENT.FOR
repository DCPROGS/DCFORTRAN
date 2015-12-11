	subroutine LABENT(QUERY,TITLE,NLEN)
c	character*1 TITLE(NLEN)
	integer*2 ktype
	character*1 ch1,getch
	character QUERY*(*),TITLE*(*)
c Version of TITENT that will accept take control characters (eg ctrl-F for
c font control in Hgraph strings)
C Typical call:
c CALL LABENT('Enter axis label:',TITLE,36)
c
	do 20 i=1,nlen
20	title(i:i)=char(32)
	print *,charnb(QUERY)
c1	FORMAT(1x,50A1)
	print 1
1	format(' .')	!first one
	DO 2 I=1,NLEN-1
2	print 3
3	FORMAT('&.')
	print 4
4	FORMAT('+')
	i=0
7	continue
	ch1=GETCH(ktype)
	ival=ichar(ch1)
	if(ktype.eq.4.and.ival.eq.13) goto 99	!ENTER key
	if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
	   print 6,ch1 		!move back
	   print 6,char(32) 	!print blank on screen
	   print 6,ch1 		!move back again to overwrite blank
	   title(i:i)=char(32)	!remove bad character from string
	   i=i-1
	   goto 7
	endif
	i=i+1
	title(i:i)=ch1
	print 6,ch1
6	format('&',a1)
	goto 7	!next char
c	read 5,TITLE
c5	FORMAT(80A1)
c
99	nlen=i
	RETURN
	END


