	SUBROUTINE ATYPD(ARRAY,title,KR,KC,KD1,KD2)
c
c 06/21/89 10:35am Lahey version; up to npr=6 cols before wrapping
c 01/25/01 08:57am Version of DATYP for any size array
C DATYPv is version of DATYP2 for dimension arrays, and 8 character title.
C VERSION THAT PRINTS EACH ROW ON 2 LINES IF MORE THAN 5 COLS
C**SIMPLIFIED VERSION OF DATYPE WITH NO FACILITY TO RENUMBER
C DOUBLE PRECISION VERSION OF ATYPE (CALL ALTERED SO KD1,KD2 SPECIFIED)
C SUBROUTINE TO TYPE SUBSET OF AN ARRAY.
C	TYPES ROW NUMBERS 1 TO KR.
C	TYPES COL NUMBERS 1 TO KC.
C	KD1,KD2=DIMENSIONS OF ARRAY DECLARED IN CALLING PROG
c	Title (8 char) also typed first
C
	real*8 ARRAY(KD1,KD2)
	character*8 title
	logical discprt
	common/dp/discprt
C
	if(kr.gt.kd1.or.kc.gt.kd2) then
c	   print 11
11	   FORMAT( ' ***ARRAY TOO BIG TO TYPE*** ',/)
	   pause
	   RETURN
	endif
c
c Define number of values per row (was 5, now 6) (but still needs to
c be set numerically in format statements at present)
	npr=6
c
c Type title
	if(title.ne.'        ') then
c	   print 20, title
         if(discprt) write(7,20)title
20	   format(1x,A8,' =')
	endif
c Number of rows on which a single row of Q must be printed, when
c there are npr values per printed row
	nr=1+(kc-1)/npr
c
C TYPE COL NUMBERS
	js=1
	do n=1,nr
	   je=js+npr-1
	   if(je.gt.kc) je=kc
c	   print 100,(j,j=js,je)
	   if(discprt) write(7,100) (j,j=js,je)
100	   FORMAT(' j=',2X,6(I2,11X))
	   js=js+npr	!for next row
	enddo
c Print values
	do i=1,kr
	   js=1
	   do n=1,nr
		je=js+npr-1
		if(je.gt.kc) je=kc
		if(n.eq.1) then
c		   print 5,i,(array(i,j),j=js,je)
		   if(discprt) write(7,5) i,(array(i,j),j=js,je)
5		   format(' i =',i3,/,' ',6g13.6,/)
	      else
c		   print 6,(array(i,j),j=js,je)
		   if(discprt) write(7,6) (array(i,j),j=js,je)
6		   format(' ',2x,6g13.6,/)
		endif
	      js=js+npr	!for next row
	   enddo
	enddo
c
	RETURN
	END

