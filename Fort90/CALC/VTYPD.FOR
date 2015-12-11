	SUBROUTINE VTYPD(VECTOR,title,KC,KD1)
c
c 06/21/89 10:35am Lahey version; up to npr=6 cols before wrapping
c 01/25/01 08:06am Version of ATYPD to print vector of any length

C	TYPES COL NUMBERS 1 TO KC.
C	KD1,KD2=DIMENSIONS OF ARRAY DECLARED IN CALLING PROG
c	Title (8 char) also typed first
C
	real*8 vector(KD1)
	character*8 title
	logical discprt
	common/dp/discprt
C
c
c Define number of values per row (was 5, now 6) (but still needs to
c be set numerically in format statements at present)
	npr=6
c
c Type title
	if(title.ne.'        ') then
	   print 20, title
         if(discprt) write(8,20)title
20	   format(1x,A8,' =')
	endif
c Number of rows on which vector must be printed, when
c there are npr values per printed row
	nr=1+(kc-1)/npr
c
C TYPE COL NUMBERS
	js=1
	do n=1,nr
	   je=js+npr-1
	   if(je.gt.kc) je=kc
	   print 100,(j,j=js,je)
	   if(discprt) write(8,100) (j,j=js,je)
100	   FORMAT(' j =',1X,6(I2,11X))
	   js=js+npr	!for next row
	enddo
c Print values
	js=1
	do n=1,nr
	   je=js+npr-1
	   if(je.gt.kc) je=kc
	   print 6,(vector(j),j=js,je)
	   if(discprt) write(8,6) (vector(j),j=js,je)
6	   format(3x,6g13.6)
	   js=js+npr	!for next row
	enddo
c
	RETURN
	END

