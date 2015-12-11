	program TDCF
c To test DCFORMAT
c	character cnum*11
	character cnum*13
c
4	print 1
1	format(' x,if,ir = ')
	read 2,x,if,ir
2	format(g13.6,2i8)
	call DCFORMAT(x,if,ir,cnum)
	print 3,cnum,cnum(1:if)
3	format(1x,a13,/,1x,a13,/)
	call EFORMAT(x,cnum)
	print 5,cnum
5	format(1x,a13)
	do i=1,13
        cnum(i:i)=char(32)
	enddo
	call REALTOCH(x,cnum,13)
	print 6,cnum
6	format(' REALTOCH(x): ',/,1x,a13)
c No point in having more than 6 sig figs for single prec
	call REALTOCH(round1(x,6),cnum,13)
	print 7,cnum
7	format(' REALTOCH(round1(x,6)): ',/,1x,a13)
	goto 4
	end


