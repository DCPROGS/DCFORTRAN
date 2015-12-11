	subroutine RAMPOPT(iramp)
c
4	format(i8)
20	     continue
	     print 25
25	     format(' Enter rise time of ramp ON and OFF [in integer us] ')
	     read 4,i
	       if (i.ge.0) then
		   iramp=int4(i)
		 else
		   goto 20		!get a correct value
		 endif
	RETURN
	end

