c To test realtoch
	character ch*11
c
3	print 1
1	format(' x = ')
	read*, x
	call REALTOCH(x,ch,11)
	print 2,ch
2	format(1x,a11)
	goto 3
	end

