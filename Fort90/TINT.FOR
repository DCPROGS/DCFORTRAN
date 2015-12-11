c	test chtoint
	character*11 cnum
c
2	call TITENT0(' ASCII integer:',cnum,11,.true.)
	call CHtoINT(cnum,int)
	print 1,int
1	format(' int = ',i11)
	if(int.ne.999) goto 2
	end


