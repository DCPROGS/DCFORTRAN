	program testm
	character*1	a,getch
	character*(19)	strn(3,5)
	integer*2	n(3),cwr(3),cwi(3)
c	color for writing
	cwr(1)=15
	cwr(2)=14
	cwr(3)=13
c	color for window
	cwi(1)=2
	cwi(2)=1
	cwi(3)=0
c	number of windows:
	irec=3
c	number of strings in each window
	n(1)=3
	n(2)=4
	n(3)=5
c	calculate maximum number of strings/window
	nmax=n(1)
	do i=2,irec
		if(n(i).ge.n(i-1)) nmax=n(i)
	end do
	strn(3,1)='this is first str3'
	strn(3,2)='this is second str3'
	strn(3,3)='this is third str3'
	strn(3,4)='this is fourth str3'
	strn(3,5)='this is fifth str3'
	strn(2,1)='this is first str2'
	strn(2,2)='this is second str2'
	strn(2,3)='this is third str2'
	strn(2,4)='this is fourth str2'
	strn(1,1)='this is first str1'
	strn(1,2)='this is second str1'
	strn(1,3)='this is third str1'
	call mode(18)
	call clears(0,480,15)
	a=getch(b)
	call multiple_menu2(20,440,strn,n,nmax,cwr,cwi,irec)
	a=getch(b)
	call mode(3)
	end
