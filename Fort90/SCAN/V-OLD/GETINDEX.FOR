	subroutine GETINDEX(k,klast,if1,iflast,ifits,nfits,ival,nfmax)
c Subroutine for LISTDATA and GOBACKS in SCAN
c Output:
c   if1= fit number (index of timsav etc) for fit that
c starts with tint(k) (returns if1=-1 if no fit so starts).
c   ival=1,2,... for first, second etc transition in fit
c   iflast= last fit number checked
c   klast= index in tint() for 1st transition of last fit (=1 initially)
c Input:
c   if1:   fit # to start search (e.g.iflast+1)
c   klast= index in tint() for 1st transition of last fit (=1 initially)
c   iflast= last fit number checked
c
	integer*2 ifits(nfmax)
c
	ival=1			!1st transition in fit
	do j=if1,nfits
	   if(ifits(j).eq.k) then     !transition # k starts fit # j
		if2=j				!fit #
		goto 1
	   else if(ifits(j).gt.k) then     !already past it
		if2=j				!fit #
		ival=k-klast+1
		goto 2
	   endif
	enddo
	if1=-1			!tint(k) is not start of a fit
	ival=k-klast+1
	iflast=nfits
	RETURN
c
1	continue
	if1=if2			!return fit # corresp to tint(k)
	klast=k
	iflast=if2
	RETURN
c
2	continue
	if1=-1			!tint(k) is not start of a fit
	iflast=if2
	RETURN
	end

