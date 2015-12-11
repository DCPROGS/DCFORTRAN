	subroutine GETINDEX(k,k0,if1,iflast,ifits,nfits,ival,nfmax)
c Subroutine for LISTDATA and GOBACKS in SCAN
c Output:
c   if1= fit number (index of timsav etc) for fit that
c 	starts with tint(k)
c   k0=index in tint() for first interval in fit #if1
c   ival=1,2,... for first, second etc transition in fit
c   iflast= last fit number checked (starts here, so set=1 for first call)
c
c Input:
c   k=index in tint()
c   iflast= last fit number checked (starts here, so set=1 for first call)
c
	integer*4 ifits(nfmax)
c
	if(iflast.le.0) iflast=1
	do j=iflast,nfits
	   ifj=ifits(j)
	   if(ifj.eq.k) then     !transition # k starts fit # j
		if1=j				!fit #
		ival=1
		iflast=if1
		k0=k				!index of tint for 1st trans in fit
		goto 1
	   else if(ifj.gt.k) then     !already past it
c         j=fit # following that which contains tint(k)
		if1=j-1			 !fit #
		iflast=if1
		k0=ifits(j-1)	!index of tint for 1st trans in fit
		ival=k + 1 - k0
		goto 1
	   else if(j.eq.nfits) then	!this is last fit
		if1=j
		iflast=if1
		k0=ifits(j)		!index of tint for 1st trans in fit
		ival=k + 1 - k0
		goto 1
	   endif
	enddo
1	continue
	RETURN
	end

