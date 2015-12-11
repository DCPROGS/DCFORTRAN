	subroutine GETFIT(ic,jfit,jfit1,jflast,timsav,dfinter,nfits)
c To get fit number for the fit that precedes consam(ic)
c The baseline, ibaselin(k), is indexed by fit number (k=1,...,nfits)
c Say j=index in consam so consam(j) is at time j*dfinter=timsav(k)
c so look for timsav(k) that is closest to t=j*dfinter -this gives fit
c number and ibaselin(k) is baseline for this fit
c	Input:
c		timsav(nfits)
c		dfinter
c		ic=index in consam
c		jflast=fit number to start looking (=1 first time)
c	Output:
c		jfit=fit number (jfit=1,...,nfits) that precedes consam(ic)
c		jfit1=fit number (jfit=1,...,nfits) nearest in time to consam(ic)
c
	real*8 timsav(nfits),time,dfinter
c
	time=dfinter*dfloat(ic-1)		!microsec for point consam(ic)
c
	if(time.lt.timsav(1)) then	!before first fit
	   jfit=1
	   jfit1=1
	   RETURN
	endif
	if(time.ge.timsav(nfits)) then	!after last fit
	   jfit=nfits
	   jfit1=nfits
	   RETURN
	endif
c
c Find the fit number that precedes consam(ic)
	jfit=jflast
	do j=jflast,nfits-1
	   if(time.ge.timsav(j).and.time.le.timsav(j+1)) then
		jfit=j	!e.g. if time>timsav(2) then jfit=2 because
c				!ibaselin(2) is baseline fit #2
		goto 1	!jump out
	   endif
	enddo
c  Should not reach here except for time<timsav(1) which is dealt with above
	print*, 'GETFIT ERROR'
1	continue
c	do while(time.lt.timsav(jfit))
c	   jfit=jfit+1
c	enddo
c
c Find closest
	if(jfit+1.le.nfits) then
	   if(dabs(time-timsav(jfit+1)).lt.dabs(time-timsav(jfit))) then
		jfit1=jfit+1
	   else
		jfit1=jfit
	   endif
	else
	   jfit1=jfit
	endif
	RETURN
	end

