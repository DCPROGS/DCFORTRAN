	INTEGER FUNCTION INDEXG(itrans,
     & iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c Returns the index in CONSAM of transition number itrans, as fitted in
c SCAN.  Note that if the transition is not the first in a fit, the extra
c intervals are added via tint0 (must supply tint0, before resolutiin imposed,
c or the number of extra intervals that are added will be wrong)
c This is a simplified version of GETINDEX, that calls GETINDEX and calculates
c index in CONSAM of transition number itrans, and returns result in indexg
c Transition number in call must be that BEFORE resolution inposed,
c ie. index(itrans) where index is the array defined in RESINT.

c Input
c itrans=transition number (index in tint0())

c Output
c indexg=index in consam of transition number itrans
c
c Auxiliary input needed for calculation
c iflast=i for first call (output value of iflast can be used as input for
c		locating later transitions
c nfits
c ifits(nfits)
c timsav(nfits)
c tint0(ninnt)
c dfinter
	integer*4 ifits(nfits)
	real*8 timsav(nfits)
	real*4 tint0(nintt)
	real*8 dfinter,tt1
c
	fint=sngl(dfinter/1.d3)		!ms between points
c	iflast=1		!start at first fit
	call GETINDEX(itrans,i0,if1,iflast,ifits,nfits,ival,nfits)
c returns if1=fit # preceding tint0(k), i0=index in tint0() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint0(k), ival=2 if tint0(k) is 2nd interval of fit #if1 etc
	tt1=timsav(if1)
	index=ifixd(tt1)/dfinter	!index for 1st trans of fit
	if(ival.gt.1) then      !add intervals that follow first in fit
	    dt=0.0
	    do j=i0,i0+ival-2	!adds one interval if ival=2 etc
		 dt=dt+tint0(j)
c		 jlast=j
	    enddo
	    index=index+ifixr(dt/fint)
	    tt1=tt1 + 1.d3*dble(dt)	!microsec
	endif
c Define positions of arrows to mark first and last transition in the sequence
c Use itotlen to define position of last transition in the sequence
c (if kval>2) (inseq1, inseq2 are like infit -argument for MARK)
c	inseq1=1 + ifixd(tt1/dfinter)	!abs index of first transition
c	inseq1=inseq1 + npfilt
c	if(kval.gt.2) then
c	   inseq2=inseq1 + ifixr(float(itotlen)*1.e-3/fint)
c	   call WDIALOG(1,
c     &	'First and last transitions marked with red arrows',11)
c	else
c	   inseq2=0
c	   call WDIALOG(1,
c     &       	'Transition marked with red arrow',11)
c	endif
c
	INDEXG=index
	RETURN
	end

