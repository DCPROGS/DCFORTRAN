	subroutine CONV41(icalc,jtrans,ntrans,idn,idnold,ke,ifilt,nfilt)
	integer icalc(1024)
	integer ifilt(128,100)
c CONV41 is designed to modify icalc() when one transition time, idn(jtrans)
c is altered; total number of points, ke, is altered if the transition time
c being altered is the last one.
c idn=new value of idn(jtrans),idnold=old value (both scalar)
c =====NB -could output 
c	(1) the indices of the bit of icalc() that have been changed so need
c		erase and redraw only these bits, not the whole thing
c		-or might be almost as fast to have version of DCPIX that tests
c		if xdisp=xlast and fcalc=fclast	-if both true then can skip
c		getpix and setpix
c	(2) indices of horizontal sections, so these can be drawn as lines
c		rather than indiv pixels (? -but could not prevent overwriting
c		of existing points on display if this done)
	jt=jtrans
c First subract old value from icalc(). Want to start at ix=itim-idn=1 so
c itim=idnold+1
c
c subtract out old filter
	do 60 ix=1,nfilt
	itim=ix+idnold
 	icalc(itim)=icalc(itim) - ifilt(ix,jt)
60	continue
c Add back the filter for new idn
	do 61 ix=1,nfilt
	itim=ix+idn
 	icalc(itim)=icalc(itim) + ifilt(ix,jt)
61	continue
c
	if(jt.eq.ntrans) then		!simplest method OK for last transition
	   ke=itim		!new number of points in icalc()
	   RETURN
	else		!for jt=2,3,...,ntrans-1
	   if(idn.lt.idnold) then	!add ilast from end of new filter
		idelt=idnold-idn		! up to start of old filter
		ilast=ifilt(nfilt,jt)
		do 64 i=1,idelt
		itim=itim+1
 		icalc(itim)=icalc(itim) + ilast
64		continue
	   else if(idn.gt.idnold) then	!subtract ilast from end of
		idelt=idn-idnold			! old filter to start of new
		ilast=ifilt(nfilt,jt)
		itim=idnold+nfilt
		do 65 i=1,idelt
		itim=itim+1
 		icalc(itim)=icalc(itim) - ilast
65		continue
	   endif
	endif
	RETURN
	end

