	subroutine CONV4(icalc,iy0,ntrans,idn,ke,ifilt,nfilt)
	integer icalc(1024)
	integer idn(99)
	integer ifilt(128,100)
	logical debug,caplock,pon,slock,end
c CONV4 all integer turbo version
c==========
c CONV3 has floating point output in fcalc() -calculates fcalc(1) to fcalc(ke)
c CONV2 is version of CONV1 with time intervals floating rather
c than integer. Values are calc with ICALC(1) being first point
c of the first filter function (t=0), and subsequent values at intervals
c of DTF microsec=time between points of FILT(). DT (microsec) replaces
c integer array IDN() used in EKFIT7.
c To calc filter function for plotting in SCAMP (modif from CONV
c subroutine in TDAT.FOR). Now written as loop so can do up to 100 (now)
c convolutions at once. JT=1,2,..,ntrans=index of current filter.
c Allocates ICALC from element ks (input) to ke (output)
c Sign of filter already incorp in step sizes in IA()
C Y0=initial value (inc baseline).
c
	pon()=slock()
	debug()=caplock()
c
	itim=1		!time=index counter
	end=.false.
c
3	continue			!return here for next time value
	iyval=iy0		!init level
c
c Now loop through all filters at each time point; index, itim, is time
c in units of dtf=sep of filter points
c -filter point i corresponds to t=float(i-1)*dtf so at t=x
	DO 541 Jt=1,NTRANS
c
	ix=itim-idn(jt)			!idn() as in SCAN
c idn(1)=0, idn(2) to idn(ntrans) now = time from first to 2nd,3,..,ntrans
c transition (shifted up one place now)
c If ix<1 then start of filter #jt not reached yet, so nothing to add
	if(ix.ge.1) then
	   if(ix.gt.nfilt) then				!after end of filter #jt
		iyval=iyval+ifilt(nfilt,jt)		! so add last value
		if(jt.eq.ntrans) end=.true.
	   else
 		iyval=iyval + ifilt(ix,jt)		!add JTth filter
	   endif
	endif
541	continue		!end of JT loop
	ind=itim		!index in icalc
	if(ind.le.1024) goto 10
	ke=ind-1
	RETURN		!too many points
10	icalc(ind)=iyval
	if(debug()) print 5451,ind,iyval
5451	format(' i, iy(i)= ',2i8)
	if(end) goto 2	!finished
	itim=itim+1		!next time step
	goto 3
c
2	continue
	ke=ind		!index of last FCALC
	iy0=icalc(ke)	!final level
c set rest of icalc to final level
c	do 8 i=ke+1,10240^^^?
c8	fcalc(i)=y0
	RETURN
	end



                                                                                                                                                                                                                                                              


















                                                                                                                                                                                                                                                          
