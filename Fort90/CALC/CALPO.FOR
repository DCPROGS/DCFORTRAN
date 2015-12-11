	subroutine CALPO(tcrit,tc0,tc1,dtc,log,fac,area,tau,ns,
     & topen,ob,stb,otb,bl,pon,discprt,noprint,imode)
c To calculate properties of bursts from a theoretical shut time pdf
c with ns components, specified in area(),tau()
c
c imode=0 lists 'all' properties for each tcrit
c imode=1,2,3 lists only one, together with slope, to test dependency on tcrit
c      imode=1 for mean burst length
c      imode=2 for mean number of openings per burst
c	 imode=3 for mean total open time per burst
c
c Used in SCBST
	real*4 tau(100),area(100)
	logical log,pon,discprt,noprint
c
	if(noprint) goto 14
	if(imode.eq.0) then
	   print 53
	   if(pon) write(7,53)
	   if(discprt) write(8,53)
53	   format(/
     & '  t(crit)    openings/bst    mean gap<tcrit    mean gap>tcrit',
     & '    Popen in bst')
	   print 152
	   if(pon) write(7,152)
	   if(discprt) write(8,152)
152	   format(
     & '             shut time/bst   open time/bst     bst length')
	else
	   print 531
	   if(pon) write(7,531)
	   if(discprt) write(8,531)
531	   format(/
     & '  Tcrit        value        d(v)/d(Tc)     d(log[v])/dlog(Tc)')
	endif
14	continue
c Start of loop
	smin1=1.e37
	smin2=1.e37
	j=0
141	continue
	j=j+1
	if(.not.log) tcrit=tc0 + float(j-1)*dtc
	if(log) tcrit=tc0*(fac**(j-1))
	if(tcrit.gt.tc1) goto 142	!jump out
	ts=0.
	ps=0.
	ob=0.
	t1=0.		!for gap>tcrit (-bet-burst) calc
	do i=1,ns
	   e1=exp1(-tcrit/tau(i))
	   ts=ts + area(i)*(tau(i)*(1.-e1)-tcrit*e1)
	   ps=ps + area(i)*(1.-e1)
	   ob=ob + area(i)*e1
	   t1=t1 + area(i)*tau(i)*e1
	enddo
	po=topen/(ts+topen)
	gwb=ts/ps
	ob=1.0/ob
	gbb=tcrit + t1*ob
	stb=(ob-1.)*gwb	!total shut time/bst
	otb=ob*topen	!total open time/bst
	bl=stb+otb
	if(.not.noprint.and.imode.eq.0) then
	print 17,tcrit,ob,gwb,gbb,po
	   if(pon) write(7,17)tcrit,ob,gwb,gbb,po
         if(discprt) write(8,17)tcrit,ob,gwb,gbb,po
17	   format(g13.6,1x,g13.6,2x,g13.6,3x,g13.6,7x,g13.6)
	   print 171,stb,otb,bl
	   if(pon) write(7,171) stb,otb,bl
         if(discprt) write(8,171) stb,otb,bl
171	   format(15x,g13.6,2x,g13.6,3x,g13.6)
c
c As check, calc overall mean shut time
c
	   ams=((ob-1)*gwb + gbb)/ob
         print 20,ams
         if(pon) write(7,20) ams
         if(discprt) write(8,20) ams
20	   format(
     & ' overall mean shut time (as check) = ',g13.6)
	endif
	if(imode.eq.1) then
	   val=bl
	   if(j.gt.1) then
		d1=(val-vlast)/(tcrit-tlast)	!slope
		if(val.gt.1.e-30.and.vlast.ge.1.e-30) then
		   d2=(alog10(val)-alog10(vlast))/
     &		(alog10(tcrit)-alog10(tlast))
		else
		   d2=-1.
		endif
	   endif
	   tlast=tcrit
	   vlast=bl
	else if(imode.eq.2) then
	   val=ob
	   if(j.gt.1) then
		d1=(val-vlast)/(tcrit-tlast)	!slope
		if(val.gt.1.e-30.and.vlast.ge.1.e-30) then
		   d2=(alog10(val)-alog10(vlast))/
     &		(alog10(tcrit)-alog10(tlast))
		else
		   d2=-1.
		endif
	   endif
	   tlast=tcrit
	   vlast=ob
	else if(imode.eq.3) then
	   val=otb
	   if(j.gt.1) then
		d1=(val-vlast)/(tcrit-tlast)	!slope
		if(val.gt.1.e-30.and.vlast.ge.1.e-30) then
		   d2=(alog10(val)-alog10(vlast))/
     &		(alog10(tcrit)-alog10(tlast))
		else
		   d2=-1.
		endif
	   endif
	   tlast=tcrit
	   vlast=otb
	endif
	if(imode.ge.1) then
	   if(j.eq.1) then
      	print 21,tcrit,val
	      if(pon) write(7,21) tcrit,val
      	if(discprt) write(8,21) tcrit,val
21		format(g13.6,1x,g13.6)
	   else
		if(d1.lt.smin1) then
		   smin1=d1
		   tmin1=tcrit
		endif
		if(d2.lt.smin2) then
		   smin2=d2
		   tmin2=tcrit
		endif
      	print 22,tcrit,val,d1,d2
	      if(pon) write(7,22) tcrit,val,d1,d2
      	if(discprt) write(8,22) tcrit,val,d1,d2
22		format(g13.6,1x,g13.6,1x,g13.6,1x,g13.6)
	   endif
	endif
c
	goto 141		!next tcrit
142	continue
	if(imode.ge.1) then
         print 23,smin1,tmin1,smin2,tmin2
	   if(pon) write(7,23) smin1,tmin1,smin2,tmin2
         if(discprt) write(8,23) smin1,tmin1,smin2,tmin2
23	   format(
     & ' Minimum d(v)/d(T)       = ',g13.6,' at Tcrit = ',g13.6,' ms',/,
     & ' Minimum dlog(v)/d(logT) = ',g13.6,' at Tcrit = ',g13.6,' ms')
	endif
	RETURN
	end


