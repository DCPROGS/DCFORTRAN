	subroutine CHKSHUT(y1,base,scrit,k,i1,delt,t0,dxf,dtf,
     &	tshift,ibk2,calfac2,SHUT,sets)
c To check if the level Y1 is close enough to BASE (both in intermed units)
c that the channel should be deemed SHUT
c NB i1=1,..,ntrans-1 (CHKSHUT not called for i1=ntrans)
	real*4 delt(0:50)		!temp storage for transition search results
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*70 string		!to put result in dialog box
	character*11 cnum1,cnum2,cnum3
	character ans
	logical shut,sets
c
	sets=.false.		!true if set as shut 'manually' below
	if(abs(y1-base).lt.0.4*scrit) then
c==	if(abs(y1-base).lt.0.0*scrit) then		!debug
	   SHUT=.true.		!final level is SHUT
	else if(abs(y1-base).gt.2.0*scrit) then
	   SHUT=.false.		!final level is OPEN
	else		!borderline case!
         call INTCONV(k,cnum1)	!transition #
	   t=delt(i1)*1.e-3		!duration (ms)
	   if(t.lt.1000.) then
	      call DCFORMAT(t,7,3,cnum2)		!duration
	   else if((t.ge.1000.).and.(t.lt.10000.)) then
	      call DCFORMAT(t,7,2,cnum2)		!duration
	   else
	      call DCFORMAT(t,7,1,cnum2)		!duration
	   endif
	   amp=calfac2*(y1-base)
	   call DCFORMAT(amp,9,5,cnum3)		!amplitude
c Mark this 'shutting' on graph with arrow.  Time to middle of shutting is
c sum of the delt(i) up to the transition before the shutting, plus half
c the length of the shutting
	   t=0.
	   do i=1,i1-1
		t=t+delt(i)
	   enddo
	   t=t + 0.5*delt(i1) + t0	!microsec
	   t=t + tshift 		!t0 is too early to get arrow in middle of event
	   ix1=ifixr(t*dxf/dtf)
	   call VARROW(ix1,445,380,12)
	   ans='Y'
	   call BELL(1)
 	   call DEFOLTa(ans,defolt)
	   n1=NBLANK(cnum1)
	   n2=NBLANK(cnum2)
	   n3=NBLANK(cnum3)
	   string=cnum1(1:n1)//' '//cnum2(1:n2)//'ms'//
     &	  ' '//cnum3(1:n3)//'pA'
	   call QDIALOG(1,CHARNB(string)//' is SHUT: O.K.'
     &	,defolt,12,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
	      SHUT=.true.
		sets=.true.
	   endif
	   call VARROW(ix1,445,380,ibk2)	!erase arrow
	endif
	RETURN
	end

