	SUBROUTINE SCVD1(NBIN,XWBASE,FITTED,PREFIT,AHIST,THIST,OBHIST,
     & IDEST,OHIST,GHIST,BHIST,IDTYPE,isg,jth,xaxis,logt,ncalc,
     & xcalc,fcalc,titlex,titley,xw,ilabel,iydat,latency)
c 05/28/89 10:54pm Lahey version
c Modif 12/06/91 05:43pm for 1st latency dists
C SCVD1 is 1st part of original SCVDU split off to save space
c in the overlay with the graphics APR87. This prog defines
c titles and superimposed gaussian for logt plots (tic length etc.
c now removed to SCVDU)
C
	dimension xaxis(510)
	dimension xcalc(512),fcalc(512)
	character*40 titlex,titley
	character word*7,num*4,text*25,text1*40
	character*66 text2
c	character*1 ans,UC
	LOGICAL AHIST,THIST,OBHIST,FITTED,PREFIT	!PARAM
	LOGICAL OHIST,GHIST,BHIST,latency
	logical pon,logt,slock,debug,caplock
C
C
	pon()=slock()
	debug()=caplock()
c
c
c Make plot postion negative so not predetrmined before call to
c VHIST2 but can be set to 'next posn' within VHIST2
c	mpos=-iabs(mpos)
c define titles for vdu/plot
cd	write(7,1114) ahist,thist,obhist,fitted,prefit
cd1114	format(' ahist,thist,obhist,fitted,prefit ',5l2)
	ID=IDTYPE
	ilabel=1
	titley='Frequency '
c	nleny=10
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins
	if(id.eq.14) xw=0.
	if(id.eq.52) xw=0.      !Popen from YVAL.DAT
	if(iydat.eq.2) goto 602		!titley already set for YVAL.DAT data
c
	IF(idest.ne.31) GOTO 698
	titlex='Transformed value '
c	nlenx=18
	xw=0.		!so Y axis NOT labelled 'per 10 ms'
	GOTO 602
698	IF(.NOT.GHIST) GOTO 699
	titlex='Shut time (ms) '
	if(latency) then
	   if(id.eq.11) titlex='latency from ON jump'
	   if(id.eq.12) titlex='latency from OFF jump'
	endif
	GOTO 602
699	IF(.NOT.AHIST) GOTO 700
	IF(ID.EQ.1) titlex=' Amplitude (pA) '
	IF(ID.EQ.2) titlex='-Amplitude (pA) '
	IF(ID.EQ.12) titlex=' Burst Amplitude (pA) '
	IF(ID.EQ.13) titlex='-Burst Amplitude (pA) '
	xw=-xwbase	!so Y axis labelled 'per 10 pA')
c	nlenx=16
c	if(id.gt.12) nlenx=22
	GOTO 602
700	IF(.NOT.OHIST) GOTO 701
	IF(ID.EQ.1) titlex='  Open time (ms) '
	IF(ID.EQ.2) titlex='Open period (ms) '
	IF(ID.EQ.3) titlex='Open time (adj to spec gap) '
c	nlenx=17
c	if(id.eq.3) nlenx=28
	GOTO 602
701	IF(.NOT.OBHIST) GOTO 702
	IF(ID.EQ.1) titlex='    Openings/burst '
	if(id.eq.2) titlex='Open periods/burst '
	xw=0.		!so Y axis NOT labelled 'per 10 ms'
c	nlenx=19
	GOTO 602
702	IF(.NOT.BHIST) GOTO 703
	if(id.eq.3) titlex='     Burst length (ms)    '
	if(id.eq.4) titlex='  Burst length (exc sub)  '
	if(id.eq.5) titlex='     Open time/burst      '
	if(id.eq.6) titlex='Open time/burst (exc sub) '
	if(debug()) then
	   print 888,isg,jth
888	   format(' isg,jth = ',2i8)
	   pause
	endif
	if(id.eq.7.or.id.eq.8) then
	   if(isg.ge.0) then
		if(isg.eq.0) then
		  text=' bursts with '//char(isg+1+48)//' opening'
		else
		  text=' bursts with '//char(isg+1+48)//' openings'
		endif
	   else if(isg.lt.0) then
		text='bursts with '//char(242)//char(-isg+1+48)//' openings'
	   endif
c NB Char(242)=equal to or greater than sign
	   call GBLANK(text,25,nl1,nl2)
	   if(id.eq.7) titlex='   Burst length: '//text(nl1:nl2)
	   if(id.eq.8) titlex='Open time/burst: '//text(nl1:nl2)
	endif
c	if(id.eq.7) titlex='   Burst length: '//text(1:nl)
c	if(id.eq.8) titlex='Open time/burst: '//text(1:nl)
c	if(id.eq.7) titlex='   Burst length (k fixed) '
c	if(id.eq.8) titlex='Open time/burst (k fixed) '
	if(id.eq.9.or.id.eq.10) then
	   if(id.eq.9) word='gap'
	   if(id.eq.10) word='opening'
	   call GBLANK(word,7,nw1,nw2)
	   ij=iabs(jth)
	   if(ij.eq.1) then
		num='1st '
	   else if(ij.eq.2) then
		num='2nd '
	   else if(ij.eq.3) then
		num='3rd '
	   else if(ij.ge.4) then
		num=char(ij+48)//'th '
	   endif
	   if(isg.eq.-10000) then	!ops/bst not specified
	      if(jth.ge.1) then
		   text1=num//word(nw1:nw2)//' in any burst'
	      else if(jth.le.-1) then
		   if(num.eq.'1st ') then
		      text1= 'Any '//word(nw1:nw2)//' in any burst'
		   else
		      text1= char(242)//num//word(nw1:nw2)//' in any burst'
		   endif
	      endif
		titlex=text1
c NB Char(242)=equal to or greater than sign
	   else if(isg.ne.-10000) then	!ops/bst specified
	      if(jth.ge.1) then
		   text1=num//word(nw1:nw2)//' in burst'
	      else if(jth.le.-1) then
		   if(num.eq.'1st ') then
		      text1= 'Any '//word(nw1:nw2)//' in burst'
		   else
		      text1= char(242)//num//word(nw1:nw2)//' in burst'
		   endif
	      endif
		if(isg.ge.0) then
		   if(isg.eq.0) then
		      text=' with '//char(isg+1+48)//' opening'
		   else
		      text=' with '//char(isg+1+48)//' openings'
		   endif
	      else if(isg.lt.0) then
		   text='with '//char(242)//char(-isg+1+48)//' openings'
	      endif
	      call GBLANK(text1,40,nw1,nw2)
	      call GBLANK(text,25,nl1,nl2)
	      text2=text1(nw1:nw2)//' '//text(nl1:nl2)
	      call LBLANK(text2,66)		!move text to start of array
	      titlex=text2(1:40)
	   endif
c	   if(debug()) then
c		print 870,text2
c		pause
c	   endif
c870	   format(' text2 = ',a66)
c	   call LBLANK(text2,66)		!move text to start of array
c	   if(debug()) then
c		print 870,text2
c		pause
c	   endif
c	   titlex=text2(1:40)
	endif
c	if(id.eq.9) titlex='    Jth shut time (ms)    '
c	if(id.eq.10) titlex='   Jth open time (ms)    '
	if(id.eq.14) titlex='       P(open)           '
c	nlenx=26
	goto 602
703	print *,' ERROR IN SCVDU'	!should not reach here
602	continue
c
	if(idest.ne.31) idest=0
101	format(a1)
108	FORMAT(/)
c For log display superimpose a dashed exponential if there is no fitted
c curve to show the shape of the dist of log(t)
	if(fitted.or.prefit) goto 210		!return
	ncalc=0
	if(.not.logt) goto 210		!return
c
	NCALC=301
	xm=xaxis(1)
	xmax=xaxis(nbin+1)	!temp- reset in SCVDU
	fmax=1.2*fmax		!temp- reset in SCVDU
	DXCALC=(alog10(XMAX)-alog10(XM))/FLOAT(NCALC-1)  !increment
c NB for dist of log(t) the values of XMIN,XMAX are in log10 units
c so dxcalc=log10(factor)
	tau=exp((alog(xm)+alog(xmax))/2.)	!half way along log scale
	scal=(fmax/1.3)*tau/(exp(alog(tau)-1.))
	DO 114 I=1,NCALC
	X=alog10(XM)+FLOAT(I-1)*DXCALC	!this is log10 value
	x=10.**x		!log10 taken in VHIST
	xcalc(i)=x
	f=scal*(1./tau)*exp(-x/tau)
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
	f=f*x*2.30259		!f(log10(t))=2.3*t*f(t)
	FCALC(I)=XWBASE*F
c	print 777,I,X,F,FCALC(I)
c777	FORMAT(I8,3(2X,G13.6))
114	continue
c
210	continue
	RETURN
	end


