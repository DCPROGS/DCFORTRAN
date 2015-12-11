	subroutine PLOTOPT(cjump,plotcirc,novlogo,noclogo,joinline,jtype,
     & arrange,consec,quarter,nplot,kplot,jplot,jpos,n1,n2,
     & idelt,iscalfix,Xmin,Xmax,Ymin,Ymax)
c For AUTPLOT, to define options to override queued values (eg for symbol
c types, jump logos,... (same for ALL the graphs plotted)
c 05/29/92 12:45pm Modified for non-consecutive and/or rearrange plots
c and mpos=5 introduced to denote whole page plots
      character*1 ans,UC
	integer jplot(nplot),jpos(nplot)	!to hold re-ordered plots
	logical cjump,plotcirc,novlogo,noclogo,joinline
	logical arrange,consec,quarter
c
c101   format(a1)
c initialise
c	plotcirc=.false.		!now taken from .ini
c	novlogo=.false.
c	noclogo=.false.
c	joinline=.false.
	arrange=.false.
	consec=.true.
c	if(cjump) then		!now take from .ini
c	   novlogo=.true.
c	   joinline=.true.
c	   jtype=0        !solid line
c	endif
c Set default positions
	if(quarter) then
	   do j=1,nplot
		jpos(j)=1 + mod(j-1,4)
	   enddo
	else
	   do 207 i=1,nplot
207	   jpos(i)=5		!whole page
	endif
c
	n1=1
	n2=nplot
	print 111,n1,n2
111	format(
     & ' Plot graphs from number N1 to N2: N1,(N2) [',
     &   i3,',',i3,'] = ')
	call INPUT2i(n1,n2)
	if(n1.eq.0) RETURN
	if(n2.eq.0) n2=n1
	if(n2.lt.n1) then
	   i=n2
	   n2=n1
	   n1=i
	endif
	if(n2.gt.nplot) then
	   call BELL(1)
	   print 2061,n2,nplot
2061	    format(1x,i4,' is larger than number of plots in queue: use '
     &   ,i4,' O.K. [Y] ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') n2=nplot
	endif
	kplot=n2-n1+1		!number of plots to be done
	do i=1,kplot
	  jplot(i)=n1+i-1		!consecutive order
	enddo
c
3	continue
	iopt=0
	if(.not.cjump) then
	   print 1
1	   format(' Override any of the queued values [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(ans.eq.'N') then
		RETURN
	   else
		print 11
11		format(/,
     & ' OPTIONS TO OVERRIDE QUEUED SETTINGS (hit enter to accept all)')
	   endif
	else if(cjump) then
		print 2
2		format(/,
     & ' OPTIONS FOR AUTOMATIC JUMP PLOTTING (<enter> to accept all)')
	endif
	print 31
31	format(' [0] No more changes')
	if(plotcirc) then
	   print 32
32	   format(' (1) Plot points as small circles')
	else
	   print 33
33	   format(' (1) Do not plot points as small circles')
	endif
	if(novlogo) then
	   print 34
34	   format(' (2) Do not plot voltage jump logo')
	else
	   print 35
35	   format(' (2) Plot voltage jump logo')
	endif
	if(noclogo) then
	   print 36
36	   format(' (3) Do not plot concentration jump logo')
	else
	   print 37
37	   format(' (3) Plot concentration jump logo')
	endif
	if(joinline) then
	   print 371
371	   format(' (4) Join data points with lines')
	else
	   print 38
38	   format(' (4) Do not join data points with lines')
	endif
	if(idelt.lt.1) idelt=1
	if(idelt.eq.1) then
	   print 39
39	   format(' (5) Plot every point')
	else
	   print 391,idelt
391      format(' (5) Plot every nth point only, n = ',i3)
	endif
	print 392
392   format(' (6) Specify fixed Xmin, Xmax, Ymin, Ymax now')
	print 393
393   format(' (7) Specify non-consecutive and/or rearranged plots')
	print 394
394	format(' Option number [0] = ')

c	print 2,plotcirc,novlogo,noclogo,joinline,idelt
c2	format(/,
c     & ' OPTIONS FOR AUTOMATIC PLOTTING (hit enter to accept all)',/,
c     & ' [0] No more changes',/,
c     & ' (1) Plot symbols specified as points as small circles ('
c     & ,l4,')',/,
c     & ' (2) Omit V-jump logos (',l4,')',/,
c     & ' (3) Omit C-jump logos (',l4,')',/,
c     & ' (4) Join data points with lines (',l4,')',/,
c     & ' (5) Plot every nth data point (',i3,')',/,
c     & ' (6) Specify Xmin, Xmax, Ymin, Ymax now',/,
c     & ' (7) Non-consecutive and/or rearranged plots',/,
c     & ' Option number [0] = ')
	iopt=0
	call INPUTi(iopt)
c	if(iopt.eq.0) goto 99
	if(iopt.eq.0) then
	   if(.not.consec.or.arrange) goto 99
	   RETURN
	endif
	goto(10,20,30,40,50,60,70) iopt
c
c	if(.not.cjump) then
c	   print 1
c1	   format(' Override any of the queued values [N] ? ')
c	else
c	   print 11
c11	   format(
c     &' (Plots for CJUMP already set to omit V-jump logo,and join dots)'
c     & ,/,' Override any other of the queued values [N] ? ')
c	endif
c	ans='N'
c      call INPUTa(ans)
c      if(ans.eq.'N') then
c	   n1=1
c	   n2=nplot
c	   print 111,n1,n2
c111	   format(
c     & ' Plot graphs from number N1 to N2: N1,(N2) [',
c     &   i3,',',i3,'] = ')
c	   call INPUT2i(n1,n2)
c	   if(n1.eq.0) RETURN
c	   if(n2.eq.0) n2=n1
c	   if(n2.lt.n1) then
c		i=n2
c		n2=n1
c		n1=i
c	   endif
c	   if(n2.gt.nplot) then
c		call BELL(1)
c		print 2061,n2,nplot
c2061	     format(1x,i4,' is larger than number of plots in queue: use '
c     &	,i4,' O.K. [Y] ')
c		ans='Y'
c		call INPUTa(ans)
c		if(UC(ans).eq.'N') n2=nplot
c	   endif
c	   kplot=n2-n1+1		!number of plots to be done
c	   do 102 i=1,kplot
c102	    jplot(i)=n1+i-1		!consecutive order
c	   RETURN
c	endif
c
c3	continue
c	iopt=0
c	print 2,plotcirc,novlogo,noclogo,joinline,idelt
c2	format(
c     & ' [0] No more changes',/,
c     & ' (1) Plot symbols specified as points as small circles ('
c     & ,l4,')',/,
c     & ' (2) Omit V-jump logos (',l4,')',/,
c     & ' (3) Omit C-jump logos (',l4,')',/,
c     & ' (4) Join data points with lines (',l4,')',/,
c     & ' (5) Plot every nth data point (',i3,')',/,
c     & ' (6) Specify Xmin, Xmax, Ymin, Ymax now',/,
c     & ' (7) Non-consecutive and/or rearranged plots',/,
c     & ' Option number [0] = ')
c	iopt=0
c	call INPUTi(iopt)
c	if(iopt.eq.0) goto 99
cc
c	goto(10,20,30,40,50,60,70) iopt
c
10	plotcirc=.not.plotcirc
	goto 3
20	novlogo=.not.novlogo
	goto 3
30	noclogo=.not.noclogo
	goto 3
40	if(joinline) then
	   jtype=-1		!default=no line
	else
	   jtype=0		!default=solid line
	endif
	joinline=.not.joinline
	print 41,jtype
41	format(
     & ' Join with linetype (-1=none; 0=solid,...8) [',i2,'] = ')
	call INPUTi(jtype)
	goto 3
50	continue
	print 51,idelt
51	format(' Plot every nth data point: n [',i3,'] = ')
	call INPUTi(idelt)
	goto 3
60	continue
	print 63
63	format(
     & ' Over-ride Xmin, Xmax ? ')
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   print 61,xmin,xmax
61	   format('& Xmin, Xmax [',g10.3,',',g10.3,'] = ')
	   call INPUT2r(xmin,xmax)
	   iscalfix=1
	endif
	print 64
64	format(
     & ' Over-ride Ymin, Ymax ? ')
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   print 62,ymin,ymax
62	   format('& Ymin, Ymax [',g10.3,',',g10.3,'] = ')
	   call INPUT2r(ymin,ymax)
	   if(iscalfix.eq.1) then
		iscalfix=3		!both
	   else
		iscalfix=2		!Y only
	   endif
	endif
	goto 3
70	continue
	call DCASK('Do consecutive plots','Y',ans)
	if(ans.eq.'N') consec=.false.
	if(quarter) then
	   iopt=1
	   print 201
201	   format(
     & ' (1) Normal (top left, top right, bottom left, bottom right)',/,
     & ' (2) arrange: top left, bottom left, top right, bottom right',/,
     & ' (3) Arrangement to be specified individually',/,
     & ' Option number [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.eq.2) then		!iopt=1 is default, set above
		do 204 j=1,nplot,4	!set up for up to nplot plots
		  jpos(j)=1
		  jpos(j+1)=3
		  jpos(j+2)=2
		  jpos(j+3)=4	!ie 1,3,2,4, 1,3,2,4, 1,3,2,4, 1,3,2,4,...
204		continue
	   else if(iopt.eq.3) then
		arrange=.true.
	   endif
	endif
	goto 3
c
99	continue
	if(consec) then		!consecutive jplot() is default, set above
	   print 111,n1,n2
c111	   format(
c     & ' Plot graphs from number N1 to N2 (0 to end): N1,(N2)= ')
	   call INPUT2i(n1,i)
	   n2=n1
	   if(i.gt.n1) n2=i
	   if(n2.gt.nplot) n2=nplot
	   kplot=n2-n1+1		!number of plots to be done
c  Set plot numbers in correct order
	   do 112 i=1,kplot
112	    jplot(i)=n1+i-1		!consecutive order
	   if(arrange) then
		ip=0		!page number
		do 118 j=1,nplot,4
		 ip=ip+1
		 print 117,ip
117		 format(
     &' Position: 1=top left 2=top right 3=bottom left 4=bottom right',/
     &,' Page ',i3,':',/)
		 kp=j
		 if(kp.gt.kplot) goto 126
121		 print 120,kp+n1-1
120		 format('&  Plot ',i3,': position # = ')
	       call INPUTi(i)
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
228		   format(' Position must be 1,2,3, or 4',/)
		   goto 121
		 endif
		 i1=i
		 jpos(kp)=i1
c
		 kp=j+1
		 if(kp.gt.kplot) goto 126
122		 print 120,kp+n1-1
	       call INPUTi(i)
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
		   goto 122
		 endif
		 if(i.eq.i1) then
		   call BELL(1)
		   print 229,i1
229		   format(' Position ',i2,' already occupied')
		   goto 122
		 endif
		 i2=i
		 jpos(kp)=i2
c
		 kp=j+2
		 if(kp.gt.kplot) goto 126
123		 print 120,kp+n1-1
	       call INPUTi(i)
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
		   goto 123
		 endif
		 if(i.eq.i1.or.i.eq.i2) then
		   call BELL(1)
		   print 229,i
		   goto 123
		 endif
		 i3=i
		 jpos(kp)=i3
c
		 kp=j+3
		 if(kp.gt.kplot) goto 126
		 do 124 k=1,4
		  if(k.ne.i1.and.k.ne.i2.and.k.ne.i3) then
		    jpos(kp)=k
		    print 125,kp+n1-1,k
125		    format('&  Plot ',i3,': position # = ',i2)
		  endif
124		 continue
118		continue
126	   continue
	   endif
	else if(.not.consec) then
	   if(.not.arrange) then
		kplot=0
		i=0
114		continue
		i=i+1
		print 113,i
113		format('&',1x,i2,': plot number [ENTER=end] = ')
	      call INPUTi(i)
		if(j.gt.0) then
		   jplot(i)=j
		   goto 114		!get next plot #
		else
		   kplot=i-1
		   goto 991		!finished
		endif
	   else if(arrange) then
		ip=0		!page number
		do 218 j=1,nplot,4
		 ip=ip+1
		 print 217,ip
217		 format(
     &' Position: 1=top left 2=top right 3=bottom left 4=bottom right',/
     &,' Page ',i3,':',/)
		 kp=j
221		 print 220
220		 format('&  Specify plot #, position # [ENTER=end] = ')
		 call INPUT2i(n,i)
		 if(n.le.0) then
		   kplot=kp-1
		   goto 991		!finished
		 endif
		 if(n.gt.nplot) then
		   call BELL(1)
		   print 230,nplot
230		   format(' Maximum plot number in queue = ',i5,/)
		   goto 221
		 endif
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
		   goto 221
		 endif
		 i1=i
		 jplot(kp)=n		!plot #
		 jpos(kp)=i1		!posn for this plot
c
		 kp=j+1
222		 print 220
		 call INPUT2i(n,i)
		 if(n.le.0) then
		   kplot=kp-1
		   goto 991		!finished
		 endif
		 if(n.gt.nplot) then
		   call BELL(1)
		   print 230,nplot
		   goto 222
		 endif
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
		   goto 222
		 endif
		 if(i.eq.i1) then
		   call BELL(1)
		   print 229,i
		   goto 222
		 endif
		 i2=i
		 jpos(kp)=i2
		 jplot(kp)=n		!plot #
c
		 kp=j+2
223		 print 220
		 call INPUT2i(n,i)
		 if(n.le.0) then
		   kplot=kp-1
		   goto 991		!finished
		 endif
		 if(n.gt.nplot) then
		   call BELL(1)
		   print 230,nplot
		   goto 223
		 endif
		 if(i.lt.1.or.i.gt.4) then
		   call BELL(1)
		   print 228
		   goto 223
		 endif
		 if(i.eq.i1.or.i.eq.i2) then
		   call BELL(1)
		   print 229,i
		   goto 223
		 endif
		 i3=i
		 jpos(kp)=i3
		 jplot(kp)=n		!plot #
c
		 kp=j+3
		 do 224 k=1,4
		  if(k.ne.i1.and.k.ne.i2.and.k.ne.i3) then
		    jpos(kp)=k
227		    print 225,k
225		    format(
     &	'&  Plot # to go in position ',i2,'  [ENTER=end] = ')
		    n=0
	          call INPUTi(n)
		    if(n.le.0) then
			kplot=kp-1
			goto 991		!finished
		    endif
		    if(n.gt.nplot) then
		      call BELL(1)
		      print 230,nplot
		      goto 227
		    endif
		    jplot(kp)=n		!plot #
		  endif
224		 continue
218		continue          !end of j loop
	   endif
	endif
991	continue
	RETURN
	end
