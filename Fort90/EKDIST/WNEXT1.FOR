	subroutine WNEXT1(idest,first)
c To do 'where next' windows for EKDIST
c If called with first=true, this only sets sbin,shist,sres,sexp then returns
	integer scrnbuf0(1024),scrnbuf1(1024)
	character*30 text(13)
	logical first,discprt,debug,caplock
	logical sbin,shist,sres,sexp
	logical colour
	integer*2 videotyp,vt
	common/sblk/sbin,shist,sres,sexp
	character mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c
      INCLUDE 'c:\lf90\lib\SPINATTI.HDR'		!modif version of SPINATTR.HDR
c
	debug()=caplock()
c
c Set defaults
	colour=icol.eq.1
	if(first) then
		sbin=.false.	!same bin widths- see SCVDU
		shist=.false.	!same histo type
		sres=.false.	!same resolution
		sexp=.false.	!same experiments
	else
		sbin=.false.	!same bins
		shist=.false.	!same histo type
		sres=.false.	!same resolution
		sexp=.true.		!same expt
	endif
c
c Seems to be a problem (app. only after precalc curve displayed) when
c call to ENDPLT() still leaves videotyp=18
	vt=videotyp()
	if(debug()) print 80,idest,vt,first
80	format(' idest,videotyp,first= ',2i5,l2)
	if(vt.eq.3) goto 7		!already in alpha mode
	print *,' Type ENTER to carry on'	!ENDPLT needs ENTER
	call devend()				!enter alpha mode
	vt=videotyp()
	if(vt.ne.3) call VIDEOMOD(3)		!try setting this way
c	call ENDTT()				!enter alpha mode
7	continue
	vt=videotyp()
c	if(debug()) print 80,idest,vt,first
	call CAPTSCN(scrnbuf0)			!keep input screen
	if(first) goto 1
	text(1)='1.IDENTICAL HISTOGRAM'
	text(2)='2.DIFFERENT BINS ONLY '
     	text(3)='3.DIFFERENT RESOLUTION ONLY'
	text(4)='4.SAME HISTOGRAM TYPE ONLY'
	text(5)='5.SAME EXPERIMENT & RESOLUTION'
	text(6)='6.SAME EXPT, DIFF RESOLUTION'
	text(7)='7.NEW EXPERIMENT '
	text(8)='8.Disc print-out file options'
	text(9)='9.   EXIT from program    '
c -attributes definedin spinatt.hdr
c	logical colour
c	colour=.true.	!for Dell 425
	if(colour) then
	   iatt=whitechr+redbkg
	else
	   iatt=inverse
	endif
	call DEFWIND(1,4,4,17,36,dblbdr+wrap+scrol,iatt)
c	call openwind(1)
	call clrb(1)
	if(idest.eq.0) then
c		write(1,400) (text(i),i=1,9)
c400		format(9(1x,a30/))
	call BLDB(1,0,0,text(1),current)
	call BLDB(1,1,0,text(2),current)
	call BLDB(1,2,0,text(3),current)
	call BLDB(1,3,0,text(4),current)
	call BLDB(1,4,0,text(5),current)
	call BLDB(1,5,0,text(6),current)
	call BLDB(1,6,0,text(7),current)
	call BLDB(1,7,0,text(8),current)
	call BLDB(1,8,0,text(9),current)
	else if(idest.eq.-1) then
c		call LOCATEW(1,5,0)	!start at row 5 of window
c		write(1,399) (text(i),i=5,9)
c399		format(5(1x,a30/))
	call BLDB(1,4,0,text(5),current)
	call BLDB(1,5,0,text(6),current)
	call BLDB(1,6,0,text(7),current)
	call BLDB(1,7,0,text(8),current)
	call BLDB(1,8,0,text(9),current)
	endif
c	write(1,401)
c401	format(' Option number = ')
	call CAPTSCN(scrnbuf1)		!save window #1
406	inext=nkey()
	if(idest.eq.0.and.(inext.lt.1.or.inext.gt.9)) goto 406	!invalid
	if(idest.eq.-1.and.(inext.lt.5.or.inext.gt.9)) goto 406	!invalid
	if(inext.ne.8) goto 9	!return with idest
	call CLS
1	continue
c
	if(first) goto 99
	call WINPRINT	!Option 8 - check disk print out options
	call DISPSCN(scrnbuf1)		!restore options menu
	goto 406
c
9	continue
c Now act on INEXT from window #1 (have already set sbin, shist, sres =.false.
c  and sexp=true)
	idest=0				!unless exiting
	if(inext.eq.9) inext=8		!to exit
	GOTO(251,252,253,254,255,228,257,258),inext
251	sbin=.true.
	shist=.true.
	sres=.true.
	goto 228
252	shist=.true.
	sres=.true.
	goto 228
253	sbin=.true.
	shist=.true.
	goto 228
254	shist=.true.
	goto 228
255	sres=.true.
	goto 228
257	sexp=.false.
	goto 228
258	idest=1	!exit
c
228	continue
c Form feed for new expt, or exit (skipped first time)
c NO -for new expt only (call ENDPRINT does it on exit)
c	if(inext.eq.7.or.inext.eq.8) then
	if(inext.eq.7) then
	   if(discprt) write(8,100)
100	   format('1 ')	!form feed
	endif
c
99	call DISPSCN(scrnbuf0)		!restore original screen
	if(first) idest=0		!never exit first time
c
	RETURN
c
	END




