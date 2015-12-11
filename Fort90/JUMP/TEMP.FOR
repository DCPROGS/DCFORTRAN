PROMPT $D    $t$_$p$g
===OLD CODE FROM REAVER3
c Display current average (for aligned data, show mean only on aligned trace)
=======skip next bit?
	if(nmean.ge.1.and.(.not.align)) then
	   call SETACT(iact,-1)		!do not draw unless
	   iact(ibk1)=12			!draw red on background
	   iact(icol)=13			!draw purple if already green
	   if(curdisp) then
		call IDCPIX1(ixdisp,iyavc,ndisp,iact,jcol,0)
	   else
		call IDCPIX1(ixdisp,iyavv,ndisp,iact,jcol,0)
	   endif
	endif
=====end of skip
c
c        Remove menu until fndcur completed
	   call FILLWIN(0,0,639,iyd1-2,ibk)	!fill DCmenu area with colour ibk
=========
	   call DCFORMAT(tintlat(ij),8,2,cnum1)
c Arrow to mark latency: dx pixel units represent itsamp*1.e-3/ndisp milliseconds
c so tintlat ms = tintlat*nsamp/(0.001*itsamp) pixel units (NB ndisp=nsamp
c if ndelt=1, but not otherwise)
		   time=tintlat(ij) + tzero
		   indexa=1+ifixr((time/tsamp)*float(ndisp))	!index in ixdisp
		   ix1=ixdisp(indexa)
		   call VARROW(ix1,iya1,iya2,14)
c		   call QDIALOG(1,'First latency for jump #'//charnb(cnum)//
c     &    		' = '//charnb(cnum1)//' ms: O.K.',
c     &		defolt,ictd,cans)
		   call WDIALOG(1,'First latency for jump #'//charnb(cnum)//
     &    		' = '//charnb(cnum1),ictd)
		endif
	   endif
c
c=	   call GETINPi(cans,ans)	!get ans after data displayed!
c  Now mark bit of baseline to fill the end after shifting
c (no shift needed if latency=0 or null sweep if bad, then offer option
c to accept anyway)
	   if((.not.null).and.(.not.zero)) then
		call WDIALOG(1,
     & 'Mark start of baseline section (to fill end after shift)',ictd)
		ix=7000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	      x1=XWORLD(ix1)
		iltype=2       !dash
		call MOVETO(x1,yd1,0,iltype)
		call MOVETO(x1,yd2,1,iltype)
		time=x1           !in pixels -640 pixels=nsamp points
		index1=1+ifixr((time/640.)*float(nsamp))	!index in Yval
		call WDIALOG(1,
     & 'Mark end of baseline section',ictd)
		ix=9000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	      x1=XWORLD(ix1)
		iltype=7       !dash-dot
		call MOVETO(x1,yd1,0,iltype)
		call MOVETO(x1,yd2,1,iltype)
		time=x1
		index2=1+ifixr((time/640.)*float(nsamp))	!index in Yval
c Now make new display array with shifted data.  Start with point that
c corresponds with latency-tzero (so still have points tzero ms before jump)
c so if latency=0 than first point on display is yval(1,jc).  Need to redefine
c Yval(), because this is what is averaged, then make new iydisp().
c First copy bit of baseline to be used to fill ends into a temporary
c array, yfill().
		nfill=index2-index1+1	!length of chunk for filling
		ALLOCATE(yfill(nfill))
		do i=1,nfill
		   yfill(i)=Yval(i+index1-1,jc)
		enddo
		srate=float(irate)
		dts=1000./srate	!ms between points
		ifst=1 + ifixr(tintlat(ij)/dts)	!=1 for latency=0
		i=0
		do i1=ifst,nsamp
		   i=i+1		!in case ndelt.ne.1
		   Yval(i,jc)=Yval(i1,jc)
		enddo
		nd1=i		!=number of points defined up to end of sample
c Now fill the rest with baseline data defined above
		if(nd1.lt.nsamp) then
		   i=nd1		!next yval to be filled
		   i1=0		!index for Yfill
		   do while(i.lt.nsamp)
			i=i+1
			i1=i1+1
			if(i1.gt.nfill) i1=1	!reset
			Yval(i,jc)=Yfill(i1)
		   enddo
		endif
		DEALLOCATE(yfill)
c Yval now shifted and filled -redefine iydisp()
		i=0
		do i1=1,nsamp,ndelt
		   i=i+1		!in case ndelt.ne.1
		   iydisp(i)=int2(ifixr(YVAL(i1,jc)*sfac3)) + iyoff   !copy current
		enddo
c===		ndisp=i	!=number to display on exit
	   endif	!end of 'not null'
c Erase last trace
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c Restore DCMENU
	   call DCMENU(nbox,5,Lb,text,icol1,icol2)
c Display new one (if null or zero then Yval() and iydisp() are unchanged
	   call IDCPIX1(ixdisp,iydisp,ndisp,iact,jcol,1)
c  Draw arrow at tzero
	   time=tzero
	   indexa=1+ifixr((time/tsamp)*float(ndisp))
	   ix1=ixdisp(indexa)
	   call VARROW(ix1,iya1,iya2,14)
c And display current mean of shifted traces
	   if(nmean.ge.1) then
		call SETACT(iact,-1)		!do not draw unless
		iact(ibk1)=12			!draw red on background
		iact(icol)=13			!draw purple if already green
		call IDCPIX1(ixdisp,iyavc,ndisp,iact,jcol,0)
	   endif
	endif		!end of 'if align'
c
c
13	continue	!end of loop for each event
