	subroutine vsplq(iptype,xval,yval,nval,jbff1,jbff2,
     &      consamdef,nhline,yhline,xhb,xhe,ltype,colseq,ic,icp,
     &      xmin,xmax,ymin,ymax,XLO,XHI,YLO,YHI,title1,
     &      titlex,titley,qfile)

c==============================================================================
	integer*2 jstrec(200),lstrec(200)		!for queue
	character*1 ans,UC
	character ndev*2,qfile*40,titlex*40,titley*40
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11		!for dialog
	logical discprt
	integer*1 idum(1)		!to fill file to length that is multiple of 1024
	logical caplock,debug
	LOGICAL slock,pon,present,newfil
	real*4 yhline(nhline)
	character*64 title1,title2	!plot title

	real*4 xval(nval),yval(nval),xhb(nhline),xhe(nhline)
      logical consamdef,colseq
	common/dp/discprt

	pon()=slock()
	debug()=caplock()

c==============================================================================
3	format(i8)
	idest=0
      iver=1100	!NB also set below
	ict=13	!text colour for DIALOG box
	idum(1)=0		!to fill file to length that is multiple of 1024
	ioffset=768
	nbytet=ioffset    !'first record'
	nbyte1=4*nval
	nbyte2=4*nval
	nbyte3=12*nhline
	nbytet=nbytet+nbyte1+nbyte2+nbyte3
	nrect=1 + (nbytet-1)/1024	!for everything
c
	call TDIALOG(1,'Give title for the plot',
     &  title1,64,.false.,ict)

441	continue
	newfil=.false.
	call GBLANK(qfile,40,n1,n2)
	if(n1.eq.0.and.n2.eq.0) then	!qfile not defined
	   goto 25
	else	  		!qfile already defined
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Queue in '//charnb(qfile)//' O.K.',
     &	   defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		ndev=qfile(1:2)
	      if(ndev.eq.'A:'.or.ndev.eq.'B:') then
	         ans='Y'
	         call DEFOLTa(ans,defolt)
	         call QDIALOG(1,'Is disc '//ndev//' mounted',
     &	      defolt,ict,cans)
	         call GETINPa(cans,ans)
		   if(ans.eq.'N') goto 25          !specify disc
		endif
		goto 48
	   endif
	endif

c Next bit when qfile not yet defined (or A: not mounted)
25	continue
	call TDIALOG(1,'Give path and name for queue file',
     &  qfile,40,.false.,ict)
48	continue
	INQUIRE(file=qfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue file '//CHARNB(qfile)//' does not exist: create it',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 4081
	   nplot=0
         iver=1100	!set here in case reset by read from old format queus
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=17,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
	   write(17,rec=1) nplot,jstrec,lstrec,iver
	   newfil=.false.		!file now exists
	else
         OPEN(unit=17,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
	endif

c Now use first record for nplot etc
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c ###For floppy at least, add name,jfirst etc as for SCDIR so each person
c can have floppies with own posh plots

	read(17,rec=1) nplot,jstrec,lstrec,iver
	if(iver.ne.1100) then
	   call WDIALOG(1,
     &	'Plot queue has old format: try again',12)
	   close(unit=17)
         goto 25
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
	   istrec=1025		!for transparent write
	 else
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	endif
4041	continue
	call INTCONV(iplot,cnum1)
	ans='Y'
	call DEFOLTa(ans,defolt)
	if(.not.newfil) then
	   call QDIALOG(1,
     & 'Next queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	else if(newfil) then
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	endif
	call GETINPa(cans,ans)
	if(UC(ans).ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c 	Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
	newfil=.false.
4081	continue
	iopt=1
408	call DEFOLTi(iopt,defolt)
	call WDIALOG(1,
     & 'Change (1) the queue number (2) name of queue file',ict)
	call QDIALOG(1,
     & '(3) abandon queueing; (4) O.K. queue now. Option #',
     &  ' ',ict,cans)
	call GETINPi(cans,iopt)
	if(iopt.eq.1) then
	   call INTCONV(iplot,cnum1)
	   call QDIALOG(1,
     &    'Queue number (up to '//CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,i)
	   if(i.ge.1) iplot=i
	else if(iopt.eq.2) then
	   call TDIALOG(1,'Give path and name for queue file',
     &     qfile,40,.false.,ict)
	   newfil=.true.
	   CLOSE(unit=17)
	   goto 48		!Open new file and check if OK now?
	else if(iopt.eq.3) then
	   goto 45		!abandon
	else if(iopt.ne.4) then
	   iopt=4		!new defolt
	   goto 408		!ask again
	endif
c Queue file etc now defined: do next bit if old file is being overwritten
405	continue
	if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	istrec=1 + (jstrec(iplot)-1)*1024	!start record transparent
	nplot1=iplot		!value to be written to 1st record
c Allow earlier plot to be overwritten in POSHPLOT.DAT only after query
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   nr1=(lstrec(iplot)-jstrec(iplot))+1
	   read(17,rec=istrec) iptype
	   if(iptype.ne.40.or.iptype.ne.41.or.iptype.ne.42.OR.
     &   IPTYPE.NE.33) then
           read(17,rec=istrec) iptype,itit,title2
	   else
	     read(17,rec=istrec) iptype,title1
	   endif
	   if(nrect.le.nr1) then
		ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Will overwrite '//CHARNB(title2)//'  O.K.',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') then
		   goto 441
		endif
	   else
		call BELL(2)
	      call QDIALOG(1,'Will overwrite next plot too: O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(UC(ans).eq.'N') then
		   goto 441
		endif
	   endif
	   call INTCONV(nplot,cnum1)
	   call INTCONV(iplot,cnum2)
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Reduce last plot # from '//CHARNB(cnum1)//
     &	' to '//CHARNB(cnum2)//' ',defolt,ict,cans)
	   call GETINPa(cans,ans)
 	   if(ans.eq.'Y') then
		nplot1=iplot  !to write to 1st record
	   else
		nplot1=nplot
	   endif
	endif
406	continue
	lastr=istrec+nbytet-1		!last record (byte #) for current plot
	lastr1=lastr		!rounded up
	n=mod(lastr,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   lastr1=lastr+nfill
	endif
	if(lastr1.gt.1457664.and.ndev.eq.'A:') then
	   call BELL(2)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
     	   call QDIALOG(1,
     & 	'Plot queue will be too big for floppy disc'//' O.K.',
     &	defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') then
		goto 441
	   endif
	endif
	jstrec(iplot)=1 + ((istrec-1)/1024)
	lstrec(iplot)=jstrec(iplot)+(nrect) - 1
	lstrec(iplot)=1 + ((lastr-1)/1024)	!should be same!
	iptype=33
      write(17,rec=istrec)iptype,title1,titlex,titley,
     &     nval,consamdef,nhline,ltype,jbff1,jbff2,colseq,ic,icp,
     &     xmin,xmax,ymin,ymax,XLO,XHI,YLO,YHI,ioffset
	istr1=istrec+ioffset
	write(17,rec=istr1) (XVAL(k),k=1,nval),
     &	(YVAL(k),k=1,nval),(yhline(k),k=1,nhline),
     &      (xhb(k),k=1,nhline),(xhe(k),k=1,nhline)
	nlen=lastr
	write(17,rec=nlen+1) (idum(1),i=1,nfill)
	write(17,rec=1) nplot1,jstrec,lstrec,iver
	call INTCONV(iplot,cnum1)
	call INTCONV(istrec,cnum2)
	call WDIALOG(1,
     & 'Plot queue # '//CHARNB(cnum1)//': start byte # '//
     &	CHARNB(cnum2)//' on disc '//ndev,ict)
	call WDIALOG(1,CHARNB(title1),ict)
c
	if(slock()) write(7,46)iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot),title1
      if(discprt) write(8,46)iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot),title1
46	format(/,44x,'**************************',/,
     & ' Plot queue no ',i3,' in bytes ',i8,' to ',i8,' in ',a40,/,
     & ' (i.e. 1024 byte records numbers ',i5,' to ',i5,')',/,
     & /,1x,a64)
c	if(debug()) pause
	call flush(7)
45	CLOSE(UNIT=17)
99	continue
	RETURN		!return to screen coordinates?

C ERROR SECTIOn
302	continue
	call BELL(1)
      call QDIALOG(1,'Error in opening PLOTQ file: try again',
     &   ' ',ict,cans)
      call GETINPa(cans,ans)
	newfil=.false.
	if(ans.eq.'Y') goto 4081
	idest=1551
	RETURN		!return to screen coordinates?

	end
c==============================================================================
