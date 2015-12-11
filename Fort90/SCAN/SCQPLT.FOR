	subroutine SCQPLT(idiskq,ndevdat,indfst,isdfst,ndisp1,idata,
     & nbuf,novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     & ntrans,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base,
     & cjump,njump)
c To queue single channel data
c
c NB iver value here must agree with that in VPLQ5 and CONVQREC (even
c though iver is not at present used for single channel plots in AUTPLOT)
c
c Modif 12/04/98 07:01am by adding common/qplot/infil, so queue
c file name can be kept in scantemp
c Modif 10/25/97 06:11pm by adding 1000 to abs(iptype) when data
c	are from cjump.dat (rather than consam.dat), and njump value
c	queued (defined only when cjump=true)
c
c Modif 09/05/94 06:04pm to add BASE (baseline in intermed units) to queue
c indfst, iend = abs index in consam of bit to be queued=current display
c   (need absolute index only in case where data from CONSAM is used)
c NB When called from FITSUB/FITSUB6 substitute 1,2048 for novlap,nbuf
c  and call with idata0() rather than idata(), and specify first point
c  to be kept in isdfst.
c
c Modif 08/26/94 07:27am to queue name of consam file (adcfil*30 in common)
c Modified 03/15/94 07:13am to queue parameters to allow fitted curve
c to be calculated -call with ntrans=0 if there is no calc curve data
c Should (usually?) be plenty of room in 1st data record for these! eg
c iptype...adctime=109 bytes +adcfil=139 +base = 143
c	   write(12,rec=krn) iptype,itit,title1,isdfst,ndisp1,ndevdat,
c				     4      4    44      4     4      2
c	 indfst,iend,srate,calfac,calfac2,ioff,nrec1,cdate,adctime,adcfil,base
c         4     4    4     4       4      4   4     11     8       30    4
c For calc curve:
c ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt(1,nt-1),stepamp(1,nt),filt(1,nfilt),adcfil
c   4    4  4    4    4   4    4   4*(ntrans-1) 4*ntrans      4*nfilt       30
c thus number of bytes for calc curve data=28+4*(2*ntrans-1+nfilt)
c
c IPTYPE=3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c
	integer*2 idata(1-novlap:nbuf+novlap)
	integer*2 jstrec(200),lstrec(200)		!for queue
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum*11,cnum1*11,cnum2*11
	logical newfil,mono,present,useconsam,extrarec
	character ans*1,UC*1,ndev*2,ndevdat*2,filnam*33,infil*35
	character*44 title1	!plot title (local)
	character cdate*11,adctime*8,cdate1*11
	character adcfil*30		!CONSAM name/path
	logical caplock,debug,pon,slock
	logical discprt
	logical cjump
c for calc curve
	real*4 stepamp(50),dt(49),filt(640)
	character*20 title 			!for popmenu
	character*55 strings(10)		!for popmenu
	character*55 helps(15)	!for popmenu
c
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/rstrtblk/nchan,idatyp,adcfil,cdate1
	common/dp/discprt
	common/qplot/infil
c	SAVE infil
c
	pon()=slock()
	debug()=caplock()
c
	iesc=0	!unless plot aborted
c	iver=1001
	iver=1100
c NB readscq does not use iver, but need iver=1100 here
c in record 1 or autplot thinks the queue file has old format for graphs/histos
c queued from vplot/vhist
	ict=13	!text colour for DIALOG box
	if(mono) ict=15
c
441	continue
c Get title
	itit=1
	iopt=1
	call WDIALOG(1,
     & 'Queue (1) data points (2) position in CONSAM (3) cancel plot'
     & ,ict)
	call DEFOLTi(iopt,defolt)
	call QDIALOG(1,
     & 'Type option # (1, 2 or 3)',defolt,ict,cans)
	call GETINPi(cans,iopt)
	if(iopt.eq.3) then
	   iesc=1
	   RETURN
	endif
	useconsam=iopt.eq.2
c
	call TDIALOG(1,'TITLE for plot:',title1,44,.true.,ict)
c
	newfil=.false.
	if(idiskq.lt.1) then
	   filnam='\plotq.plq'
	else if(idiskq.ge.1) then	  !already defined
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue in (path/filename) '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	   ndev=infil(1:2)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') then
	      call TDIALOG(1,'Queue file path/name:',infil,35,.true.,ict)
		if(infil(2:2).eq.':') then
		   ndev=infil(1:2)
		else
		   call DISCDIALOG(1,'Specify disc for plot queue file',
     &		idiskq,ndev,-1,1,ict)
		   if(idiskq.eq.-1) then	!if ESC hit then do not queue anything
			iesc=1
			RETURN
		   endif
		endif
	   endif
	   if(ndev.eq.'A:'.or.ndev.eq.'B:') then
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Is disc '//ndev//' mounted',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') goto 25          !specify disc
	   endif
	   goto 481
	endif
25	continue
	call DISCDIALOG(1,'Specify disc for plot queue file',
     &	idiskq,ndev,-1,1,ict)
	if(idiskq.eq.-1) then	!if ESC hit then do not queue anything
	   iesc=1
	   RETURN
	endif
48	continue
	infil=ndev//filnam
481	continue
	INQUIRE(file=INFIL,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue file '//CHARNB(infil)//' does not exist: create it',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 4081
c
	   nplot=0
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	   write(12,rec=1) nplot,jstrec,lstrec,iver
	   newfil=.false.		!file now exists
	else
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	endif
c Now use first record for nplot etc
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c ###For floppy at least, add name,jfirst etc as for SCDIR so each person
c can have floppies with own posh plots
	read(12,rec=1) nplot,jstrec,lstrec,iver1
	if(iver1.ne.1001.and.iver1.ne.1100) then
	   call WDIALOG(1,
     &	'Plot queue has old format: converted to new',ict)
	   call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,12,1)
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
	   istrec=2
	 else
	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   iplot=nplot+1		!next number
	endif
4041	continue
	call INTCONV(iplot,cnum1)
	ans='Y'
	call DEFOLTa(ans,defolt)
	if(.not.newfil) then
	   call QDIALOG(1,
     & 'Next queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	else if(newfil) then
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	endif
	call GETINPa(cans,ans)
	if(ans.ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c
c Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
c
	newfil=.false.
4081	continue
	iopt=0	!default
c==========
c	call WDIALOG(1,
c     & 'Change (1) the number of the plot in the queue',ict)
c	call WDIALOG(1,
c     & ' (2) name/path of the queue file, or (3) abandon queueing',ict)
c408	call QDIALOG(1,
c     & 'Type option # (1,2,3) [or hit <enter> if O.K. now]:',
c     &  ' ',ict,cans)
c	call GETINPi(cans,iopt)
c=================
c
	iline=1		!default
28	title='Alter queueing'
      call INTCONV(iplot-1,cnum)
	strings(1)='change name/path of the queue File'
	strings(2)='Overwrite the last plot (number '//charnb(cnum)//')'
	strings(3)='Abandon queueing'
	strings(4)='Write to plot queue now'
	nval=4
	helps(1)='Specify another name (and path) for queue file'
	helps(2)='Overwrite the queue entry last written'
	helps(3)='Do not queue anything -return to program'
	helps(4)='Queue file name and number OK now -write to queue'
	nhelp=4
	call BELL(2)
	ibkm1=1
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	   ibkm1,title,helps,nhelp,iline,ans,ival)
	if(iline.lt.0.or.iline.gt.4) goto 28
	if(iline.eq.4) goto 411		!all changes done
	if(iline.eq.0) iline=3		!iline=0 for ESC=cancel
	if(iline.eq.3) then
	   iesc=1
	   goto 45
	endif
	if(iline.eq.1) then
	   call TDIALOG(1,'Queue file path\name:',infil,35,.false.,ict)
	   newfil=.true.
	else if(iline.eq.2) then
	   call INTCONV(iplot-1,cnum1)
	   call INTCONV(iplot,cnum2)
	   i=iplot-1	!default
	   call DEFOLTi(i,defolt)
	   call QDIALOG(1,
     &    'Queue number: last='//CHARNB(cnum1)//' (or next='//
     &	charnb(cnum2)//')',defolt,ict,cans)
	   call GETINPi(cans,i)
	   if(i.ge.1) iplot=i
	endif
	iline=4
	goto 28	!any more changes
c Come here if any changes made
411	continue
	if(newfil) then
	   CLOSE(unit=12)
	   goto 481		!Open new file and check if OK now?
	else			!if queue number only changed
	   call INTCONV(iplot,cnum1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.ne.'N') then		!O.K.
		if(iplot.le.nplot) goto 405	!overwriting existing record
		nplot1=iplot		!value to be written to 1st record
		goto 406
	   endif
	endif
c
c Queue file etc now defined: do next bit if old file is being overwritten
405	continue
	if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	istrec=int4(jstrec(iplot))	!start record for plot to be overwritten
	nplot1=iplot		!value to be written to 1st record
c Allow earlier plot to be overwritten in POSHPLOT.DAT only after query
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   lr1=int4(lstrec(iplot))		!last rec of plot to be overwritten
	   nr1=lr1-istrec+1		!no of records for ditto
	   read(12,rec=istrec) iptype,itit,title1
	   if(nrect.le.nr1) then
	      call QDIALOG(1,'Will overwrite '//CHARNB(title1)//'  O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   else
		call BELL(2)
	      call QDIALOG(1,'Will overwrite next plot too: O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   endif
	   call INTCONV(nplot,cnum1)
	   call INTCONV(iplot,cnum2)
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Reduce last plot # from '//CHARNB(cnum1)//
     &	' to'//CHARNB(cnum2)//' ',defolt,ict,cans)
	   call GETINPa(cans,ans)
 	   if(ans.eq.'Y') then
		nplot1=iplot  !to write to 1st record
	   else
		nplot1=nplot
	   endif
	endif
c
406	continue
c Check that data will fit in: make max file size=1423 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/1024=1423.5   (1457664/2048=711.75)
	if(useconsam) then
	   nrect=2		!in this version all goes on 1 record
	else
	   nbyte=ndisp1*2	!no of bytes for data
	   nrec1=1 + (nbyte-1)/1024	!for data
	   nrect=1+nrec1			!+1 for 1st record always written
	endif
	extrarec=.false.
	if(ntrans.ge.0) then	!queue data for calc curve
	   nbyte1=28+4*(2*ntrans-1+nfilt)	!extra bytes needed
	   if(nbyte1+143.gt.1024) then         !won't fit in first record
		nrect=nrect+1
		extrarec=.true.
	   endif
	endif
	iptype=3				!for single channel plots
	if(ntrans.gt.0) then
	   if(extrarec) then
		iptype=32	!queue calc data
	   else
		iptype=31	!queue calc data
	   endif
	endif
	if(cjump) iptype=iptype+1000
	if(.not.useconsam) iptype=-iptype
c
	lastr=istrec+nrect-1		!last record for current plot
	if(lastr.gt.1423) then
	   call BELL(2)
	   call WDIALOG(1,
     &    'Not enough room in queue file: start on another disc:',12)
	   idiskq=-1
	   goto 441
	endif
	jstrec(iplot)=int2(istrec)	!set value for current plot
	lstrec(iplot)=int2(lastr)	!set value for current plot
c Update rec #1 -now done AFTER data written so nplot does not get
c altered if there is some problem in writing the data (FIXPLQ1.FOR can
c alter nplot etc)
c
c Things to go in 1st record of each plot (Nplot copied into rec 1 also
c so it can be retrieved)
c
	krn=istrec
c (idum,ymin,ymax for compatibility with SAMPED only)
	if(useconsam) then
	   iend=indfst+ndisp1-1
	   if(ntrans.le.0) then
	      write(12,rec=krn) iptype,itit,title1,ndevdat,idum,indfst,
     &     iend,srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	adcfil,base,njump
	   else		!queue data for calc curve
		if(.not.extrarec) then
	        write(12,rec=krn) iptype,itit,title1,ndevdat,idum,indfst,
     &       iend,srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	  (filt(i),i=1,nfilt),adcfil,base,njump
		else		!need sep record for filt(i),adcfil
	       write(12,rec=krn) iptype,itit,title1,isdfst,ndisp1,ndevdat,
     &	  indfst,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		  write(12,rec=krn) (filt(i),i=1,nfilt),adcfil,base,njump
		endif
	   endif
	else		!queue actual data
	   if(ntrans.le.0) then
	      write(12,rec=krn) iptype,itit,title1,isdfst,ndisp1,ndevdat,
     &  indfst,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &   adcfil,base,njump
	   else		!queue data for calc curve
		if(.not.extrarec) then
	       write(12,rec=krn) iptype,itit,title1,isdfst,ndisp1,ndevdat,
     &	  indfst,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	  (filt(i),i=1,nfilt),adcfil,base,njump
		else		!need sep record for filt(i)
	       write(12,rec=krn) iptype,itit,title1,isdfst,ndisp1,ndevdat,
     &	  indfst,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		  write(12,rec=krn) (filt(i),i=1,nfilt),adcfil,base,njump
		endif
	   endif
	   iend=isdfst+ndisp1-1
c       Write ndisp1 data values starting from idata(isdfst)
	   n=isdfst
	   do j=1,nrec1         !write idata()
		m=n+511
		if(m.gt.iend) m=iend
		krn=krn+1
		write(12,rec=krn) (idata(i),i=n,m)
		n=n+512
	   enddo
	endif
c
c Update rec #1 (after data written safely!)
	write(12,rec=1) nplot1,jstrec,lstrec,iver
	call INTCONV(iplot,cnum1)
	call INTCONV(istrec,cnum2)
	call WDIALOG(1,
     & 'Plot queued as # '//CHARNB(cnum1)//': start record # '//
     &	CHARNB(cnum2)//' in '//charnb(infil),ict)
c define ind1 as absolute index whether useconsam or not, for print
	iend1=indfst+ndisp1-1
	call INTCONV(indfst,cnum1)
	call INTCONV(iend1,cnum2)
	call WDIALOG(1,
     & 'Index in data: '//CHARNB(cnum1)//' to '//
     &	CHARNB(cnum2),ict)
c	call WDIALOG(1,CHARNB(title1),ict)
	if(slock()) write(7,46)iplot,istrec,krn,infil,indfst,iend1,title1
      if(discprt) write(8,46)iplot,istrec,krn,infil,indfst,iend1,title1
46	format(/,
     & ' Plot queue no ',i3,' in records ',i5,' to ',i5,' in ',a33,/,
     & ' Index in data: ',i9,' to ',i9,/,
     &  1x,a44,'**************************')
	call flush(7)
45	CLOSE(UNIT=12)
99	continue
	RETURN		!return to screen coordinates?
c
C ERROR SECTION
302	continue
	call BELL(1)
      call QDIALOG(1,'Error in opening PLOTQ file: try again',
     &   ' ',ict,cans)
      call GETINPa(cans,ans)
	newfil=.false.
	if(ans.eq.'Y') goto 4081
	RETURN
c
	end

