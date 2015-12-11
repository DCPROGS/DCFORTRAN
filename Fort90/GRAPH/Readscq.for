	subroutine READSCQ(krn,istrec,iptype,useconsam,itit,title1,
     & ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,
     & cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,
     & ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,
     & ndelt,ffilt,fcz,fczoom,njump)
c
	real*4 stepamp(50),dt(49),filt(640)
	character ndev*2,adcf1*30,cdate*11,adctime*8,title1*64
	character*44 title2	!for queues from SCAN
	logical debug,caplock,pon,slock
	logical useconsam
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
c To do first read of queued single channel data in AUTPLOT
c Modif 10/26/97 06:43pm to read njump (defined only when data from
c   cjump.dat rather than consam.dat)
c Modif 07/22/97 05:11pm so that title1 is read as 44 char as in SCAN though
c   it is 64 char in all F90 versions
c Modif 09/05/94 06:13pm to include BASE (baseline in intermed units) in queue
c
	krn=istrec
	if(iptype.eq.30.or.iptype.eq.301.or.iptype.eq.302) goto 100
	if(useconsam) then
	   if(iptype.eq.3) then		!no calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	 adcf1,base,njump
c		if(ioff.gt.5000.or.calfac2.lt.1.e-6) then
c		if(calfac2.lt.1.e-6.or.calfac2.gt.1.) then
c If very low gain (nA) currents queued in consam, calfac2 may be >1
		if(calfac2.lt.1.e-6.or.calfac2.gt.30.) then
	         read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	   istart,iend,srate,calfac,calfac2,ioff,nrec1,cdate,adctime
		   call BELL(2)
	         print 7,calfac2
      	   if(pon()) write(7,7) calfac2
      	   if(discprt) write(8,7) calfac2
7		   format(
     &	    ' CALFAC2 wrong: reading as for old queue gives ',g13.6)
		endif
	   else if(iptype.eq.31) then	!calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	 ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	 (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	 (filt(i),i=1,nfilt),adcf1,base,njump
	   else if(iptype.eq.32) then	!calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		  read(11,rec=krn) (filt(i),i=1,nfilt),adcf1,base,njump
	   endif
	   jdat=iend-istart+1
	   if(debug()) then
		print 201,title2,ib,istart,iend,srate,ymin,ymax,calfac
201		format(1x,a44,3i8,4g13.6)
		pause
	   endif
	else		!not using CONSAM data
	   if(iptype.eq.3) then		!no calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &   istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	 adcf1,base,njump
c		if(ioff.gt.5000.or.calfac2.lt.1.e-6) then
c		if(calfac2.lt.1.e-6.or.calfac2.gt.1.) then
c If very low gain (nA) currents queued in consam, calfac2 may be >1
		if(calfac2.lt.1.e-6.or.calfac2.gt.30.) then
	         read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	   istart,iend,srate,calfac,calfac2,ioff,nrec1,cdate,adctime
		   call BELL(2)
	         print 7,calfac2
      	   if(pon()) write(7,7) calfac2
      	   if(discprt) write(8,7) calfac2
c7		   format(
c     &	    ' CALFAC2 wrong: reading as for old queue gives ',g13.6)
		endif
	   else if(iptype.eq.31) then	!calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	  istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	  (filt(i),i=1,nfilt),adcf1,base,njump
	   else if(iptype.eq.32) then	!calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	  istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		read(11,rec=krn) (filt(i),i=1,nfilt),adcf1,base,njump
	   endif
c	   iend=isdfst+ndisp1-1	!this is relative to section #, but not used
	   iend=istart+ndisp1-1	!abs index in consam
	   jdat=ndisp1
	endif
	title1(1:44)=title2
	do i=45,64
	   title1(i:i)=' '
	enddo
	RETURN
c
c Now section for single channels queued from vwersion of SCAN with filtering
100	continue
	if(useconsam) then
	   if(iptype.eq.30) then		!no calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	 adcf1,base,ndelt,ffilt,fcz,fczoom,njump
c		if(ioff.gt.5000.or.calfac2.lt.1.e-6) then
c		if(calfac2.lt.1.e-6.or.calfac2.gt.1.) then
c If very low gain (nA) currents queued in consam, calfac2 may be >1
		if(calfac2.lt.1.e-6.or.calfac2.gt.30.) then
	         read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	   istart,iend,srate,calfac,calfac2,ioff,nrec1,cdate,adctime
		   call BELL(2)
	         print 7,calfac2
      	   if(pon()) write(7,7) calfac2
      	   if(discprt) write(8,7) calfac2
c7		   format(
c     &	    ' CALFAC2 wrong: reading as for old queue gives ',g13.6)
		endif
	   else if(iptype.eq.301) then	!calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	 ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	 (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	 (filt(i),i=1,nfilt),adcf1,base,ndelt,ffilt,fcz,fczoom,njump
	   else if(iptype.eq.302) then	!calc curve
		read(11,rec=krn) idum,itit,title2,ndev,ib,istart,iend,
     & 	 srate,ymin,ymax,calfac,ioff,calfac2,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		  read(11,rec=krn) (filt(i),i=1,nfilt),adcf1,base,
     &		ndelt,ffilt,fcz,fczoom,njump
	   endif
	   jdat=iend-istart+1
	   if(debug()) then
		print 201,title2,ib,istart,iend,srate,ymin,ymax,calfac
c201		format(1x,a44,3i8,4g13.6)
		pause
	   endif
	else		!not using CONSAM data
	   if(iptype.eq.30) then		!no calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &   istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	 adcf1,base,ndelt,ffilt,fcz,fczoom,njump
c		if(ioff.gt.5000.or.calfac2.lt.1.e-6) then
c		if(calfac2.lt.1.e-6.or.calfac2.gt.1.) then
c If very low gain (nA) currents queued in consam, calfac2 may be >1
		if(calfac2.lt.1.e-6.or.calfac2.gt.30.) then
	         read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	   istart,iend,srate,calfac,calfac2,ioff,nrec1,cdate,adctime
		   call BELL(2)
	         print 7,calfac2
      	   if(pon()) write(7,7) calfac2
      	   if(discprt) write(8,7) calfac2
c7		   format(
c     &	    ' CALFAC2 wrong: reading as for old queue gives ',g13.6)
		endif
	   else if(iptype.eq.301) then	!calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	  istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans),
     &	(filt(i),i=1,nfilt),adcf1,base,ndelt,ffilt,fcz,fczoom,njump
	   else if(iptype.eq.302) then	!calc curve
	      read(11,rec=krn) idum,itit,title2,isdfst,ndisp1,ndev,
     &	  istart,iend,srate,calfac,ioff,calfac2,nrec1,cdate,adctime,
     &	  ntrans,y0,t0,nfilt,dtf,tif1,tif2,
     &	  (dt(i),i=1,ntrans-1),(stepamp(i),i=1,ntrans)
		  krn=krn+1
		read(11,rec=krn) (filt(i),i=1,nfilt),adcf1,base,
     &		ndelt,ffilt,fcz,fczoom,njump
	   endif
c	   iend=isdfst+ndisp1-1	!this is relative to section #, but not used
	   iend=istart+ndisp1-1	!abs index in consam
	   jdat=ndisp1
	endif
c
	title1(1:44)=title2
	do i=45,64
	   title1(i:i)=' '
	enddo
	RETURN
	end


