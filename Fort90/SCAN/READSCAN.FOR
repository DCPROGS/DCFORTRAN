	subroutine READSCAN(kt,tint,iampl,iprops,ntmax,nfits,timsav,
     & ifits,ibaselin,nfmax,nbuf,novlap,srate,finter,ndisc,errfac,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,confac,stpfac,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,ffilt,filtf,
     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     & disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     & itrig,itrigvga,itriglev,inc,incabs,
     & indfst,isdfst,isec,ndisp,ndisp1,
     & filt1,nfilt1,dtf1,timamax,
     & tf01,tf1,tf50,tf99,tf999,npfilt,tstring,
     & iscanver,tcfac,facjump,derivfac,mtitle,
     & jfreq1,jfreq2,jfreq3,imin,imax,im1,im2,ifmin,ifmax,
     & cjump,njdim,jump1,kjump1,ijd,tzerod,intzero,tsample,ntotjump,
     & ktjump,njump,njfit,ijump,istrec,jumplen,nnull,ktlast,
     & newfile,ioffset,izoom,fcz,fczoom,ampz,avampsav,gostrt,
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     & nchan,idatyp,cdate1,titles,
     & adcfil,expdate,defname,tapeID,ipatch,npatch,Emem,temper)
c
c To read back stuff written by AUTWRIT and do necessary calcs with
c them, for restart in SCAN.
c Modif 03/10/99 08:01am by adding total number of jumps as parameter, ntotjump
c
c Following to be recalc following restart (along with filt1() etc)
c     & filt1,nfilt1,dtf1,timamax,
c     & tf01,tf1,tf50,tf99,tf999,npfilt,
c
	real*8 dfinter,tlast
	logical invert,opendown,newpar,sdone,present,fcdone
	logical disp,usepots,gostrt,newfile
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	logical discprt
	logical pon,slock,debug,caplock
	character mtitle*40
	character titles*70	!for scan.dat file
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,cdate2*11,ndev*2,ans,UC
	character scantfil*40
	character adctime*8,adctime2*8,cs*3
	character*76 title
	character title1*70
	character ftitle*80,filtfile*20
	character expdate*11,defname*6,tapeID*24
c Declarations for temporary data files
	real*8 timsav(nfmax)
c	integer*2 ifits(nfmax),ibaselin(nfmax)
	integer*2 ibaselin(nfmax)
	integer*4 ifits(nfmax)		!int*4 in iscanver=102
	integer*2 idt,inchan,inchan2,id1,id2,iver,iver2
	allocatable::kjump2,ifits2	!needed to read integer*2 versions
	integer*2 kjump2(:),ifits2(:)	!ditto
	logical good
c for point amp
	integer*4 jfreq1(im1:im2),jfreq2(im1:im2),jfreq3(im1:im2)
	integer*4 ifmin(4),ifmax(4)
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)
c Declarations for things calculated here for restart
	real*4 timamax(49)
	real*4 filt(100),filt1(1024)
	character tstring*12,microsec*3,cnum*11
	integer*4 ipatch
c=	common/miscdat/ipatch,Emem	!for DSKWRIT/READJUMP/AUTWRIT/READSCAN
c
c For cjumps
	real*8 tzerod,tsample
c=	integer*2 jump1(ijd),kjump1(ijd)
	integer*2 jump1(ijd)
	integer*4 kjump1(ijd)		!in iscanver=102
	integer*4 istrec(1000)		!new format
c=	integer*2 istrec(250)
	logical cjump
      character ctime*11
c
	character*35 qfile
	common/qplot/qfile
c
c=	common/rstrtblk/nchan,idatyp,adcfil,cdate1
	common/dp/discprt
	common/aafunc/ trise
	external AFUNC
c
c Data for SCANTEMP.DAT
c (1) First write individual values needed for restart (401 bytes) starting
c	at 1st byte
c  (2) Start writing arrays at byte #=ioffset
c
c
	pon()=slock()
c
101	format(a1)
	scantfil='scantemp.dat'
	call TITENT0('Name of SCANTEMP file is: ',scantfil,40,.false.)
c
13	continue
	INQUIRE(file=scantfil,exist=present,flen=len)
	if(present.and.len.gt.0) then
c
	   OPEN(unit=13,file=scantfil,status='UNKNOWN',
     &     access='TRANSPARENT')
	   read(unit=13,rec=1) iscanver
	   if(iscanver.le.102) then
	      read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioff3,
     &   nbuf,novlap,srate,finter,
     &   tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &   nwrit,nwsav,newpar,opendown,invert,
     &   usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &   expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &   nsetup,filtfile,ffilt,npfilt,
     &   sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &   ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &   tsfac,tlfac,sdone,dfinter,tlast,
     &   shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &   infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &   itrig,itrigvga,itriglev,inc,incabs,
     &   indfst,isdfst,isec,ndisp,ndisp1,
     &   idatyp,adcfil,cdate1,nchan,
     &   iscanver,tcfac,facjump,shutsav,goback,
     &   imin,imax,errfac,derivfac,confac,
     &   cjump,ijump,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &   njump,nnull,ipatch,Emem,ktlast,
     &   izoom,fcz,fczoom,ampz,avampsav,
     &   itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     &   qfile,
     &   adcfil,expdate,defname,tapeID,ipatch,npatch,Emem,temper,
     &   titles
	else if(iscanver.ge.103) then
	   read(unit=13,rec=1) iscanver,ioffset,kt,titles,expdate,
     & defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile,cjump,
     & nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,ffilt,npfilt,
     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,sdone,dfinter,tlast,
     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     & itrig,itrigvga,itriglev,inc,incabs,
     & indfst,isdfst,isec,ndisp,ndisp1,
     & idatyp,cdate1,nchan,
     & tcfac,facjump,shutsav,goback,
     & imin,imax,errfac,derivfac,confac,
     & ijump,njdim,tzerod,intzero,tsample,ktjump,njfit,
     & njump,nnull,ktlast,
     & izoom,fcz,fczoom,ampz,avampsav,
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     & stpfac
	endif
c
	   if(stpfac.lt.0.01.or.stpfac.gt.5.) stpfac=0.1
c
c check valid queue file name present
	   do i=1,35
		ival=ichar(qfile(i:i))
		if(ival.lt.0.or.ival.gt.127) idiskq=-1
	   enddo
	   if(iscanver.eq.100) then
		cjump=.false.	!older data
		ijump=0		!prevents error from undefined ijump value
	   endif
c
	   if(iscanver.eq.100) then
		ALLOCATE(ifits2(nfmax),kjump2(ijd))	!integer*2 versions
	      read(unit=13,rec=ioff3)
     &      (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &      (timsav(i),i=1,nfits),(ifits2(i),i=1,nfits),
     &      (ibaselin(i),i=1,nfits),
     &      ifmin,ifmax,
     &      (jfreq1(i),i=ifmin(1),ifmax(1)),
     &      (jfreq2(i),i=ifmin(2),ifmax(2)),
     &      (jfreq3(i),i=ifmin(3),ifmax(3))
	   else if(iscanver.eq.101) then
		ALLOCATE(ifits2(nfmax),kjump2(ijd))	!integer*2 versions
	      read(unit=13,rec=ioff3)
     &      (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &      (timsav(i),i=1,nfits),(ifits2(i),i=1,nfits),
     &      (ibaselin(i),i=1,nfits),
     &      ifmin,ifmax,
     &      (jfreq1(i),i=ifmin(1),ifmax(1)),
     &      (jfreq2(i),i=ifmin(2),ifmax(2)),
     &      (jfreq3(i),i=ifmin(3),ifmax(3)),
     &      (jump1(i),i=1,ijump),(kjump2(i),i=1,ijump)
	   else if(iscanver.eq.102) then
	      read(unit=13,rec=ioff3)
     &      (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &      (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &      (ibaselin(i),i=1,nfits),
     &      ifmin,ifmax,
     &      (jfreq1(i),i=ifmin(1),ifmax(1)),
     &      (jfreq2(i),i=ifmin(2),ifmax(2)),
     &      (jfreq3(i),i=ifmin(3),ifmax(3)),
     &      (jump1(i),i=1,ijump),(kjump1(i),i=1,ijump)
	   else if(iscanver.ge.103) then
		if(.not.cjump) then
		   read(unit=13,rec=ioffset)
     &	  (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &	  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &        (jfreq1(i),i=ifmin(1),ifmax(1)),
     &        (jfreq2(i),i=ifmin(2),ifmax(2)),
     &        (jfreq3(i),i=ifmin(3),ifmax(3)),
     &	  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &	  (ibaselin(i),i=1,nfits)
		else if(cjump) then
		   read(unit=13,rec=ioffset)
     &	  (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &	  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &        (jfreq1(i),i=ifmin(1),ifmax(1)),
     &        (jfreq2(i),i=ifmin(2),ifmax(2)),
     &        (jfreq3(i),i=ifmin(3),ifmax(3)),
     &        (jump1(i),i=1,ijump),(kjump1(i),i=1,ijump),
     &	  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &	  (ibaselin(i),i=1,nfits)
		endif
	   endif
c
	   CLOSE(unit=13)
c  Convert to integer*2 if nec
	   if(iscanver.le.101) then
		do i=1,nfits
		   j=int4(ifits2(i))
	         if(j.le.-1.and.j.ge.-32768) then
	      	j=j+65536
	   	   else if(j.lt.-32768) then
	      	print*,' IFITS2() IS TOO BIG!!'
	      	STOP
	         endif
	   	   ifits(i)=j
		enddo
		do i=1,ijump
		   j=int4(kjump2(i))
	         if(j.le.-1.and.j.ge.-32768) then
	      	j=j+65536
	   	   else if(j.lt.-32768) then
	      	print*,' KJUMP2() IS TOO BIG!!'
	      	STOP
	         endif
	   	   kjump1(i)=j
		enddo
		DEALLOCATE(ifits2,kjump2)
	   endif
c check for bad fczoom vakue
	   if(fczoom.lt.0.001) then
		if(fcz.ne.ffilt) then
		   if(fcz.ge.ffilt) then
			fcz=ffilt
			fczoom=ffilt
		   else
		      fczoom=1./sqrt((1.0/(fcz*fcz)) - (1.0/(ffilt*ffilt)))
		   endif
		endif
	   endif
c Insert check for izoom values, in case the scantemp is an old one
c without these values
	   if(izoom.lt.1.or.fcz.lt.0.001.or.abs(ampz).lt.0.01.or.
     &	abs(avampsav).lt.0.01) then
		call BELL(1)
		print 3
3		format(' Values for zoom-filter are invalid (old SCANTEMP?)'
     &	,/,' Plausible values allocated (check in parameter menu)')
		if(izoom.lt.1) izoom=4
		if(fcz.lt.0.001.or.fcz.gt.ffilt) then
		   fcz=ffilt
c When fcz=ffilt, fczoom->infinity ie no filtering needed, and filtzoom set
c to false later
c==		   fczoom=1./sqrt((1.0/(fcz*fcz)) - (1.0/(ffilt*ffilt)))
		endif
c		if(abs(ampz).lt.0.01) ampz=avamp*calfac2	!pA
c		if(abs(avampsav).lt.0.01) avampsav=avamp
		ampz=avamp*calfac2	!pA
		avampsav=avamp
	   endif
c Now reset iprops(kt)=0 because, at CASE(9) in scan, this would be set
c to '64' to signify 'interval from last transition to end of sample', which
c it may not be if we intended to restart later.
c NO -now set iprops=64 only if all openings fitted!
c===	   iprops(kt)=0
c
	   print 20
	   if(pon()) write(7,20)
	   if(discprt) write(8,20)
20	   format(/,' RESTART ANALYSIS:')
	   call DCASK(
     &  'Go straight to write data to permanent file','n',ans)
	   gostrt=ans.eq.'Y'
	   if(gostrt) RETURN
c
2	   continue

	   INQUIRE(file=ADCFIL,exist=present,flen=len1)
c
	   if(present.and.len1.gt.0.and.(.not.cjump)) then
		OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
     &		access='TRANSPARENT')
		if(idatyp.ne.2) then
		   read(14,rec=1) iver
	         if(iver.eq.1002) then	!already have expdate etc
			read(14,rec=1)iver,title1,cdate2,adctime2
	   	   else
	            read(14,rec=1) title,cdate2,adctime2
			title1=title(1:70)
		   endif
		else
		   call READAXON(adcfil,title,cdate2,adctime2,ilen2,inchan2,
     &            cfac,pAV,srate2,ffilt2,iver2,ioff2,good)
			title1=title(1:70)
		endif
	      CLOSE(unit=14)
	      print 111,ndev,adcfil
111	      format(' Disk ',a2,' CONSAM file = ',a30)
		print 112,cdate2,adctime2,title1
112		format(' Date= ',a11,' time= ',a8,/,1x,a70,/,
     &	' Is this correct [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') then
c		   print 50
50		   format(' Specify disc to take data from (CONSAM.DAT):')
c		   call DISCNUM1(idisc,ndev,0,0)
c		   adcfil(1:2)=ndev
		   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
		   goto 2	!check it is there
		endif
	   else if(present.and.len1.gt.0.and.cjump) then
            OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=512)
	       read(14,rec=1) jver,njd
	      CLOSE(unit=14)
		if(jver.gt.0) njd=jver	!old format file
		print 410,adcfil,njd
410         format(' CJUMP file: ',a30,'(contains ',i6,' jumps)',/,
     &	' Is this correct [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') then
		   print 51
51		   format(' Specify disc to take data from (CJUMP.DAT):')
		   call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
		   adcfil(1:2)=ndev
		   goto 2	!check it is there
		endif
		ntotjump=njd
	   else
		call BELL(3)
		print 113,adcfil
113		format(' CANNOT FIND RAW DATA FILE: ',a30)
		if(ndisc.gt.1) then
		   if(.not.cjump) print 50
		   if(cjump) print 51
		   call DISCNUM1(idisc,ndev,0,1)
		   if(idisc.eq.-1) STOP
	      else
		   ndev='C:'
		endif
		if(.not.cjump) then
c		   ADCFIL=ndev//'\CONSAM.DAT'
		   ADCFIL(1:2)=ndev
		   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
		else
		   ADCFIL=ndev//'\CJUMP.DAT'
		   call TITENT0('Input CJUMP file name:',ADCFIL,30,.false.)
		endif
		goto 2		!check it is there
	   endif
	else
	   call BELL(3)
	   print 11,scantfil
11	   format(' CANNOT FIND SCANTEMP.DAT file: ',a40)
	   call TITENT0('Name of SCANTEMP file is: ',scantfil,40,.false.)
	   goto 13		!try again
	endif
c
c
12	continue
c Initialise unused parts of tint() etc
	do i=kt+1,ntmax
	   iampl(i)=-9999	!until defined
	   tint(i)=0.
	   iprops(i)=0
	enddo
c Now need to open data files, get step response function etc
	if(idatyp.eq.1) then
	   ioff=0
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   goto 54		!skip next bit
	endif
c For normal (not PDP) data:
c Open data file and print details
c             (NB changed so ffilt read from scantemp (in kHz) not altered)
	if(.not.cjump) then
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   if(idatyp.eq.2) then
		call READAXON(adcfil,title,cdate2,adctime2,ilen,inchan,
     &            cfac,pAV,srate,ffilt2,iver,ioff2,good)	!use FFILT from scantemp
		title1=title(1:70)
	   else
	      read(14,rec=1) iver
	      if(iver.eq.1002) then	!already have expdate etc
		  read(14,rec=1)iver,title1,cdate2,adctime2,idt,ioff,ilen,
     &	  inchan,id1,id2,cs(1:3),cfac,srate,ffilt2
	      else
	        read(14,rec=1) title,cdate2,adctime2,idt,ioff,ilen,inchan,
     &        id1,id2,cs(1:3),cfac,srate,ffilt2
		  title1=title(1:70)
		endif
	   endif
	   ff=ffilt*1000.	!convert to Hz for print
         print 35, adcfil,cdate2,adctime2,ff,title1,srate,ilen
         if(pon()) write(7,35)adcfil,cdate,adctime,ff,title1,srate,ilen
       if(discprt) write(8,35)adcfil,cdate,adctime,ff,title1,srate,ilen
35	   format(
     &   ' CONSAM file: ',a30,/,' Date= ',a11,' time= ',a8,
     &   ' filter (Hz) = ',f9.1,/,1x,a70,/,
     &   ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/)
c==	  ffilt=ffilt/1000.	!convert back to kHz -already kHz in scantemp
c
	else if(cjump) then
c        Call READJUMP with imode=1 to get istrec() etc (READJUMP returns
c ffilt in kHz, but safer to use ffilt from SCANTEMP in case the jump from
c which we start has not had ffilt recorded in cjump.dat)
c Readjump now returns ioffset=offset for new cjump5 data files (newfile=
c true if this sort of file is in use)
	   njump=jump1(ijump)	!current jump to restart with
	   call READJUMP(njump,nsamp,ioff,ndisc,adcfil,calfac,
     &    srate,ffiltdum,ktjump,nfjump,njdim,tzero,intzero,
     &    tsample,readini,istrec,jumplen,njfirst,ntotjump,
     &    newfile,ioffset,ipatch,Emem,idest,1)
	   if(idest.eq.999) STOP	!abort DISCNUM
c==========do this print within readjump?
         print 36, adcfil,1000.*ffilt
         if(pon())write(7,36)adcfil,1000.*ffilt
         if(discprt)write(8,36)adcfil,1000.*ffilt
36	   format(
     &   ' CJUMP file: ',a30,/,
     &   ' filter (Hz) = ',f9.1)
	endif
	if(.not.disp) then
	   filtf=ffilt	!default=value from CONSAM
	   nsetup=-iabs(nsetup)	!so filter checked (eg in case Gaussian in error)
	   call GETFILT(nfilt,dtf,filt,nsetup,filtf,filtfile,ftitle,
     &	.true.,fcdone)
	   fcdone=.true.		!set true in getfilt anyway after restart
	   ffilt=filtf		!in case wrong value from CONSAM has been corrected
c 'normalise' the step response.
	   call NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,
     &  tf01,tf1,tf50,tf99,tf999,nsetup,filtf)
c Make table of amp/amp(max) vs length (microsec) , for A/Amax=.05,(.05),.95
	   trise=332.1/filtf		!rise time (microsec)
c	   print 228
c	   if(pon()) write(7,228)
c	   if(discprt) write(8,228)
c228	   format(' A/Amax    length (ms)')
	   do i=1,49
		aam=float(i)*0.02	!a/amax
		wlo=0.0		!low guess for w
		whi=20.*trise
		epsx=0.1	!0.1 microsec accuracy
		nd=-2	!no printing
		call BISEC0(AFUNC,wlo,whi,aam,wout,Yout,EPSx,epsy,
     &	 Nerr,nd,.false.)
		timamax(i)=wout
c		print 23,timamax(i),aam
c		if(pon()) write(7,23) timamax(i),aam
c		if(discprt) write(8,23) timamax(i),aam
c23		format(2(g13.6,2x))
	   enddo
	endif
c
54	continue
	aam=abs((float(itriglev)-base)/avamp)
	call AMAX(aam,trise,ttrig,nerr)
	microsec=' '//char(230)//'s'	!char(230)=greek mu
	if(nerr.eq.0) then
     	   call INTCONV(ifixr(ttrig),cnum)
	   n=NBLANK(cnum)
	   tstring='trig='//cnum(1:n)//microsec
	else
	   tstring='trig= ?     '
	endif
c
	ffilt=filtf		!in case wrong value from CONSAM has been corrected
c
	RETURN
	end


