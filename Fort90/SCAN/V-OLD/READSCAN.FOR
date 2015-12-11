	subroutine READSCAN(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     & ibaselin,nfmax,nbuf,novlap,srate,finter,ndisc,errfac,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,confac,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,ffilt,
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
     & cjump,njdim,jump1,kjump1,ijd,tzerod,intzero,tsample,
     & ktjump,njump,njfit,ijump,istrec,jumplen,nnull,ktlast)
c
c To read back stuff written by AUTWRIT and do necessary calcs with
c them, for restart in SCAN.
c
c Following to be recalc following restart (along with filt1() etc)
c     & filt1,nfilt1,dtf1,timamax,
c     & tf01,tf1,tf50,tf99,tf999,npfilt,
c
	real*8 dfinter,tlast
	logical invert,opendown,newpar,sdone,present
	logical disp,pdpdata,usepots
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	logical discprt
	logical pon,slock,debug,caplock
	character mtitle*40
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,cdate2*11,ndev*2,ans,UC
	character adctime*8,adctime2*8,cs*3
	character*76 title
	character ftitle*80,filtfile*20
c Declarations for temporary data files
	real*8 timsav(nfmax)
	integer*2 ifits(nfmax),ibaselin(nfmax)
	integer*2 idt,inchan,id1,id2
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
	common/miscdat/ipatch,Emem	!for DSKWRIT/READJUMP/AUTWRIT/READSCAN
c
c For cjumps
	real*8 tzerod,tsample
	integer*2 jump1(ijd),kjump1(ijd)
	integer*2 istrec(250)
	logical cjump
      character ctime*11
	character title1*79
c
	common/rstrtblk/nchan,pdpdata,adcfil,cdate1
	common/dp/discprt
	common/aafunc/ trise
	external AFUNC
c
c Data for SCANTEMP.DAT
c (1) First write individual values needed for restart (401 bytes) starting
c	at 1st byte
c  (2) Start writing arrays at byte #=ioffset
c
c==	ioffset=512
c
	pon()=slock()
c
101	format(a1)
c
	INQUIRE(file='scantemp.dat',exist=present,flen=len)
	if(present.and.len.gt.0) then
c
	   OPEN(unit=13,file='SCANTEMP.DAT',status='UNKNOWN',
     &     access='TRANSPARENT')
	   read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset,
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
     &   pdpdata,adcfil,cdate1,nchan,
     &   iscanver,tcfac,facjump,shutsav,goback,
     &   imin,imax,errfac,derivfac,confac,
     &   cjump,ijump,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &   njump,nnull,ipatch,Emem,ktlast
c
	   read(unit=13,rec=ioffset)
     &   (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &   (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &   (ibaselin(i),i=1,nfits),
     &   ifmin,ifmax,
     &   (jfreq1(i),i=ifmin(1),ifmax(1)),
     &   (jfreq2(i),i=ifmin(2),ifmax(2)),
     &   (jfreq3(i),i=ifmin(3),ifmax(3)),
     &  (jump1(i),i=1,ijump),(kjump1(i),i=1,ijump)
c
	   if(iscanver.eq.100) cjump=.false.	!older data
c
	   CLOSE(unit=13)
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
c
2	   continue
	   INQUIRE(file=ADCFIL,exist=present,flen=len1)
c
	   if(present.and.len1.gt.0.and.(.not.cjump)) then
		OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
     &		access='TRANSPARENT')
	      read(14,rec=1) title,cdate2,adctime2 !don't overwrite vals from scantemp
	      CLOSE(unit=14)
	      print 111,ndev,adcfil
111	      format(' Disk ',a2,' CONSAM file = ',a30)
		print 112,cdate2,adctime2,title
112		format(' Date= ',a11,' time= ',a8,/,1x,a76,/,
     &	' Is this correct [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 50
50		   format(' Specify disc to take data from (CONSAM.DAT):')
		   call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
		   adcfil(1:2)=ndev
		endif
	   else if(present.and.len1.gt.0.and.cjump) then
            OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=512)
	       read(14,rec=1) njd
	      CLOSE(unit=14)
		 print 410,adcfil,njd
410          format(' CJUMP file: ',a30,'(contains ',i6,' jumps)',/,
     &	' Is this correct [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 51
51		   format(' Specify disc to take data from (CJUMP.DAT):')
		   call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
		   adcfil(1:2)=ndev
		endif
	   else
		call BELL(3)
		print 113,adcfil
113		format(' CANNOT FIND: ',a30)
		if(ndisc.gt.1) then
		   if(.not.cjump) print 50
		   if(cjump) print 51
		   call DISCNUM1(idisc,ndev,0,1)
		   if(idisc.eq.-1) STOP
	      else
		   ndev='C:'
		endif
		if(.not.cjump) then
		   ADCFIL=ndev//'\CONSAM.DAT'
		   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
		else
		   ADCFIL=ndev//'\CJUMP.DAT'
		   call TITENT0('Input CJUMP file name:',ADCFIL,30,.false.)
		endif
		goto 2		!check it is there
	   endif
	endif
c
c
c Initialise unused parts of tint() etc
	do i=kt+1,ntmax
	   iampl(i)=-9999	!until defined
	   tint(i)=0.
	   iprops(i)=0
	enddo
c Now need to open data files, get step response function etc
	if(pdpdata) then
	   ioff=0
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   goto 54		!skip next bit
	endif
c For normal (not PDP) data:
c Open data file and print details
	if(.not.cjump) then
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   read(14,rec=1) title,cdate2,adctime2,idt,ioff2,ilen,inchan,
     &   id1,id2,cs(1:3),cfac,srate,ffilt
         print 35, adcfil,cdate2,adctime2,ffilt,title,srate,ilen
         if(pon())write(7,35)adcfil,cdate,adctime,ffilt,title,srate,ilen
       if(discprt)write(8,35)adcfil,cdate,adctime,ffilt,title,srate,ilen
35	   format(
     &   ' CONSAM file: ',a30,/,' Date= ',a11,' time= ',a8,
     &   ' filter (Hz) = ',f9.1,/,1x,a76,/,
     &   ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/)
	  ffilt=ffilt/1000.		!convert to kHz
	else if(cjump) then
c        Call READJUMP with imode=1 to get istrec() etc (READJUMP returns
c ffilt in kHz, but safer to use ffilt from SCANTEMP in case the jump from
c which we start has not had ffilt recorded in cjump.dat)
	   njump=jump1(ijump)	!current jump to restart with
	   call READJUMP(njump,nsamp,ioff,ndisc,adcfil,calfac,
     &    srate,ffiltdum,ktjump,nfjump,njdim,tzero,intzero,
     &    tsample,readini,istrec,jumplen,idest,1)
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
     &	.true.)
c 'normalise' the step response.
	   call NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,
     &  tf01,tf1,tf50,tf99,tf999)
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
	RETURN
	end


