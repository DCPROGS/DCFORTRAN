	subroutine DSKWRIT2(tint,iampl,iprops,nint,ffilt,rms,avamp,
     & calfac2,expdate,iscanver,treso,tresg,vkin,ndim,
     & cjump,njump,tjmax)
c
c DSKWRIT2.FOR is version of DSKWRIT1.FOR	 that writes simulated data in SCSIM
c to a SCAN.SCN file (rather than the old scandat.dat data disk).  As before
c the data reacord contains only tint(), ampl() and iprops(), but the header
c part needs to be different from real data because (a) many things that are
c defined for real data in SCAN are undefined (and not needed) here, and
c (b) need to add here treso, tresg (in ms), which are not defined in SCAN
c So ekdist (diskin1) and hjcfit (***) and cjfit (***) need to be altered to read in simulated
c data from new scan.dat files that are written here
c OLD DATA FORMAT (scandat.dat), NOW DEFUNCT is still in DSKWRIT1 (not used now)
c
c DSKWRIT1 is for writing simulated data to SCANDAT.DAT in SCSIM (modif
c  of DSKWRIT which writes permanent data files in SCAN, omitting
c  options concerning restart of SCAN, omitting point-amp data but
c  including treso, tresg (in ms) as specified in HJCDAT1).
c NB in call expdate=cdate (date of run), iscanver=iscan=-3001
c
c Modified 11/30/97 04:34pm to write cjump data too
c  This necessitates making iprops() a parameter, rather than allocating
c  locally here, because iprops needed for jumps
c  For simulated jumps, jumps(i)=i so no need to save it, and kjumps()
c  can be found simply from iprops() (bit 5='32' set), so no need to
c  write these -construct them in EKDIST when iscanver=-3001. There is no
c  'record' before t=0 so tzero=0.0
c
c NB nint=number of defined intervals=kt-1 (last duration not defined)
c Modif 03/03/94 09:37am to write point amplitude frequencies (non-zero
c  elements only are kept). Space taken =4*(n1+n2+n3) bytes, where
c  n1=ifmax(1)-ifmin(1)+1 etc.
c
c Data=
c real*4 tint(1...nint) 	   4nint bytes
c integer*2 iampl(1..nint)   2*nint bytes
c integer*1 iprops(1..nint)  nint  bytes
c Total storage needed=7*nint bytes
c +header=ioffset=256 bytes
c
	real*4 tint(ndim)
	integer*2 iampl(ndim)
	integer*1 iprops(ndim)
c	allocatable::iprops
c	integer*1 iprops(:)		!now local
c	integer*4 DSKSPC			!utility library
      character*32 filnam*32,scanfil*40,defname*6
	character*40 mtitle		!machine names
	character ans*1,expdate*11,title*70
	character tapeID*24
	character title1*70
	logical pon,slock,debug,caplock,present,simulat
	logical discprt
c
	common/dp/discprt
	character*4 prtport
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c For cjumps
	logical cjump
	integer*4 ipatch
c
c Find out where data file is (default=A:\scandat.dat) (options for other
c disc/other path/other name). (NB if disk contains SCDIRm.DAT this shows
c that it is an old-style single channel disk)
c
	pon()=slock()
	debug()=caplock()
c
	if(.not.cjump) then
	   do i=1,nint
		iprops(i)=0
	   enddo
	endif
c
c Code to write as SCAN.SCN file (as in scanout.for, but header different for
c simulated data, and only tint, ampl, iprops written as data
	call DCASK(' Write results to SCAN.SCN file now','y',ans)
	if(ans.eq.'N') goto 999
c
	call TITENT0(
     & 'Name (and path) for SCAN.SCN file:',scanfil,40,.false.)
	ipatch=5
	tapeID='Data simulated in SCSIM '
	Emem=vkin
3	INQUIRE(file=scanfil,exist=present,flen=len)
c
c If file already present check the title etc -Expdate should start at byte 422
	if(present.and.len.gt.0) then
	   OPEN(unit=15,file=SCANFIL,status='UNKNOWN',
     &	access='TRANSPARENT')
	   call BELL(2)
	   read(15,rec=1) iscan1
	   simulat=iscan1.eq.-3001.or.iscan1.eq.-103
	   if(.not.simulat) then
		read(unit=15,rec=13) title1,expdat1,defname,tapeID1,
     &	  ipatch1,npatch1
		print 2,scanfil,title1,expdat1,npatch1,tapeID1
	   else if(iscan1.eq.-103) then  	!simulated scan.dat
		read(unit=15,rec=13) title1,expdat1,tapeID1,ipatch1
		print 21,scanfil,title1,expdat1
	   else if(iscan1.eq.-3001) then  	!simulated scandat.dat file
		print 211,scanfil
	   endif
	   CLOSE(unit=15)
2	   format(' There is already a scan.scn file named ',a40,/,
     & '   title: ',a70,/,
     & '   Date of experiment: ',a11,' (patch number ',i3,')',/,
     & '   Tape details: ',a24)
21	   format(
     & ' There is already a SIMULATED scan.scn file named ',a40,/,
     & '   title: ',a70,/,
     & '   Date of simulation: ',a11)
211	   format(
     & ' There is already a SIMULATED scandat.scn file named ',a40,/)
	   print 22
22	   format(
     & ' (1) Overwrite the existing file ',/,
     & ' (2) Write the file with a different name',/,
     & ' (3) Don''t write anything ',/,
     & '  Option number [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.eq.2) then
		call TITENT0(
     &	 'Name (and path) for SCAN.SCN file:',scanfil,40,.false.)
		goto 3 	!in case new name exists too
	   else if(iopt.eq.3) then
		goto 999
	   endif
	   title=title1	!use existing title as default
	endif
c
	call TITENT0('Title for simulated patch: ',title,70,.false.)
c
	ioffset=768
	OPEN(unit=15,file=SCANFIL,status='UNKNOWN',access='TRANSPARENT')
c
c Write new scantemp/scan.dat header
c COMPLETE WRITE FOR NON-SIM DATA AS IN SCAN
c	write(unit=15,rec=1) iscanver,ioffset,nint,titles,expdate,
c     & defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile,cjump,
c     & nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
c     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
c     & nwrit,nwsav,newpar,opendown,invert,
c     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
c     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
c     & nsetup,filtfile,ffilt,npfilt,
c     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
c     & ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
c     & tsfac,tlfac,sdone,dfinter,tlast,
c     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
c     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
c     & itrig,itrigvga,itriglev,inc,incabs,
c     & indfst,isdfst,isec,ndisp,ndisp1,
c    & idatyp,cdate1,nchan,
c     & tcfac,facjump,shutsav,goback,
c     & imin,imax,errfac,derivfac,confac,
c     & nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
c     & njump,nnull,ktlast,
c     & izoom,fcz,fczoom,ampz,avampsav,
c     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
c     & stpfac
c NEW FORMAT FOR SIMULATED DATA IN SCAN.DAT -WRITE THE SAME THINGS AS IN
C IN THE OLD SCANDAT FORMAT
	if(.not.cjump) then
c Write the header for simulated steady state
	   write(15,rec=1) iscanver,ioffset,nint,title,expdate,
     &    tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &    treso,tresg
c Write the data
	   write(15,rec=ioffset)
     &    (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint)
c
	else if(cjump) then		!write cjump data
c Write the header for simulated jumps
	   nsweep=njump
	   tsamp=tjmax*1000.	!ekdist expects microsec
	   tzero=0.
	   write(15,rec=1) iscanver,ioffset,nint,title,expdate,
     &    tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &    treso,tresg,nsweep,tzero,tsamp
c Write the data
	   write(15,rec=ioffset)
     &    (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint)
	endif
c
	print 26,title,scanfil,expdate,nint,
     & Emem,ffilt,rms,calfac2,ioffset
	if(discprt)write(8,26) title,scanfil,expdate,nint,
     &  Emem,ffilt,rms,calfac2,ioffset
c
26	format(/,
     & ' Title: ',a70,/,
     & ' SIMULATED Data written to SCAN.DAT file named ',a40,/,
     & ' Date of experiment: ',a11,/,
     & ' Number of transitions = ',i8,/,
     & ' Membrane potential (mV) = ',f9.2,/,
     & ' Filter (kHz, -3 dB) = ',f12.5,/,
     & ' RMS noise (pA) = ',f10.4,/,
     & ' Calibration factor (amplitude to pA)',g13.6,/,
     & ' Offset (bytes) from start of header to start of data = ',i8)

999	continue
	RETURN
	end

