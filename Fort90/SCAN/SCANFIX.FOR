	program SCANFIX
c To read header of a new scan.dat file, and correct entries if necessary.
c  Replaces SCDIR which is for old scandat.dat disks
c
	character ans*1,expdate*11,title*70,ndev*2
	character tapeID*24,defname*6
	character cnum1*11,cnum2*11,cnum3*11
	character ctime*11
c Other declarations:
	character*40 scanfil
	character*14 ptype(5)
	logical cjump
	LOGICAL debug,pon,slock,caplock,present
	logical invert,opendown,newpar,sdone
	logical disp,usepots
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	logical discprt
c Extra declarations for new scan.dat files
	real*8 dfinter,tlast
	real*8 tzerod,tsample
	character ADCFIL*30,cdate*11,cdate1*11,filtfile*20
	character*1 qfile0*35,qfile*35	!as in scan.dat
c	character*1 qfile*40	!as in vplot and vhist
	character adctime*8
c
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='SCANFIX.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1      FORMAT(' SCANFIX: To read/correct header of scan.dat file.',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
c	print 3
c3       format(
c     & ' SCROLL LOCK on for printing'/
c     & ' CAPS LOCK on for debugging')
c

	INQUIRE(file='SCANFIX.INI',exist=present,flen=len)
      OPEN(unit=15,file='SCANFIX.INI',status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=256)
	if(present.and.len.gt.0) then
	   read(15,rec=1) scanfil
	else
	   scanfil='                                       '
	   write(15,rec=1) scanfil
	endif
	CLOSE(unit=15)
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
	call TITENT0('Data file name (scan.dat format) =',
     &   scanfil,40,.false.)

11	INQUIRE(file=SCANFIL,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   print 10,scanfil
10	   format(' File named ',a40,' NOT FOUND')
	   call TITENT0('Data file (scan.dat format) =',
     &   scanfil,40,.false.)
	   goto 11
	endif
c
	OPEN(unit=18,file=scanfil,status='UNKNOWN',access='TRANSPARENT')
c   	read(unit=18,rec=1) iscan,ioffset,nint,ntmax,nfits,
c     &	 nfmax,nbuf,novlap,srate,finter,
c     &	 tsect,ndat,nsec,nrlast,avamp,rms,imin,imax,
c     &	 cjump,nsweep,njdim,tzero,intzero,toffset,tsamp,
c     &	 ktjump,njump,njfit,nnull,
c     &	 smult,scrit,vary,iboff,
c     &	 nsetup,filtfile,npfilt,ffilt,trise,
c     &	 sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
c     &	 ampfac,tmin,tsfac,tlfac,tcfac,dfinter,vard,nshutfit,
c     &	 infit,infirst,idiskq,ifirst,base,basevga,ibasevga,
c     &	 izoom,fcz,fczoom,ampz,avampsav,
c     &	 errfac,itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
c     &	 ndev,cdate,adctime,qfile,adcfil,cdate1,nchan,idatyp,
c     &	 expdate,title,tapeID,npatch,ipatch,Emem,temper
	   read(unit=18,rec=1) iscanver,ioffset,nint,title,expdate,
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
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
	CLOSE(unit=18)
	amp=avamp*calfac2
	if(iscanver.lt.103) then
	   call BELL(2)
	   print 63,iscanver
63	   format(' ERROR IN SCAN VERSION NUMBER = ',i5)
	   pause
	endif
	print 28,
     &   expdate,npatch,title,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &   temper,ffilt,amp,rms,calfac2,qfile
      if(discprt) write(8,28)
     &	 expdate,npatch,title,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt,amp,rms,calfac2,qfile
28		format(/,
     &	 ' Date of experiment: ',a11,' (patch number ',i3,')'/,
     &	 ' Title: ',a70,/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Raw data file: ',a30,/,
     &	 ' Tape details: ',a24,/,
     &	 ' Patch type: ',a14,/,
     &	 ' Membrane potential (mV) = ',f9.2,/,
     &	 ' Temperature = ',f9.1,/,
     &       ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &       ' Full amplitude (pA) (approx) = ',f9.2,/,
     &       ' RMS noise (pA) = ',f10.4,4x,/,
     &       ' Calibration (amplitude to pA) = ',g13.6,/,
     &	 ' Plot queue file: ',a40)
C
	ans='Y'
	call DCASK('Change any values',ans,ans)
	if(ans.eq.'N') goto 999
120	continue
	iopt=0
	print 12,iopt
12	format(/,' Change: ',/,
     & ' (0) No more changes ',/,
     & ' (1) Date of experiment ',/,
     & ' (2) Patch number ',/,
     & ' (3) Title of file ',/,
     & ' (4) Raw data file name ',/,
     & ' (5) Tape details ',/,
     & ' (6) Patch type ',/,
     & ' (7) Membrane potential (mV)',/,
     & ' (8) Temperature ',/,
     & ' (9) Filter (kHz, -3 dB) ',/,
     & ' (10) Full amplitude (pA) (approx) ',/,
     & ' (11) RMS noise (pA) ',/,
     & ' (12) Calibration (amplitude to pA) ',/,
     & ' (13) Plot queue file name',/,
     & '   Option number [',i3,'] = ' )
	call INPUTi(iopt)

	SELECT CASE(iopt)
	CASE(0)
	   goto 90
	CASE(1)
331	   print 33
33	   format(' Give the date on which the experiment was done:',/,
     &'  Date (1-31) = ')
	   call INPUTi(idat)
	   if(idat.lt.1.or.idat.gt.31) goto 331
341	   print 34
34	   format('  Month (1-12) = ')
	   call INPUTi(imon)
	   if(imon.lt.1.or.imon.gt.12) goto 341
351	   print 35
35	   format('  Year (1999, 2000, 2001 etc) = ')
	   call INPUTi(iyr)
	   if(iyr.lt.1990.or.imon.gt.3000) goto 351
	   call INTCONV(idat,cnum1)
	   if(idat.le.9) then
		cnum1(2:2)=cnum1(1:1)
		cnum1(1:1)='0'
	   endif
	   call INTCONV(imon,cnum2)
	   if(imon.le.9) then
		cnum2(2:2)=cnum2(1:1)
		cnum2(1:1)='0'
	   endif
	   call INTCONV(iyr,cnum3)
C	   defname(1:6)=cnum3(3:4)//cnum2(1:2)//cnum1(1:2)	!for default file name
c Put esperiment date in nicer format into expdate*11
	   expdate(1:2)=cnum1(1:2)
	   expdate(3:3)='-'
	   if(cnum2(1:2).eq.'01') expdate(4:6)='Jan'
	   if(cnum2(1:2).eq.'02') expdate(4:6)='Feb'
	   if(cnum2(1:2).eq.'03') expdate(4:6)='Mar'
	   if(cnum2(1:2).eq.'04') expdate(4:6)='Apr'
	   if(cnum2(1:2).eq.'05') expdate(4:6)='May'
	   if(cnum2(1:2).eq.'06') expdate(4:6)='Jun'
	   if(cnum2(1:2).eq.'07') expdate(4:6)='Jul'
	   if(cnum2(1:2).eq.'08') expdate(4:6)='Aug'
	   if(cnum2(1:2).eq.'09') expdate(4:6)='Sep'
	   if(cnum2(1:2).eq.'10') expdate(4:6)='Oct'
	   if(cnum2(1:2).eq.'11') expdate(4:6)='Nov'
	   if(cnum2(1:2).eq.'12') expdate(4:6)='Dec'
	   expdate(7:7)='-'
	   expdate(8:11)=cnum3(1:4)

	CASE(2)
	   print 36,expdate
36	  format(' For experiment on ',a11,' this is patch number [1] = ')
	   call INPUTi(npatch)

	CASE(3)
	   call TITENT0('Title for file: ',title,70,.false.)

	CASE(4)
	   call TITENT0('Raw data file name: ',adcfil,30,.false.)

	CASE(5)
	   call TITENT0('Enter tape details: ',tapeID,24,.false.)

	CASE(6)
111	   print 311
311	   format(
     & ' Enter patch type:',/,
     & ' (1) outside-out',/,
     & ' (2) inside-out',/,
     & ' (3) cell-attached',/,
     & ' (4) whole-cell',/,
     & ' (5) simulated data',/,
     & '   Option number = ')
	   call INPUTi(ipatch)
	   if(ipatch.lt.1.or.ipatch.gt.5) goto 111

	CASE(7)
	   print 13,Emem
13	   format(' Membrane potential (mV) [',f8.2,'] = ')
	   call INPUTr(Emem)
	CASE(8)
	   print 121,temper
121	   format(' Temperature [',f8.2,'] = ')
	   call INPUTr(temper)

	CASE(9)
	   print 30,ffilt
30	   format(
     & ' Filter cutoff (kHz, -3 dB) [',f9.3,'] = ')
	   call INPUTr(ffilt)

	CASE(10)
	   print 31,amp
31	   format(
     & '  Full amplitude (pA) (approx)  [',f9.3,'] = ')
	   call INPUTr(amp)
	   avamp=amp/calfac2

	CASE(11)
	   print 32,rms
32	   format(
     & ' RMS noise (pA)  [',f9.4,'] = ')
	   call INPUTr(rms)

	CASE(12)
	   print 38,calfac2
38	   format(
     & ' Calibration (amplitude to pA) [',g11.5,'] = ')
	   call INPUTr(rms)

	CASE(13)
c use default based on scan.dat file name
	   qfile0=qfile
	   qfile=scanfil
	   n=nblank(qfile)
	   do i=1,n
		if(qfile(i:i).eq.'.') then
		   qfile(i+1:i+2)='pq'
		   goto 39
		endif
	   enddo
39	   call TITENT0('Plot queue file name: ',qfile,35,.false.)
	   call RENAME(qfile0,qfile)
	   print 40,qfile0,qfile
         if(discprt) write(8,40) qfile0,qfile
40	   format(/,
     & ' The plot queue file originally named ',a35,/,
     & ' has been renamed on disk as ',a35)

	END SELECT
	goto 120	!another change?

c Write altered values
90	continue
	OPEN(unit=18,file=scanfil,status='UNKNOWN',access='TRANSPARENT')
	write(unit=18,rec=1) iscanver,ioffset,nint,title,expdate,
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
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
	CLOSE(unit=18)

999	continue
      OPEN(unit=15,file='SCANFIX.INI',status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=256)
	write(15,rec=1) scanfil
	CLOSE(unit=15)
	call ENDPRINT
	end


