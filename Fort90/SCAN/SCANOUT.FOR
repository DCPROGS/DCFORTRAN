	subroutine SCANOUT(nint,tint,iampl,iprops,ntmax,nfits,timsav,
     & ifits,ibaselin,nfmax,nbuf,novlap,srate,finter,errfac,derivfac,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,confac,stpfac,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,npfilt,ffilt,
     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     & disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     & itrig,itrigvga,itriglev,inc,incabs,
     & indfst,isdfst,isec,ndisp,ndisp1,
     & iscanver,tcfac,facjump,
     & ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & cjump,nsweep,njdim,jumps,kjumps,tzerod,intzero,tsample,
     & ktjump,njump,njfit,nnull,ktlast,
     & izoom,fcz,fczoom,ampz,avampsav,
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     & nchan,idatyp,cdate1,
     & adcfil,expdate,defname,tapeID,ipatch,npatch,Emem,temper,
     & titles,scanfil,qfile,nlig,conc)
c
c For output of scan.dat data files from SCAN -they new data format that
c supersedes SCANDAT.DAT data files, with only one analysis per scan.dat file.
c
c This subroutine replaces DSKWRIT for final output of data for use in
c EKDIST etc (code was based on AUTWRIT which writes the scantemp.dat file).
c AUTWRIT is left as before, but SCANTEMP need no longer be kept -everything
c of interest from it is now in scan.dat.
c
c The new SCAN.DAT file contains more information than the old scandat.dat.
c In addtion to what was in that file, the new one contains all information
c from scantemp.dat -it is now essentially same as scantemp.dat and can
c be used for restarting scan
c
c New scantemp/scan.dat header starts in different order from before
c	write(unit=15,rec=1) iscanver,ioffset,nint,titles,expdate,
c                              4        4     4   70      11
c     & defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile,
c          6      24     4       4     4    4      30     35
c     & nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
c         4     4     4     4    4      4     4
c
	real*4 conc(10)
c
	real*8 dfinter,tlast
	logical csfound,digchar,scritvar
	logical discprt
	character*14 ptype(5)
	character ans*1,cnum*11
c	character UC*1
	logical invert,opendown,newpar,sdone
	logical disp,usepots
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
c	logical pon,slock,debug,caplock
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2
	character adctime*8
	character titles*70
	character expdate*11,defname*6,scanfil*40,tapeID*24
	character expdat1*11,defnam1*6,tapeID1*24,titl1*70
	character path*30,pname*8,suffix*3	!for parsname
	logical present,nopath
c for point amp histos
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
c Declarations for data files
	real*8 timsav(nfmax)
	integer*2 ibaselin(nfmax)
	integer*4 ifits(nfmax)
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)
	character filtfile*20
c For cjumps
	real*8 tzerod,tsample
	integer*2 jumps(njdim)
	integer*4 kjumps(njdim)
	logical cjump
	integer*4 ipatch
c
	character*35 qfile

	common/dp/discprt
c
c Find largest and smallest non-zero entries in ifreq()
	call GETIFMAX(imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax)
	print 11
11	format(
     & ' Writing data to permanent data file (SCAN.DAT):',/,
     & '  (1) In this version of the program, the output data file',/,
     & '    SCAN.SCN (with a default name like 990401s1.scn) is',/,
     & '    identical with the file (scantemp.dat) which SCAN',/,
     & '    produces to allow it to be restarted.',/,
     & '  (2) It is best, therefore, to write the data to the output',/,
     & '    file each time you leave the program, because this ',/,
     & '    automatically makes a backup of scantemp.dat.  If SCAN is',/,
     & '    and more transitions measured, then simply overwrite the',/,
     & '    earlier version of the scan.scn file.  If scantemp.dat',/,
     & '    gets lost, you can restart scan by specifying the name',/,
     & '    of the scan.scn file.    ',/,
     & '  (3) You can write the output data file at any time while ',/,
     & '    temporary (restart) file (SCANTEMP.DAT) is still on disk',/,
     & '    by restarting SCAN, and choosing ''Go straight to write',/,
     & '    permanent data file'' option',/)
c
	call DCASK(' Write results to SCAN.SCN now','y',ans)
	if(ans.eq.'N') goto 999
c
c Set default file name for scan.dat use same name as CONSAM except sc not cs
c after date
c	n=nblank(adcfil)
	scanfil(1:30)=adcfil		!initially
c	do i=1,n-1
c	   if(adcfil(i:i+1).eq.'cs') then	!eg d:\990102cs.dat
c		n1=i
c		csfound=.true.
c		goto 1
c	   endif
c	enddo
c1	continue
c	if(npatch.ge.0.and.npatch.le.9) then
c	   call INTCONV(npatch,cnum)
c	   suffix='da'//cnum(1:1)
c	endif
c	if(csfound) then
c	   scanfil(n1:n1+1)='sc'	!replace cs with sc
c	else
c	   scanfil=defname//'sc'//'.'//suffix(1:3)
c	endif
c Check that adcfil starts with numerical date
	call PARSNAME(adcfil,path,ndev,pname,suffix,nopath,30)
	csfound=.true.
	do i=1,6
	   ival=ICHAR(pname(i:i))
	   if(.not.DIGCHAR(ival)) then
		csfound=.false.
		goto 4
	   endif
	enddo
4	continue
c	suffix='dat'
	suffix='scn'
	if(csfound) then
	   pname(7:7)='S'	!replace C with S
	   if(cjump) pname(7:7)='J'	!replace C with S
	   scanfil=charnb(path)//charnb(pname)//'.'//suffix
	else
	   if(npatch.ge.0.and.npatch.le.9) then
		call INTCONV(npatch,cnum)
		scanfil=defname//'S'//cnum(1:1)//'.'//suffix(1:3)
	   else
		scanfil=defname//'SC'//'.'//suffix(1:3)
	   endif
	endif
	call TITENT0(
     & 'Name (and path) for SCAN.SCN file:',scanfil,40,.false.)
	if(ipatch.lt.1.or.ipatch.gt.5) then
	  call GETEXPT(expdate,defname,tapeID,ipatch,
     &		npatch,Emem,temper)
	endif
c
3	INQUIRE(file=scanfil,exist=present,flen=len)
c If file already present check the title etc -Expdate should start at byte 422
	iopt=0 	!file name not present
	if(present.and.len.gt.0) then
	   OPEN(unit=15,file=SCANFIL,status='UNKNOWN',
     &	access='TRANSPARENT')
	   call BELL(2)
	   read(unit=15,rec=13) titl1,expdat1,defnam1,tapeID1,
     &	ipatch1,npatch1
	   CLOSE(unit=15)
	   print 2, scanfil,titl1,expdat1,npatch1,tapeID1
2	   format(' There is already a scan.scn file named ',a40,/,
     & '   title: ',a70,/,
     & '   Date of experiment: ',a11,' (patch number ',i3,')',/,
     & '   Tape details: ',a24,/,
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
	   titles=titl1	!use existing title as default
	endif
c
	call TITENT0('Enter title for patch: ',titles,70,.false.)
c
c	ioffset=768		!for iver=103
c	iscanver=104		!current
c	ioffset=1024	!for iver=104

c
	OPEN(unit=15,file=SCANFIL,status='UNKNOWN',access='TRANSPARENT')
c
c If overwrite is chosen then must write the new data here, so that if
c scan was restarted using the same .scn file name as output, the old
c .scn is not read in at the beginning of fixscan
	if(iopt.ne.0) then
	 read(unit=15,rec=1) iscanver
	 if(iscanver.le.103) then	!conc not defined yet
	   do i=1,10
		conc(i)=0.0
	   enddo
	 endif
c Write exactly as in scantemp.dat (nint replaces kt, nsweep replaces ijump)
	 write(unit=18,rec=1) iscanver,ioffset,nint,titles,expdate,
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
     & nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     & njump,nnull,ktlast,
     & izoom,fcz,fczoom,ampz,avampsav,
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     & stpfac,nlig,conc
	endif
c
c Check header, and fix if necessary
c NB call with 'titles'
	call FIXSCAN(scanfil,expdate,npatch,titles,adcfil,tapeID,
     & Emem,temper,ffilt,avamp,rms,calfac2,qfile,ipatch,nlig,conc,
     & nint,.false.)
c
c New scantemp/scan.dat header

	iscanver=104
	ioffset=1024
	write(unit=15,rec=1) iscanver,ioffset,nint,titles,expdate,
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
     & nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     & njump,nnull,ktlast,
     & izoom,fcz,fczoom,ampz,avampsav,
     & itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     & stpfac,nlig,conc
c nlig, conc added in iscanver=104, and ioffset increased to 1024
c
	if(.not.cjump) then
	   write(unit=15,rec=ioffset)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
     &  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &  (ibaselin(i),i=1,nfits)
	else if(cjump) then
	   write(unit=15,rec=ioffset)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
     &  (jumps(i),i=1,nsweep),(kjumps(i),i=1,nsweep),
     &  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &  (ibaselin(i),i=1,nfits)
	endif
c
	CLOSE(unit=15)
c
c Print out what has been done
c Check header, and fix if necessary (not on disk)
c NB call with 'titles'
c	call FIXSCAN(scanfil,expdate,npatch,titles,adcfil,tapeID,
c     & Emem,temper,ffilt,avamp,rms,calfac2,qfile,ipatch,nlig,conc,
c     & nint,.false.)
	amp=avamp*calfac2
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
	if(.not.cjump) then
	   print 9
         if(discprt) write(8,9)
9	   format(' OUTPUT FROM SCAN OF STEADY STATE DATA ')
	else
	   print 91
         if(discprt) write(8,91)
91	   format(' OUTPUT FROM SCAN OF CJUMP DATA ')
	endif
	print 26,titles,scanfil,expdate,npatch,nint,tapeID,
     & ptype(ipatch),Emem,temper,ffilt,amp,rms,calfac2,ioffset,
     & qfile,nlig
	if(discprt)write(8,26) titles,scanfil,expdate,npatch,nint,tapeID,
     &  ptype(ipatch),Emem,temper,ffilt,amp,rms,calfac2,ioffset,
     & qfile,nlig
c
26	format(/,
     & ' Title: ',a70,/,
     & ' Data written to SCAN.SCN file named ',a40,/,
     & ' Date of experiment: ',a11,' (patch number ',i3,')',/,
     & ' Number of transitions = ',i8,/,
     & ' Tape details: ',a24,/,
     & ' Patch type: ',a14,/,
     & ' Membrane potential (mV) = ',f9.2,/,
     & ' Temperature = ',f9.1,/,
     & ' Filter (kHz, -3 dB) = ',f12.5,/,
     & ' Average full amplitude (pA) (approx) = ',f9.2,/,
     & ' RMS noise (pA) = ',f10.4,/,
     & ' Calibration factor (amplitude to pA)',g13.6,/,
     & ' Offset (bytes) from start of header to start of data = ',i8,/,
     & ' Plot queue file name: ',a40,/,
     & ' Number of ligands = ',i3)
	do i=1,nlig
	   print 281,i,conc(i)
	   if(discprt)write(8,281) i,conc(i)
281	   format(' Ligand #',i2,': concentration (micromolar) = ',f8.3)
	enddo
999	continue
	RETURN
	end


