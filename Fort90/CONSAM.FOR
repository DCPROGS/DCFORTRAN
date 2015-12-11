	program CONSAM
c
c Lahey V5.x version. 03/23/93 01:54pm
c	NB Unlike other CED programs, AD5, ADCDSK etc MUST be called from
c	DOS prompt -will not work under Windows at present.
c Modified 07/29/91 02:23pm for 1401-plus. This means only calling the latest
c version of AD5.EXE, which must be in same directory as CONSAM.  This version
c has header as specified in 'CED Hard Disk Suport Software' manual (earlier AD5
c was not exactly as in manual -had slight error) -see tests in TAD5.FOR
c 10/17/90 12:07pm: use AD.EXE which gives more reliable error messages then AD5.EXE
c===NO- header seems to be different for AD.EXE!
c
c General-purpose continuous sampling to disk (see TADCDSK.FOR), via call
c to the turbo pascal program, AD5.exe.
c
c For 2 channel data, this prog converts interleaved data (0 1 0 1 0 1...)
c to the form (0 0 0 ... 1 1 1...) and writes it back to CONSAM.DAT in this
c form. NO IT DOES NOT -BETTER TO DO IN PROG THAT READS DATA!
c
c NB MUST HAVE AD5.EXE IN SAME DIRECTORY AS THIS PROG!
C
c  Tests with TDISC.FOR and TCONSAM.FOR show that
c (1) 'DIRECT' file gives error if we try to read from a record number such
c	that the end of the record extends beyond actual length of file on
c	the disc, even if the read(14,rec=i1) (idata(i),i=n,m) has a value
c	of m that is within the length of the file (this is necessary to get at
c	the end of the idata() array if the file produced by AD5.EXE has length
c	that is not an integer multiple of the record length used to read it back
c	So reading back as 'DIRECT' does not work in general.
c (2) 'TRANSPARENT' file, although book says it has record length of 1 (byte)
c	can be read as above, and idata(n) will contain the 2 bytes from the
c	file viz bytes #i1 and #i1+1, and idata(n+1) the next 2 bytes etc
c    	SO USE TRANSPARENT METHOD
c Modif 12/28/98 06:37pm to add patch details here
c
c Size of original header=136
c	write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inc,
c				76    11    8      2    4    4   2
c     & id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,iver
c        2  2     3       4     4     4     4     4      2
c now include also
c	& expdate,defname,tapeID,ipatch,Emem,temp
c         11      6       24      4    4    4
c so header is now 136+53 =189 bytes, but title is now 70, not 76
c so actually 183 bytes
c
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2
	character cs*3,adctime*8,ctime*11
c	character header*28
      character*1 ans,UC
	character*11 cnbyte,cnchan,cnd1,cnd2,cctrig
	character*76 command
c==	character*76 title	!now 70, as in scan
	character dosdate*12,dostime*12
	character expdate*11,defname*6,cnum*11,suffix*3
	character*14 ptype(5)
	character tapeID*24
	character title*70
	integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
	integer*2 idt,inc,id1,id2,irev,iver
	integer*4 ioff,ilen
c	logical second		!for debug
c	character adcdate*8	!use cdate instead
	logical pon,debug,slock,caplock,present
	logical discprt
c	integer*2 idata(40960)		!for separation of two channel data
c	integer*2 idata1(2,20480)
c	equivalence (idata,idata1)
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	pon()=slock()
	debug()=caplock()
c
c      OPEN(unit=7,file='PRN',iostat=nerr)
      filnam='CONSAM.PRT'
	call WINPRINT		!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 102
	if(pon()) write(7,102)
	if(discprt) write(8,102)
102	format(' CONSAM- Continuous sampling to disk',/)
c
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
      OPEN(unit=17,file='PRIMES.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=7600)
c NB recl is in bytes (7600 bytes=1900 integer*4)
	read(17,rec=1) nprime
      close(unit=17)
c
c Now put ll calibration stuff before sampling, so suitable default
c file name can be defined.
c Get calibration
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
	ftape=1.0		!some defaults
	errfac=1.0
	gain=10.0
	amVpA1=50.		!mV/pA on patch clamp
      print 215
      if(pon()) write(7,215)
      if(discprt) write(8,215)
215	format(' Calibration for ADC0')
	call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & itape,ftape,gain,errfac)
c calfac*ADC units gives pA; need keep only this value
c Get filter and tape speed correction
	print 210
210	FORMAT('&Filter setting (-3dB, Hz)= ')
	call INPUTr(filt)
c	read 202,filt
202	format(g13.6)
	if(nchan.eq.2) then
	   print 213
213	   format('&Same calibration factor for ADC1 [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		calfac1=calfac
	   else
      	print 214
      	if(pon()) write(7,214)
      	if(discprt) write(8,214)
214		format(' Calibration for ADC1')
		call GETCAL(calfac1,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & 	itape,ftape,gain,errfac)
	   endif
	   print 2101
2101	   format('&Filter setting (-3db, Hz) for ADC1= ')
	   call INPUTr(filt1)
c	   read 202,filt1
	endif
	if(itape.eq.1) then		!taped
	   print 71
71	   FORMAT('&Tape speed factor (integer, 1 or greater) = ')
	   call INPUTi(ifac)
	   tapfac=FLOAT(ifac)
	   srate=tapfac*srate	!effective sampling rate
	   if(ifac.ne.1) then
		print 32
32		FORMAT('&Filtered after taping ? ')	!if N filtering not affected
	      ans='N'
	      call INPUTa(ans)
		if(UC(ans).eq.'Y') then
		   filt=tapfac*filt
		   filt1=tapfac*filt1
		endif
		if(nchan.eq.1) then
         	   print 211,ifac,srate,filt
      	   if(pon()) write(7,211) ifac,srate,filt
      	   if(discprt) write(8,211) ifac,srate,filt
211		   format(' Tape slowed ',i3,' times, so',/,
     &	 ' Effective sampling rate (Hz)  = ',f13.6,/,
     &	 ' Effective filter setting (-3db, Hz) = ',f13.6)
		else
         	   print 212,ifac,srate,filt,filt1
      	   if(pon()) write(7,212) ifac,srate,filt,filt1
      	   if(discprt) write(8,212) ifac,srate,filt,filt1
212		   format(' Tape slowed ',i3,' times, so',/,
     &	 ' Effective sampling rate (Hz)  = ',f13.6,/,
     &	 ' Effective filter setting for ADC0 (-3db, Hz) = ',f13.6,/,
     &	 ' Effective filter setting for ADC1 (-3db, Hz) = ',f13.6)
		endif
	   endif
	endif
c
29	continue
c Now get experiment/patch details
	call GETEXPT(expdate,defname,tapeID,ipatch,npatch,Emem,temp)
	call TITENT0('Enter title for patch: ',title,70,.true.)
c	call TITENT('Enter temporary title for sample:',TITLE,76)
	if(debug()) pause
c
c Now suggest default consam file name
c At present the OPEN statement won't accept long file names, so patch
c number must be omitted from the file name -us YYMMDDcs.dat for consam
	if(ndev.eq.'  ') ndev='D:'		!default winchester
	call DISCNUM1(id,ndev,0,0)	!ask for winchester (only)- no default
c	ADCFIL=ndev//'\CONSAM.DAT'
c	suffix='dat'
	suffix='ssd'
	if(npatch.ge.0.and.npatch.le.9) then
	   call INTCONV(npatch,cnum)
c=	   suffix='ss'//cnum(1:1)
	   adcfil=ndev//'\'//defname//'C'//cnum(1:1)//'.'//suffix(1:3)
	else
	   adcfil=ndev//'\'//defname//'CS'//'.'//suffix(1:3)
	endif
	call TITENT0('Name for CONSAM.SSD file',adcfil,30,.false.)
c
	print 4
4	format(
     & ' (1) Sample one channel (ADC0) only',/,
     & ' (2) Sample two channels (ADC0 and ADC1)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.ne.2) then
	   nchan=1
	else
	   nchan=2
	endif
	if(nchan.eq.1) cnchan='0'		!ADC 0 only
	if(nchan.eq.2) cnchan='01'		!ADC 0 and 1
c Get sample rate
282	continue
	print 31
31	format('&Sampling rate (integer Hz) = ')
	call INPUTi(irate)
	irate1=irate*nchan		!actual ADC rate
	rate=float(irate1)
	nclock=4000000	!Hz for 'H' parameter
	clock=float(nclock)
	div=clock/rate
	ndiv=IFIXR(div)
c Now define ndiv1,ndiv2 (both in range 2-32767) such that ndiv=ndiv1*ndiv2
c Start with ndiv1=2, and increase it if necessary.
	errmin=1.e35
	do i=1,1900
	   np=nprime(i)	!1st 1900 primes (up to 16381) (inc the 1st=2)
	   ndiv1=np
	   ndiv2=ndiv/np
	   if(ndiv2.eq.1) goto 22	!must be=>2, so use vals with smallest error so far
	   ndtry=ndiv1*ndiv2
	   if(debug()) then
		error=float(ndtry)/float(ndiv)
		if(error.lt.1.0) error=1.0/error
		error=error-1.0
		print 24,i,ndiv,ndtry,ndiv1,ndiv2,error
24		format(1x,i5,' n,n1*n2,n1,n2,err = ',4i8,3x,f10.6)
		if(ndtry.eq.ndiv) goto 23		!exact solution found
	   else
		if(ndtry.eq.ndiv) goto 23		!exact solution found
c	   If no exact factors found then look for least error
		error=float(ndtry)/float(ndiv)
		if(error.lt.1.0) error=1.0/error
		error=error-1.0
	   endif
	   if(error.lt.errmin) then
		errmin=error
		imin=i		!record index for least error
	   endif
	enddo
c
22	continue
	ndiv1=nprime(imin)		!most precise factors found
	ndiv2=ndiv/ndiv1
23	continue	!ndiv1,ndiv2 now defined as precisely as possible
c
	d1=float(ndiv1*ndiv2)		!actual divisor
	srate1=clock/d1    		!actual sample rate
	srate=srate1/float(nchan)	!return 1-channel sample rate
	if(nchan.eq.1) then
	   print 6,irate,clock/float(ndiv),ndiv,ndiv1*ndiv2,
     &   ndiv1,ndiv2,srate
6	   format(
     & ' Requested sample rate (Hz) = ',i8,/,
     & ' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,
     & ' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,
     & ' Actual sample rate (Hz) = ',g15.8)
	else
	   print 61,irate,nchan,irate1,clock/float(ndiv),ndiv,
     &   ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
61	   format(
     & ' Requested sample rate (Hz) = ',i8,/,
     & ' Sample rate needed for ',i2,' channels(Hz) = ',i8,/,
     & ' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,
     & ' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,
     & ' Actual total sample rate (Hz) = ',g15.8,/,
     & ' Actual sample rate per channel (Hz) = ',g15.8)
	endif
	print 281
281	format('& O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') goto 282
c Convert final ndiv1,ndiv2 to character form
	call INTCONV(ndiv1,cnd1)
	call INTCONV(ndiv2,cnd2)
c
42	print 2
2	FORMAT('&Duration of sample (seconds) = ')
	call INPUTr(sec1)
c	read 202,sec1
	nint=ifixr(sec1*srate)
	secs=float(nint)/srate	!actual length
	nbyte=2*nint
	nbyte=nbyte*nchan		!total number to be taken
	print 41,nint,nbyte+512,secs		!AD5 adds 512 byte header
41	format('&Length of data = ',i9,' values (per channel)',/,
     & '  (total number of bytes needed on disk = ',i9,')',/,
     & ' Duration (seconds) = ',g13.6,')',/,
     & ' O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') goto 42
	call INTCONV(nbyte,cnbyte)
c
c Take sample via the Pascal command line program AD5.EXE; make
c command string next, eg AD5 MYDATA.DAT 1000000 0123 CT 10 10
	print 27
27	format(
     & ' (1) Start sample by hitting <ENTER> now',/,
     & ' (2) Start sample with trigger (on Ev 4)',/,
     & ' Enter option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   cctrig='H'		!4 Mhz clock, no trigger
c	   call ANYKEY
	else
	   cctrig='HT'		!4 Mhz clock, wait for trigger
	endif
c
	command='AD5 '//charnb(adcfil)//' '//charnb(cnbyte)//
     & ' '//charnb(cnchan)//' '//charnb(cctrig)//' '//charnb(cnd1)//
     & ' '//charnb(cnd2)
cc try AD.EXE
c	command='AD  '//charnb(adcfil)//' '//charnb(cnbyte)//
c     & ' '//charnb(cnchan)//' '//charnb(cctrig)//' '//charnb(cnd1)//
c     & ' '//charnb(cnd2)
	if(debug()) then
	   print 26,command
	   if(discprt) write(8,26) command
26	   format(1x,a76)
	   pause
	endif
c
	print 72,secs
72	format(/' SAMPLING NOW',/,' Wait for ',f8.1,
     &' seconds (you can''t stop it so get some coffee')
c
c Take the sample
	call SYSTEM(command)
c
c Sample taken
c
c If this point reached without crashing, print out values used (and
c write some details to CONDAT.DAT file? Or can a title for the sample
c be put into the header (by reading it back here, and rewriting it with
c a title added?)
c Write title to header, also srate=actual sample rate (may differ
c from clock/(id1*id2) if tape slowed); filt,filt1=filter setting (Hz) for
c ADC0,1 corrected for tape speed if nec.
      OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
c      OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=512)
c Try recl=512 because this is length (bytes) of header, so header can be read
c separately.  Thus have 256 integer*2 ADC values per record.
c now get nbyte from header
c	nint=nbyte/2	!number of int*2 sampled
c	nrec=nbyte/512	!number of 512-byte records sampled
c In new version revision number, DOS date and time occupy the first 2+12+12
c bytes = bytes 1 to 26; want to skip these.
c [OLD VERSION: Find by trial and error that the integers (some *2, some *4)
c that follow the date and time strings start at byte #29 in the header (not #26
c as stated in the book- seem to have some extra bytes in the strings)]
c	if(OLD) then
c	  read(14,rec=1) header(1:28),idt,ioff,ilen,inc,id1,id2,cs(1:3)	!old
c	  adctime=header(17:24)			!old
c	  write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inc,
c     &  id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1
c	else
	read(14,rec=1) irev,dosdate,dostime,idt,ioff,ilen,inc,
     & id1,id2,cs(1:2)
	adctime=DOStime(1:8)
c write back in slightly different form, including title etc, and use CDATE
c which is nicer format then in orig header (adcdate)
c Write also the integer*2 IVER to indicate that this is new version
c though should not really be necessary since changes are here rather than
c in prog that reads CONSAM.DAT (write cs(1:3) here for compatibility, though
c clock spec, eg 'H ' or 'HT' are in cs(1:2) only)
c	iver=1001
c	write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inc,
c     & id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,iver
	iver=1002	!new format
	write(14,rec=1) iver,title,cdate,adctime,idt,ioff,ilen,inc,
     & id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,
     & expdate,defname,tapeID,ipatch,npatch,Emem,temp
      CLOSE(unit=14)
c
c If NCHAN=2 then read in all data, separate the two channels, and write data
c back.
c	if(nchan.eq.1) goto 370
c For 2 channels have
c	ntot=nint*2		!total # of data values
c Size of idata(.) is
c	nmax=40960
c Problem: If ntot>nmax need to read data back in chunks, but can separate only
c one chunk at a time, 20480 of chan 0 followed by 20480 of chan1; latter should
c be written back to last part of entire data file, but last part has not yet
c been read, so it would get overwritten (but OK if it is read back in SAME size
c chunks (eg in NOISED), which will then be already separated. Could solve this
c by writing the 1st and 2nd half of the chunk to separate disc files (one for
c chan 0, another for chan 1) (which, if either, should have header?).
c Alternatively might be easier to do separation in NOISED!
c370	continue
c
c print final values used
	if(nchan.eq.1) then
	   print 6,irate,clock/float(ndiv),ndiv,
     &    ndiv1*ndiv2,ndiv1,ndiv2,srate
	   if(pon()) write(7,6) irate,clock/float(ndiv),ndiv,
     &    ndiv1*ndiv2,ndiv1,ndiv2,srate
	   if(discprt) write(8,6) irate,clock/float(ndiv),ndiv,
     &    ndiv1*ndiv2,ndiv1,ndiv2,srate
         print 12,filt,nint,secs
         if(pon()) write(7,12) filt,nint,secs
         if(discprt) write(8,12) filt,nint,secs
12	   format(' Filter (-3db, Hz) for ADC0 = ',f10.1,/,
     & ' Sample of ',i9,' points on ADC0',/,
     & ' Sample duration (sec) = ',f12.2)
	else if(nchan.eq.2) then
	   print 61,irate,nchan,irate1,clock/float(ndiv),ndiv,
     &   ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
         if(pon()) write(7,61) irate,nchan,irate1,clock/float(ndiv),
     &   ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
         if(discprt) write(8,61) irate,nchan,irate1,clock/float(ndiv),
     &   ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
         print 121,filt,filt1,nint,secs
         if(pon()) write(7,121) filt,filt1,nint,secs
         if(discprt) write(8,121) filt,filt1,nint,secs
121	   format(
     & ' Filter (-3db, Hz) for ADC0 = ',f10.1,/,': for ADC1 = ',f10.1,/,
     & ' Samples of ',i9,' points on ADC0 and ADC1',/,
     & ' Sample duration (sec) = ',f12.2,/,' Title:')
	endif
	print *,title
      if(pon()) write(7,*) title
      if(discprt) write(8,*) title
c
c and print details of patch
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
	print 28,adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temp
      if(discprt) write(8,28)
     & adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temp

28	format(/,
     & ' CONSAM file name: ',a30,/,
     & ' Date of experiment: ',a11,' (patch number ',i3,')'/,
     & ' Tape details: ',a24,/,
     & ' Patch type: ',a14,/,
     & ' Membrane potential (mV) = ',f9.2,/,
     & ' Temperature = ',f9.1)

c For debugging, read back the file just sampled (see TCONSAM)
c
999	continue
	call ENDPRINT()
	end

