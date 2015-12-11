	subroutine GETFILT(nfilt,dtf,filt,nsetup,filtf,filtfile,ftitle,
     & restart,fcdone)
C
c 03/02/99 11:01am Add parameter fcdone=false if we request here that fc
c is to be determined by fitting rising phase of observed openings
c 03/02/93 12:48pm 06:55am Lahey V5 version of GFILT1
c
c Modif 08/02/94 09:09pm by addition of restart parameter
c Modif 07/13/94 05:12pm so that nsetup=-1000 is a calculated Gaussian step
c response function with frequency specified by filtf (not from F*.DAT).
c
c Modif 09/22/93 09:21am so that if called with negative value of nsetup,
c  then asks for setup, but uses value as default
c To read a step response function file (must be in same directory as prog)
c Setup number = nsetup (formerly jfilt1)
c Filter frequency (-3dB) = filtf (real*4) (formerly jfilt)
c FILT(100) = step response function (formerly only 50 points)
c
c FILTFILE=name of the step response function file
c If called with nsetup=0 then asks for setup number and filter freq
c If called with valid value of nsetup and FILTFILE the reads the step
c response function without asking
c
c GFILT1 is same as GFILT but returns file name and title too (for CMPFLT)
c Modif Jan 1989 to list new files separately
c NOTE that new set-up numbers have 1000 added to them internally
c
c TO READ A FILTER FUNCTION FROM DISC AND OUTPUT:
c (FILTER FUNCION FILE CREATED BY FILTFL.FOR)
c	NFILT=NO OF ELEMENTS IN IT
c	DTF=TIME INC (MICROSEC) BETWEEN VALUES
c	FILT=ARRAY OF VALUES
c
	real*4 filt(100),filtset(40,10)
	integer nfreq(40)
	character*1 ans,UC
	character ftitle*80,filtfile*20,filnam*12
	logical pon,slock,discprt,present,restart,fcdone
	common/dp/discprt
c
c define function
	pon()=slock()
c
101	format(a1)
	nsmax=24		!number of setups defined
	ffilt=filtf		!input value=default for filter freq (kHz -3db)
	fcdone=.true.	!when filter determined here
c
c Define table of the filter frequencies available for each setup
c filtset(i,j)=jth available filter setting (-3dB) for setup #i
c nfreq(i)=total number of settings available for setup #i
	i=1		!setup #1
	nfreq(1)=2
	filtset(i,1)=0.5
	filtset(i,2)=1.0
	i=2		!setup #2
	nfreq(i)=3
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	i=3		!setup #3
	nfreq(i)=1
	filtset(i,1)=2.0
	i=4		!setup #
	nfreq(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	i=5		!setup #
	nfreq(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	filtset(i,4)=3.5
	filtset(i,5)=4.0
	i=6		!setup #
	nfreq(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	i=7		!setup #
	nfreq(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	i=8		!setup #
	nfreq(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	i=9		!setup #
	nfreq(i)=7
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=3.5
	filtset(i,6)=4.0
	filtset(i,7)=4.5
	i=10		!setup #
	nfreq(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	filtset(i,4)=3.5
	filtset(i,5)=4.0
	i=11		!setup #
	nfreq(i)=2
	filtset(i,1)=0.25
	filtset(i,2)=0.5
	i=12		!setup #
	nfreq(i)=5
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	filtset(i,4)=2.5
	filtset(i,5)=3.0
	i=13		!setup #
	nfreq(i)=5
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=4.0
	i=14		!setup #
	nfreq(i)=4
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	filtset(i,4)=2.5
	i=15		!setup #
	nfreq(i)=6
	filtset(i,1)=1.0
	filtset(i,2)=2.0
	filtset(i,3)=3.0
	filtset(i,4)=4.0
	filtset(i,5)=5.0
	filtset(i,6)=100.       !=unfiltered
	i=16		!setup #
	nfreq(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	filtset(i,4)=5.0
	filtset(i,5)=100.       !=unfiltered
	i=17		!setup #
	nfreq(i)=3
	filtset(i,1)=6.0
	filtset(i,2)=8.0
	filtset(i,3)=100.       !=unfiltered
	i=18		!setup #
	nfreq(i)=6
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=3.5
	filtset(i,6)=4.0
	i=19		!setup #
	nfreq(i)=5
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=3.5
c end of 'old' filters in PDP version, now 'new'
	i=20		!setup #
	nfreq(i)=1
	filtset(i,1)=2.0        !should be 2.0 (-3dB)?
	i=21		!setup #
	nfreq(i)=2
	filtset(i,1)=2.0
	filtset(i,2)=4.0
	i=22		!setup #
	nfreq(i)=1
	filtset(i,1)=4.0
	i=23		!setup #
	nfreq(i)=4
	filtset(i,1)=1.0
	filtset(i,2)=2.0
	filtset(i,3)=3.0
	filtset(i,4)=4.0
	i=24		!setup #
	nfreq(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
c
	if(restart) then
	   nsetup=iabs(nsetup)
	   if(nsetup.ge.1.and.nsetup.le.nsmax) then
		print 30,nsetup,1000.*filtf
30	     format(' Set-up number = ',i3,': filter cutoff (Hz,-3dB) = ',
     &	g13.6,/,' O.K. [Y] ' )
	   else if(nsetup.eq.1000) then
		ftitle='Calculated Gaussian step response function'
		print 31,1000.*filtf
31		format(
     &      ' Calculated Gaussian filter: fc = ',g13.6,
     &	'(Hz, -3dB): O.K. [Y] ')
	   endif
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') goto 59
c	   print 61,1000.*filtf,ftitle
c	   if(pon()) write(7,61) 1000.*filtf,ftitle
c        if(discprt) write(8,61) 1000.*filtf,ftitle
c       now go on to read straight away
	endif
c Read straight away if already defined
	if(nsetup.ge.1.and.nsetup.le.nsmax) then
	   OPEN(unit=15,file=filtfile,status='OLD',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	   read(15,rec=1) FTITLE,NFILT,DTF,FILT,filtf
	   CLOSE(unit=15)
	   print 3000,nsetup,1000.*filtf,ftitle
	   if(pon()) write(7,3000)nsetup,1000.*filtf,ftitle
         if(discprt) write(8,3000)nsetup,1000.*filtf,ftitle
3000	  format(' Set-up number = ',i5,':  filter cutoff (Hz,-3dB) = ',
     &	g13.6,/,' ',a80,/)
	   RETURN
	else if(iabs(nsetup).eq.1000) then
	   goto 56
	endif
c
59	continue
	if(nsetup.eq.0) then
	   print 50
50	   format(' Set-up number [HELP] = ')
	   call INPUTi(nsetup)
	else
	   iopt=2					!default
c	   iopt=1					!default
c	   if(nsetup.eq.-1000) iopt=2		!default
c	   nsetup=iabs(nsetup)
53	   print 51,nsetup,iopt
51	   format(
     & ' (1) Step response measured on set-up #',i3,/,
     & ' (2) Step response calculated for Gaussian filter',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.2) goto 53
	   if(iopt.eq.1) then
	      print 54,nsetup
54		format(' Set-up number (0=HELP) [',i4,'] = ')
		call INPUTi(nsetup)
	   else
		nsetup=-1000
		x=1000.*filtf		!Hz
		print 55,x
55		format(' Frequency (Hz, -3dB) [',f9.1,'] = ')
		call INPUTr(x)
		filtf=x/1000.		!kHz
		goto 56
	   endif
	endif
	if(nsetup.lt.0.or.nsetup.gt.nsmax) goto 59
	if(nsetup.eq.0) goto 13	!help notes (then return to 59)
c
141	print 14,nsetup
14	format(' Filter frequencies available for setup #',i3,':',/,
     & ' Option #     Filter (-3dB)       filter (2*-3bB)')
	jfilt=1	!in case default not found
	do j=1,nfreq(nsetup)
	  freq=filtset(nsetup,j)	!-3dB
	  freq2=2.*freq			!not -3dB
c==	  if(abs(freq2-ffilt).lt.0.01) jfilt=j	!default option (unless -3dB)
	  if(abs(freq-ffilt).lt.0.01) jfilt=j	!default option (ffilt=-3dB)
	  if(freq.lt.99.) then
	     print 15,j,freq,freq2
15	     format(2x,i3,9x,f9.2,9x,f9.2)
	  else
	     print 151,j
151	     format(2x,i3,9x,' unfiltered')
	  endif
	enddo
	print 16,jfilt
16	format(' Option number [',i3,'] = ')
	call INPUTi(jfilt)
	if(jfilt.lt.1.or.jfilt.gt.nfreq(nsetup)) goto 141
	filtf=filtset(nsetup,jfilt)
c
	goto (501,502,503,504,505,506,507,508,509,510,511,
     & 512,513,514,515,516,517,518,519,520,521,522,523,524) nsetup
501	if(jfilt.eq.1) filnam='FILT1 '
	if(jfilt.eq.2) filnam='FILT2 '
	goto 600
502	if(jfilt.eq.1) filnam='FILTS2 '
	if(jfilt.eq.2) filnam='FILTS3 '
	if(jfilt.eq.3) filnam='FILTS4 '
	goto 600
503	if(jfilt.eq.1) filnam='FILTB4 '
	goto 600
504	if(jfilt.eq.1) filnam='FILBS4 '
	if(jfilt.eq.2) filnam='FILBS6 '
	if(jfilt.eq.3) filnam='FILBS8 '
	goto 600
505	if(jfilt.eq.1) filnam='FILBF4 '
	if(jfilt.eq.2) filnam='FILBF5 '
	if(jfilt.eq.3) filnam='FILBF6 '
	if(jfilt.eq.4) filnam='FILBF7 '
	if(jfilt.eq.5) filnam='FILBF8 '
	goto 600
506	if(jfilt.eq.1) filnam='F1BS4 '
	if(jfilt.eq.2) filnam='F1BS5 '
	if(jfilt.eq.3) filnam='F1BS6 '
	goto 600
507	if(jfilt.eq.1) filnam='F2BS4 '
	if(jfilt.eq.2) filnam='F2BS5 '
	if(jfilt.eq.3) filnam='F2BS6 '
	goto 600
508	continue
	if(jfilt.eq.1) filnam='F1DC4 '
	if(jfilt.eq.2) filnam='F1DC6 '
	if(jfilt.eq.3) filnam='F1DC8 '
	goto 600
509	if(jfilt.eq.1) filnam='F283S3'
	if(jfilt.eq.2) filnam='F283S4'
	if(jfilt.eq.3) filnam='F283S5'
	if(jfilt.eq.4) filnam='F283S6'
	if(jfilt.eq.5) filnam='F283S7'
	if(jfilt.eq.6) filnam='F283S8'
	if(jfilt.eq.7) filnam='F283S9'
	goto 600
510	if(jfilt.eq.1) filnam='F584S4'
	if(jfilt.eq.2) filnam='F584S5'
	if(jfilt.eq.3) filnam='F584S6'
	if(jfilt.eq.4) filnam='F584S7'
	if(jfilt.eq.5) filnam='F584S8'
	goto 600
511	if(jfilt.eq.1)filnam='FI500 '
	if(jfilt.eq.2) filnam='FIDO1 '
	goto 600
512	if(jfilt.eq.1) filnam='F86AM2'
	if(jfilt.eq.2) filnam='F86AM3'
	if(jfilt.eq.3) filnam='F86AM4'
	if(jfilt.eq.4) filnam='F86AM5'
	if(jfilt.eq.5) filnam='F86AM6'
	goto 600
513	if(jfilt.eq.1) filnam='F86A23'
	if(jfilt.eq.2) filnam='F86A24'
	if(jfilt.eq.3) filnam='F86A25'
	if(jfilt.eq.4) filnam='F86A26'
	if(jfilt.eq.5) filnam='F86A28'
	goto 600
514	if(jfilt.eq.1) filnam='F86MU2'
	if(jfilt.eq.2) filnam='F86MU3'
	if(jfilt.eq.3) filnam='F86MU4'
	if(jfilt.eq.4) filnam='F86MU5'
	goto 600
515	if(jfilt.eq.1) filnam='F87H12'
	if(jfilt.eq.2) filnam='F87H14'
	if(jfilt.eq.3) filnam='F87H16'
	if(jfilt.eq.4) filnam='F87H18'
	if(jfilt.eq.5) filnam='F7H110'
	if(jfilt.eq.6) filnam='F87H1U'
	goto 600
516	if(jfilt.eq.1) filnam='F87H24'
	if(jfilt.eq.2) filnam='F87H26'
	if(jfilt.eq.3) filnam='F87H28'
	if(jfilt.eq.4) filnam='F7H210'
	if(jfilt.eq.5) filnam='F87H2U'
	goto 600
517	if(jfilt.eq.1) filnam='F7H312'
	if(jfilt.eq.2) filnam='F7H316'
	if(jfilt.eq.3) filnam='F87H3U'
	goto 600
518	if(jfilt.eq.1) filnam='FCH103'
	if(jfilt.eq.2) filnam='FCH104'
	if(jfilt.eq.3) filnam='FCH105'
	if(jfilt.eq.4) filnam='FCH106'
	if(jfilt.eq.5) filnam='FCH107'
	if(jfilt.eq.6) filnam='FCH108'
	goto 600
519	if(jfilt.eq.1) filnam='FCH203'
	if(jfilt.eq.2) filnam='FCH204'
	if(jfilt.eq.3) filnam='FCH205'
	if(jfilt.eq.4) filnam='FCH206'
	if(jfilt.eq.5) filnam='FCH207'
	goto 600
520	if(jfilt.eq.1) filnam='fna204'
	goto 600
521	if(jfilt.eq.1) filnam='fnd104'
	if(jfilt.eq.2) filnam='fnd108'
	goto 600
522	if(jfilt.eq.1) filnam='fnd1p8'
	goto 600
523	if(jfilt.eq.1) filnam='FDA312'
	if(jfilt.eq.2) filnam='FDA314'
	if(jfilt.eq.3) filnam='FDA316'
	if(jfilt.eq.4) filnam='FDA318'
	goto 600
524	if(jfilt.eq.1) filnam='FBLN12'
	if(jfilt.eq.2) filnam='FBLN13'
	if(jfilt.eq.3) filnam='FBLN14'
	goto 600
c
600	CONTINUE
	filtfile=filnam//'.DAT'
	INQUIRE(file=filtfile,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   print 212,filtfile
212	   format(' Step response file ',a20, 'not present')
	   call BELL(2)
	   goto 59
	endif
	OPEN(unit=15,file=filtfile,status='OLD',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(15,rec=1) FTITLE,NFILT,DTF,FILT,filtf
	CLOSE(unit=15)
	print 58,ftitle
58	FORMAT(1x,a80)
	call DCASK('  O.K. ','y',ans)
	if(ans.eq.'N') goto 59
c Print which filter used
	if(pon()) write(7,3000)nsetup,filtf,ftitle
      if(discprt) write(8,3000)nsetup,filtf,ftitle
	call flush(7)
	RETURN		!end of measured step-response function bit
c
c Section for calculated step-response functions
56	continue
	ftitle='Calculated Gaussian step response function'
c	NFILT=NO OF ELEMENTS IN IT
c	DTF=TIME INC (MICROSEC) BETWEEN VALUES
c	FILT=ARRAY OF VALUES
c NB -500*fc corresponds to filt()<0.0001 (see GAUFILT.MCD)
c -could start at -450*fc (filt=0.0034 approx) and still leave enough
c points before filt=0.001 for good interpolation in NORMFILT
	nfilt=100
	dtf=10.0/filtf		!microsec (filtf=fc in kHz)
	t0=-500./filtf		!ditto
	do i=1,100
	   t=(t0 + float(i)*dtf)*0.001
	   filt(i)=0.5*(1.0 + erfs(5.336446*filtf*t))
	enddo
	if(.not.restart) then
	   print 58,ftitle
c         print 60,1000.*filtf
c60	   format('& fc (-3 dB) = ',g13.6,' Hz:')
	   iopt=1
	   print 40,filtf*1000.,iopt
40	   format(
     &	' CHOOSE THE FILTER FUNCTION',/,
     &	' (1) Use nominal filter frequency (fc = ',g13.6,' Hz)',/,
     &	' (2) Calculate effect of pre-filter',/,
     &      ' (3) Measure effective fc by fitting openings',/,
     &	' (4) Specify a different filter here ',/,
     &	'  Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.eq.2) then
		print 41
41		format(' Cut-off frequency for pre-filter (kHz) = ')
		call INPUTr(fcpre)
		f2=1.0/(filtf)**2
		f2p=1.0/(fcpre**2)
		fc1=1.0/sqrt(f2+f2p)
		print 42,fc1
42		format(
     &	' Effective filter fc (kHz) = ',g13.6,': O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   filtf=fc1	!kHz
		   ffilt=filtf
		else
		   goto 59
		endif
	   else if(iopt.eq.3) then
	      fcdone=.false.
		RETURN
	   else if(iopt.eq.4) then
		goto 59
	   endif
	endif
c Print which filter used
	print 61,1000.*filtf,ftitle
	if(pon()) write(7,61) 1000.*filtf,ftitle
      if(discprt) write(8,61) 1000.*filtf,ftitle
61	format(' Filter cutoff (Hz,-3dB) = ',
     &	g13.6,/,' ',a80,/)
	call flush(7)
	RETURN		!end of calc Gaussian step response
c
c Help notes
13	continue
	print 122
122	FORMAT( ' 1=SAKMANN (1980) (1,2 KHZ)',/
     & ,' 2=SIGWORTH (1980) (2,3,4 KHZ) ',/
     & ,' 3=SAK/SIG (1981) (4 KHZ) ',/
     & ,' 4=SAKMANN (1982) TAPE=15 IPS.(4,6,8 KHZ) ',/
     & ,' 5=SAKMANN (1982) TAPE=30 IPS.(4,5,6,7,8 KHZ) ',/
     & ,' 6=SAKMANN (1982 3-12 AUG) TAPE=30 IPS.(4,5,6 KHZ) ',/
     & ,' 7=SAKMANN (1982 13-17 AUG) TAPE=30 IPS.(4,5,6 KHZ) ',/
     & ,' 8=OGDEN-COLQUHOUN (JAN 1983) TAPE=15 IPS.(4,6,8 KHZ) ',/
     & ,' 9=SAKMANN (FEB 1983). 30 IPS.(3,4,5,6,7,8,9 KHZ) ',/
     & ,' 10=SAKMANN (MAY 1984). 30 IPS.(4,5,6,7,8 KHZ) ',/
     & ,' 11=BARR & STROUD ALONE (0.5 (TYPE -2),1 KHZ) ',/
     & ,' 12=Dyett #1 (15ips:2,3,4,5,6 kHz) ',/
     & ,' 13=Dyett #1 (30ips:3,4,5,6,8 kHz) ',/
     & ,' 14=Usowicz (Plymouth PC) (15ips:2,3,4,5 kHz) ',/
     & ,' 15=Axopatch #1 (Howe) (15ips:0=unfilt,2,4,6,8,10 kHz) ',/
     & ,' 16=Axopatch #1 (Howe) (30ips:0=unfilt,4,6,8,10 kHz) ',/
     & ,' 17=Axopatch #1 (Howe) (60ips:0=unfilt,12,16 kHz)',/
     & ,' 18=Axopatch #1a (Herron) Prefilt at 10kHz on axopatch',
     & ' (15ips:3,4,5,6,7,8 kHz)',/
     & ,' 19=Axopatch #1a (Herron) Prefilt at 20kHz on axopatch',
     & ' (15ips:3,4,5,6,7 kHz)',/,
     & ' 20=Axopatch #2(10 kHz lim); B&S#1 4 kHz NORMAL;tape#1 15ips',/,
     & ' 21=Dyett #1. MEAN TRANSITIONS. Tape#1 30 ips B&S#1(4,8 kHz)',/,
     & ' 22=Dyett #1. Tape #1 30 ips B&S #1(8 kHz)',/,
     & ' 23=Axopatch #3 (Gibb) 15 ips (B&S 2,4,6,8 kHz)',/,
     & ' 24=List #1 (Edmonds). 15 ips (-3dB 2,3,4 kHz)')
	goto 59
c
	END



