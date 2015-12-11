	program CONVFALL
c Version of CONVFILT to convert all filter files, and insert cutoff freqs
c To convert filter functions from PDP to DOS (like SCCONV)
c Size of filter array now increased to 100
c
	real*4 filt1(50),filt(100)
	CHARACTER*4 dtf1c,filt1c(50)
	integer*2 nfilt1c
	logical present
	character*1 ans
c	character*1 ans,UC
	character ftitle*80,ndev*2,INFIL*30,fname*7,filnam*12
	real*4 filtset(40,10)
	integer nfilt(40)
c
      OPEN(unit=8,file='h:\convfilt.prt',status='UNKNOWN',
     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	REWIND(unit=8)
c Define table of the filter frequencies available for each setup
c filtset(i,j)=jth available filter setting (-3dB) for setup #i
c nfilt(i)=total number of settings available for setup #i
	i=1		!setup #1
	nfilt(1)=2
	filtset(i,1)=0.5
	filtset(i,2)=1.0
	i=2		!setup #2
	nfilt(i)=3
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	i=3		!setup #3
	nfilt(i)=1
	filtset(i,1)=2.0
	i=4		!setup #
	nfilt(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	i=5		!setup #
	nfilt(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	filtset(i,4)=3.5
	filtset(i,5)=4.0
	i=6		!setup #
	nfilt(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	i=7		!setup #
	nfilt(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	i=8		!setup #
	nfilt(i)=3
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	i=9		!setup #
	nfilt(i)=7
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=3.5
	filtset(i,6)=4.0
	filtset(i,7)=4.5
	i=10		!setup #
	nfilt(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=2.5
	filtset(i,3)=3.0
	filtset(i,4)=3.5
	filtset(i,5)=4.0
	i=11		!setup #
	nfilt(i)=2
	filtset(i,1)=0.25
	filtset(i,2)=0.5
	i=12		!setup #
	nfilt(i)=5
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	filtset(i,4)=2.5
	filtset(i,5)=3.0
	i=13		!setup #
	nfilt(i)=5
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5
	filtset(i,4)=3.0
	filtset(i,5)=4.0
	i=14		!setup #
	nfilt(i)=4
	filtset(i,1)=1.0
	filtset(i,2)=1.5
	filtset(i,3)=2.0
	filtset(i,4)=2.5
	i=15		!setup #
	nfilt(i)=6
	filtset(i,1)=1.0
	filtset(i,2)=2.0
	filtset(i,3)=3.0
	filtset(i,4)=4.0
	filtset(i,5)=5.0
	filtset(i,6)=100.       !=unfiltered
	i=16		!setup #
	nfilt(i)=5
	filtset(i,1)=2.0
	filtset(i,2)=3.0
	filtset(i,3)=4.0
	filtset(i,4)=5.0
	filtset(i,5)=100.       !=unfiltered
	i=17		!setup #
	nfilt(i)=3
	filtset(i,1)=6.0
	filtset(i,2)=8.0
	filtset(i,3)=100.       !=unfiltered
	i=18		!setup #
	nfilt(i)=6
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5        !=unfiltered
	filtset(i,4)=3.0
	filtset(i,5)=3.5
	filtset(i,6)=4.0        !=unfiltered
	i=19		!setup #
	nfilt(i)=5
	filtset(i,1)=1.5
	filtset(i,2)=2.0
	filtset(i,3)=2.5        !=unfiltered
	filtset(i,4)=3.0
	filtset(i,5)=3.5
c end of 'old' filters in PDP version, now 'new'
	i=20		!setup #
	nfilt(i)=1
	filtset(i,1)=4.0
	i=21		!setup #
	nfilt(i)=2
	filtset(i,1)=2.0
	filtset(i,2)=4.0
	i=22		!setup #
	nfilt(i)=1
	filtset(i,1)=4.0
c
c Open all files here
c Assume that SC data disc in DEC format is in d:\kermit\
c
	print 2
2	format(
     &' Specify disc for PDP filter files (in \KERMIT\ directory):')
	ndev='D:'		!default
	call DISCNUM1(id,ndev,-1,1)	!ask for winchester or floppy
	print 1
1	format(' (Output goes to G:\fortran\scan)')
c1	format(' Specify disc where output (converted files) are to go')
c	call DISCNUM1(id,ndev1,-1,1)	!ask for winchester or floppy
c
c Start loop for all filters
	do 200 nsetup=1,22
	print 13,nsetup
	write(8,13) nsetup
13	format(' Setup number: ',i3)
c
	do 201 jfilt=1,nfilt(nsetup)
c
	goto (501,502,503,504,505,506,507,508,509,510,511,
     & 512,513,514,515,516,517,518,519,520,521,522) nsetup
501	if(jfilt.eq.1) filnam='FILT1 '
	if(jfilt.eq.2) filnam='FILT2 '
	goto 600
C
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
C
600	CONTINUE
	filtf=filtset(nsetup,jfilt)
c Now input PDP version
	n=NBLANK(filnam)
	infil=ndev//'\kermit\'//filnam(1:n)//'.DAT'
c Input:
	INQUIRE(file=infil,exist=present,flen=len)
	if(present.and.len.gt.0) then
	   print 10,infil
	   write(8,10) infil
10	   format(' Converting ',a30)
	else
	   print 11,infil
	   write(8,11) infil
11	   format(' File ',a30,' is not present')
	   goto 201
	endif
	OPEN(UNIT=10,file=infil,status='old',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	read(10,rec=1) FTITLE,NFILT1c,DTF1c,FILT1c
	CLOSE(unit=10)
	nfilt1=int4(nfilt1c)
	if(nfilt1.gt.50) then
	   print 581,nfilt1
581	   format(' NFILT1 TOO BIG= ',i8)
	   goto 201
	endif
c Convert real value, dtf1
	call CONVDEC4(dtf1c,dtf1,1)
c Convert filter array
	call CONVDEC4(filt1c,filt1,nfilt1)
c
c Output
c Copy filt1 into larger array
	do i=1,50
	  filt(i)=filt1(i)
	enddo
	do i=51,100
	  filt(i)=0.0
	enddo
c	infil=ndev1//filnam
	infil='g:\fortran\scan\'//filnam(1:n)//'.DAT'
	OPEN(UNIT=11,file=infil,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	write(11,rec=1) ftitle,nfilt1,dtf1,filt,filtf
	CLOSE(unit=11)
	print 12,infil,filtf
	write(8,12) infil,filtf
12	format(' Writing ',a30,'; filter freq (kHz) = ',f9.2,/)
c
201	continue
200	continue
	end
