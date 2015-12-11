	program CONVFILT
c To convert filter functions from PDP to DOS (like SCCONV)
c Size of filter array now increased to 100
c
	real*4 filt1(50),filt(100)
	CHARACTER*4 dtf1c,filt1c(50)
	integer*2 nfilt1c
	logical blank
	character*1 ans
c	character*1 ans,UC
	character ftitle*80,ndev*2,INFIL*30,fname*7,filnam*12
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
	blank=.true.		!first time
c
10	continue		!return here for next one
	call TITENT0('Filter file name (exc .DAT):',fname,7,blank)
	filnam=fname//'.DAT'
	infil=ndev//'\kermit\'//filnam
	print 12
12	format(' Filter freq (-3dB) Hz = ')
	call INPUTr(filtf)
c Input:
	OPEN(UNIT=10,file=infil,status='old',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	read(10,rec=1) FTITLE,NFILT1c,DTF1c,FILT1c
	CLOSE(unit=10)
	nfilt1=int4(nfilt1c)
	if(nfilt1.gt.50) print 581,nfilt1
581	format(' NFILT1 TOO BIG= ',i8)
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
	infil='g:\fortran\scan\'//filnam
	OPEN(UNIT=11,file=infil,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	write(11,rec=1) ftitle,nfilt1,dtf1,filt,filtf
	CLOSE(unit=11)
c
	call DCASK('Convert another file','n',ans)
	if(ans.eq.'Y') then
	   blank=.false.
	   goto 10
	endif
	end
