	program RDJUMP
c     c=================================
c file: transparent
c first nextrec=2048
c istrec(1:500) integer*4

c ioffset=1024 (for header)
      character adcfil*30
c
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10),iadc
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,jpatch,ioffset
	integer*4 istrec(500)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
      character cdate*11,ctime*11,ndev*2
      character cdate0*11,ctime0*11
	character title1*79
	logical discprt,pon,slock,present,vjump,sampv,sampatch
	logical control
c	logical vramp,first
	logical fixgain,allocated,readini
	character*1 UC,ans
	real*8 tsample
c for CJUMP3 data
	integer*2 iver1
c=	integer*2 ikeep1(4,2),kstep(5),jmask(20480)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	logical keepall
c
10	print*,'adcfil='
	read*,adcfil
	njump=4
      OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	read(14,rec=1) njd,nxtrec,ioffset
	print*,ioffset
	if(ioffset.ne.1024) then
	   close(unit=14)
	   goto 10
	endif
      close(unit=14)
      OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &    access='TRANSPARENT')
	read(14,rec=1) njd,nxtrec,ioffset,(ISTREC(i),i=1,njd)
	irec=istrec(njump)
	read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,jpatch,ffilt
	print*,irec,ctime,cdate,vjump,iver1,nsweep,ioffset
	irec=irec+ioffset
	read(14,rec=irec) iadc
	print*,irec,iadc
	pause
         close(unit=14)
	end


