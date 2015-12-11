========to be converted
========see notes in RELWRT

	subroutine READAT(nj1,nj2,ncon,nav1,nav2,YVAL,idest)
c To read back 1 (or 2 if nj2 not zero) relaxations from relaxation
c disc in CJFIT for refitting/display
c 07/15/91 04:48pm Yval made (2048,15) for compat with CJVDU (not needed here)
c  and dimensions of AVCUR,AVOLT changed
c Data read back directly into Yval (as for reading from CJUMP.DAT in READREL)
	real*4 YVAL(2048,15)
	real*4 avcur(20480),avolt(10240)	!needed only for COMMON
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)
      character cdate*11,cdate1*11,ndev*2,datfil*15,dirfil*15
      character name*12,ctext*17
	character title*66,title1*66
	logical discprt,pon,slock,vjump,sampv,control,control1,keepall
      character*1 ans,UC
=======avcur, avolt removed from common
	COMMON/cjpar/nsweep,
     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     & irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     & t1v,t2v,t1c,t2c,nDv,iDd,keepall,ivhold

	common/dp/discprt
c
	pon()=slock()
c
101   format(a1)
	idest=0
421	continue
	ndev='A:'		!default = floppy
	call DISCNUM(id,ndev,-1,1)	!which disc?
	if(id.eq.-1) goto 999		!if escape hit in DISCNUM
	DIRFIL=ndev//'\RELDIR.DAT'
	DATFIL=ndev//'\RELDAT.DAT'
	if(ndev.eq.'A:') pause ' Now mount data floppy:'
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=128)
	read(19,rec=1)Nrel,nxtrec,Jdisc,jfirst,name,whole
	print 42,name,jdisc,jfirst,nrel
42	   format(1x,a12,' Relaxation data disc number ',i2,/,
     & '  -contains events ',i4,' to ',i4,':  O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).eq.'N') then
	   CLOSE(unit=19)
	   goto 421
	endif
	print 37
37	format(
     & ' To fit a single record give its number; to fit difference',/,
     & ' between two sweeps (drug and control) specify both numbers',/,
     & ' Read events:  n1 (,n2)  =  ')
	input 63,nj1,nj2
63	format(2i8)
c
c Read in directory record for expt #nj1 first
	irec=nj1-jfirst+2
	read(19,rec=irec) iprog
	if(iprog.ne.1) then
	   call BELL(1)
	   print 38
38	   format(' Data not from CJFIT')
	   CLOSE(unit=19)
	   goto 421
	endif
	read(19,rec=irec) iprog,cdate,title,istrec,nrec,
     &    control,ncjump,nvjump,nsweep	!116 bytes (recl=128)
	lstrec=istrec+nrec-1
	ctext=': ''Agonist'' run'
	if(control) ctext=': ''Control'' run'
	print 25,nj1,cdate,ctext,title,nsweep,ncjump,nvjump,
     &	nrec,istrec,lstrec
      if(pon()) write(7,25) nj1,cdate,ctext,title,
     &	nsweep,ncjump,nvjump,nrec,istrec,lstrec
      if(discprt) write(8,25) nj1,cdate,ctext,title,
     &	nsweep,ncjump,nvjump,nrec,istrec,lstrec
25	format(' Expt ',i4,2x,a11,'  Data from CJFIT',a17,/,
     & 1x,a66,/,
     & ' Sweep # ',i5,'; ',i3,' C-jumps and ',i3, ' V-jumps/ramps',/,
     & ' (',i4,' records, from ',i8,' to ',i8,')',/,' O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).eq.'N') then
	   CLOSE(unit=19)
	   goto 421
	endif
c
c Read in directory record for expt #nj2 if req.
	if(nj2.gt.0) then
	   irec=nj2-jfirst+2
	   read(19,rec=irec) iprog
	   if(iprog.ne.1) then
		call BELL(1)
		print 38
	      CLOSE(unit=19)
	      goto 421
	   endif
	   read(19,rec=irec) iprog,cdate1,title1,istrec1,nrec1,
     &    control1,ncjump1,nvjump1,nsweep1	!116 bytes (recl=128)
	   lstrec=istrec1+nrec1-1
	   ctext=': ''Agonist'' run'
	   if(control) ctext=': ''Control'' run'
	   print 25,nj2,cdate1,ctext,title1,
     &	nsweep1,ncjump1,nvjump1,nrec1,istrec1,lstrec
         if(pon()) write(7,25) nj2,cdate1,ctext,title1,
     &	nsweep1,ncjump1,nvjump1,nrec1,istrec1,lstrec
         if(discprt) write(8,25) nj2,cdate1,ctext,title1,
     &	nsweep1,ncjump1,nvjump1,nrec1,istrec1,lstrec
	   input 101,ans
	   if(UC(ans).eq.'N') then
		CLOSE(unit=19)
		goto 421
	   endif
	endif
c
c Read in data records for #nj1
	jc=1		!current in yval(i,1)
	jv=2		!voltage in yval(i,2)
	if(nj2.gt.0) jv=3		!voltage in yval(i,3)
c
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	krs=istrec		!for expt #nj1
	read(20,rec=krs) naver,navc,nsamp,itsamp,calfac,calvolt,
     & vjump,sampv,nAv,dnAv,iTPREv,ivhold,irate,iTPREc,nAc,dnAc,
     & ilen2c,igap2c,ilen2v,igap2v,ivolt1,ivolt2,
     & t1v,t2v,t1c,t2c,nDv,iDd
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &  ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	krs=krs+1
	nrec0=1 + (nsamp-1)/128            ! For CJFIT data -AVCUR
c	  Now read data array(s)
	n=1
	do 501 j=1,nrec0
	m=n+127			!128 REALS=256 WORD=1 RECORD
c	read(20,rec=krs) (AVCUR(i),i=n,m)
	read(20,rec=krs) (YVAL(i,jc),i=n,m)
	krs=krs+1
	n=n+128
501	continue
	if(sampv) then
	   n=1
	   do 521 j=1,nrec0
		m=n+127			!128 REALS=256 WORD=1 RECORD
c		read(20,rec=krs) (AVOLT(i),i=n,m)
		read(20,rec=krs) (YVAL(i,jv),i=n,m)
		krs=krs+1
		n=n+128
521	  continue
	endif
c
c Now read data records for 2nd expt (if nj2 > 0)
	if(nj2.gt.0) then
	   jc=2		!current in yval(i,2)
	   jv=4		!voltage in yval(i,4)
c
	   krs=istrec1		!for expt #nj2
	   read(20,rec=krs) naver1,navc1,nsamp1,itsamp1,calfac,calvolt,
     &   vjump,sampv,nAv,dnAv,iTPREv,ivhold,irate1,iTPREc,nAc,dnAc,
     &   ilen2c,igap2c,ilen2v,igap2v,ivolt1,ivolt2,
     &   t1v,t2v,t1c,t2c,nDv,iDd
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &    ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	   krs=krs+1
	   if(itsamp.ne.itsamp1.or.irate.ne.irate1.or.nsamp.ne.nsamp1)then
		call BELL(3)
		print 20
20		format(' Parameters for the 2 data sets do not match')
		CLOSE(unit=19)
		CLOSE(unit=20)
		goto 421
	   endif
	   nrec0=1 + (nsamp-1)/128            ! For CJFIT data -AVCUR
c	  Now read data array(s)
	   n=1
	   do 601 j=1,nrec0
	   m=n+127			!128 REALS=256 WORD=1 RECORD
c	   read(20,rec=krs) (AVCUR(i),i=n,m)
	   read(20,rec=krs) (YVAL(i,jc),i=n,m)
	   krs=krs+1
	   n=n+128
601	   continue
	   if(sampv) then
		n=1
		do 602 j=1,nrec0
		m=n+127			!128 REALS=256 WORD=1 RECORD
c		read(20,rec=krs) (AVOLT(i),i=n,m)
		read(20,rec=krs) (YVAL(i,jv),i=n,m)
		krs=krs+1
		n=n+128
602	  	continue
	   endif
	endif		!end of read for #nj2
	CLOSE(unit=19)
	CLOSE(unit=20)
c
c Get number of traces averaged
	nav1=naver
	if(control) nav1=navc
	if(nj2.gt.0) then
	   nav2=naver1
	   if(control1) nav2=navc1
	endif
c
c Check which is control, as in main prog
	   if(control.and.(.not.control1)) then
		ncon=1		!#nj1 is control
	   else if(control1.and.(.not.control)) then
		ncon=2		!#nj2 is control
	   endif
	   if(control.and.control1) then
		call BELL(2)
		print 130
130		format(' Both records are controls O.K. [N] ? ')
		input 101,ans
		if(UC(ans).ne.'Y') goto 421	!read again
		ncon=1		!#nj1 is control
		print 132
132		format('&For net trace subtract #1 from #2 [Y] ? ')
		if(UC(ans).eq.'N') ncon=2
	   else if((.not.control).and.(.not.control1)) then
		call BELL(2)
		print 131
131		format(' Neither record is a control O.K. [N] ?')
		input 101,ans
		if(UC(ans).ne.'Y') goto 421	!read again
		ncon=1		!#nj1 is control
		print 132
		if(UC(ans).eq.'N') ncon=2
	   endif
c
	RETURN
999	continue
	idest=999
	RETURN
	end
