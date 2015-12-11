	subroutine READJUMP(njump,nsamp,ioff,ndisc,adcfil,calfac,
     & srate,ffilt,ktjump,nfjump,njdim,tzero,intzero,
     & tsample,readini,istrec,jumplen,njfirst,ntotjump,
     & newfile,ioffset,ipatch,Emem,idest,imode)
c
c To read jump data from CJUMP.DAT into SCAN to fit single channels
c after a jump (results written to SCANDAT.DAT in WRITJUMP).
c
c Modif 01/28/95 09:29am so
c (1) imode=0 for normal function when call from INSCAN -returns njump etc
c     imode=1 to read params for jump #njump when called from READSCAN
c (2) returns length of (first) cjump (jumplen), for logo
c
c Will want to read in one sweep at a time, for a series of replicate
c sweeps, and keep results for all the replicate sweeps in one
c file in SCANDAT so that can get distribution of, for example,
c 1st latency by taking one value from each sweep.
c   This routine is for start of new analysis (called from INSCAN) -for
c auto-restart, see changes to READSCAN.
c
      character adcfil*30,defname*6
c
	ALLOCATABLE::jmask
	integer*1 jmask(:)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,jpatch
	integer*4 istrec(1000)		!new format
c=	integer*2 istrec(250)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
	allocatable istrec2		!now temporary
	integer*2 istrec2(:)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
      character cdate*11,ctime*11,ndev*2
      character cdate0*11,ctime0*11
	character title1*79
	logical discprt,pon,slock,present,vjump,sampv,sampatch
	logical control,newfile
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
c	common/miscdat/ipatch,Emem	!for DSKWRIT/READJUMP/AUTWRIT/READSCAN
	common/dp/discprt
c
	pon()=slock()
c
c	njlast=-1
	sampatch=.false.
c
101	format(a1)
c
	ALLOCATE(istrec2(250))
	if(imode.eq.1) then
         OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
c==  read(14,rec=1) njd,nxtrec,(ISTREC(i),i=1,250)
	    read(14,rec=1) njd
	    if(njd.lt.1) then
		newfile=.true.
		CLOSE(unit=14)
c          Re-open in correct format
	      OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &	 access='TRANSPARENT')
		read(14,rec=1) jver,njd,nxtrec,ioffset,(ISTREC(i),i=1,njd)
	    else
		newfile=.false.
	      read(14,rec=1) njd,nxtrec,(ISTREC2(i),i=1,250)
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
		do i=1,njd
		   irec=int4(istrec2(i))
		   if(irec.le.-1.and.irec.ge.-32768) then
			irec=irec+65536
		   else if(irec.lt.-32768) then
			print*,' IREC IS TOO BIG!!'
			STOP
		   endif
		   istrec(i)=irec
		enddo
	    endif
c Now defined njd, nxtrec, integer*4 istrec(1000), and ioffset
	   goto 10		!read params for jump #njump
	endif
c
c Get name of CJUMP.DAT file (as in CJFIT) (but call file name ADCFIL, as
c for consam data, rather than datfil)
25	continue
	if(.not.readini) then
	 if(ndisc.gt.1) then
	   print 251
251	   format(' Specify disk partition for CJUMP data file:')
	 endif
	 call DISCNUM1(idisc,ndev,-1,1)
	 if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	 adcfil=ndev//'\CJUMP.DAT'
	endif
	call TITENT0('Give name/path for CJUMP data file:',adcfil
     & ,30,.false.)
	if(adcfil(2:2).eq.':') ndev=adcfil(1:1)
	n=NBLANK(adcfil)
c Look for extension on file name. If '.' present assume its given, otherwise
c add extension .DAT
	do i=1,n
	   if(adcfil(i:i).eq.'.') goto 255	!extension given
	enddo
	adcfil=adcfil(1:n)//'.dat'			!add extension
	n=n+4
255	continue
	if(ndev.eq.'A:') pause ' Now mount data floppy:'
	INQUIRE(file=adcfil,exist=present,flen=len)
	if(.not.present) then
	   call BELL(3)
	   print 24,adcfil
24	   format(' Cannot find ',a30)
	   goto 25
	else
         OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	    read(14,rec=1) njd
	    if(njd.lt.1) then
		newfile=.true.
		CLOSE(unit=14)
c          Re-open in correct format
	      OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &	 access='TRANSPARENT')
		read(14,rec=1) jver,njd,nxtrec,ioffset,(ISTREC(i),i=1,njd)
	    else
		newfile=.false.
	      read(14,rec=1) njd,nxtrec,(ISTREC2(i),i=1,250)
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
		do i=1,njd
		   irec=int4(istrec2(i))
		   if(irec.le.-1.and.irec.ge.-32768) then
			irec=irec+65536
		   else if(irec.lt.-32768) then
			print*,' IREC IS TOO BIG!!'
			STOP
		   endif
		   istrec(i)=irec
		enddo
	    endif
c Now defined njd, nxtrec, integer*4 istrec(1000), and ioffset
	    irec=istrec(njd)
	    read(14,rec=irec) cdate0,ctime0
	    if(pon()) write(7,411) cdate0
	    if(discprt) write(8,411) cdate0
411       format(' Date of experiment: ',a11)
	endif
c
67	continue
	ntotjump=njd
	print 62,njd,adcfil,ctime0(1:8),cdate0
62	format(' ',i4,
     & ' sweeps on disc in ',a30,'. Last sweep at ',a8,' on ',a11,/,
     & ' [1] No (more) listing',/,
     & ' (2) List brief details only (scroll lock on to print)',/,
     & ' (3) List full details (scroll lock on to print)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	iopt=iopt-1
	if(iopt.eq.0) goto 42
	iprt=1
	idprt=1
	if(iopt.eq.2) then
	   iprt=2
	   idprt=2
	endif
	if(discprt) write(8,622) njd,adcfil
622	format(' ',i4,' sweeps on disc in ',a30)
	print 60
60	format(' List for sweeps n1 to n2:  n1 (,n2) = ')
	call INPUT2i(n1,n2)
	if(n1.eq.0) goto 67
	if(n2.eq.0) n2=n1
50	continue
c NB need preliminary read here to get version number, so we know
c whether to read ikeep1 as integer*2 (=ikeep12) or integer*4(=ikeep1)
	irec=istrec(n1)
	imode1=1	!so READ14 returns iver1 only
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,jpatch,ffilt,sampatch,imode1,defname,npatch,temp)
	ipatch=int4(jpatch)	!integer*4 for SCAN
	do 64 i=n1,n2
261	   irec=istrec(i)
	   imode1=0		!so READ14 reads header (and corrects it if nec)
	   call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,jpatch,ffilt,sampatch,imode1,defname,npatch,temp)
	   ipatch=int4(jpatch)	!integer*4 for SCAN
	   ffilt=ffilt/1000.		!convert to kHz
c Now have value for nmax, so arrays can be allocated (iver1=-1004)
c This 'list details' loop needs only jmask (for calcns2)
	   nsamp0=nsamp		!# of points if all points kept
	   if(iver1.le.-1003) then
		if(allocated(jmask)) DEALLOCATE(jmask)
		ALLOCATE(jmask(nsamp0))
		call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &		 keepall,nsamp0)
	      nsamp0=nsamp		!orig # of points samples
	      nsamp=nsamp1		!# of points kept on disc
	   endif
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	   call PREC(i,title1,cdate,ctime,naver,navc,iav,vjump,
     &   control,iver1,1)
	   call PPAR(iprt,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,jpatch,ffilt*1000.)
64	continue		!end of loop for listing specified sweep details
c
26	continue
	if(pon()) then
	   write(7,6)
6	   format('1') !form feed (in case scroll lock used to print file summary)
	   call FLUSH(7)
	endif
c
	if(.not.keepall) then
	   call BELL(3)
	   print 43
43	   format(
     &' SCAN WILL ONLY FIT RECORDS IN WHICH ALL POINTS WERE KEPT ON ',/,
     &'  THE HARD DISK, AT PRESENT. ')
	   goto 67
	endif
c
	print 41
41	format(/,' Use this disc file [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
         CLOSE(unit=14)
	   goto 25		!try another disc
	endif
	goto 67	!list more?
c First record contains number of jumps recorded so far =NJD, next vacant
c record # = NXTREC and an array ISTREC(ijd)=record # for 1st record of
c each jump (that with the header info) (kept as int*2 to save space)
C ### MUST CONVERT ISTREC() TO INTEGER*4 BEFORE USING FOR READ
c Each jump written to 2 or more records. First contains directory-type stuff,
c the others contain data. 256 bytes=128 int*2 values of iADC
c Keep NAVER=number of sweeps averaged. Iav=0 (whatever naver/navc) means that
c iADC follows header; Iav=1 (naver/navc>1) means that AVCUR() follows header;
c then data that follows is twice as long (=AVCUR(i)=real*4)
c From CJDISK:
c If IAV=0 then writes only the current sweep (in iADC, and IADC1 if voltage
c sampled too), regardless of value of NAVER or NAVC
c Otherwise (IAV=1) records the average that has just been
c completed, ie if no v-jump then records AVCUR, if vjump done then
c (1) if CONTROL is false ('drug' -c-jump also done) records AVCUR,AVOLT or
c (2) if CONTROL is true ('con' -no c-jump) records AVCURCON,AVOLTCON
c NB may have CONTROL=true with SAMPV=false; eg if doing only jumps, not
c ramps then voltage is not recorded on ADC1, but will want to do
c jumps with and without agonist (c-jump)
c    When IAV=1 then returns without recording anything if naver (or navc)=1
c since no point in recording average of one trace (already recorded in iADC)
c THUS when reading back, any record with naver/navc=1 is iADC not AVCUR.
c BUT converse not nec true: if naver/navc>1 then data may nevertheless be
c for a single sweep (in iADC/iADC1); thus data record now contains IAV also.
c
42	continue
c=======up to line 580 in CJFIT
c WHICH TRACE TO FIT?
c
33	continue
	njdim=njd	!default dimension for jumps() array (alloc in main prog)
	print 37,njdim
37	format(
     & ' Maximum number of jumps to be fitted [',i5,'] = ')
	call INPUTi(njdim)
	print 34,njd
34	format(' Number of records on disc = ',i5,/,
     & ' First sweep # to be fitted:  n [enter=more details] =  ')
	njump=0
	call INPUTi(njump)
	njfirst=njump
	if(njump.le.0) goto 67
c
c
10	continue	!jump here for restart (imode=1)
c
	irec=istrec(njump)
c Read the specified record  'header'
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,jpatch,ffilt,sampatch,0,defname,npatch,temp)
	if(imode.eq.0) then
	   ipatch=int4(jpatch)	!integer*4 for SCAN
	   ivh=ivhold+ivhclamp
	   if(ipatch.eq.2.or.ipatch.eq.3) ivh=-ivh	!reverse pipette pot for i/o or c/a
	   Emem=float(ivh)
	   ffilt=ffilt/1000.		!convert to kHz
	endif
c Allocate jmask()
	nsamp0=nsamp		!# of points if all points kept
	if(iver1.le.-1003) then
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &    keepall,nsamp0)
	   nsamp0=nsamp		!orig # of points samples
	   nsamp=nsamp1		!# of points kept on disc
	endif
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	call PREC(njump,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iver1,2)
c
	if(imode.eq.1) goto 38
c
	if(nsweep.gt.1.and.isweep.eq.1) then
	   print 30,nsweep
	   if(pon()) write(7,30) nsweep
	   if(discprt) write(8,30) nsweep
30	   format(' First sweep of series of ',i3)
	else if(nsweep.gt.1.and.isweep.eq.nsweep) then
	   print 31,nsweep
	   if(pon()) write(7,31) nsweep
	   if(discprt) write(8,31) nsweep
31	   format(' Last sweep of series of ',i3)
	endif
	call PPAR(iprt,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,jpatch,ffilt*1000.)
	print 12
12	format(/,
     & ' (1) Use these records as they are',/,
     & ' (2) Specify new calibration (value from CJUMP is wrong)',/,
     & ' (3) Specify different records',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.le.0.or.iopt.gt.3) iopt=1
	if(iopt.eq.3) goto 33
	if(fixgain) then
	   call BELL(1)
	   print 681
681	   format('&Continue to override gain read from disc [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		fixgain=.false.
	   else
		calfac=calfac1
		calvolt=calvolt1
	   endif
	endif
	if(iopt.eq.2) then
	   calvolt=10.		!factor by which Vout is greater then true Em
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &   itape,ftape,gain,errfac)
	   i=ifixr(calvolt)
	   print 68,i
68	   format(
     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   call INPUTi(i)
	   calvolt=float(i)
	   calfac1=calfac		!save values to overwrite those from disc
	   calvolt1=calvolt	!save values to overwrite those from disc
	   fixgain=.true.
	endif
c	read 101,ans
c	if(UC(ans).eq.'N') goto 33
c
c
c Set Vramp=true if sweep contains at least one voltage ramp (used to
c set ndimd below, and in CJVDU.
c	vramp=.false.
c	if(nvjump.gt.0) then
c	   do i=1,nvjump
c       	if(ivolt1(i).ne.ivolt2(i)) vramp=.true.
c	   enddo
c	endif
c
c=======up to line 980 in CJFIT
c
c
	tsample=dble(float(itsamp))	!total sample length (microsec)
	tzero=float(itPREc)/1000.     !'ideal' value
	print 35,tzero
35	format(/,' Zero time (ms from first data point) [',f6.1,'] = ')
	call INPUTr(tzero)
      if(pon()) write(7,36) tzero
      if(discprt) write(8,36) tzero
36	format(/,
     &' Zero time for the jump set at ',f9.3,' ms from start of data')
c
c Define sample rate, and absolute index in data of tzero point
	srate=float(irate)	!sample rate (Hz)
	intzero=ifixr(tzero*srate/1000.)
	ktjump=0	!# of transitions for current jump (kt=for all jumps)
	nfjump=0    !# of fits for current jump (nfits=for all jumps)
c
38	continue		!jump here for imode=1
c
	jumplen=ilenc(1)			!jump length
	if(pon()) write(7,229) adcfil
	if(discprt) write(8,229) adcfil
229   format(/,' JUMP DATA file is: ',a30,/)
c
c Close data file and reopen as 'transparent' (see READSEC) whether
c cjump.dat has the old form (written in 512 byte records) or new
c (written as transparent)
	CLOSE(unit=14)
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &	access='TRANSPARENT')
c
c Now return the necessary values back to SCAN and return with
c the data file left open, and irec=the record to starting read of data
c   In data, as written in 512-byte records, data set #njump starts at
c record # irec=int4(istrec(njump)), which is record # for the header for
c sweep # njump; the binary data for this sweep starts 1 record (512 bytes)
c later.  Therefore set byte offset, ioff, for reading data as 'transparent',
c as
c	ir=istrec(njump)
c	ioff=512*ir
	if(newfile) then
	   ioff=ioffset+istrec(njump) - 1	!offset to read data in READSEC
	else
	   ioff=512*istrec(njump)  !offset to read data in READSEC
	endif
c e.g. ioff=512 for sweep #1, so data reading starts at #513
c If istrec(2)=3 say then
c header for sweep #1 is in byte # 1-512       record 1
c   data for sweep #1 is in byte # 513-1024    record 2
c header for sweep #2 is in byte # 1025-1536   record 3
c   data for sweep #2 is in byte # 1537-2048   record 4
c  -so ioff=512*3=1536 and data reading starts at byte # 1537.
c
	if(allocated(jmask)) DEALLOCATE(jmask)
	DEALLOCATE(istrec2)
	idest=0
	RETURN
c
999	continue
	if(allocated(jmask)) DEALLOCATE(jmask)
	DEALLOCATE(istrec2)
	idest=999
	RETURN
	end


