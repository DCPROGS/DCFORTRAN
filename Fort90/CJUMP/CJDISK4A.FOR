	subroutine CJDISK4(iav,ndev,naver,navc,vjump,control,cdate,ctime,
     & iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     & nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     & nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     & ijd,nxtrec,istrec,kmax,nfull,nsweep,swtime,isweep,
     & nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     & ipatch,ivhclamp,ffilt,datfil,newtype)
c
c To write jumps to disk in CJUMP4 (now has iver=-1004)
c (kmax2 removed -all arrays allocated as kmax now, and kmax written to header)
c
c Modif 07/12/91 11:51am: dimension of AVCUR etc changed to kmax2
c Version for CJUMP3 06/03/91 11:36am. Add ikeep1,nkeep,nsamp1,kstep, and
c also the separate calibration values amVpA1,ftape,gain,errfac (rather than
c just the overall calibration)
c Version for CJUMP2 03/29/91 11:09am. Added nsweep,swtime but keep rest
c the same so do not need to make another version of CJFIT (the actual
c values for the multiple sweeps will be shown in record of each sweep
c so no need to keep SWVAL or ISMODE on disk). Also add a parameter (int*2)
c IVER1  =-1003 to disk so CJFIT (and VJDISPRD) can tell that the disk
c records were written in CJUMP3
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
c First record contains number of jumps recorded so far =NJD, next vacant
c record # = NXTREC and an array ISTREC(ijd)=record # for 1st record of
c each jump (that with the header info) (kept as int*2 to save space)
C ### MUST CONVERT ISTREC() TO INTEGER*4 BEFORE USING FOR READ
c Each jump written to 2 or more records. First contains directory-type stuff,
c the others contain data. 256 bytes=128 int*2 values of iADC
c Keep NAVER=number of sweeps averaged. Iav=0 (whatever naver/navc) means that
c iADC follows header; Iav=1 (naver/navc>1) means that AVCUR() follows header;
c then data that follows is twice as long (=AVCUR(i)=real*4)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10),videotyp
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 istrec(250)		!=500 bytes (kept as int*2 to fit in 1 rec)
	integer*2 iver1,ivhclamp,ipatch
	integer*4 ikeep1(4,2)
      integer*2 kstep(5)
c=	real*4 avcur(kmax2),avcurcon(kmax2)	!average current (drug,control)
	real*4 avcur(kmax),avcurcon(kmax)	!average current (drug,control)
	real*4 avolt(kmax),avoltcon(kmax)	!average voltage (drug,control)
c=	integer*2 iADC(kmax2),iADC1(kmax)
	integer*2 iADC(kmax),iADC1(kmax)
      character cdate*11,ctime*11,ndev*2,datfil*24
	character title1*79,textg*64,getint*10
	logical discprt,pon,slock,vjump,sampv,control,pprt,alpha
	common/dp/discprt
c
	pon()=slock()
c
	pprt=pon()		!call pon() only once (rare problem with INT11 crash)
	nfull=0		!OK
	iver1=-1004       !for cjump4
	alpha=videotyp().eq.3
c
	if(.not.vjump) nvjump=0
c
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,0)	!convert ilenc to ilen2c etc
c
	if(ijd.eq.250) then
c	   call VIDEOMOD(3)		!redraw completely for each jump
	   call BELL(4)
	   if(alpha) then
	   print 1,ndev
1	   format(
     &   ' DISC FILE ON ',a2,' IS FULL -CONTINUE ON ANOTHER DISC')
	   else
	   call newpen(13)
	   call gtext(20,2,' DISC FILE ON '//getint(ndev,nm)(1:nm)//
     &   ' IS FULL -CONTINUE ON ANOTHER DISC')
	   endif
	   nfull=1
	   RETURN
	endif
c

	IF(VJUMP.and.control) GOTO 90		!Sep section to record controls
c
	if(iav.ne.0.and.naver.eq.1) then
	   if(alpha)  then
         print 71
	   else
		call newpen(13)
	   call gtext(20,2,'Average of one trace only, so not recorded')
	   endif
         if(pprt) write(7,71)
         if(discprt) write(8,71)
71	   format(' Average of one trace only, so not recorded')
	   goto 999		!return
	endif
c	nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
	nrec=1 + (nsamp1-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
	if(iav.ne.0) nrec=2*nrec	!for AVCUR(i) =real*4
c	nrectot=nrec
c	if(sampv) nrectot=2*nrec	!to keep voltage too
	irec=nxtrec
	ir1=irec		!save for print
c	datfil=ndev//'\CJUMP.DAT'
      OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
c NB recl is in bytes (4*value for PDP).
c Order in which data is written decided for convenience in VJDISP which
c needs only values up to calvolt (or ivhold)
	write(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
	if(iav.eq.0) then
	   n=1
	   do 501 j=1,nrec
c	   m=n+127
	   m=n+255
	   irec=irec+1
	   write(14,rec=irec) (iADC(i),i=n,m)
c	   n=n+128
	   n=n+256
501	   continue
	else
	   n=1
	   do 502 j=1,nrec
	   m=n+127
	   irec=irec+1
	   write(14,rec=irec) (AVCUR(i),i=n,m)
	   n=n+128
502	   continue
	endif
c If voltage sampled, keep it too
	if(.not.sampv) goto 80
c	if(naver.eq.1) then
	if(iav.eq.0) then
	   n=1
	   do 503 j=1,nrec
	   m=n+255
	   irec=irec+1
	   write(14,rec=irec) (iADC1(i),i=n,m)
	   n=n+256
503	   continue
	else
	   n=1
	   do 504 j=1,nrec
	   m=n+127
	   irec=irec+1
	   write(14,rec=irec) (AVOLT(i),i=n,m)
	   n=n+128
504	   continue
	endif
c
c Update record #1
80	continue
	ijd=ijd+1
	njd=ijd
	nxtrec=irec+1
	istrec(ijd)=ir1
	write(14,rec=1) njd,nxtrec,ISTREC
      CLOSE(unit=14)
c
c	if(naver.eq.1) then
	if(iav.eq.0) then
c      	print 75,njd,ndev,ir1,irec
      	if(pprt) write(7,75) njd,ndev,ir1,irec
      	if(discprt) write(8,75) njd,ndev,ir1,irec
75		format(/,' Sweep number ',i5,': Agonist',/,
     & ' Single sweep on disc ',a2,' in records ',i5,' to ',i5)
	else
c      	print 76,njd,naver,ndev,ir1,irec
      	if(pprt) write(7,76) njd,naver,ndev,ir1,irec
      	if(discprt) write(8,76) njd,naver,ndev,ir1,irec
76		format(/,' Sweep number ',i5,': Agonist',/,
     & ' Mean of',i6,' sweeps on disc ',a2,' in records ',i5,' to ',
     &   i5,/)
	endif
	goto 999		!return
c
c Separate section to record controls (control=true)
90	continue

	if(iav.ne.0.and.navc.eq.1) then
	   if(alpha) then
         print 71
	   else
		call newpen(13)
	   call gtext(20,2,'Average of one trace only, so not recorded')
	   endif
         if(pprt) write(7,71)
         if(discprt) write(8,71)
c71	   format(' Average of one trace only, so not recorded')
	   goto 999		!return
	endif
c	nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2
	nrec=1 + (nsamp1-1)/256		!for iADC(i)  =int*2
	if(iav.ne.0) nrec=2*nrec	!for AVCURCON(i) =real*4
c	if(navc.gt.1) nrec=2*nrec	!for AVCURCON(i) =real*4
c	nrectot=nrec
c	if(sampv) nrectot=2*nrec	!to keep voltage too
	irec=nxtrec
	ir1=irec		!save for print
c	datfil=ndev//'\CJUMP.DAT'
      OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
c NB recl is in bytes (4*value for PDP).
	write(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
c	if(navc.eq.1) then
	if(iav.eq.0) then
	   n=1
	   do 401 j=1,nrec
	   m=n+255
	   irec=irec+1
	   write(14,rec=irec) (iADC(i),i=n,m)
	   n=n+256
401	   continue
	else
	   n=1
	   do 402 j=1,nrec
	   m=n+127
	   irec=irec+1
	   write(14,rec=irec) (AVCURCON(i),i=n,m)
	   n=n+128
402	   continue
	endif
c If voltage sampled, keep ADC1 too
	if(.not.sampv) goto 81
c	if(navc.eq.1) then
	if(iav.eq.0) then
	   n=1
	   do 403 j=1,nrec
	   m=n+255
	   irec=irec+1
	   write(14,rec=irec) (iADC1(i),i=n,m)
	   n=n+256
403	   continue
	else
	   n=1
	   do 404 j=1,nrec
	   m=n+127
	   irec=irec+1
	   write(14,rec=irec) (AVOLTCON(i),i=n,m)
	   n=n+128
404	   continue
	endif
c
c Update record #1
81	continue
	ijd=ijd+1
	njd=ijd
	nxtrec=irec+1
	istrec(ijd)=ir1
	write(14,rec=1) njd,nxtrec,ISTREC
      CLOSE(unit=14)
c
c	if(navc.eq.1) then
	if(iav.eq.0) then
c      	print 77,njd,ndev,ir1,irec
      	if(pprt) write(7,77) njd,ndev,ir1,irec
      	if(discprt) write(8,77) njd,ndev,ir1,irec
77		format(/,' Sweep number ',i5,': Control',/,
     & ' Single sweep on disc ',a2,' in records ',i5,' to ',i5)
	else
c      	print 78,njd,navc,ndev,ir1,irec
      	if(pprt) write(7,78) njd,navc,ndev,ir1,irec
      	if(discprt) write(8,78) njd,navc,ndev,ir1,irec
78		format(/,' Sweep number ',i5,': Control',/,
     & ' Mean of',i6,' sweeps on disc ',a2,' in records ',i5,' to ',
     &   i5,/)
	endif
c
999	continue
	RETURN
	end


