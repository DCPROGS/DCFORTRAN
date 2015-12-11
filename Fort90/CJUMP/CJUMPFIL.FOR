	program CJUMPFIL
c This program is to concatenate one or more CJUMP.DAT files into a single
c file for use in CJFIT.  The header block on the second and subsequent
c files are removed, and replaced by a single header block of the sort
c used in the new CJUMP program, which allows any number of jumps to
c be collected.
c Start record #1 with a version nunber that applies to the entire file (distinct
c from iver1 which can differ from one jump to another within thefile)
c Make this the first value and make it negative, so if first value read
c is negative it is immediately obvious that this is a new data file, since
c the next value, njd, must always be positive.  If the number of jumps
c is not pre-determined then it is not clear how much space must be left
c for istrec4(), but surely 1000 jumps max will be OK!
c Record 1
c Originally (2*4)+(250*2)=508 bytes
c	    read(14,rec=1) njd,nxtrec,(ISTREC(i),i=1,250)
c New (4*4)+(1000*4)=4016 bytes
c	    read(14,rec=1) jver,njd,nxtrec,ioffset,(istrec4(i),i=1,1000)
c where iver=-1 here
c Record number, irec, in cjump4 was record number for each 512 byte
c record so
c irec=1 is bytes 1 to 512,
c irec=2 is bytes 513 to 1024,
c irec=3 is bytes 1024 to 1536 etc
c But file header written starting at record 1 is now 4016 bytes, and
c now extends from irec4=1 to irec4=5119, with header for the first jump
c starting at irec4=5120 (more than 1024 bytes gap, in case want to add more
c at rec=1 later)
c
c Data is read from each old data file as in (original) CJFIT subroutines
c read14 (header) and readrel (data), and written back to new file
c as in new cjdisk5 (in CJUMP5)
c
c Declarations for main prog
	integer*4 istrec4(1000)		!replacement for istrec()
	integer*2 istrec(250)
	character*33 datfil(10),newfil
	character*11 cnum
c
	ALLOCATABLE::avcur,avcurcon
	ALLOCATABLE::avolt,avoltcon
	ALLOCATABLE::iADC,iADC1
	real*4 avcur(:),avcurcon(:)
	real*4 avolt(:),avoltcon(:)
 	integer*2 iADC(:)
	integer*2 iADC1(:)		!for voltage on ADC1, if req
c  Declarations as in read14
      character cdate*11,ctime*11
	character title1*79
	logical vjump,sampv,control
c	logical discprt,pon,slock,debug,caplock
	integer*2 iver1
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch
	logical discprt,pon,slock,debug,caplock,pprt,present
c
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='CJUMPFIL.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1     FORMAT(
     &	' CJUMPFIL: To concatenate 2 or more old CJUMP.DAT files',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
	pprt=pon()
c
	kmax=51200		!as in cjump5
	ALLOCATE(avcur(kmax),avcurcon(kmax))
	ALLOCATE(avolt(kmax),avoltcon(kmax))
	ALLOCATE(iADC(kmax),iADC1(kmax))
c
	print 4
4	format(' Number of old CJUMP.DAT files to be concatenated [2] = ')
	nfil=2
	call INPUTi(nfil)
	call TITENT0(
     &   'Give name/path for new concatenated CJUMP data file',
     &    newfil,33,.false.)
c  Open the new file for output (unit=15 here)
      OPEN(unit=15,file=newfil,status='NEW',
     & access='TRANSPARENT')
c
	nextrec=5120	!record for start of first header (after rec=1)
	njd4=0	!for total number of jumps in all files
	k=0		!index for new istrec4() array
c
	do jfil=1,nfil
	   call INTCONV(jfil,cnum)
5	   call TITENT0(
     &   'Give name/path for old CJUMP data file #'//charnb(cnum)//':',
     &    datfil(jfil),33,.false.)
	   INQUIRE(file=DATFIL(jfil),exist=present,flen=len)
	   if(.not.(present.and.len.gt.0)) then
		goto 5	!file not present
	   else
      	OPEN(unit=14,file=DATFIL(jfil),status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
	 	read(14,rec=1) njd,nxtrec,(istrec(i),i=1,250)
		if(njd.lt.1) then
		   print 6,njd
6		   format(
     &	    'CJUMP.DAT is the new format: version number = ',i3)
		   STOP
		endif
      	print 17,njd,datfil(jfil),newfil
      	if(pprt) write(7,17) njd,datfil(jfil),newfil
      	if(discprt) write(8,17) njd,datfil(jfil),newfil
17		format(/,
     &	' Input jumps #1 to ',i4,' from ',a33,/,' copied to ',a33)
c
		njd4=njd4+njd	!accum total number of jumps
c
c Now go through all jumps in this file to determine what size
c the output file must be
		do ij=1,njd
c Accumulate new istrec4() array from istrec()
		   irec=int4(istrec(ij))
		   if(irec.le.-1.and.irec.ge.-32768) then
			irec=irec+65536
		   else if(irec.lt.-32768) then
			print*,' IREC IS TOO BIG!!'
			STOP
		   endif
		   ir2=irec		!save for print
c
c Read (a) header and (b) jump data, and copy into appropriate records
c of the new file
c         Read the header for current jump, staring at rec=irec in old file
c Read header below (OK for iver1=-1004 -otherwise need to use READ14
c (call first with imode=1 to get iver1, then call again with imode=0
c to do appropriate read for the version)
c	      call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
c     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
c     &      ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
c     &      ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
c     &      ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
c     &      ivhclamp,ipatch,ffilt,sampatch,imode)
c Next read the data from the old file -don't read as in CJFIT (readrel.for)
c because that is designed for display -read as written in cjdisk4 (which
c writes old files in CJUMP4)
		   read(14,rec=irec) cdate,ctime,title1,naver,navc,
     &		iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
		   nsamp1=nsamp
		   nrec=1 + (nsamp-1)/256  !for iADC(i)  =int*2 so 1 rec=256 values
		   if(iav.ne.0) nrec=2*nrec	!for AVCUR(i) =real*4
		   if(iav.eq.0) then
			n=1
			do j=1,nrec
			   m=n+255
			   irec=irec+1
			   read(14,rec=irec) (iADC(i),i=n,m)
			   n=n+256
		      enddo
		   else
			n=1
			do j=1,nrec
			   m=n+127
			   irec=irec+1
			   read(14,rec=irec) (AVCUR(i),i=n,m)
			   n=n+128
		      enddo
		   endif
c If voltage sampled, read it too
		   if(sampv) then
			if(iav.eq.0) then
			   n=1
			   do j=1,nrec
			      m=n+255
			      irec=irec+1
			      read(14,rec=irec) (iADC1(i),i=n,m)
			      n=n+256
			   enddo
			else
			   n=1
			   do j=1,nrec
			      m=n+127
			      irec=irec+1
			      read(14,rec=irec) (AVOLT(i),i=n,m)
			      n=n+128
			   enddo
			endif
		   endif
c
c Input value of nextrec is where writing of header will start in
c the output file (=newfil) (writ15 is based on cjdisk5)
		   k=k+1
		   istrec4(k)=nextrec
		   ir4=nextrec		!save for print
      	   print 175,ij,k,ir2,ir4
      	   if(pprt) write(7,175) ij,k,ir2,ir4
      	   if(discprt) write(8,175) ij,k,ir2,ir4
175		   format(/,' Input jump # ',i9,' = output jump #',i9,/,
     &	   '(start records = ',i9,', ',i9,')')
c
		   ijd=k		!jump # in output -for call
		   call WRIT15(iav,naver,navc,vjump,control,cdate,ctime,
     & iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     & nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilen2c,igap2c,ivhold,
     & nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,ivolt1,ivolt2,sampv,nDv,iDd,
     & ijd,nextrec,kmax,nfull,nsweep,swtime,isweep,
     & nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     & ipatch,ivhclamp,ffilt,newfil)
c On exit nextrec has been updated to point to next vacant byte
		enddo		!end of loop for each jump in current file
		CLOSE(unit=14)
	   endif		!end of 'if present'
	enddo		!end of loop for each input file
c Finally update first record -njd4=total number written to output file
c Write record #1
	jver=-1		!new cjump.dat format
	ioffset=1024
	write(15,rec=1) jver,njd4,nextrec,ioffset,ISTREC4
c
	CLOSE(unit=15)	!close new file
	DEALLOCATE(avcur,avcurcon)
	DEALLOCATE(avolt,avoltcon)
	DEALLOCATE(iADC,iADC1)
	end



	subroutine WRIT15(iav,naver,navc,vjump,control,cdate,ctime,
     & iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     & nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilen2c,igap2c,ivhold,
     & nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,ivolt1,ivolt2,sampv,nDv,iDd,
     & ijd,nextrec,kmax,nfull,nsweep,swtime,isweep,
     & nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     & ipatch,ivhclamp,ffilt,newfil)
c To write concatenated (old) cjump.dat files in new format in CJUMPFIL.FOR
c (based on CJDISK5)
c Unit number=15 here to distinguish from input files
c NB file already open when called
c Here irec is called irec4 to distinguish byte address (=irec4) for new
c transparent file, from irec=record # for old files (512 byte records)
c
c ioffset=1024 (for header)
c Still has iver=-1004 for each jump,
c because data format is the same as for CJUMP4, but now has jver=-1 in
c record 1 to indicate that the whole file has the CJUMP5 format)
c
c	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
c	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
c	integer*4 istrec(1000)		!=4000 bytes (now int*4)
	integer*2 iver1,ivhclamp,ipatch
	integer*4 ikeep1(4,2)
      integer*2 kstep(5)
	real*4 avcur(kmax),avcurcon(kmax)	!average current (drug,control)
	real*4 avolt(kmax),avoltcon(kmax)	!average voltage (drug,control)
	integer*2 iADC(kmax),iADC1(kmax)
      character cdate*11,ctime*11,newfil*33
	character title1*79
	logical discprt,pon,slock,vjump,sampv,control,pprt
	common/dp/discprt
c
	pon()=slock()
c
	pprt=pon()		!call pon() only once (rare problem with INT11 crash)
	nfull=0		!OK
	iver1=-1004       !for cjump4
	ioffset=1024
c
	IF(VJUMP.and.control) GOTO 90		!Sep section to record controls
c
	irec4=nextrec
	ir1=irec4		!save for print
c NB recl is in bytes (4*value for PDP).
c Order in which data is written decided for convenience in VJDISP which
c needs only values up to calvolt (or ivhold)
	write(15,rec=irec4) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
      irec4=ioffset+irec4
	ir2=irec4	!save for print
	if(iav.eq.0) then
	   write(15,rec=irec4) (iADC(i),i=1,nsamp)
         irec4=nsamp*2+irec4
	else
	   write(15,rec=irec4) (AVCUR(i),i=1,nsamp)
         irec4=nsamp*4+irec4
	endif
c NB if start at irec=10, and write 4 int*2 values, they are in bytes
c 10,11; 12,13; 14,15; 16,17 and irec=10+4*2=18 at end
	ir3=irec4-1		!save for print
c
c If voltage sampled, keep it too
	if(sampv) then
	   ir4=irec4		!save for print
	   if(iav.eq.0) then
		write(15,rec=irec4) (iADC1(i),i=1,nsamp1)
      	irec4=nsamp1*2+irec4
	   else
		write(15,rec=irec4) (AVOLT(i),i=1,nsamp1)
		irec4=nsamp1*4+irec4
	   endif
	endif
	ir5=irec4-1		!save for print
c
c Update nextrec
	nextrec=irec4
c
	if(iav.eq.0) then
c      	print 75,ijd,newfil,ir1,ir5,ir2,ir3
      	if(pprt) write(7,75) ijd,newfil,ir1,ir5,ir2,ir3
      	if(discprt) write(8,75) ijd,newfil,ir1,ir5,ir2,ir3
75		format(/,' Sweep number ',i5,': Agonist',/,
     & ' Single sweep in ',a33,': in records ',i8,' to ',i8,/,
     & ' (current in bytes ',i8,' to ',i8,')',/)
	else
c      	print 76,ijd,naver,newfil,ir1,ir5,ir2,ir3
      	if(pprt) write(7,76) ijd,naver,newfil,ir1,ir5,ir2,ir3
      	if(discprt) write(8,76) ijd,naver,newfil,ir1,ir5,ir2,ir3
76		format(/,' Sweep number ',i5,': Agonist',/,
     & ' Mean of',i6,' sweeps in ',a33,': in records ',i8,' to ',
     &   i8,/,
     & ' (current in bytes ',i8,' to ',i8,')',/)
	endif
	if(sampv) then
c        print 77,ir4,ir5
         if(pprt) write(7,77) ir4,ir5
         if(discprt) write(8,77) ir4,ir5
77	   format(
     & '& (voltage in bytes ',i8,' to ',i8,')',/)
	endif
	goto 999		!return
c
c Separate section to record controls (control=true)
90	continue
	irec4=nextrec
	ir1=irec4		!save for print
c NB recl is in bytes (4*value for PDP).
	write(15,rec=irec4) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
c	if(navc.eq.1) then
	irec4=irec4+ioffset
	ir2=irec4	!save for print
	if(iav.eq.0) then
	   write(15,rec=irec4) (iADC(i),i=1,nsamp)
         irec4=nsamp*2+irec4
	else
	   write(15,rec=irec4) (AVCURCON(i),i=1,nsamp)
         irec4=irec4+4*nsamp
	endif
	ir3=irec4-1		!save for print
c If voltage sampled, keep ADC1 too
	if(sampv) then
	   ir4=irec4		!save for print
	   if(iav.eq.0) then
		write(15,rec=irec4) (iADC1(i),i=1,nsamp1)
      	irec4=irec4+2*nsamp1
	   else
		write(15,rec=irec4) (AVOLTCON(i),i=1,nsamp1)
		irec4=irec4+4*nsamp1
	   endif
	endif
	ir5=irec4-1		!save for print
c
c Update nextrec
	nextrec=irec4
c
	if(iav.eq.0) then
c      	print 78,ijd,newfil,ir1,ir5,ir2,ir3
      	if(pprt) write(7,78) ijd,newfil,ir1,ir5,ir2,ir3
      	if(discprt) write(8,78) ijd,newfil,ir1,ir5,ir2,ir3
78		format(/,' Sweep number ',i5,': Control',/,
     & ' Single sweep in ',a33,': in records ',i8,' to ',i8,/,
     & ' (current in bytes ',i8,' to ',i8,')',/)
	else
c      	print 79,ijd,navc,newfil,ir1,ir5,ir2,ir3
      	if(pprt) write(7,79) ijd,navc,newfil,ir1,ir5,ir2,ir3
      	if(discprt) write(8,79) ijd,navc,newfil,ir1,ir5,ir2,ir3
79		format(/,' Sweep number ',i5,': Control',/,
     & ' Mean of',i6,' sweeps in ',a33,': in records ',i8,' to ',
     &   i8,/,
     & ' (current in bytes ',i8,' to ',i8,')',/)
	endif
	if(sampv) then
c        print 77,ir4,ir5
         if(pprt) write(7,77) ir4,ir5
         if(discprt) write(8,77) ir4,ir5
c77	   format(
c     & '& (voltage in bytes ',i8,' to ',i8,')',/)
	endif
c
999	continue
	RETURN
	end


