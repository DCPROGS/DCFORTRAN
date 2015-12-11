	subroutine GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     & srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
c To get name of CONSAM file in AUTPLOT
c Modif 10/26/97 06:43am to get name of cjump.dat if cjdat=true
c  (parameters added: cjdat, irecfst,newform,ioffset
	character cdate1*11,adctim1*8		!read from consam
	character adcfil*33			!path for consam file
	character ans*1,UC*1
	character ndev*2
	logical noconsam,present,pon,slock,discprt,axfile
c for read of consam header
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,expdate*11,title*76,title1*70
c for cjump.dat
	logical cjdat,newform
	integer*4 irecfst(1000)		!now use int*4 version
	allocatable::irecfst2
	integer*2 irecfst2(:)		!=500 bytes for CJUMP
c
	common/dp/discprt
c
	pon()=slock()
c
101	format(a1)
	idest=0
	noconsam=.true.
c	adcfil=ndev//'\CONSAM.DAT'	!use value from .INI as default
	if(cjdat) then
	   call TITENT0('CJUMP file name:',adcfil,33,.false.)
	else
	   print 1
1	   format(' Continuous data sample from:',/,
     &	' (1) CONSAM (CED1401) file',/,
     &	' (2) Axon binary file ',/,
     &	' (3) Exit without display of bursts ',/,
     &	' Option [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.eq.3) then
		noconsam=.true.
		RETURN
	   endif
	   axfile=iopt.eq.2
	   call TITENT0('CONSAM file name:',adcfil,33,.false.)
	endif
14	continue
	INQUIRE(file=adcfil,exist=present,flen=len)
	if(present.and.len.gt.0) then
	   noconsam=.false.
	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &      access='TRANSPARENT')
c don't overwrite CDATE!
	   if(.not.cjdat) then
c	      call DCASK(
c     &     'Is this an AXON file ','n',ans)
	      if(axfile) then
		   call READAXON(adcfil,title,cdate1,adctime1,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
		   print 4, adcfil,title,cdate1,adctim1
4		   format(' CONSAM data from: ',a33,/,1x,a76,/,
     &	   ' Date ',a11,' time ',a8,':   O.K. [Y] ? ')
	      else
c	         read(14,rec=1) title,cdate1,adctim1,idt,ioff,ilen,inchan,
c     &         id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
		   read(14,rec=1) iver
		   if(iver.eq.1002) then	!already have expdate etc
			read(14,rec=1)iver,title1,cdate1,adctim1,idt,ioff,
     &	      ilen,inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,
     &		calfac1,expdate
			title(1:70)=title1
			title(71:76)='      '
			print 41, adcfil,title,expdate,cdate1,adctim1
41			format(' CONSAM data from: ',a33,/,1x,a76,/,
     &	      ' Experiment date ',a11,/,
     &	      ' Date ',a11,' time ',a8,':   O.K. [Y] ? ')
	  	   else
	    		read(14,rec=1) title,cdate1,adctim1,idt,ioff,ilen,
     &		inchan,id1,id2,cs(1:3),
     &            calfac,srate,ffilt,ffilt1,calfac1,iver
			title1=title(1:70)
			print 4, adcfil,title,cdate1,adctim1
c4			format(' CONSAM data from: ',a33,/,1x,a76,/,
c     &	      ' Date ',a11,' time ',a8,':   O.K. [Y] ? ')
		   endif
		endif
	   	nsam=ilen/2		!number of data points in CONSAM
		ans='Y'
		call INPUta(ans)
		if(UC(ans).eq.'N') then
		   present=.false.
	         goto 5
		endif
		if(iver.eq.1002) then
		   if(pon()) write(7,3) adcfil,title,expdate,cdate1,adctim1
		   if(discprt) write(8,3)adcfil,title,expdate,cdate1,adctim1
3		   format(' CONSAM data from: ',a33,/,1x,a76,/,
     &		' Experiment date ',a11,' date ',a11,' time ',a8)
		else
		   if(pon()) write(7,31) adcfil,title,cdate1,adctim1
		   if(discprt) write(8,31) adcfil,title,cdate1,adctim1
31		   format(' CONSAM data from: ',a33,/,1x,a76,/,
     &		' date ',a11,' time ',a8)
		endif
		idest=91
	   else if(cjdat) then
		read(14,rec=1) njd
		if(njd.lt.1) then
		   newform=.true.	!new format 'transparent' file
		   CLOSE(unit=14)
c          Re-open in correct format
		   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &		 access='TRANSPARENT')
		   read(14,rec=1) jver,njd,nxtrec,ioff,irecfst
		else
		   newform=.false.	!old format file (512 byte records)
		   ALLOCATE(irecfst2(250))
		   read(14,rec=1) njd,nxtrec,irecfst2
		endif
		nplot=njd	!name used in this prog
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
		if(.not.newform) then
		   do i=1,njd
			irec=int4(irecfst2(i))
		 	if(irec.le.-1.and.irec.ge.-32768) then
			   irec=irec+65536
			else if(irec.lt.-32768) then
			   print*,' IREC IS TOO BIG!!'
			   STOP
			endif
		      irecfst(i)=irec
		   enddo
		   DEALLOCATE(irecfst2)
		endif
	   endif
	endif
c NB remove check of date and time of CONSAM for now (they are
c not queued by old SAMPED at present)
c	if(.not.present.or.len.eq.0.or.cdate1.ne.cdate.or.
c     &		adctim1.ne.adctime) then
5	if(.not.present.or.len.eq.0) then
	   noconsam=.true.
	   call BELL(2)
	   print 13,adcfil,cdate1,adctim1
13	   format(' Data file ',a33,/,' date ',a11,' time ',a8,
     &   ' is not on disk')
         call DCASK('Specify another disk/path for CONSAM','y',ans)
	   if(ans.eq.'Y') then
		call DISCNUM1(id,ndev,-1,0)	!ask for hard or floppy
		call TITENT0('CONSAM file name:',adcfil,33,.false.)
		goto 14
	   else
c	     goto 99	!leave prog
	     idest=99
	   endif
	endif
c
	ioffset=ioff
	RETURN
	end

