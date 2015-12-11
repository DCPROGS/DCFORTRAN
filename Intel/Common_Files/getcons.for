	subroutine GETCONS(adcfil,noconsam,title,cDATEW1,adctim1,nsam,
     & srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	
	USE DFLIB
	use gino_f90
	use menu_f90
      TYPE (FILE$INFO) info
	character cDATEW1*11,adctim1*8		!read from consam
	character*33 adcfil
	character*60 dfile,ddir
	character*10 dfilt
	character*200 message			!path for consam file
	character ans*1,UC*1
	character ndev*2
	logical noconsam,present,pon,slock,discprt,axfile
c for read of consam header
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,expDATEW*11,title*76,title1*70
c for cjump.dat
	logical cjdat,newform
	integer*4 irecfst(1000)		!now use int*4 version
	allocatable::irecfst2
	integer*2 irecfst2(:),idataformat		!=500 bytes for CJUMP
c
	logical good
c
	common/dp/discprt
c

	axfile=.false.
	idest=0
	noconsam=.true.
	dfile=' '
	ddir='.'
	dfilt='*.*'

	CALL gmFileBROWSER(DFILE,DDIR,DFILT,gmBrowseType=0)
	                                                                                                                                                     
	if(dfile.ne.' ') then
		adcfil=dfile(1:33)
		if(cjdat) then
			Icall=gmDisplayMessageBox(' ','CJUMP file',
     &		GEXCLAMATION,GOK)
		else
			Icall=gmDisplayMessageBox(' ','Axon binary file ?',
     &		gquestion,gyesno)
			if(icall.eq.gyesbutton) axfile=.true.
		endif
		INQUIRE(file=adcfil,exist=present)
		if(present) then
			noconsam=.false.
			nlen=0
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(adcfil, info, ihandle)
				nLEN=info%length 
			OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &		access='DIRECT', form='BINARY', RECL=1)

			if(.not.cjdat) then

			if(axfile) then
				call READAXON(dfile,title,cDATEW1,adctim1,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,idataformat,good)
				message=' CONSAM data from: '//adcfil//
     &			' DATEW '//cDATEW1//' time '//adctim1
				Icall=gmDisplayMessageBox('',message,Gquestion,Gyesno)
				   
					idt=0
					id1=0
					id2=0
					cs(1:3)='H  '
					iver=1002	!new format header
					filt1=0.0
					calfac1=-1.0
					title=title1(1:70)
					ioff=nlen-ilen
		  	else
				read(14,rec=1) iver
				if(iver.eq.1002) then	!already have expDATEW etc
				read(14,rec=1)iver,title1,cDATEW1,adctim1,idt,ioff,
     &			ilen,inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,
     &			calfac1,expDATEW
				title(1:70)=title1
				title(71:76)='      '
				message=' CONSAM data from: '//adcfil//
     &			' Experiment DATEW '//expDATEW//
     &			' DATEW '//cDATEW1//' time '//adctim1
				Icall=gmDisplayMessageBox('',message,Gquestion,Gyesno)
	  			else
	    		read(14,rec=1) title,cDATEW1,adctim1,idt,ioff,ilen,
     &			inchan,id1,id2,cs(1:3),
     &            calfac,srate,ffilt,ffilt1,calfac1,iver
				title1=title(1:70)
				message=' CONSAM data from: '//adcfil//title//
     &			' DATEW '//cDATEW1//' time '//adctim1
				Icall=gmDisplayMessageBox('',message,GquesTION,Gyesno)
				endif
			endif
	   		nsam=ilen/2		!number of data points in CONSAM
			if(icall.eq.gnobutton) then
				idest=99
				noconsam=.true.
			else
				if(iver.eq.1002) then
		   
				if(discprt) write(7,3)adcfil,title,expDATEW,cDATEW1,
     &				adctim1
3				format(' CONSAM data from: ',a33,/,1x,a76,/,
     &			' Experiment DATEW ',a11,' DATEW ',a11,' time ',a8)
				else
		   
				if(discprt) write(7,31) adcfil,title,cDATEW1,adctim1
31				format(' CONSAM data from: ',a33,/,1x,a76,/,
     &			' DATEW ',a11,' time ',a8)
				idest=91
				endif
			endif
			
			else if(cjdat) then
				read(14,rec=1) njd
				if(njd.lt.1) then
				newform=.true.	!new format 'transparent' file
				CLOSE(unit=14)
c				Re-open in correct format
				OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &			access='DIRECT', form='BINARY', RECL=1)
				read(14,rec=1) jver,njd,nxtrec,ioff,irecfst
				else
				newform=.false.	!old format file (512 byte records)
				ALLOCATE(irecfst2(250))
				read(14,rec=1) njd,nxtrec,irecfst2
				endif
				nplot=njd	!name used in this prog
c				Convert the integer*2 istrec2() into integer *4 for all subsequent use
				if(.not.newform) then
				do i=1,njd
					irec=int4(irecfst2(i))
		 			if(irec.le.-1.and.irec.ge.-32768) then
						irec=irec+65536
					else if(irec.lt.-32768) then
						Icall=gmDisplayMessageBox(' ',
     &					' IREC IS TOO BIG!!',GEXCLAMATION,GOK)
						STOP
					endif
					irecfst(i)=irec
				enddo
				DEALLOCATE(irecfst2)
				endif
			endif
		else
			noconsam=.true.
			idest=99
		endif
	else
		noconsam=.true.
		idest=99
	endif
	ioffset=ioff
	end

