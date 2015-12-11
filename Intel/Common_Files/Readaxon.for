	subroutine READAXON(testfil,title,cDATEW,adctimew,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,dataformat,good,ioff)

	use menu_f90
c To read Axon data files for SCAN.  Leaves file open for READSEC
	character   cDATEW*11,istring*11,adctimew*8,adcfil*30,title*76
	logical	good
	real*4    fiscale(16),foffset(16),fsgain(16),fsoffset(16)
      real*4	fsfilter(16),fpgain(16),fdampl(16),fdoffset(16)
      real*4    fTelegraphAdditGain(16)
	character*8 units1(16)
	character*56 comment
	character*16 creator
	character*60 testfil
	character*4  filetype
	integer*2   operationmode,dataformat,nexp,inchan,iver,nAutosampleEnable
	integer*2   nADCNumChannels,nADCSamplingSeq(16),nADCPtoLChannelMap(16)
	integer*4 	ioff,ilen,DATEW,time,actualength
	logical discprt
	common/dp/discprt

      OPEN(unit=14,file=testfil,status='UNKNOWN',access='DIRECT'
     &	, form='BINARY', RECL=1		)
      read(14,rec=1) filetype,fileversion,operationmode
      read(14,rec=101) dataformat
      read(14,rec=121) inchan
     
      if(filetype(1:3).ne.'ABF'.and.filetype(1:3).ne.'abf') then
        Icall=gmDisplayMessageBox(' ',
     &	' Not a valid Axon binary continuous data file',gstop,gok) 
	  good=.false.
         CLOSE(unit=14)
	   goto 99
	endif
      AutosampleAdditGain=1.
	read(14,rec=11) actualength
	read(14,rec=17) lActualEpisodes
      read(14,rec=21) DATEW,time
      read(14,rec=33) fHeaderVersionNumber
      read(14,rec=41)lDataSectionPtr,lTagSectionPtr,lNumTagEntries,
     & lScopeConfigPtr
      read(14,rec=121) nADCNumChannels
      read(14,rec=123) samplint
	read(14,rec=245) adcrange,dacrange,iadcresolution,idacresolution,
     & nexp
      read(14,rec=263) nAutosampleEnable
	read(14,rec=269) AutosampleAdditGain
	
	read(14,rec=295) creator,comment
	read(14,rec=379) (nADCPtoLChannelMap(i), i=1,16)
	read(14,rec=411) (nADCSamplingSeq(i), i=1,16)
	read(14,rec=603) (units1(i),i=1,16),(fpgain(i),i=1,16),
     & (fdampl(i),i=1,16),
     & (fdoffset(i),i=1,16),(fiscale(i),i=1,16),(foffset(i),i=1,16),
     & (fsgain(i),i=1,16),(fsoffset(i),i=1,16),(fsfilter(i),i=1,16)
	fADCProgrammableGain=fpgain(1)
	fInstrumentScaleFactor=fiscale(1)
	fSignalGain=fsgain(1)
	read(14,rec=2035)lHeaderSize
	if( fHeaderVersionNumber.ge.1.8) then
	  read(14,rec=4577)	(fTelegraphAdditGain(i), i=1,16)
        AutosampleAdditGain=fTelegraphAdditGain(1)
        read(14,rec=923) (fiscale(i),i=1,16)
        fInstrumentScaleFactor=fiscale(1)
        read(14,rec=1051)(fsgain(i),i=1,16)
        fSignalGain=fsgain(1)
c        irec=lheadersize+2*actualength+1
c        if(dataformat.eq.1) irec=lheadersize+4*actualength+1
c        read(14,rec=irec) lstart,lLength
	endif
      CLOSE(unit=14)		!leave open
      goto 100
      if(.not.(operationmode.eq.3.and.inchan.eq.1)) then
	  	Icall=gmDisplayMessageBox(' ',
     &	' Not a valid Axon binary continuous data file',gstop,gok) 
c	   print 1,filetype,fileversion,operationmode,dataformat,inchan
	   if(discprt) write(7,1) filetype,fileversion,
     &	operationmode,dataformat,inchan
1	   format(' Not a valid Axon binary continuous data file',/,
     &   ' filetype      = ',a4,/,
     &   ' fileversion   = ',f9.3,/,
     &   ' operationmode = ',i4,/,
     &   ' dataformat    = ',i4,/,
     &   ' inchan        = ',i4,/)
	   good=.false.
         CLOSE(unit=14)
	   goto 99
	endif
100	continue
c      Operation mode: 1 = Event-driven, variable length;  
c      2 = Oscilloscope, loss free (Same as Event-driven, fixed length);  
c      3 = Gap-free;  
c      4 = Oscilloscope, high-speed;  
c      5 = episodic stimulation (Clampex only).

	iver=1001
	ilen=2*actualength
	if(dataformat.eq.1) ilen=4*actualength
	good=.true.
	if(nexp.eq.0) then
	    title='Voltage Clamp;'
	else
	    title='Current Clamp;'
	endif
	j=len_trim(title)
	lco=len_trim(comment)
	lcc=len_trim(creator)
	title=title(1:j)//comment(1:lco)//';'//creator(1:lcc)
	call intconv(DATEW,istring)
	cDATEW=istring(5:6)//'/'//istring(3:4)//'/'//istring(1:2)
	ih=time/3600
	min=(time-ih*3600)/60
	isec=(time-ih*3600)-min*60
	call intconv(ih,istring)
	adctimew=istring(1:2)//':'
	call intconv(min,istring)
	adctimew=adctimew(1:3)//istring(1:2)//':'
	call intconv(isec,istring)
	adctimew=adctimew(1:6)//istring(1:2)
c For calfac get [V/((V/pA)*ADC)]->pA/ADC
      if(AutosampleAdditGain.le.0.0) AutosampleAdditGain=1.
c      if(fInstrumentScaleFactor.le.0.0) fInstrumentScaleFactor=1.0
	calfac=adcrange/(fInstrumentScaleFactor*float(iadcresolution))
	calfac=calfac/(AutosampleAdditGain*fADCProgrammableGain)
	calfac=calfac/fSignalGain
c
	pAV=calfac*float(iadcresolution)/adcrange
	srate=1000000/samplint

	ffilt=fsfilter(1)
	ioff=lHeaderSize
99	continue
	RETURN
	end
