	subroutine READAXON(adcfil,title,cdate,adctime,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
c To read Axon data files for SCAN.  Leaves file open for READSEC
	character   cdate*11,istring*11,adctime*8,adcfil*30,title*76
	logical	good
	real*4      fiscale(16),foffset(16),fsgain(16),fsoffset(16)
      real*4	fsfilter(16),fpgain(16),fdampl(16),fdoffset(16)
	character*8 units(16)
	character*56 comment
	character*16 creator
	character*4  filetype
	integer*2   operationmode,dataformat,nexp,inchan,iver
	integer*4 	ioff,ilen,date,time,actualength
	logical discprt
	common/dp/discprt

      OPEN(unit=14,file=adcfil,status='UNKNOWN',access='TRANSPARENT')
      read(14,rec=1) filetype,fileversion,operationmode
      read(14,rec=101) dataformat
      read(14,rec=121) inchan

	if(.not.(filetype(1:3).eq.'ABF'.and.operationmode.eq.3.and.
     &   dataformat.eq.0.and.inchan.eq.1)) then
	   call BELL(3)
	   print 1,filetype,fileversion,operationmode,dataformat,inchan
	   if(discprt) write(8,1) filetype,fileversion,
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

	read(14,rec=11) actualength
      read(14,rec=21) date,time
      read(14,rec=123) samplint
	read(14,rec=245) adcrange,dacrange,iadcresolution,idacresolution,
     & nexp
	read(14,rec=269) AutosampleAdditGain
	read(14,rec=295) creator,comment
	read(14,rec=603) (units(i),i=1,16),(fpgain(i),i=1,16),
     & (fdampl(i),i=1,16),
     & (fdoffset(i),i=1,16),(fiscale(i),i=1,16),(foffset(i),i=1,16),
     & (fsgain(i),i=1,16),(fsoffset(i),i=1,16),(fsfilter(i),i=1,16)
	fADCProgrammableGain=fpgain(1)
	fInstrumentScaleFactor=fiscale(1)
	fSignalGain=fsgain(1)
c==   CLOSE(unit=14)		!leave open

	ioff=2048
	iver=1001
	ilen=2*actualength
	good=.true.
	if(nexp.eq.0) then
	    title='Voltage Clamp;'
	else
	    title='Current Clamp;'
	endif
	j=nblank1(title)
	lco=nblank1(comment)
	lcc=nblank1(creator)
	title=title(1:j)//comment(1:lco)//';'//creator(1:lcc)
	call intconv(date,istring)
	cdate=istring(5:6)//'/'//istring(3:4)//'/'//istring(1:2)
	ih=time/3600
	min=(time-ih*3600)/60
	isec=(time-ih*3600)-min*60
	call intconv(ih,istring)
	adctime=istring(1:2)//':'
	call intconv(min,istring)
	adctime=adctime(1:3)//istring(1:2)//':'
	call intconv(isec,istring)
	adctime=adctime(1:6)//istring(1:2)
c For calfac get [V/((V/pA)*ADC)]->pA/ADC
	calfac=adcrange/(fInstrumentScaleFactor*float(iadcresolution))
	calfac=calfac/(AutosampleAdditGain*fADCProgrammableGain)
	calfac=calfac/fSignalGain
c
	pAV=calfac*float(iadcresolution)/adcrange
	srate=1000000/samplint
	ffilt=fsfilter(1)
99	continue
	RETURN
	end
