c====================================================================
	program axonfile
c====================================================================
	character    cdate*11,datfil*24
	character    adctime*8
      character*1  ans,getch,cans*30,def*30
	character*76 title
	character*13 getreal,getint
      integer*2    iver,idata(:)
	logical      present,good

	allocatable :: idata
	good=.false.
	icwd=11
	icqd=14
	call iniplt(0,.false.,1.0)
      call DEFDIALOG(1,2,2,27,77,0)
	call opendialog(1,1,.true.)
	datfil='AXON.DAT'
765	continue
	nc=nblank1(datfil)
	call qdialog(1,'Enter File Name ',
     &' ['//datfil(1:nc)//'] = ',icqd,cans)
	if(cans.eq.' ') goto 654
	nc=nblank1(cans)
	if(nc.le.24) then
	   if(cans(nc-3:nc).eq.'.dat'.or.cans(nc-3:nc).eq.'.DAT') then
		datfil=cans(1:nc)
		goto 654
	   endif
	endif
	call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',icwd)
	goto 765
654   continue
	INQUIRE(file=DATFIL,exist=present)
	if(.not.present) then
	  if (itry.lt.3) then
	  call wdialog(1,'FILE NOT PRESENT ; PLEASE TRY AGAIN !',12)
	  itry=itry+1
	  goto 765
	  endif
	  call wdialog(1,'FILE NOT PRESENT ; GO OUT AND CHECK!',12)
	  ans=getch(b)
	  call EXIT(2)
	else
	    call readaxon(datfil,title,cdate,adctime,ilen,nchan,
     &    calfac,pAV,srate,filt,iver,ioff,good)
      endif
	if(.not.good) goto 1
	ans=getch(b)
	n=ilen/2
	call wdialog(1,title,icwd)
	call wdialog(1,'Date of analysis : '//cdate//
     &' Time of analysis : '//adctime(1:8),icwd)
	call wdialog(1,'Actual sample rate (Hz) : '//
     &getreal(srate,nm)(1:nm),icwd)
	call wdialog(1,'Sample of '//getint(int(ilen/2),nm)(1:10)//
     &      ' points on ADC0 ',icwd)
	call wdialog(1,'Calibration factor (pA per ADC unit) : '//
     &	getreal(calfac,nm)(1:nm),icwd)
      call wdialog(1,'Filter (Hz) for ADC0 : '//getreal(filt,nm)(1:nm),
     &	icwd)

	ioff=ioff+1
	allocate(idata(n))
      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
	read(14,rec=ioff) (idata(i),i=1,16)
	ioff=ilen-32+ioff
	read(14,rec=ioff) (idata(i),i=17,32)
      CLOSE(unit=14)
	do i=1,16
	   inti=idata(i)
	   int16=idata(i+16)
         call wdialog(1,getint(inti,nm)(1:nm)//' = '//
     &   getreal((idata(i)*calfac),nm)(1:nm)//
     &   '   '//getint(int16,nm)(1:nm)
     &   //' = '//getreal((idata(i+16)*calfac),nm)(1:nm),icwd)
	enddo
1	ans=getch(b)
	call endplt
	end

c=====================================================================
	subroutine readaxon(datfil,title,cdate,adctime,ilen,nchan,
     &            calfac,srate,filt,iver,ioff,good)
c=====================================================================
	character   cdate*11,istring*11,adctime*8,datfil*24,title*76
	logical	good
	real*4      fiscale(16),foffset(16),fsgain(16),fsoffset(16)
      real*4	fsfilter(16),fpgain(16),fdampl(16),fdoffset(16)
	character*8 units(16)
	character*56 comment
	character*16 creator
	character*4  filetype
	integer*2   operationmode,dataformat,nexp,ichan
	integer*4 	ioff,ilen,date,time,actualength

      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
      read(14,rec=1) filetype,fileversion,operationmode
      read(14,rec=101) dataformat
      read(14,rec=121) ichan
      CLOSE(unit=14)

	if(filetype(1:3).eq.'ABF'.and.operationmode.eq.3.and.
     &   dataformat.eq.0.and.ichan.eq.1) then
	   call realtoch(fileversion,istring,11)
	   call wdialog(1,filetype//' v'//istring(1:4)//';'//
     &      'gap-free.',11)
	else
	   call wdialog(1,'Not an Good Axon Binary File',11)
	   goto 99
	endif

      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
	read(14,rec=11) actualength
      read(14,rec=21) date,time
      read(14,rec=123) samplint
	read(14,rec=245) adcrange,dacrange,iadcresolution,idacresolution,
     & nexp
	read(14,rec=295) creator,comment
	read(14,rec=603) (units(i),i=1,16),(fpgain(i),i=1,16),
     & (fdampl(i),i=1,16),
     & (fdoffset(i),i=1,16),(fiscale(i),i=1,16),(foffset(i),i=1,16),
     & (fsgain(i),i=1,16),(fsoffset(i),i=1,16),(fsfilter(i),i=1,16)
      CLOSE(unit=14)

	ioff=2048
	nchan=ichan
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
	calfac=adcrange/(fiscale(1)*iadcresolution)  ! [V/((V/pA)*ADC)]->pA/ADC
	srate=1000000/samplint
	filt=fsfilter(1)
99    end
c=======================================================================





