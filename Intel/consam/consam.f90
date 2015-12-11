Program consam
USE DFLIB
use gino_f90
use menu_f90
USE, INTRINSIC:: ISO_C_BINDING

INTERFACE
INTEGER FUNCTION AdcDisk(adcdfil, ioff, nbyte, cnchan, cctrig, &
   ndiv1, ndiv2, CancelFunc, cdir, nbuffer,icounter, ibutton,sec) 
!DEC$ ATTRIBUTES C:: AdcDisk    
CHARACTER*(*) adcdfil,cnchan,cctrig,cdir[REFERENCE]
INTEGER nbyte,ndiv1,ndiv2,ioff,nbuffer, icounter,ibutton [REFERENCE]
real sec
!DEC$ ATTRIBUTES REFERENCE :: sec 
INTEGER :: CancelFunc
END FUNCTION AdcDisk


INTEGER*2 FUNCTION U14Open1401[c,alias:'_U14Open1401@4'](n1401)
!DEC$ ATTRIBUTES DLLIMPORT :: U14Open1401     
INTEGER*2 n1401
!DEC$ ATTRIBUTES VALUE :: n1401   
END FUNCTION U14Open1401

INTEGER*2 FUNCTION U14Close1401[c,alias:'_U14Close1401@4'](oErr)
!DEC$ ATTRIBUTES DLLIMPORT :: U14Close1401     
INTEGER*2 oErr
!DEC$ ATTRIBUTES VALUE :: oErr  
END FUNCTION U14Close1401

INTEGER*2 FUNCTION U14TypeOf1401[c,alias:'_U14TypeOf1401@4'](oErr)
!DEC$ ATTRIBUTES DLLIMPORT :: U14TypeOf1401     
INTEGER*2 oErr
!DEC$ ATTRIBUTES VALUE :: oErr   
END FUNCTION U14TypeOf1401

END INTERFACE
CHARACTER adcfil*30,cnchan*11,cctrig*11,charint*11,adcdfil*70
!INTEGER, POINTER :: CancelFunc => NULL()
INTEGER :: CancelFunc = 1
INTEGER*4 nbyte,ndiv1,ndiv2,ioff

! Widget identifier definitions

integer :: iform1
integer :: iform1MainPanel
integer :: Panel1
integer :: Panel1_1
integer :: Panel1_1_1
integer :: Panel1_2
integer :: Static0
integer :: Static1
integer :: Static1_1 
integer :: Static1_2 
integer :: Static1_3 
integer :: Static1_4 
integer :: Static1_5 
integer :: Static1_6 
integer :: Static1_7 
integer :: Static1_8 
integer :: Static1_9 
integer :: Static1_10 
integer :: Static1_11 
integer :: Static1_12
integer :: Static1_13
integer :: Static1_14

integer :: Edit1_1 
integer :: Edit1_2 
integer :: val1_3 
integer :: Edit1_4 
integer :: val1_6 
integer :: val1_7 
integer :: Edit1_10 
integer :: val1_11 
integer :: val1_25 
integer :: val1_12 
integer :: Toggle1_5(5)
integer :: Toggle1_8(5)
integer :: Button1(5)
integer  Toggle1_9(3)
integer :: Button1_10
integer :: Edit2_2 
integer :: Edit2_3a 
integer :: Edit2_3 
integer :: Edit2_4 
integer :: Edit2_5
integer :: Edit2_6 
integer :: Edit2_7 
integer :: Edit2_8 
integer :: Edit2_9 

integer :: Edit2_12 
integer :: Edit2_13a 
integer :: Edit2_13 
integer :: Edit2_14 
integer :: Edit2_15
integer :: Edit2_16 
integer :: Edit2_17 
integer :: Edit2_18 
integer :: Toggle2_6(2)
integer :: Toggle2_16(2)
integer :: Panel2
integer :: Static2_00
integer :: Static2_01
integer :: Static2_1 
integer :: Static2_2 
integer :: Static2_3 
integer :: Static2_3a 
integer :: Static2_4 
integer :: Static2_5 
integer :: Static2_6 
integer :: Static2_7 
integer :: Static2_8 
integer :: Static2_9 
integer :: Static2_11 
integer :: Static2_12
integer :: Static2_13 
integer :: Static2_14 
integer :: Static2_15 
integer :: Static2_16 
integer :: Static2_17 
integer :: Static2_18 
integer :: Static2_19 

real :: tedit2_2=0.5, tedit2_12=0.5
real :: tedit2_3a=50, tedit2_13a=50
real :: tedit2_3=10, tedit2_13=10
real :: tedit2_4=1.0, tedit2_14=1.0
real :: tedit2_5=100000., tedit2_15=100000.
real :: tedit2_7=1, tedit2_17=1
real :: tedit2_8=1, tedit2_18=1
integer :: graph
type (GLIMIT)    :: limits 
type (GWIDGET)   :: grframe

integer :: Sliderh1
type(GARRAYCELL) arrayattribs
integer :: ISTBAR(32)
integer :: callid
type (GWIDGET) :: Slider_attribs
type (GACTION) :: actlst
character*70 tEdit1_1,tedit1_2,tedit1_4,tedit1_10, itEdit1_1,itedit1_2
character*40 path,pathd
character*80 wdir,cdir,adir
TYPE (FILE$INFO) info
type (Gwidget) :: widget
character*60 iniinifile,inifile,qfile,ifile,testfil
character ndev*3

integer :: iform3
integer :: iform3MainPanel
integer :: Panel3
integer :: Static3_1 
integer :: Static3_2 
integer :: Static3_3 
integer :: Static3_4 
integer :: Static3_5 
integer :: Static3_6
integer :: Button3(2)

integer :: iform4,iform2
integer :: iform4MainPanel
integer :: Panel4
integer :: Static4 
integer :: Button4

integer :: iform5
integer :: iform5MainPanel
integer :: Panel5
integer :: Static5 
integer :: Toggle5(2)

integer :: iform6
integer :: iform6MainPanel
integer :: Panel6
integer :: Static6_1
integer :: Toggle6(2) 

integer :: iform7
integer :: iform7MainPanel
integer :: Panel7
integer :: Static7_1
integer :: Static7_2
integer :: Static7_3
integer :: Static7_4


character*11 cdate,expdate,CNPATCH,prevdate
character*14 ptype(5)
character*24 tapeID
character*6  defname,defname0
character*70 title,messg
character cs*3,adctime*8,qdate*9, dname*2
CHARACTER*30 SDIR,SFILT,SFILE,adcfil0
character*50 PFILE
character*11 cnbyte,cnd1,cnd2
integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
integer*2 idt,inchan,id1,id2,irev,iver
integer*4 ilen
logical good

integer :: iErr
integer*2 oErr,n1401,iType,sErr



logical sampled, start1401,present

common/sendvalue/ICOUNTER,iForm1,iForm1MainPanel,Panel1
  
nbuffer=400000
inchan=1
nchan=1
tval1_25=0.
good=.true.
call DATE(qdate)
   cdate=qdate(1:7)//'20'//qdate(8:9)
   if(qdate(4:6).eq.'JAN') dname='01'
   if(qdate(4:6).eq.'FEB') dname='02'
   if(qdate(4:6).eq.'MAR') dname='03'
   if(qdate(4:6).eq.'APR') dname='04'
   if(qdate(4:6).eq.'MAY') dname='05'
   if(qdate(4:6).eq.'JUN') dname='06'
   if(qdate(4:6).eq.'JUL') dname='07'
   if(qdate(4:6).eq.'AUG') dname='08'
   if(qdate(4:6).eq.'SEP') dname='09'
   if(qdate(4:6).eq.'OCT') dname='10'
   if(qdate(4:6).eq.'NOV') dname='11'
   if(qdate(4:6).eq.'DEC') dname='12'
    cdate=qdate(1:7)//'20'//qdate(8:9)
   		defname=qdate(1:2)//dname//qdate(8:9)

   call TIME(adctime)

 idir=-1
! Initialise Gino, Device & Menu
   call gOpenGino
   call gGuiwin
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)
   call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
 call gDefineRGB(38,1.,0.9,0.7)	!orange
    
	iresult = FULLPATHQQ ('consam.exe', adir)
	nb=nblank1(adir)
	iniinifile=adir(1:nb-10)//'consamini.ini'
	pathd=adir(1:nb-10)
    INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
	if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    INQUIRE(IOLENGTH=LEN) iniiniFILE	
    	    if(nlen.gt.0) then
				read(19,rec=1) inifile
			else
				inifile=adir(1:nb-10)//'consam.ini'
				write(19,rec=1) inifile
			endif			
    else	
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			inifile=adir(1:nb-10)//'consam.ini'
			write(19,rec=1) inifile
    endif
	close(unit=19)
	INQUIRE (FILE=inifile,EXIST=PRESENT)
	if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(inifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			
			OPEN(UNIT=17,FILE=inifile,&
            ACCESS='DIRECT',form='BINARY', RECL=1)
		    INQUIRE(IOLENGTH=LEN) iniFILE		
			if(nlen.gt.0) then
			read(17,rec=1) iverrec
			if(iverrec.eq.101) then
				read(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
			else if(iverrec.eq.100) then
			path=ndev
				read(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen
					iverrec=101
				close(unit=17)
				OPEN(UNIT=17,FILE=inifile,&
				ACCESS='DIRECT',form='BINARY', RECL=1)
		 
				write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
			else
				read(17,rec=1) ndev,npatch,nchan,expdate,title(1:6),tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,prevdate
				iverrec=101
				path=ndev
				close(unit=17)
				OPEN(UNIT=17,FILE=inifile,&
				ACCESS='DIRECT',form='BINARY', RECL=1)
		 
				write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
			endif	
			else
			    ndev=adir(1:3)
				path=ndev
				npatch=1
				ipatch=3
			emem=100.
			temp=23.
			nchan=1
			srate=100000.
			sec=300.
			expdate='Fill date '
			prevdate=qdate
		
			itape=0
			VpA=0.5
			amVpA=50.
			gain=10.
			errfac=1.0
			filt=100000.
			ftape=1
			ifac=1
			VpA1=0.5
			amVpA1=50.
			gain1=10.
			errfac1=1.0
			filt1=100000.
			ftape1=1
			ifac1=1
			title='Fill in title '
			tapeid='Fill in tape details '
				iverrec=101
				irate=srate
					write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
				

			endif			
    else	
			inifile=adir(1:nb-10)//'consam.ini'
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			
			write(19,rec=1) inifile
			close(unit=19)
			OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',FORM='BINARY',RECL=1,IOSTAT=I_VAR)
		
			readini=.true.
			ndev=adir(1:3)
		    path=ndev
			npatch=1
			ipatch=3
			emem=100.
			temp=23.
			nchan=1
			srate=100000.
			irate=srate
			sec=300.
			expdate='Fill date '
			prevdate=qdate
			title='Fill in title '
			tapeID='Fill in tape details '
			itape=1
			VpA=0.5
			amVpA=50.
			gain=10.
			errfac=1.0
			filt=100000.
			ftape=1
			ifac=1
			VpA1=0.5
			amVpA1=50.
			gain1=10.
			errfac1=1.0
			filt1=100000.
			ftape1=1
			ifac1=1
			
			iverrec=101
				write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
    endif
100 continue
   	
	
   	
   if(prevdate.ne.qdate) then
		npatch=1
		prevdate=qdate
			
				write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
			
	
	endif
	close(unit=17)
	iverrec=100
	if(itape.eq.0) then
		expdate=qdate
		tapeid='no tape'
		if(title.eq.' ') title='sampling live'
	endif
		defname=qdate(1:2)//dname//qdate(8:9)
		adcfil=defname//'C'//'.ssd'
		adcfil0=adcfil
		call intconv(npatch,cnpatch)
		nb=nblank1(path)
		IF(Npatch.le.9) THEN
			adcfil=ADCFIL(1:7)//CNPATCH(1:1)//'.SSD'
			qFILE=path(1:nb)//ADCFIL
		ELSE 
			adcfil=ADCFIL(1:6)//CNPATCH(1:2)//'.SSD'
			qFILE=path(1:nb)//ADCFIL
		
		ENDIF

	
	path=ndev
	adcfil0=adcfil
	tedit1_1=expdate
	tedit1_2=tapeID
	tval1_3=npatch
	tedit1_4=title
	tval1_6=emem
	tval1_7=temp
	tval1_11=irate
	tval1_12=sec					
    tedit2_2=VpA
	tedit2_12=VpA1
    
	tedit2_3a=amVpA
	tedit2_13a=amVpA1
    tedit2_3=gain
	tedit2_13=gain1
    tedit2_4=errfac
	tedit2_14=errfac1
    tedit2_5=filt
	tedit2_15=filt1
    tedit2_7=ftape
	tedit2_17=ftape1
    tedit2_8=ifac
	tedit2_18=ifac1
	itedit1_2=qfile
	itedit1_1=inifile
	if(nchan.eq.1) cnchan='0'
	if(nchan.eq.2) cnchan='0 1'
	irate1=irate*nchan		

! Set up master window Form1
	call main_consam(iform1,ipanel1_1,icounter,countval,graph,static1_14,val1_25,tval1_25,iedit1_1,itedit1_1,&
	iedit1_2,itedit1_2,ibutton1,ibutton2,edit1_1,tedit1_1,edit1_2,tedit1_2,edit1_4,tedit1_4,&
	val1_3,tval1_3,val1_6,tval1_6,val1_7,tval1_7,val1_11,tval1_11,val1_12,tval1_12,&
	button1,toggle1_5,toggle1_8,toggle1_9,edit2_2,tedit2_2,edit2_3a,tedit2_3a,edit2_3,tedit2_3,&
	edit2_4,tedit2_4,edit2_5,tedit2_5,edit2_7,tedit2_7,edit2_8,tedit2_8,edit2_12,tedit2_12,&
	edit2_13a,tedit2_13a,edit2_13,tedit2_13,&
	edit2_14,tedit2_14,edit2_15,tedit2_15,edit2_17,tedit2_17,edit2_18,tedit2_18,&
	iform4,button4,iform5,toggle5,iform6,toggle6,iform7)
  
   ptype(1)='Outside-out'
   ptype(2)='Inside-out'	
   ptype(3)='Cell-attached'
   ptype(4)='Whole-cell'
   ptype(5)='Simulated    '

	
	ftape=1.0
	ftape1=1.0

   
   ioff=512
   n1401=0
   oerr=0
   ierr=0
   serr=0
   iall=100
   call form_status(iall,itape,nchan,Edit2_2,Edit2_3,Edit2_3a,Edit2_4,Edit2_5,&
   Edit2_7,Edit2_8,Edit2_12,Edit2_13,Edit2_13a,Edit2_14,Edit2_15,Edit2_17,Edit2_18,toggle1_9,&
	toggle1_8)
   do i=1,5
   call gmSetToggleSwitch(Toggle1_5(i),GOff)
   
   enddo
   if(ipatch.eq.0) ipatch=3
   call gmSetToggleSwitch(Toggle1_5(ipatch),GON)

   
! Start management
   call gmManage
   
! Set required callbacks for forms

! Action loop
1   do while (gmAction(callid) /= -1)
2	continue
	select case(callid)
	case(102) !file name
		 call gmEnqTextSetting(iedit1_2,itedit1_2)
			if(itedit1_2.ne.' ') then
					qfile=itedit1_2
					nb=nblank1(qfile)
					nl=0
					do i=1,nb
						if(qfile(i:i).eq.'\') nl=i
					enddo
					adcfil=qfile(nl+1:nb)
					path=qfile(1:nl)
					
					continue
			else
				status=gmDisplayMessageBox(' ','Please enter file name', &
			                 GSTOP,GOK)
				goto 1
			endif
		case(42)
		  sfile=adcfil
		  if(idir.eq.-1) call gEnqWorkingDir(wdir)
		  idir=1
		  SDIR=ndev(1:3)
		  SFILT='*.SSD'
		  CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GOUTPUT, &
		  gmTitle='Consam files')
		  ns=nblank1(sfile)
		  if (sfile.ne.'     ') then
		   ADCFIL=SFILE
		   nb=nblank1(sdir)
		   if(sdir(nb:nb).eq.'\')then
		    qfile=sDIR(1:Nb)//adcfil
			path=sdir(1:nb)
		  else
		   qfile=sDIR(1:Nb)//'\'//adcfil
		   path=sDIR(1:Nb)//'\'
		  endif
		   
		   call gmsettextsetting(iedit1_2,qfile)
		    itedit1_2=qfile
		  continue
		  endif	
	    case(104) ! change npatch
		tval1_3= gmEnqValueSetting(val1_3)
		if(tval1_3.ne.0) then
	    if(tval1_3.ne.npatch) sampled=.false.  
		npatch=Tval1_3 
		call intconv(npatch,cnpatch) 
		np=nblank1(path)
		if(adcfil(1:6).eq.adcfil0(1:6)) then	
	
			IF(Npatch.le.9) THEN
			adcfil=ADCFIL(1:7)//CNPATCH(1:1)//'.SSD'
			qFILE=path(1:np)//ADCFIL
		
			else
			adcfil=ADCFIL(1:6)//cnpatch(1:2)//'.SSD'
			qFILE=path(1:np)//ADCFIL
		
			ENDIF
		endif
		
		call gmsettextsetting(iedit1_2,qfile)
		call gmdrawwidget(iedit1_2)
		endif
	
		   
		

		case(17) ! save settings
	    call gmEnqTextSetting(edit1_1,expdate)
		call gmEnqTextSetting(edit1_2,tapeid)
		call gmEnqTextSetting(iedit1_2,itedit1_2)
		ndev=itedit1_2(1:3)
		nq=nblank1(itedit1_2)
		do i=1,nq
			if(itedit1_2(i:i).eq.'\') ni=i

		enddo

		path=itedit1_2(1:ni)
		call gmEnqTextSetting(edit1_4,title)
		npatch=gmEnqValueSetting(val1_3)
		  emem=gmEnqValueSetting(val1_6)
		  temp=gmEnqValueSetting(val1_7)
		  irate=gmEnqValueSetting(val1_11)
		  sec=gmEnqValueSetting(val1_12)
		  
		  vpa=gmEnqValueSetting(edit2_2)
		  vpa1=gmEnqValueSetting(edit2_12)
		  amvpa=gmEnqValueSetting(edit2_3a)
		  amvpa1=gmEnqValueSetting(edit2_13a)
		  gain=gmEnqValueSetting(edit2_3)
		  gain1=gmEnqValueSetting(edit2_13)
	      errfac=gmEnqValueSetting(edit2_4) 
		  errfac1=gmEnqValueSetting(edit2_14)
		  
		  filt=gmEnqValueSetting(edit2_5)
		  filt1=gmEnqValueSetting(edit2_15)
		  ftape=gmEnqValueSetting(edit2_7)
		  ftape11=gmEnqValueSetting(edit2_17)
		  ifac=gmEnqValueSetting(edit2_8) 
		  ifac1=gmEnqValueSetting(edit2_18)

		 iverrec=101
		  OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',FORM='BINARY',RECL=1,iostat=i_var)
			 if(i_var.ne.0) then
			 close(unit=17)
				imes=gmdisplaymessagebox('','Invalid path/file name',gstop,gok)
				goto 1
			 endif
			 write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
					 if(i_var.ne.0) continue
		  close(unit=17) 
		case(151:155)
		   status=gmEnqToggleSwitch(Toggle1_5(callid-150))
		   if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_5(callid-150),GOFF)
		   else	
		     if((callid-150).ne.ipatch) sampled=.false.
		     ipatch=callid-150
			 do i=1,5
			    if(i.ne.callid-150) call gmSetToggleSwitch(Toggle1_5(i),GOFF)
			 enddo
		   endif	
		case(181:182)
		   status=gmEnqToggleSwitch(Toggle1_8(callid-180))
		   if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_8(callid-180),GOFF)
		   else	
		     if((callid-180).ne.nchan) sampled=.false.
		     nchan=callid-180
			 
		    if(nchan.eq.1) cnchan='0'
			if(nchan.eq.2) cnchan='0 1'
			 do i=1,2
			    if(i.ne.callid-180) call gmSetToggleSwitch(Toggle1_8(i),GOFF)
			 enddo
		   endif
			
		  call form_status(iall,itape,nchan,Edit2_2,Edit2_3,Edit2_3a,Edit2_4,Edit2_5,&
Edit2_7,Edit2_8,Edit2_12,Edit2_13,Edit2_13a,Edit2_14,Edit2_15,Edit2_17,Edit2_18,toggle1_9,&
	toggle1_8)

		case(191,192,193)
		if(callid.eq.191) then
				iall=100
				call gmSetToggleSwitch(Toggle1_9(2),GOFF)
				call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		else if(callid.eq.192) then
				iall=0
				call gmSetToggleSwitch(Toggle1_9(1),GOFF)
				call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		else
			iall=-1
			calfac=-1.
		    calfac1=-1.
				call gmSetToggleSwitch(Toggle1_9(2),GOFF)
				call gmSetToggleSwitch(Toggle1_9(1),GOFF)
		endif

		 call form_status(iall,itape,nchan,Edit2_2,Edit2_3,Edit2_3a,Edit2_4,Edit2_5,&
Edit2_7,Edit2_8,Edit2_12,Edit2_13,Edit2_13a,Edit2_14,Edit2_15,Edit2_17,Edit2_18,toggle1_9,&
	toggle1_8)

		 	case(261,262)
		    if(callid.eq.261) then
		   		call gmSetToggleSwitch(Toggle1_8(3),GOFF)
				itape=1
				title='Fill in title '
				tapeid='Fill in tape details '
			!	itedit1_2=qfile
				tedit1_2=tapeid
				tedit1_4=title
				expdate='Fill date'
				tedit1_1=expdate
				call gmsettextsetting(edit1_1,tedit1_1)
			!	 call gmsettextsetting(iedit1_2,qfile)
				  call gmsettextsetting(edit1_2,tedit1_2)
				   call gmsettextsetting(edit1_4,tedit1_4)
			else
				itape=0
				title='Fill in title '
				tapeid='no tape'
				tedit1_2=tapeid
				tedit1_4=title
				expdate=qdate
				tedit1_1=expdate
				call gmsettextsetting(edit1_1,tedit1_1)
				call gmsettextsetting(edit1_4,tedit1_4)
				call gmSetToggleSwitch(Toggle1_8(4),GOFF)
				call gmsettextsetting(edit1_2,tedit1_2)
				
			endif
		call form_status(iall,itape,nchan,Edit2_2,Edit2_3,Edit2_3a,Edit2_4,Edit2_5,&
			Edit2_7,Edit2_8,Edit2_12,Edit2_13,Edit2_13a,Edit2_14,Edit2_15,Edit2_17,Edit2_18,toggle1_9,&
	toggle1_8)
		case(41,101)
		    if(callid.eq.41) then
			ifile=inifile
			sfilt='*.INI'
			CALL gmFileBrowser(iFILE,aDIR,SFILT,gmType=GINPUT, gmTitle='Ini files')
			if(ifile.eq.' ') then
			ifile=inifile 
			else	
			nb=nblank1(adir)
			
		    if(adir(nb:nb).eq.'\')then
				inifile=aDIR(1:Nb)//ifile
			else
				inifile=aDIR(1:Nb)//'\'//ifile
			endif
			endif
		        call gmsettextsetting(iedit1_1,inifile)
				itedit1_1=inifile
			
			else
				
		  call gmEnqTextSetting(iedit1_1,itedit1_1)
			if(itedit1_1.ne.' ') then
					inifile=itedit1_1

			else
				status=gmDisplayMessageBox(' ','Please enter inifile name', &
			                 GSTOP,GOK)
				goto 1
			endif

			endif
			if(ifile.ne.' ') then 
			open(unit=19,file=iniinifile,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
					write(19,rec=1) inifile
						close(unit=19)
			
			INQUIRE (FILE=iniFILE,EXIST=PRESENT)
			if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(inifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			
			OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',form='BINARY', RECL=1,iostat=i_var) 
			if(i_var.ne.0) continue
		    INQUIRE(IOLENGTH=LEN) iniFILE		
			if(nlen.gt.0) then
			read(17,rec=1,iostat=i_var) iverrec
			if(iverrec.eq.101) then
				read(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
				 if(i_var.ne.0) continue
			else if(iverrec.eq.100) then
				iverrec=101
				read(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen

				 if(i_var.ne.0) continue
				 close(unit=17)
				 	OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',form='BINARY', RECL=1,iostat=i_var) 
			if(i_var.ne.0) continue
		  
				write(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
			 if(i_var.ne.0) continue
			else
				iverrec=101
				read(17,rec=1,iostat=i_var) ndev,npatch,nchan,expdate,title(1:6),tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,prevdate
				 if(i_var.ne.0) continue
				 close(unit=17)
				 	OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',form='BINARY', RECL=1,iostat=i_var) 
			if(i_var.ne.0) continue
		  
				write(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
			 if(i_var.ne.0) continue
			endif
			 	if(itape.eq.0) expdate=qdate
	tedit1_1=expdate
	tedit1_2=tapeID
	tval1_3=npatch
	tedit1_4=title
	tval1_6=emem
	tval1_7=temp
	tval1_11=irate
	tval1_12=sec					
    tedit2_2=VpA
	tedit2_12=VpA1
    
	tedit2_3a=amVpA
	tedit2_13a=amVpA1
    tedit2_3=gain
	tedit2_13=gain1
    tedit2_4=errfac
	tedit2_14=errfac1
    tedit2_5=filt
	tedit2_15=filt1
    tedit2_7=ftape
	tedit2_17=ftape1
    tedit2_8=ifac
	tedit2_18=ifac1
	itedit1_2=qfile
	itedit1_1=inifile
	if(nchan.eq.1) cnchan='0'
	if(nchan.eq.2) cnchan='0 1'
	irate1=irate*nchan	
		 call gmsettextsetting(edit1_1,expdate)
		 call gmsettextsetting(edit1_2,tapeID)
		 call gmsettextsetting(edit1_4,title)
		 call gmSetValueSetting(val1_3,tval1_3)
		 call gmSetValueSetting(val1_6,tval1_6)
		 call gmSetValueSetting(val1_7,tval1_7)
		 call gmSetValueSetting(val1_11,tval1_11)
	     call gmSetValueSetting(val1_12,tval1_12)
		 call intconv(npatch,cnpatch)
		   call gmSetValueSetting(Edit2_2,vpa)
			
		
			 call gmSetValueSetting(Edit2_12,vpa1)
			call gmSetValueSetting(Edit2_3a,amvpa)
			call gmSetValueSetting(Edit2_3,gain)
			call gmSetValueSetting(Edit2_4,errfac)
			call gmSetValueSetting(Edit2_5,filt)
			call gmSetValueSetting(Edit2_7,ftape)
			call gmSetValueSetting(Edit2_8,ifac)
			
		
				call gmSetValueSetting(Edit2_13a,amvpa1)
				call gmSetValueSetting(Edit2_13,gain1)
				call gmSetValueSetting(Edit2_14,errfac1)
				call gmSetValueSetting(Edit2_15,filt)
				call gmSetValueSetting(Edit2_17,ftape1)
				call gmSetValueSetting(Edit2_18,ifac1)
				qfile=ndev//adcfil
					call gmsettextsetting(iedit1_2,qfile)
		if(itape.eq.0) then
		call gmSetToggleSwitch(Toggle1_8(3),GON)
		call gmSetToggleSwitch(Toggle1_8(4),GOff)
		itogon=1
		call gmSetToggleSwitch(Toggle1_5(3),GON)
		call gmSetToggleSwitch(Toggle1_8(1),GON)
		call gmSetToggleSwitch(Toggle1_9(1),GON)
		iall=100
		call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
		call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
		if(nchan.eq.2) then
			call gmSetWidgetStatus(Edit2_12,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
		endif	
	else
		call gmSetToggleSwitch(Toggle1_8(4),GON)
		call gmSetToggleSwitch(Toggle1_8(3),Goff)
	endif
	
			else
			call gmEnqTextSetting(edit1_1,expdate)
		call gmEnqTextSetting(edit1_2,tapeid)
			call gmEnqTextSetting(edit1_4,title)
		  npatch=gmEnqValueSetting(val1_3)
		  emem=gmEnqValueSetting(val1_6)
		  temp=gmEnqValueSetting(val1_7)
		  irate=gmEnqValueSetting(val1_11)
		  sec=gmEnqValueSetting(val1_12)
		  
		  vpa=gmEnqValueSetting(edit2_2)
		  vpa1=gmEnqValueSetting(edit2_12)
		  amvpa=gmEnqValueSetting(edit2_3a)
		  amvpa1=gmEnqValueSetting(edit2_13a)
		  gain=gmEnqValueSetting(edit2_3)
		  gain1=gmEnqValueSetting(edit2_13)
	      errfac=gmEnqValueSetting(edit2_4) 
		  errfac1=gmEnqValueSetting(edit2_14)
		  
		  filt=gmEnqValueSetting(edit2_5)
		  filt1=gmEnqValueSetting(edit2_15)
		  ftape=gmEnqValueSetting(edit2_7)
		  ftape11=gmEnqValueSetting(edit2_17)
		  ifac=gmEnqValueSetting(edit2_8) 
		  ifac1=gmEnqValueSetting(edit2_18)
		iverrec=101
				write(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
			
				if(i_var.ne.0) continue
			endif			
		else	
			call gmEnqTextSetting(edit1_1,expdate)
		    call gmEnqTextSetting(edit1_2,tapeid)
			call gmEnqTextSetting(edit1_4,title)
		  npatch=gmEnqValueSetting(val1_3)
		  emem=gmEnqValueSetting(val1_6)
		  temp=gmEnqValueSetting(val1_7)
		  irate=gmEnqValueSetting(val1_11)
		  sec=gmEnqValueSetting(val1_12)
		  
		  vpa=gmEnqValueSetting(edit2_2)
		  vpa1=gmEnqValueSetting(edit2_12)
		  amvpa=gmEnqValueSetting(edit2_3a)
		  amvpa1=gmEnqValueSetting(edit2_13a)
		  gain=gmEnqValueSetting(edit2_3)
		  gain1=gmEnqValueSetting(edit2_13)
	      errfac=gmEnqValueSetting(edit2_4) 
		  errfac1=gmEnqValueSetting(edit2_14)
		  
		  filt=gmEnqValueSetting(edit2_5)
		  filt1=gmEnqValueSetting(edit2_15)
		  ftape=gmEnqValueSetting(edit2_7)
		  ftape11=gmEnqValueSetting(edit2_17)
		  ifac=gmEnqValueSetting(edit2_8) 
		  ifac1=gmEnqValueSetting(edit2_18)

			OPEN(UNIT=17,FILE=iniFILE,&
            ACCESS='DIRECT',FORM='BINARY',RECL=1,iostat=i_var)
			if(i_var.ne.0) continue
			readini=.true.
			iverrec=101
				write(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
				if(i_var.ne.0) continue
        endif
		close(unit=17)
			
		endif
		
		 
		case(11) ! reset
		 sampled=.false.  
		 call gmSetTextSetting(edit1_1,'')	
		 call gmSetTextSetting(edit1_2,'')
		 call gmSetValueSetting(val1_3,0.)	
		 call gmSetTextSetting(edit1_4,'')
		 call gmSetValueSetting(val1_6,0.)	
		 call gmSetValueSetting(val1_7,0.)
		 call gmSetValueSetting(val1_11,0.)	
		 call gmSetValueSetting(val1_12,0.)	
		 call gmSetValueSetting(icounter,0.)			
		 
		do i=1,5
			call gmSetToggleSwitch(Toggle1_5(i),GOFF)
		enddo
		do i=1,2
			call gmSetToggleSwitch(Toggle1_8(i),GOFF)
		enddo
		do i=1,3
			call gmSetToggleSwitch(Toggle1_9(i),GOFF)
		enddo
		
		   
		    call gmSetValueSetting(Edit2_2,0.)
			
			
			call gmSetValueSetting(Edit2_12,0.)
			
			
		 	
		   
			call gmSetValueSetting(Edit2_3a,0.)
			call gmSetValueSetting(Edit2_3,0.)
			call gmSetValueSetting(Edit2_4,0.)
			call gmSetValueSetting(Edit2_5,0.)
			call gmSetValueSetting(Edit2_7,0.)
			call gmSetValueSetting(Edit2_8,0.)
			
			
				call gmSetValueSetting(Edit2_13a,0.)
				call gmSetValueSetting(Edit2_13,0.)
				call gmSetValueSetting(Edit2_14,0.)
				call gmSetValueSetting(Edit2_15,0.)
				call gmSetValueSetting(Edit2_17,0.)
				call gmSetValueSetting(Edit2_18,0.)
			
				
		  
	 Static1_14 = gmCreateTextEntry(Panel1_1, 10, 1, 14, 1,'Fill in/check experimental details', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
		call gFillRect(GSOLID,GWHITE,limits)
		call gFlushGraphics
		case(12)
		!	CALL WINDRA(FORM6)
		!	CALL ACTWIN(FORM6)
		 callid=61
		 goto  2  
		case(13,14) !sampling
		good=.true.
		call gmActivateGraphicsFrame(graph)
          call gFillRect(GSOLID,GWHITE,limits)
	      
		  CALL gmSetValueSetting(ICOUNTER,0.) 
		  CALL gmSetValueSetting(val1_25,0) 
          call gFlushGraphics
		   
		   call gmEnqTextSetting(iedit1_1,itedit1_1)
			
			if(itedit1_1.ne.' ') then
			  !  if(itedit1_1.ne.inifile) then
			    INQUIRE (FILE=itedit1_1,EXIST=PRESENT)
				if(.not.present) then
					OPEN(UNIT=17,FILE=inifile,&
					ACCESS='DIRECT',form='BINARY', RECL=1,iostat=i_var)
					if(i_var.ne.0) then
						close(unit=17)
						imes=gmdisplaymessagebox('','Invalid path/file name',gstop,gok)
			
						goto 1
					else
							write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
						close(unit=17)

					endif
				endif
			  !  endif
				inifile=itedit1_1
			
			else
				status=gmDisplayMessageBox(' ','Please enter inifile name', &
			                 GSTOP,GOK)
				goto 1
			endif
            
		    call gmEnqTextSetting(iedit1_2,itedit1_2)
			if(itedit1_2.eq.' '.or.itedit1_2.eq.'Fill in name') then
				status=gmDisplayMessageBox(' ','Please enter file name (including path)', &
			                 GSTOP,GOK)
				goto 1
			else
				!	if(qfile.ne.itedit1_2) then
						nbl=nblank1(itedit1_2)
						nl=0
						do i=1,nbl
							if(itedit1_2(i:i).eq.'\') nl=i
						enddo
						testfil=itedit1_2(1:nl)//'test.ssd'
						OPEN(UNIT=16,FILE=testfil,&
						ACCESS='DIRECT',form='BINARY', RECL=1,iostat=i_var)
						if(i_var.ne.0) then
							close(unit=16)
							imes=gmdisplaymessagebox('','Invalid path/file name',gstop,gok)
			
							goto 1
						else
							close(unit=16,status='DELETE')
						endif
				!	endif
					qfile=itedit1_2
					nb=nblank1(qfile)
					nl=0
					do i=1,nb
						if(qfile(i:i).eq.'\') nl=i
					enddo
					adcfil=qfile(nl+1:nb)
					path=qfile(1:nl)
					continue
					if(nl.eq.0) then 
						    qfile=ndev//adcfil
							path=ndev
							call gmsettextsetting(iedit1_2,qfile)
							call gmdrawwidget(iedit1_2)
					endif
					
			endif

		tedit2_5= gmEnqValueSetting(edit2_5)
		if(tedit2_5.ne.0) then
			  if(tedit2_5.ne.filt) sampled=.false.
			  filt=tedit2_5
			  calfac=-1
		      if(nchan.eq.2) then
				tedit2_15= gmEnqValueSetting(edit2_15)
		        if(tedit2_15.ne.0) then
					if(tedit2_15.ne.filt1) sampled=.false.
					filt1=tedit2_15
					calfac1=-1
				else
					status=gmDisplayMessageBox(' ','Please enter filter setting', &
			                 GSTOP,GOK)
					goto 1
				endif
			  endif
		else
				status=gmDisplayMessageBox(' ','Please enter filter setting', &
			                 GSTOP,GOK)
				goto 1
		endif
		if(iall.eq.100) then		  		  
			  val= gmEnqValueSetting(edit2_2)
			  if(val.ne.0.) then
		        if(val.ne.tedit2_2) sampled=.false.
			    tedit2_2=val
		        VpA=tedit2_2
			    amVpA=VpA*1000.		!mV (at 1401) per pA
			    pAV=1.0/VpA			!pA per volt
	            calfac=pAV/6553.6	!
              else
			   status=gmDisplayMessageBox(' ','Please enter calibration', &
			                 GSTOP,GOK)
				goto 1
			  endif
           	  if(nchan.eq.2) then
				val= gmEnqValueSetting(edit2_12)
				if(val.ne.0) then
				    if(val.ne.tedit2_12) sampled=.false.
					tedit2_12=val
					VpA1=val
					amVpA1=VpA1*1000.		!mV (at 1401) per pA
					pAV1=1.0/VpA1			!pA per volt
					calfac1=pAV1/6553.6	!=
				else
					status=gmDisplayMessageBox(' ','Please enter calibration', &
			                 GSTOP,GOK)
					goto 1
				endif
			  endif
			
		else
			i=ifixr(amVpA)
			val= gmEnqValueSetting(edit2_3a)
			if(val.ne.0) then
		     if(val.ne.tedit2_3a) sampled=.false.
			 tedit2_3a=val
		     amVpA=val
            else
				status=gmDisplayMessageBox(' ','Please enter clamp setting', &
			                 GSTOP,GOK)
				goto 1
			endif
			val= gmEnqValueSetting(edit2_3)
			if(val.ne.0) then
		     if(val.ne.tedit2_3) sampled=.false.
			 tedit2_3=val
		     gain=val
            else
				status=gmDisplayMessageBox(' ','Please enter amplifier gain', &
			                 GSTOP,GOK)
				goto 1
			endif
			istatus1=gmEnqToggleSwitch(Toggle1_8(4)) 
			istatus2=gmEnqToggleSwitch(Toggle1_8(3))
			if(istatus1.eq.GOFF.and.istatus2.eq.GOFF) then
				status=gmDisplayMessageBox(' ','Please enter cosaming way', &
			                 GSTOP,GOK)
				goto 1
			else 
				if(istatus1.eq.GON) itape=1
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_7)
					if(val.ne.0.) then
						if(val.ne.tedit2_7) sampled=.false.
						tedit2_7=val
						ftape=val
					else
					status=gmDisplayMessageBox(' ','Please enter tape (V peak)', &
			                 GSTOP,GOK)
						goto 1
					endif
				endif
			    val= gmEnqValueSetting(edit2_4)
			    if(val.ne.0) then
					if(val.ne.tedit2_4) sampled=.false.
					tedit2_4=val
					errfac=val
					VPa=amVpA/1000.		!V (from clamp)/pA
					pAV=1.0/VpA		!pA per V (from clamp)
					pAV=pAV*ftape	!correct for tape
					pAV=pAV/gain	
					pAV=pAV/errfac	!pA per Volt in computer
					calfac=pAV/6553.6	
                else
				status=gmDisplayMessageBox(' ','Please enter error factor', &
			                 GSTOP,GOK)
					goto 1
				endif
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_8)
					if(val.ne.0) then
						if(val.ne.tedit2_8) sampled=.false.
						tedit2_8=val
						ifac=val
						tapfac=float(ifac)
					else
						status=gmDisplayMessageBox(' ','Please enter tape speed factor', &
			                 GSTOP,GOK)
						goto 1				
					endif
               	endif
			endif
			if(nchan.eq.2) then
			i=ifixr(amVpA1)
			val= gmEnqValueSetting(edit2_13a)
			if(val.ne.0) then
		     if(val.ne.amVpA1) sampled=.false.
		     amVpA1=val
            else
			status=gmDisplayMessageBox(' ','Please enter clamp setting', &
			                 GSTOP,GOK)
				goto 1
			endif
			val= gmEnqValueSetting(edit2_13)
			if(val.ne.0) then
		     if(val.ne.gain1) sampled=.false.
		     gain1=val
            else
				status=gmDisplayMessageBox(' ','Please enter amplifier gain', &
			                 GSTOP,GOK)
				goto 1
			endif
			istatus1=gmEnqToggleSwitch(Toggle1_8(4)) 
			istatus2=gmEnqToggleSwitch(Toggle1_8(3))
			if(istatus1.eq.GOFF.and.istatus2.eq.GOFF) then
				goto 1
			else 
				if(istatus1.eq.GON) itape=1
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_17)
					if(val.ne.0.) then
						if(val.ne.tedit2_17) sampled=.false.
						tedit2_17=val
						ftape1=val
					else
					status=gmDisplayMessageBox(' ','Please enter tape recorder (V peak)', &
			                 GSTOP,GOK)
						goto 1
					endif
				endif
			    val= gmEnqValueSetting(edit2_14)
			    if(val.ne.0) then
					if(val.ne.errfac1) sampled=.false.
					errfac1=val
					VPa1=amVpA1/1000.		!V (from clamp)/pA
					pAV1=1.0/VpA1	!pA per V (from clamp)
					pAV1=pAV1*ftape1	!correct for tape
					pAV1=pAV1/gain1	
					pAV1=pAV1/errfac1	!pA per Volt in computer
					calfac1=pAV1/6553.6	
                else
				status=gmDisplayMessageBox(' ','Please enter error factor', &
			                 GSTOP,GOK)
					goto 1
				endif
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_18)
					if(val.ne.0) then
						if(val.ne.ifac1) sampled=.false.
						ifac1=val
						tapfac1=float(ifac1)
					else
					status=gmDisplayMessageBox(' ','Please enter tape speed factor', &
			                 GSTOP,GOK)
						goto 1				
					endif
               	endif
			endif
			endif
		endif
		
		call gmEnqTextSetting(edit1_1,tedit1_1)
		  if(tedit1_1.ne.'    ')  then
		    if(tedit1_1(1:11).ne.expdate) sampled=.false.
			expdate=tedit1_1(1:11) 
		    call gmEnqTextSetting(edit1_2,tedit1_2)
		    if(tedit1_2.ne.'   ') then
			  if(tedit1_2(1:24).ne.tapeid) sampled=.false.
			  tapeid=tedit1_2(1:24)
		      tval1_3= gmEnqValueSetting(val1_3)
		      if(tval1_3.ne.0) then
			    if(tval1_3.ne.npatch) sampled=.false.  
		        npatch=Tval1_3 	
		        call gmEnqTextSetting(edit1_4,tedit1_4)
		        if(tedit1_4.ne.'   ') then
				  if(tedit1_4.ne.title) sampled=.false.
		           
				  title=tedit1_4
			      tval1_6= gmEnqValueSetting(val1_6)
		          if(tval1_6.ne.0) then
				    if(tval1_6.ne.emem) sampled=.false.
					Emem=tval1_6
					tval1_7= gmEnqValueSetting(val1_7)
		            if(tval1_7.ne.0) then
					  if(tval1_7.ne.temp) sampled=.false.
					  Temp=tval1_7
                      tval1_11= gmEnqValueSetting(val1_11)
		              if(tval1_11.ne.0) then
					    if(tval1_11.ne.irate) sampled=.false.
						irate=tval1_11
                        tval1_12= gmEnqValueSetting(val1_12)
		                if(tval1_12.ne.0) then
						if(tval1_12.ne.sec) sample=.false.
					    sec=tval1_12
						if(ipatch.ne.0) then
						  if(nchan.ne.0) then
						    
						    nosel=-1
			                do k=1,3
							  istatus=gmEnqToggleSwitch(toggle1_9(k))
			                  if(istatus.eq.1) nosel=1
		                    enddo	
				            if(nosel.eq.1) then
		                       
							   STATUS=GYESBUTTON
							   if(sampled) then
							    sampled=.false.
								call gmSetValueSetting(icounter,0.0)
								
								call gFillRect(GSOLID,GWHITE,limits)
								call gFlushGraphics
                              !  status=gmDisplayMessageBox(' ','Do you want to repeat the same sequence ?', GQUESTION,GYESNO)
                	
		                       endif
		                       IF(STATUS.EQ.GYESBUTTON) THEN
								if(callid.eq.13) cctrig='HT'
								if(callid.eq.14) cctrig='H'
								srate=float(irate)
		 						call sampleinfo(iForm1,iform3,iform3MainPanel,Panel3, &
								nchan,irate,sec,nbyte,ndiv1,ndiv2,srate,srate1,ndiv,ifac,ftape,itape,pathd)
								
								if(itape.eq.0) then
									callid=32
									goto 2
                                endif
								 
							   ELSE
									status=gmDisplayMessageBox(' ','Press RESET and restart', GINFORMATION,GOK)
                	
							   ENDIF
	
							else
							   status=gmDisplayMessageBox(' ','Please enter calibration',&
			                   GSTOP,GOK)
							endif
				
				          else
							 status=gmDisplayMessageBox(' ','Please enter number of channels',&
			                 GSTOP,GOK)
		                  endif
                        else
						   status=gmDisplayMessageBox(' ','Please enter patch type', &
			                 GSTOP,GOK)
						endif
                      else
					   status=gmDisplayMessageBox(' ','Please enter sample duration', &
			                 GSTOP,GOK)
					  endif  
                    else
					 status=gmDisplayMessageBox(' ','Please enter sampling rate', &
			                 GSTOP,GOK)
					endif
                  else
				   status=gmDisplayMessageBox(' ','Please enter temperature', &
			                 GSTOP,GOK)
				  endif
                else
				 status=gmDisplayMessageBox(' ','Please enter membrane potential', &
			                 GSTOP,GOK)
				endif
              else
			   status=gmDisplayMessageBox(' ','Please enter patch name', &
			                 GSTOP,GOK)
			  endif
			else
			 status=gmDisplayMessageBox(' ','Please enter patch number', &
			                 GSTOP,GOK)
			endif 
		  else
		    status=gmDisplayMessageBox(' ','Please enter tape details', &
			                 GSTOP,GOK) 
		  endif
		  else
		    status=gmDisplayMessageBox(' ','Please enter date', &
			                 GSTOP,GOK)
		  endif  	
		 
	
	

		case(31)
		if(itape.eq.1) then
		  CALL WINREM(iForm3)
	      CALL ACTWIN(ifORM1)
		  endif

		case(32)
		  ioff=512
          n1401=0
		  oerr=0
		  ierr=0
          serr=0
          if(itape.eq.1) then
		  CALL WINREM(iForm3)
	      CALL ACTWIN(ifORM1)
		  endif
		  ilen=nbyte
	        idt=ndiv1*ndiv2
	        id1=int2(ndiv1)
	        id2=int2(ndiv2)  
		  open(unit=17,FILE=iniFILE,&
      access='DIRECT',form='BINARY', RECL=1,iostat=i_var)
	  if(i_var.ne.0) continue
	  write(17,rec=1,iostat=i_var) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
		
			
				if(i_var.ne.0) continue
		  
		  close(unit=17)
		
		  adcdfil=qfile
		  oErr=U14Open1401(n1401)
          if(oErr.eq.0) then
	        START1401=.TRUE.
	        iType = U14TypeOf1401(oErr)	  
	        if(itype.eq.0) then
				messg='1401 STANDARD '
				nbuffer=400000
	        else if(itype.eq.1) then
				messg='1401 PLUS '
				nbuffer=400000
			else
				messg='1401 Power '
				nbuffer=1000000
			endif
			sErr=U14Close1401(oErr)
		!	IF(NBUFFER.GT.NBYTE) NBUFFER=NBYTE
		  else
			select case(oErr)
			case (-500)
	     		messg='1401 SWITCHED OFF ; PLEASE SWITCH ON'
	   		case (-501)
	     		messg='1401 NOT CONNECTED: CHECK AND RESTART'
			case (-502)
	     		messg='1401 ILL ( CALL THE DOCTOR ? ): '// &
         		'SWITCH OFF/ON 1401 AND REBOOT '
	   				sErr=U14Close1401(oErr)
			case (-503)
	     		messg='I/F CARD MISSING: CHECK AND RESTART'
					sErr=U14Close1401(oErr)
	   		case (-505)
	     		messg='I/F CARD BAD SWITCHES: '// &
          		'CHECK AND RESTART '
					sErr=U14Close1401(oErr)
	   		case (-506)
	     		messg='1401+ FAILED TO COME READY : '// &
          		'SWITCH OFF/ON 1401 AND REBOOT'
					sErr=U14Close1401(oErr)
			case (-508)
	     		messg='1401 IS ALREADY IN USE : PLEASE WAIT'
	            sErr=U14Close1401(%val(oErr))
				call gTimeDelay(3000) 
	            		
	   		case (-509)
	     		messg='COULD NOT GET DMA CHANNELS : SWITCH OFF/ON &
         		1401 AND REBOOT'
					sErr=U14Close1401(oErr)
			case(-580)
	     		messg='NOT 386 ENHANCED MODE: CHECK AND RESTART'
					sErr=U14Close1401(oErr)
	   		case(-581)
	     		messg='NO DEVICE DRIVER : CHECK AND RESTART'
					sErr=U14Close1401(oErr)
			case(-582)
	     		messg='DEVICE DRIVER TOO OLD : NO COMMENTS !'
					sErr=U14Close1401(oErr)
			end select

			messg='SORRY:'//MESSG
			START1401=.FALSE.
			status=gmDisplayMessageBox(' ',messg, GINFORMATION,GOK)
		
!			stop
          endif
		 
		  if(oErr.eq.0) then
		 
		  isec=int(sec)
          limits%xmin=0.
		 ! limits%xmax=sec
		  limits%xmax=0.5*float(nbyte)
		  limits%ymin=0.
		  limits%ymax=1.
		  call gmSetGraphicsFrameBounds(graph,limits)

		!  call gmEnqWidgetInfo(graph,grframe)
        !
		  call gmActivateGraphicsFrame(graph)
          call gFillRect(GSOLID,GWHITE,limits)
	      call gsetlinecolour(GBLACK)
          call gmoveto2d(0.,0.)
          call gdrawlineto2d(0.5*float(nbyte),0.)
  
          call gdrawlineto2d(0.5*float(nbyte),1.)
  
          call gdrawlineto2d(0.,1.)
   
          call gdrawlineto2d(0.,0.)
		  x=0.
		  dvalue=0.05*float(nbyte)
		  do while(x.le.nbyte/2-dvalue)
             x=x+dvalue
		     call gmoveto2d(x,0.)
		     call gdrawlineto2d(x,0.3)
		  enddo
		  
		  CALL gmSetValueSetting(ICOUNTER,0.) 
		  CALL gmSetValueSetting(val1_25,nbyte/2) 
          call gFlushGraphics
			clock=4000000.
  		 	inchan=int2(nchan)
		  	nlp=nblank1(adcdfil)
	        PFILE=ADCDFIL(1:nlp-4)//'.txt'
	        OPEN(unit=8,file=PFILE,status='UNKNOWN', &
            access='APPEND',form='FORMATTED',IOSTAT=I_VAR)
			 if(i_var.ne.0) continue
            REWIND(unit=8,IOSTAT=I_VAR)
			 if(i_var.ne.0) continue
		    WRITE(8,800,IOSTAT=I_VAR) 
			 if(i_var.ne.0) continue
			WRITE(8,801,IOSTAT=I_VAR) cdate,adctime
			 if(i_var.ne.0) continue
	        WRITE(8,802,IOSTAT=I_VAR)
			 if(i_var.ne.0) continue
			IF(IALL.NE.100)THEN
               write(8,803,IOSTAT=I_VAR) amVpA,ftape,gain
			    if(i_var.ne.0) continue
            endif
			 write(8,804,IOSTAT=I_VAR) pAV,calfac
			  if(i_var.ne.0) continue
            IF(NCHAN.EQ.2) THEN
		      IF(IALL.NE.100)THEN
			  write(8,803,IOSTAT=I_VAR) amVpA1,ftape1,gain1
			   if(i_var.ne.0) continue
		      ENDIF
		      write(8,804,IOSTAT=I_VAR) pAV1,calfac1
			   if(i_var.ne.0) continue
	        ENDIF
	        IF(ITAPE.EQ.1.AND.IFAC.NE.1) THEN
				WRITE(8,805,IOSTAT=I_VAR) IFAC,SRATE,FILT
				 if(i_var.ne.0) continue
				if(nchan.eq.2) then
					write(8,806,IOSTAT=I_VAR) filt1
					 if(i_var.ne.0) continue
				endif
			ENDIF
			write(8,807,IOSTAT=I_VAR) irate,clock/float(ndiv),ndiv,&
			ndiv1*ndiv2,ndiv1,ndiv2,srate,inchan
			 if(i_var.ne.0) continue
			nint=ifixr(sec*srate)
			write(8,808,IOSTAT=I_VAR) filt,nint,sec
			 if(i_var.ne.0) continue
			if(nchan.eq.2) then
				write(8,809,IOSTAT=I_VAR) irate,nchan,irate1,clock/float(ndiv),&
				ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
	 if(i_var.ne.0) continue
				write(8,810,IOSTAT=I_VAR) filt,filt1,nint,sec
				 if(i_var.ne.0) continue
			endif
			WRITE(8,811,IOSTAT=I_VAR)
			 if(i_var.ne.0) continue
			write(8,812) title,expdate,tapeid,npatch,ptype(ipatch),emem,temp
			 if(i_var.ne.0) continue
			CLOSE(unit=8,IOSTAT=I_VAR)
			 if(i_var.ne.0) continue
		  status=gmDisplayMessageBox(' ','SAMPLE NOW ?', GINFORMATION,GOKCANCEL)
		  
		  if(status.eq.gokbutton) then
		   call gmsettextsetting(static1_14,'SAMPLING NOW!')

		    
		  idirerr=gSetWorkingDir('C:')
		  ibutton=0
		 ! iErr = AdcDisk(adcdfil, ioff, nbyte, cnchan, cctrig, ndiv1, ndiv2, &
		  !       CancelFunc,cdir, nbuffer, icounter, ibutton,sec)
		  iflen=ierr+ioff	
		  call intconv(iflen,charint)
		  if(iErr.gt.0.and.iErr.le.nbyte) then
		  sampled=.true.
		    clock=4000000.
		    iver=1002
		   ! ilen=nbyte+ioff
		    ilen=ierr
	        idt=ndiv1*ndiv2
	        id1=int2(ndiv1)
	        id2=int2(ndiv2)  
			inchan=int2(nchan)
			   call intconv(iflen,charint)
			    COUNTVAL=0.5*FLOAT(IERR)
				CALL GSETSTRJUSTIFY(GCENTRE)
				CALL gmSetValueSetting(ICOUNTER,COUNTVAL) 
				
 				sampled=.true.
				limits%xmax=0.5*float(ierr)
				limits%ymin=0.5
				call gFillRect(GSOLID,GRED,limits) 
			 call gFlushGraphics
		!	 status=gmDisplayMessageBox(' ','Done: File length='//charint, GINFORMATION,GOK)
		   
 			INQUIRE (FILE=adcdfil,EXIST=PRESENT)
			i_var=0
			if(PRESENT) then
            OPEN(unit=14,file=ADCDFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
            IF(I_VAR.EQ.0) THEN
	        WRITE(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
	        expdate,defname,tapeID,ipatch,npatch,Emem,temp
	        if(i_var.ne.0) continue
	        else
			!	imes=gmdisplaymessagebox('','error - doggy message',ginformation,gok)
				i_var=0
				WRITE(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
	        expdate,defname,tapeID,ipatch,npatch,Emem,temp
	        if(i_var.ne.0) continue
			ENDIF
			CLOSE(unit=14)
			endif
		    OPEN(unit=8,file=PFILE,access='APPEND',form='FORMATTED')
            REWIND(unit=8)
		    WRITE(8,800) 
800			format('CONSAM - Continuous sampling on disk')
		    WRITE(8,801) cdate,adctime
801	        format('Date of analysis: ',a11,' Time of analysis: ',a8)
	        WRITE(8,802)
802         FORMAT('CALIBRATION')	
            IF(IALL.NE.100)THEN
               write(8,803) amVpA,ftape,gain
803            format(' Clamp setting (mV/pA)= ',g13.6,/, &
               '  tape (V peak) = ',g13.6,/, &
               '  amplifier gain= ',g13.6)
	        ENDIF
	        write(8,804) pAV,calfac
804	        format( &
            ' Calibration factor (pA per Volt in computer)= ',g13.6,/, &
            ' Calibration factor (pA per ADC unit        )= ',g13.6,/)
            IF(NCHAN.EQ.2) THEN
		      IF(IALL.NE.100)THEN
			  write(8,803) amVpA1,ftape1,gain1
		      ENDIF
		      write(8,804) pAV1,calfac1
	        ENDIF
	        IF(ITAPE.EQ.1.AND.IFAC.NE.1) THEN
				WRITE(8,805) IFAC,SRATE,FILT
805				FORMAT(' Tape slowed ',i3,' times,so effective sampling rate (Hz)=',&
				f13.6,/,'effective filter setting for ADC0(-3dB, Hz) =',f13.6)
				if(nchan.eq.2) then
					write(8,806) filt1
806					format('effective filter setting for ADC1 (-3dB,hz) =',f13.6)
				endif
			ENDIF
			write(8,807) irate,clock/float(ndiv),ndiv,&
			ndiv1*ndiv2,ndiv1,ndiv2,srate,inchan
807			format(&
			' Requested sample rate (Hz) = ',i8,/,&
			' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,&
			' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,&
			' Actual sample rate (Hz) = ',g15.8,/,&
			' Channels = ',i8)
			nint=ifixr(sec*srate)
			write(8,808) filt,nint,sec
808			format(' Filter (-3db, Hz) for ADC0 = ',f10.1,/,&
			' Sample of ',i9,' points on ADC0',/,&
			' Sample duration (sec) = ',f12.2)
			if(nchan.eq.2) then
				write(8,809) irate,nchan,irate1,clock/float(ndiv),&
				ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
	
809				format(&
				' Requested sample rate (Hz) = ',i8,/,&
				' Sample rate needed for ',i2,' channels(Hz) = ',i8,/,&
				' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,&
				' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,&
				' Actual total sample rate (Hz) = ',g15.8,/,&
				' Actual sample rate per channel (Hz) = ',g15.8)

				write(8,810) filt,filt1,nint,sec
810				format(&
				' Filter (-3db, Hz) for ADC0 = ',f10.1,/,': for ADC1 = ',f10.1,/,&
				' Samples of ',i9,' points on ADC0 and ADC1',/,&
				' Sample duration (sec) = ',f12.2,/,' Title:')
			endif
			WRITE(8,811)
811			FORMAT('EXPERIMENT')
			write(8,812) 	title,expdate,tapeid,npatch,ptype(ipatch),emem,temp
812			format(&
			' Patch title :',a70,/,&
			' Date :',a11,/,&
			' Tape details :',a24,/,&
			' Patch number :',i5,/,&
			' Patch type :',a14,/,&
			' Membrane potential [mV] :',g10.2,/,&
			' Temperature (Celsius):',g10.2)
			if(ierr.gt.0.and.ierr.lt.nbyte) then
				write(8,813) float(ierr)/2.
813				format( ' Sample stopped at: ',g10.2)
			endif
		
			CLOSE(unit=8)
			if(ierr.eq.nbyte) then
   		     Static1_14 = gmCreateTextEntry(Panel1_1, 10, 1, 14, 1,'Sampling finished! File saved as:'//qfile, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
				 else if(ierr.gt.0.and.ierr.lt.nbyte) then
		             Static1_14 = gmCreateTextEntry(Panel1_1, 10, 1, 14, 1,'Sampling ended by "esc" key. File saved as:'//qfile, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
			  endif
			open(unit=17,file=inifile,access='DIRECT',form='BINARY', RECL=1,IOSTAT=i_var)
			if(i_var.ne.0) continue
			if(itape.eq.0) npatch=npatch+1
			tval1_3=npatch
				 write(17,rec=1) iverrec,ndev,npatch,nchan,expdate,title,tapeID,ipatch,Emem,temp,&
				irate,sec,itape,VpA,amVpA,gain,errfac,filt,ftape,ifac,calfac,&
				VpA1,amVpA1,gain1,errfac1,filt1,ftape1,ifac1,calfac1,prevdate,&
				idt,id1,id2,srate,idt,ioff,ilen,path
			 if(i_var.ne.0) continue
			close(unit=17)
			call intconv(npatch,cnpatch)
		
		
			if(adcfil(1:6).eq.adcfil0(1:6)) then	
		    nl=nblank1(qfile)
			IF(Npatch.le.9) THEN
			adcfil=ADCFIL(1:7)//CNPATCH(1:1)//'.SSD'
			qfile=qfile(1:nl-5)//CNPATCH(1:1)//'.SSD'
		    
			else
			adcfil=ADCFIL(1:6)//cnpatch(1:2)//'.SSD'
			qFILE=qfile(1:nl-6)//CNPATCH(1:2)//'.SSD'
		    itedit1_2=qfile
			ENDIF
			 call gmsettextsetting(iedit1_2,qfile)
			 call gmsetvaluesetting(val1_3,tval1_3)
			endif
			
		  endif
		  if(iErr.eq.nbyte) then
		   
		 
			else 
		  
			    messg='UNKNOWN'	   
		  select case(iErr)
			case (-540)
	     		messg='COMMAND FILE NOT FOUND'              
	   		case (-1000)
	     		messg='CREATE FILE FAILED'                    
			case (-1001)
	     		messg='BAD WRITE TO FILE'                   
         	
	   		case (-1002)
	     		messg='BAD SEEK'                           
	   		case (-1003)
	     		messg='SAMPLING TOO FAST'          
          	
	   		case (-1004)
	     		messg='SAMPLING ABORTED'                
          		
			case (-1005)
	     		messg='BAD PARAMETER'                       
	            
	   		case (-541)
	     		messg='ERROR READING COMMAND FILE'
			case(-542)
	     		messg='UNKNOWN COMMAND'                         
	   		case(-560)
	     		messg='TOHOST/1401 FAILED'                  
			case(-543)
	     		messg='NOT ENOUGH HOST SPACE' 
			             
			end select

		     call intconv(ierr,charint)
			 nm=nblank1(messg)
!			 messg=messg(1:nm)//'Error='//charint

			 status=gmDisplayMessageBox(' ',messg, GINFORMATION,GOK)
		     Static1_14 = gmCreateTextEntry(Panel1_1, 10, 1, 14, 1,'SAMPLING ABORTED ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
			if(ibutton.gt.0.and.ibutton.le.nbyte) then
			clock=4000000.
		    iver=1002
		   ! ilen=nbyte+ioff
		    ilen=ibutton
	        idt=ndiv1*ndiv2
	        id1=int2(ndiv1)
	        id2=int2(ndiv2)  
			inchan=int2(nchan)
			   call intconv(iflen,charint)
			    COUNTVAL=0.5*FLOAT(Ibutton)
				CALL GSETSTRJUSTIFY(GCENTRE)
				CALL gmSetValueSetting(ICOUNTER,COUNTVAL) 
				
 				sampled=.true.
				limits%ymin=0.5
				limits%xmax=0.5*float(ibutton)
				call gFillRect(GSOLID,GRED,limits) 
			 call gFlushGraphics
		!	 status=gmDisplayMessageBox(' ','Done: File length='//charint, GINFORMATION,GOK)
		   
 			INQUIRE (FILE=adcdfil,EXIST=PRESENT)
			i_var=0
			if(PRESENT) then
            OPEN(unit=14,file=ADCDFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
            IF(I_VAR.EQ.0) THEN
	        WRITE(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
	        expdate,defname,tapeID,ipatch,npatch,Emem,temp
	        if(i_var.ne.0) continue
	        else
			!	imes=gmdisplaymessagebox('','error - doggy message',ginformation,gok)
				i_var=0
				WRITE(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
	        expdate,defname,tapeID,ipatch,npatch,Emem,temp
	        if(i_var.ne.0) continue
			ENDIF
			CLOSE(unit=14)
			endif
			nlp=nblank1(adcdfil)
	        PFILE=ADCDFIL(1:nlp-4)//'.txt'
	        OPEN(unit=8,file=PFILE,status='UNKNOWN', &
            access='APPEND',form='FORMATTED')
            REWIND(unit=8)
		    WRITE(8,800) 
			WRITE(8,801) cdate,adctime
	        WRITE(8,802)
            IF(IALL.NE.100)THEN
               write(8,803) amVpA,ftape,gain
              
	        ENDIF
	        write(8,804) pAV,calfac
            IF(NCHAN.EQ.2) THEN
		      IF(IALL.NE.100)THEN
			  write(8,803) amVpA1,ftape1,gain1
		      ENDIF
		      write(8,804) pAV1,calfac1
	        ENDIF
	        IF(ITAPE.EQ.1.AND.IFAC.NE.1) THEN
				WRITE(8,805) IFAC,SRATE,FILT
				if(nchan.eq.2) then
					write(8,806)
				endif
			ENDIF
			write(8,807) irate,clock/float(ndiv),ndiv,&
			ndiv1*ndiv2,ndiv1,ndiv2,srate,inchan
			nint=ifixr(sec*srate)
			write(8,808) filt,nint,sec
			if(nchan.eq.2) then
				write(8,809) irate,nchan,irate1,clock/float(ndiv),&
				ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
	
				write(8,810) filt,filt1,nint,sec
			endif
			WRITE(8,811)
			write(8,812) 	title,expdate,tapeid,npatch,ptype(ipatch),emem,temp
			if(ibutton.gt.0.and.ibutton.lt.nbyte) then
				write(8,813) float(ibutton)/2.
			endif
			CLOSE(unit=8)
			
			endif
		  endif
		  endif
		  endif
		case(40)
		  CALL WINREM(iform4)
	      CALL ACTWIN(0)
		case(51)
		  CALL WINREM(iform5)
		  CALL ACTWIN(0)
		  
          call gmsettextsetting(static1_14,'SAMPLING CANCELLED ')

				CancelFunc=1
		case(52)
			CALL WINREM(iform5)
			CALL ACTWIN(0)
		case(61)
			CALL WINREM(iform6)
			CALL ACTWIN(0)
			call Exit
		case(62)
			CALL WINREM(iform6)
			CALL ACTWIN(0)
        case(100)
			
			CALL WINDRA(iform7)
		case(200)
			call conshelp(iform1)	
	end select
! Code to handle callbacks

   end do


! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

stop
end


