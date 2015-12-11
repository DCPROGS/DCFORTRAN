Program repair_consam
USE DFLIB
use gino_f90
use menu_f90

character*11 cdate,expdate,CNPATCH,prevdate
character*14 ptype(5)
character*24 tapeID

character*6  defname,defname0,cctrig
character*70 title,messg
character cs*3,adctime*8,qdate*9, dname*2
CHARACTER*30 adcfil,wdir,adcfil1
character*50 SDIR,SFILE,PFILE
character*40 :: sfilt='*.ssd'//char(124)//'consam files (ssd)'//&
	char(124)//'*.dat'//char(124)//'Data files (dat)'
character*11 cnbyte,cnd1,cnd2
integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
integer*2 idt,inchan,id1,id2,irev,iver
ALLOCATABLE::idata
integer*2 idata(:)
integer*4 ilen,iedit(10),ival(20)
character*20 itextst(10),itextsv(20)
character*60 tedit(10)
character*3 ndev
real tval(20)
logical present
TYPE (FILE$INFO) info

	call gOpenGino
	call gGuiwin
	call gmInitializeMenu
	call gmSetGuiGridMode(GON)
! Start management
idir=-1
itextst(1)='file name:'
itextst(2)='title'
itextst(3)='cdate'
itextst(4)='adctime'

itextst(5)='cctrig'
itextst(6)='expdate'
itextst(7)='defname'
itextst(8)='tapeID'

itextsv(1)='file length'

itextsv(2)='iver'
itextsv(3)='idt'
itextsv(4)='ioff'
itextsv(5)='id1'
itextsv(6)='id2'
itextsv(13)='calfac'
itextsv(14)='filt'
itextsv(15)='calfac1'
itextsv(16)='filt1'
itextsv(17)='inchan'
itextsv(12)='emem'
itextsv(9)='temp'
itextsv(8)='npatch'
itextsv(7)='ipatch'

itextsv(10)='ilen'
itextsv(11)='srate'
sdir='.'
!	SFILT='*.SSD'//char(124)//'Consam Files (SSD)'//char(124)//'*.dat'//char(124)//'Data Files (DAT)'
		
   iForm = gmCreateMasterWindow(4, 4, 38, 30, GALL, 'Restauration of consam file header (Copyright 2005 D. Colquhoun & I. Vais)', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1005')

! Create main panel for form
   iPanel=gmCreatePanel(iForm, 0, 0, 38, 30, &
              	gmHpos=GLEFT, gmVpos=Gmiddle, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
  
iStatic = gmCreateTextEntry(iPanel, 1, 1, 3, 1,itextst(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	iEdit(1) = gmCreateTextEntry(iPanel, 4, 1, 17, 1,tedit(1), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=1)
	iButton = gmCreatePushButton(iPanel, 22, 1, 5, 1, 'Browse', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=55)
   do i=2,8
   iStatic = gmCreateTextEntry(iPanel, 1, 3*(i-1)+2, 3, 1,itextst(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	iEdit(i) = gmCreateTextEntry(iPanel, 4, 3*(i-1)+2, 12, 1,tedit(i), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=i)
   enddo
   ist=gmCreateTextEntry(iPanel, 4,9 , 12, 1,'[consam date;i.e: 20-FEB-2005]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 4,12 , 12, 1,'[experiment time;i.e.: 14:20:33]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 4,18 , 12, 1,'[experiment date;i.e.: 10-FEB-05]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 4,21 , 12, 1,'[i.e.: 100205]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

	iStatic = gmCreateTextEntry(iPanel, 29, 1, 3, 1,itextsv(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(1) = gmCreateValueEntry(iPanel, 32, 1, 5, 1,tval(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=11)
   do i=2,9
   iStatic = gmCreateTextEntry(iPanel, 19, 3*(i-1)+2, 3, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(i) = gmCreateValueEntry(iPanel, 22, 3*(i-1)+2, 5, 1,tval(i), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=10+i)
   enddo
   iStatic = gmCreateTextEntry(iPanel, 29, 5, 3, 1,itextsv(10), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(10) = gmCreateValueEntry(iPanel, 32, 5, 5, 1,tval(10), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=20)
 do i=11,16
   iStatic = gmCreateTextEntry(iPanel, 29, 3*(i-10)+5, 3, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(i) = gmCreateValueEntry(iPanel, 32, 3*(i-10)+5, 5, 1,tval(i), 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=10+i)
   enddo
    ist=gmCreateTextEntry(iPanel, 19,9 , 8, 1,'[idt=4000000/srate;i.e.: 40]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
				  ist=gmCreateTextEntry(iPanel, 19,21 , 8, 1,'[i.e.: 3 (for cell-attached)]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
				  ist=gmCreateTextEntry(iPanel, 19,24 , 8, 1,'[patch number;i.e.: 4]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 29,9 , 8, 1,'[sample rate;i.e.: 100000]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 29,12 , 8, 1,'[membrane potential;i.e.: 100]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ist=gmCreateTextEntry(iPanel, 29,6 , 8, 1,'[ilen=file length-ioff;i.e.: 400000]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 ist=gmCreateTextEntry(iPanel, 29,15 , 8, 1,'[calfac=1./(VpA*6553.6);', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
				ist=gmCreateTextEntry(iPanel, 32,16 , 8, 1,'i.e.: 0.000305175]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ist=gmCreateTextEntry(iPanel, 29,18 , 8, 1,'[Filter;i.e.: 10000', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel, 29, 26, 3, 1,itextsv(17), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(17) = gmCreateValueEntry(iPanel, 32, 26, 5, 1,tval(17), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=27)

    iButton1 = gmCreatePushButton(iPanel, 4, 26, 6, 1, 'Save changes', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=51)
	call gmsetwidgetstatus(ibutton1,GUNSELECTABLE)
	 iButton2 = gmCreatePushButton(iPanel, 10, 26, 6, 1, 'Quit', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=52)
! Set required callbacks for forms
	call gmManage
! Action loop
1   do while (gmAction(icallid) /= -1)
2		continue
		select case(icallid)
		case(1,55)
			if(icallid.eq.55) then
				sfile=adcfil
				if(idir.eq.-1) then 
					call gEnqWorkingDir(wdir)
					idir=1
					SDIR=(wdir)
				endif
				sfile='*.ssd'
				CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
				gmTitle='Consam files')
				if (sfile.ne.'     ') then
					ADCFIL=SFILE
				
		 			call gmsettextsetting(iedit(1),sfile)
					tedit(1)=sfile
				else
					goto 1
				endif	
			else
				call gmenqtextsetting(iedit(1),sfile)
				if (sfile.ne.'     ') then
					ADCFIL=SFILE
					tedit(1)=sfile
				else
					goto 1
				endif
			endif
			INQUIRE (FILE=adcfil,EXIST=PRESENT)
			i_var=0

			if(PRESENT) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(adcfil, info, ihandle)
				nLEN=info%length
				nint=(nlen-512)/2
				OPEN(unit=14,file=ADCFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				IF(I_VAR.EQ.0) continue
				READ(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
				id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
				expdate,defname,tapeID,ipatch,npatch,Emem,temp
				
				close(unit=14)
				tedit(1)=adcfil
				tedit(2)=title
				tedit(3)=cdate
				tedit(4)=adctime
				if(cctrig(1:3).ne.'H'.or.cctrig(1:3).ne.'HT') cctrig='H'
				tedit(5)=cctrig
				tedit(6)=expdate
				tedit(7)=defname
				tedit(8)=tapeID

				tval(1)=nlen
				if(iver.ne.1002) iver=1002
				tval(2)=iver
				tval(3)=idt
				if(ioff.ne.512) ioff=512
				tval(4)=ioff
				tval(5)=id1
				tval(6)=id2
				tval(7)=ipatch
				tval(8)=npatch
				tval(9)=temp

				tval(10)=ilen
				tval(11)=srate
				tval(13)=calfac
				tval(14)=filt
				tval(12)=emem

				tval(15)=filt1
				tval(16)=calfac1
				if(inchan.ne.1.or.inchan.ne.2) inchan=1
				tval(17)=inchan
				do i=2,8
				call gmsettextsetting(iedit(i),tedit(i))
				enddo
				do i=1,17
				call gmsetvaluesetting(ival(i),tval(i))
				enddo
			
				call gmsetwidgetstatus(ibutton1,GSELECTABLE)

			else
				goto 1	
			endif
	case(2:8)
	case(11:27)
	case(51)
		do i=2,8
			call gmenqtextsetting(iedit(i),tedit(i))
		
		enddo
		
		do i=2,17
			tval(i)=gmenqvaluesetting(ival(i))
		enddo
	
				title=tedit(2)
				cdate=tedit(3)
				adctime=tedit(4)
				
				cctrig=tedit(5)
				expdate=tedit(6)
				defname=tedit(7)
				tapeID=tedit(8)

				iver=tval(2)
				idt=tval(3)
				ioff=tval(4)
				id1=tval(5)
				id2=tval(6)
				ipatch=tval(7)
				npatch=tval(8)
				temp=tval(9)

				ilen=tval(10)
				srate=tval(11)
				calfac=tval(13)
				filt=tval(14)
				emem=tval(12)                  

				filt1=tval(15)
				calfac1=tval(16)
				inchan=tval(17)
				CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Consam files')
				if (sfile.ne.'     ') then
					ADCFIL1=SFILE
				OPEN(unit=15,file=ADCFIL1,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				IF(I_VAR.EQ.0) continue
				write(15,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
				id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
				expdate,defname,tapeID,ipatch,npatch,Emem,temp
				if(i_var.ne.0) continue
				if(adcfil.ne.adcfil1) then
				open(unit=14,file=ADCFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				
				if(allocated(idata)) deallocate(idata)
				allocate(idata(nint))
			    read (14,REC=513,IOSTAT=I_VAR) (idata(i),i=1,nint)
				close(unit=14)
			     
			
				write(15,REC=513,IOSTAT=I_VAR) (idata(i),i=1,nint)
				endif
				iStatics = gmCreateTextEntry(iPanel, 1, 3, 12, 1,'Saved as   :'//sfile, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=5, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

		 			
				close(unit=15)		
				else
					goto 1
				endif
				
	
				
	case(52)
		goto 3
	end select
	end do

3   continue
! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino
	deallocate(idata)
stop
end
