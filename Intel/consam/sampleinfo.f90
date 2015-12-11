subroutine sampleinfo(iForm1,iForm3,iForm3MainPanel,Panel3,nchan,irate,sec, &
   nbyte,ndiv1,ndiv2,srate,srate1,ndiv,ifac,ftape,itape,path)
	use gino_f90
	use menu_f90
    Integer :: iform3
    INTEGER :: iform1
    Integer :: iform3MainPanel
    Integer :: Panel3
    Integer :: Static3_1 
    Integer :: Static3_2 
    integer :: Static3_3 
    integer :: Static3_4 
    integer :: Static3_5 
    integer :: Static3_6
	integer :: Static3_7
    integer :: Static3_8
   character*40 path
    integer :: Button3(2)
	integer :: nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
	character*11 cndiv,cndiv1,cndiv2,cnd1,cirate,cirate1,cnchan
	character*11 csrate,csrate1,cprate,CHARN,csecs,cnbyte,cnint
    k=1
    I_VAR=0
	npa=nblank1(path)
	OPEN(unit=11,file=path(1:npa)//'PRIMES.DAT', access='DIRECT',form='BINARY', RECL=1,IOSTAT=I_VAR)

	READ(11,REC=1,IOSTAT=I_VAR) (NPRIME(K), K=1,1900)
	IF(I_VAR.NE.0) THEN
	 !  CALL INTCONV(I_VAR,CHARN)
	  ! Istatus=gmDisplayMessageBox(' ',CHARN, &
	!		                 GSTOP,GOK)
	continue
    ENDIF
    CLOSE(unit=11)
	if(itape.eq.1) then
		srate=ifac*srate
		if(ifac.ne.1) then
				status=gmDisplayMessageBox(' ','Filtered after taping ?', &
										GQUESTION,GYESNO)
				if(status.eq.GYESBUTTON) then
					filt=ifac*filt
					filt1=ifac*filt1
				endif
		endif
				     
	endif
	irate1=irate*nchan		!actual ADC rate
	rate=float(irate1)
	nclock=4000000	!Hz for 'H' parameter
	clock=float(nclock)
	div=clock/rate
	ndiv=IFIXR(div)
	errmin=1.e35
	do i=1,1900
	  np=nprime(i)	!1st 1900 primes (up to 16381) (inc the 1st=2)
	  ndiv1=np
	  ndiv2=ndiv/np
	  if(ndiv2.eq.1) EXIT	!must be=>2, so use vals with smallest error so far
	  ndtry=ndiv1*ndiv2
	  if(ndtry.eq.ndiv) EXIT		!exact solution found
	  error=float(ndtry)/float(ndiv)
	  if(error.lt.1.0) error=1.0/error
	  error=error-1.0
	  if(error.lt.errmin) then
	   errmin=error
	   imin=i		!record index for least error
	  endif
    enddo
    if(ndtry.ne.ndiv) then
	  ndiv1=nprime(imin)		!most precise factors found
	  ndiv2=ndiv/ndiv1
	endif
    nd1=ndiv1*ndiv2
	d1=float(ndiv1*ndiv2)		!actual divisor
	srate1=clock/d1    		!actual sample rate
	srate=srate1/float(nchan)	!return 1-channel sample rate
	prate=clock/float(ndiv)
    nint=ifixr(sec*srate)
	secs=float(nint)/srate
	nbyte=2*nint
	nbyte=nbyte*nchan

    call intconv(ndiv,cndiv)
	call intconv(ndiv1,cndiv1)
	call intconv(ndiv2,cndiv2)
	call intconv(nd1,cnd1)
	call intconv(irate,cirate)
	call intconv(irate1,cirate1)
	call intconv(nchan,cnchan)
	call realtoch(srate,csrate,11)
	call realtoch(srate1,csrate1,11)
    call realtoch(prate,cprate,11)
	call intconv(nint,cnint)
	call intconv(nbyte,cnbyte)
	call realtoch(secs,csecs,11)
	
	if(itape.eq.1) then
									
!
!	SAMPLE RATE IMFORMATION
!
! Set up complex dialogue box Form3 child of Form1
   iform3 = gmCreateComplexDialogueBox(iform1, 15, 9, 24, 10, GMINIMIZE, 'Sampling rate information', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1077')

! Create main panel for form
   iform3MainPanel=gmCreatePanel(iform3, 0, 0, 24, 10, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel3 child of Form3MainPanel
   Panel3 = gmCreatePanel(iform3MainPanel, 0, 0, 24, 10, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_1 child of Panel3
   Static3_1 = gmCreateTextEntry(Panel3, 1, 0, 22, 1,&
                'Requested sample rate is '//cirate, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_2 child of Panel3
   Static3_2 = gmCreateTextEntry(Panel3,  1, 1, 22, 1,&
                'Sample rate needed for '//cnchan//' channel (Hz) = '//&
	            cirate1, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_3 child of Panel3
   Static3_3 = gmCreateTextEntry(Panel3, 1, 2, 22, 1, &
                'Nearest possible sample rate is '//cprate//' Hz (n='//cndiv//')',&
				32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_4 child of Panel3
   Static3_4 = gmCreateTextEntry(Panel3, 1, 3, 22, 1,&
                'Nearest actual divisor (n1 x n2) is '//cnd1//&
				' (n1='//cndiv1//', n2='//cndiv2//')', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_5 child of Panel3
   Static3_5 = gmCreateTextEntry(Panel3, 1, 4, 22, 1,&
                'Actual total sample rate is '//Csrate1//' Hz', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_6 child of Panel3
   Static3_6 = gmCreateTextEntry(Panel3, 1, 5, 22, 1,&
               'Actual sample rate per channel is '//Csrate//' Hz', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3_6 child of Panel3
   Static3_7 = gmCreateTextEntry(Panel3, 1, 6, 22, 1,&
               'Lengths of data='//Cnint//' values/channel, (bytes = '//cnbyte//' )', &
			    32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

				! Create text entry Static3_6 child of Panel3
   Static3_8 = gmCreateTextEntry(Panel3, 1, 7, 22, 1,&
               'Duration = '//csecs//' seconds', &
			   32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button3_1 child of Panel3
   Button3(1) = gmCreatePushButton(Panel3, 17, 8, 3, 1, 'Reset', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=31)

! Create button Button3_2 child of Panel3
   Button3(2) = gmCreatePushButton(Panel3, 20, 8, 3, 1, 'Continue', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=32)
	CALL WINDRA(iform3)
									CALL ACTWIN(iform3)
								 endif
   sec=secs
   return
   end