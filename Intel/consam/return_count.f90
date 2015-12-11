subroutine return_count(ndone,icounter,ibutton,sec,nbyte,icancel)

use menu_f90
USE DFLIB
USE DFWIN

real*4 sec [VALUE]
integer*4 ndone [VALUE]
integer*4 nbyte [VALUE]
integer*4 icounter [VALUE]
integer*4 ibutton [VALUE]
integer*4 icancel [REFERENCE]
integer*2 istat
CHARACTER*1 CHA
type (GLIMIT)    :: limits 
type (GACTION) :: actlst

	CHA=GETCHARQQ()
	IF(CHA.EQ.'q'.OR.CHA.EQ.'Q') ICANCEL=1
	istat=GetAsyncKeyState(13)
	if(icounter.ne.-1) then
		point=0.5*float(ndone)
		CALL GSETSTRJUSTIFY(GCENTRE)
		CALL gmSetValueSetting(ICOUNTER,point) 
		limits%xmax=0.5*float(ndone)
		limits%ymin=0.5
		limits%xmin=0.
		limits%ymax=1.
		call gFillRect(GSOLID,GRED,limits)
		call gFlushGraphics
	endif
	
!	status=gmDisplayMessageBox(' ','Stop', &
!			                 GQUESTION,GYESNO)	
!   
!   if (STATUS.EQ.GYESBUTTON) then
!	icancel=1
!   endif
!	icall=gmAction(icall)
!	if(icall.eq.15.or.icall.eq.-15) icancel=1
!	CALL GMENQACTIONSTATE(ACTLST)
!	call gmDefineKeyCallback(27,10027)
!	if(actlst%key.eq.541.or.actlst%callbk.eq.15.or.actlst%callbk.eq.-15) then
!	endif
		
end