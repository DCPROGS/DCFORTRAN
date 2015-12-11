	subroutine falsev1(tres,fc,rms,avamp,frate)
c Version for EKDIST/new SCAN (avamp, rms already in pA)
c Dec 87: modif so fc rather than jfilt in call
c To calc false event rate (per sec) in EKDIST (from RESINT)
c First calc 'threshold' as amp attained by pulse of length=tres
c (in ms)
	!call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c	more difficult to track underflow,etc:
c
c
c
	use dflib
	INTERFACE TO SUBROUTINE FPRESET[C, ALIAS:'__fpreset']
      END
      INTERFACE TO INTEGER*2 FUNCTION CLEARfp[C, ALIAS:'_clearfp']
      END

      INTEGER*2 CLEARFP
!	real*8 tres
	integer(2) jstatus
	
c		call getstatusfpqq(jstatus)
c			if((jstatus.and.fpsw$overflow)>0) then
c				errtext='overflow: please enter new values!'
c				status=gmDisplayMessageBox(' ',errtext, GSTOP,GOK)
c				goto 1	
c			else if((jstatus.and.fpsw$underflow)>0) then
c				errtext='underflow: please enter new values!'	
c				status=gmDisplayMessageBox(' ',errtext, GSTOP,GOK)
c				goto 1	
c			else if((jstatus.and.fpsw$zerodivide)>0) then
c				errtext='divide by zero; please enter new values!'
c				status=gmDisplayMessageBox(' ',errtext, GSTOP,GOK)
c				goto 1			
c			endif
	call getstatusfpqq(jstatus)
	if((jstatus.and.fpsw$underflow)>0) call fpreset()
	w=tres*1.e-3			!sec
	u=erfs(2668.*fc*w)
	amp=avamp			!full amp (pA)
	phi=u*amp			!'threshold' (pA)
	var=(rms)**2		!noise variance (pA)**2
c Calc rate from C & Sigworth eq. 9, with k=1
	frate=1000.*fc*exp(-(phi*phi)/(2.*var))
	call fpreset()
	return
	end


