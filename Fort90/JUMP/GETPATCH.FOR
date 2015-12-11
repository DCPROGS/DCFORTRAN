	subroutine GETPATCH(ivhclamp,ipatch,ffilt,ivhclam1,ipatch1,ffilt1,
     &  setpatch,sampatch)
	integer*2 ivhclamp,ipatch
	integer*2 ivhclam1,ipatch1
	character ptype*14,UC*1,ans*1
	logical setpatch,sampatch
c To get values missing from CJUMP.DAT
c (now called by READ14)
c Input ivhclam1,ipatch1,ffilt1 = values just read from disk
c       ivhclamp,ipatch,ffilt = default values (and output)
c Modif 03/24/95 11:13am so that if sampatch is true on entry, uses
c the current values of ivhclamp,ipatch,ffilt (sampatch should initially
c be false -may be set true here, after values supplied)
c
	if(sampatch) RETURN	!use current ipatch etc
c
	setpatch=(ipatch1.lt.1.or.ipatch1.gt.5).or.
     &  (ivhclam1.lt.-250.or.ivhclam1.gt.250).or.
     &  (ffilt1.lt.1.0.or.ffilt1.gt.25000.)
c
	if(.not.setpatch) then
	   ipatch=ipatch1
	   ivhclamp=ivhclam1
	   ffilt=ffilt1
	else			!ask for values!
	   if(ipatch.eq.1) then
		ptype='outside-out   '
	   else if(ipatch.eq.2) then
		ptype='inside-out    '
	   else if(ipatch.eq.3) then
		ptype='cell-attached '
	   else if(ipatch.eq.4) then
		ptype='whole-cell    '
	   else if(ipatch.eq.5) then
		ptype='simulated data'
	   else
		ptype='              '
	   endif
c
	   call BELL(3)
	   print 4
4	   format(' CJUMP data incomplete: please specify!')
c	   print 3,ptype,ivhclamp,ffilt
c3	   format(' Patch type = ',a14,/,
c     &	' Holding potential set on patch clamp (mV) = ',i4,/,
c     &	' Filter frequency (-3dB) (Hz) = ',f9.1,/,
c     &	' O.K. [Y] ? ')
c	   read 101,ans
c101	   format(a1)
c	   if(UC(ans).ne.'N') then
c		RETURN		!return with setpatch=true to write back to disk
c	   endif
c
111	   print 311,ipatch
311	   format(
     &    ' Enter patch type:',/,
     &    ' (1) outside-out',/,
     &    ' (2) inside-out',/,
     &    ' (3) cell-attached',/,
     &    ' (4) whole-cell',/,
     &    ' (5) simulated data',/,
     &    '   Option number [',i3,'] = ')
	   i=int4(ipatch)
	   call INPUTi(i)
	   if(i.lt.1.or.i.gt.5) goto 111
	   ipatch=int2(i)
c
	   i=0
	   print 1,i
1	   format(' Holding potential set on patch clamp [',i2,'] = ')
	   call INPUTi(i)
	   ivhclamp=int2(i)
c
	   print 2,ffilt
2	   format(' Filter frequency (-3dB) (Hz) [',f8.1,'] = ')
	   call INPUTr(ffilt)
c
	   call DCASK(
     &' Use these values for all subsequent patches in this run',
     &   'y',ans)
	   sampatch=ans.eq.'Y'
	endif
c
	RETURN 	!with setpatch=true to write to disk
	end
