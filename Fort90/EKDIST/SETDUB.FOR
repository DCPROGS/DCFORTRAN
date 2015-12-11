	subroutine SETDUB(iscan,exass,shist,prnt,tc,trise,tcfac)
	logical exass,shist,prnt
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c If called with tc=-1. then does only first part (does not ask
c whether to exclude short ones)
101	format(A1)
108	format(/)
2021	if(iscan.lt.0.and.iscan.ne.-1003) print 44
44	format(' Exclude ''assumed'' amplitudes? ')
	if(iscan.eq.-1003.or.iscan.ge.100) print 441
441	format(' Exclude ''dubious'' amplitudes [Y] ? ')
	read 101,ans
	exass=UC(ans).ne.'N'
	call pdub(exass,shist,iscan,prnt)
	if(tc.lt.-0.5) RETURN
c
	print 40
40	format('&Exclude amplitudes of short events [Y] ? ')
	read 101,ans
	tc=-1.		!do not exclude if neg
	if(UC(ans).eq.'N') goto 41
	call RISTIM(trise,-1.,prnt)
232	print 42,tcfac
42	format(
     & ' -exclude events shorter than f risetimes (0 to calc more)',
     & ' [',f5.1,']  f = ')
	call INPUTr(tcfac)
	if(tcfac.eq.0.) then
	   print 231
231	   format(' length/trise= ')
	   call INPUTr(tc)
	   call RISTIM(trise,tcfac,prnt)
	   goto 232
	else
	   tc=tcfac*trise*1.e-3	!ms
	endif
c
41	continue
	if(prnt.and.tc.gt.0.) write(7,43) tc,tc*1.e3/trise
	if(discprt.and.tc.gt.0.) write(8,43) tc,tc*1.e3/trise
43	format(/,
     & ' Amplitudes of events shorter than ',f8.4,' ms(',f6.1,
     & ' rise times) excluded',/)
	return
	end		!of SETDUB



