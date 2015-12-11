	subroutine pdub(bool,shist,iscan,prnt)
	logical bool,shist,prnt,discprt
	common/dp/discprt
c iscan=100 is new SCAN
	if(iscan.eq.-1003.or.iscan.ge.100) then
	   if(bool.and.prnt) write(7,50)
	   if(bool.and.discprt) write(7,50)
c	   if(bool.and.shist)print 50
50	   format(' Values with ''dubious'' amplitudes excluded')
	else
	   if(bool.and.prnt) write(7,26)
	   if(bool.and.discprt) write(7,26)
c	   if(bool.and.shist)print 26
26	   format(' Values with ''assumed'' amplitudes excluded')
	endif
	return
	end

