	subroutine pdub(bool,shist,iscan,prnt)
	logical bool,shist,prnt,discprt
	common/dp/discprt
c
	if(iscan.ne.-1003.and.bool.and.prnt) write(7,26)
	if(iscan.ne.-1003.and.bool.and.discprt) write(8,26)
c	if(iscan.ne.-1003.and.bool.and.shist)print 26
26	format(' Values with ''assumed'' amplitudes excluded')
	if(iscan.eq.-1003.and.bool.and.prnt) write(7,50)
	if(iscan.eq.-1003.and.bool.and.discprt) write(8,50)
c	if(iscan.eq.-1003.and.bool.and.shist)print 50
50	format(' Values with ''dubious'' amplitudes excluded')
	return
	end

