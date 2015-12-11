	subroutine FINDOPEN(in,j,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
c To find next (good) opening in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good opening. Ibad=0 normally, but ibad=1 if end of data reached without
c finding a good opening
c Modified 06/09/02 06:02pm so that nbo,nbg accumulate bad openings, gaps
	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical open,burst,chsvec,good,btest,badend
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10),badend(10)
c
	ibad=0
	do 1 i=in,nint(j)
	   iop=i
	   open=ampl(i,j).ne.0.
c=	   if(open.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	   good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	   if(.not.good) then
		if(open) then
		   nbo=nbo+1
		else
		   nbg=nbg+1
		endif
	   endif
	   if(.not.good) ibad=ibad+1
	   if(open.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=-1
	RETURN
	end

	subroutine FINDGAP(in,j,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
c To find next (good) gap in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good gap. Ibad=0 normally, but ibad=-1 if end of data reached without
c finding a good gap
c Modified 06/09/02 06:02pm so that nbo,nbg accumulate bad openings, gaps

	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical shut,burst,chsvec,good,btest,badend
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10),badend(10)
c
	ibad=0
	do 1 i=in,nint(j)
	   iop=i
	   shut=ampl(i,j).eq.0.
c=	   if(shut.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	   good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	   if(.not.good) then
		if(shut) then
		   nbg=nbg+1
		else
		   nbo=nbo+1
		endif
	   endif
	   if(shut.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=-1
	RETURN
	end
