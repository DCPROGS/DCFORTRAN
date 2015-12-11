	program toper
c  to test GETOPER
	real*4 tint(10,1),ampl(10,1)
	integer*1 iprops(10,1)
c	integer nint(10)
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10)
c
c For exclusion of openings
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excop(10)
	common/exop/excop,gaplo,gaphi,nskip
c
	tint(1,1)=2.
	ampl(1,1)=-6.
	iprops(1,1)=0
	tint(2,1)=4.
	ampl(2,1)=0.
	iprops(2,1)=0

	tint(3,1)=1.
	ampl(3,1)=-6.
	iprops(3,1)=0
	tint(4,1)=50.
	ampl(4,1)=0.
	iprops(4,1)=0

	tint(5,1)=0.1
	ampl(5,1)=-6.
	iprops(5,1)=0
	tint(6,1)=100.
	ampl(6,1)=0.
	iprops(6,1)=0

	tint(7,1)=0.2
	ampl(7,1)=-6.
	iprops(7,1)=0
	tint(8,1)=500.
	ampl(8,1)=0.
	iprops(8,1)=0

	tint(9,1)=3.
	ampl(9,1)=-6.
	iprops(9,1)=0
	tint(10,1)=6.
	ampl(10,1)=0.
	iprops(10,1)=0

	nd1=10
	nd2=1
	nint(1)=10
	jset=1
	excop(1)=.true.
	gaplo(1)=10.
	gaphi(1)=1000.
c
	print 1
1	format(' i       tint            ampl         iprops')
	do i=1,nint(1)
	   print 2,i,tint(i,1),ampl(i,1),iprops(i,1)
2	   format(i6,2g13.6,i5)
	enddo
c
	call GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2)
	do i=1,nint(1)
	   print 2,i,tint(i,1),ampl(i,1),iprops(i,1)
c2	   format(i6,2g13.6,i5)
	enddo
	end

	subroutine FINDOPEN(in,j,iop,ibad,ampl,iprops,nd1,nd2)
c To find next (good) opening in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good opening. Ibad=0 normally, but ibad=1 if end of data reached without
c finding a good opening
	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical open,burst,chsvec,good,btest
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10)
c
	ibad=0
	do 1 i=in,nint(j)
	  iop=i
	  open=ampl(i,j).ne.0.
c=	  if(open.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	  good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	  if(open.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=1
	RETURN
	end

	subroutine FINDGAP(in,j,iop,ibad,ampl,iprops,nd1,nd2)
c To find next (good) gap in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good gap. Ibad=0 normally, but ibad=1 if end of data reached without
c finding a good gap
	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical shut,burst,chsvec,good,btest
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10)
c
	ibad=0
	do 1 i=in,nint(j)
	  iop=i
	  shut=ampl(i,j).eq.0.
c=	  if(shut.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	  good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	  if(shut.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=1
	RETURN
	end

