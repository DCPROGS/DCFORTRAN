	subroutine CHKGUESS(nt,t0,delt,ylevel,kfix,opening,scrit,
     & base,iampcrit,shutprev,prevlevel)
c To check initial guesses and remove direct transitions between open
c levels that differ in amplitude by less than iampcrit.
c NB Size of ampcrit is calc from AVAMP, but amplitudes vary should make it
c variable, depending on whether this is a big opening or a small one.
c (Are init guesses reliable enough to do this, or should this
c be done AFTER fit (in CHECKFIT) instead, or as well?)
c In GETALL the values of ylevel() and t0, delt() have been defined
	real*4 delt(0:50),ylevel(50)
	integer kfix(0:50)
	logical open1,open2,open2a,direct,shutprev
	logical opening(50)
	logical cjumpfst
	common/jump/cjumpfst
c
c First look for direct transitions between open states -problem is to know
c which are open!  If shutprev then initially shut
c delt(0) not defined at this stage, so use it as temp storage for t0
c	if(nt.le.1) RETURN
	delt(0)=t0
	ampcrit=float(iampcrit)
	nt1=nt				!new number of transitions
	open1=.not.shutprev		!initial state before first trans
2	continue
	do i=1,nt
	   open2a=abs(ylevel(i+1)-base).gt.scrit
	   open2=(opening(i).or.open2a)	!open after trans #i
	   direct=open1.and.open2		!direct open -> open transition
	   open1=open2			!for next cycle
	   if(direct.and.abs(ylevel(i)-ylevel(i+1)).lt.ampcrit) then !remove transition
		nt1=nt1-1
		s=delt(i-1)+delt(i)
		f1=delt(i-1)/s		!weight
		delt(i-1)=s			!add lengths
c		ylevel(i)=0.5*(ylevel(i)+ylevel(i+1))	!average level
c Use weighted mean
		ylevel(i)=f1*ylevel(i)+(1.-f1)*ylevel(i+1)	!average level
c
		do j=i,nt-1		!move others down one place
		   delt(j)=delt(j+1)			!=0,...,ntrans-1
		   kfix(j)=kfix(j+1)			!kfix 0,..,ntrans
		enddo
		kfix(i)=0			!reset to 0 for concatenated intervals
		do j=i+1,nt  		!move others down one place
		   ylevel(j)=ylevel(j+1)		!=1,...,ntrans+1
		enddo
		do j=i,nt-1  		!move others down one place
		   opening(j)=opening(j+1)		!opening=1,...,ntrans
		enddo
		goto 1		!jump out as soon as one transition removed
	   endif
	enddo
	goto 3
c Next bit done only after a transition is removed
1	continue
	nt=nt1
	if(nt.gt.1) goto 2	!look for more to remove
c
c All transitions now removed
3	continue
	t0=delt(0)		!restore t0
c
c If initially open the entire (possibly concatenated) interval before
c first transition must be set to previous level) (actually done in
c SETPARAM so should not be nec here)
	if(.not.shutprev.and.(.not.cjumpfst)) then  !initially open
	   ylevel(1)=prevlevel
	endif
c
	RETURN
	end


