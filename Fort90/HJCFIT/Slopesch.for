	subroutine SLOPESCH(DETWX,slo,shi,np,istep,slast,s,
     &      detlast,ndslast,idslast,det,nds,ids,
     &      sloplast,nsslast,isslast,slope,nss,iss,
     &	slast1,slast2,dprt,ired,label)
c remove dmin/dmax, add scale fac and sign for det, and add sign for slope
c	subroutine SLOPESCH(DETWX,slo,shi,np,istep,
c     &      slast,s,detlast,det,sloplast,slope,
c     &	slast1,slast2,dmin,dmax,dprt,ired,label)
c Called by ROOTSCH to do search from slo to shi on log scale, with np points
c spaced logarithmically, using local estimation of slope
c NB dprt, ired and label included in args only for printout
c NB need not start with istep=0 so can resume search from specified step
c Covers range istep+1, istep+2, . . . ,np
c Returns with istep<np if all points between slo, shi not yet tested
c Input:  slo,shi,np,istep
c Output:
c   slast, s = last pair of values of s to be tested
c   detlast, det =detW(slast), detW(s)
c   sloplast, slope =slope(slast), slope(s)
c   slast1 = value of s after last sign change in detW(s)
c   slast2 = value of s after last sign change in slope
c   dmin, dmax = smallest, largest value of tetw(s) in range searched
c   nerr=value after call to detW() (in common with rootserach so not param)
c
c Modified 07/11/03 07:23am to use altered detwa/detwf which calls determ2()
c which cannot overflow -returns det within valid range and ndscale=number
c of factors of 1.d-10 that have been used to keep det within range
c Each value of det also returns (via common/det4/) two values,
c (1) ndscale=number of factors of 1.d-10 that have been applied to stop
c det from overflowing/underflowing (if ndscale=0 then det is correct value,
c otherwise it is scaled)
c (2) idsign = sign of det (integer=0,1 or -1)
c Also keep similar scale factor and sign for slope (nss and iss)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	logical end,dprt
	character label*6,text*19
	common/detw2/tres,km,nerr				   !for DETWA,DETWF
	logical bothpos,bothneg,min,max,even	!for debug printout
c for debug
	common/deb/idebug,idebug1
c from detwf, detwa
	common/det4/ndscale,idsign
c
	EXTERNAL detwx
c
	end=.false.
	dlogs=(dlog10(-slo)-dlog10(-shi))/dfloat(np - 1)
c	dmin=1.d308
c	dmax=-1.d308
	nerr=0
c
c     evaluate step #istep before the loop to evaluate start values of sign
c     and slope
	s=slo/(10**(dfloat(istep)*dlogs))
	if(s.gt.-0.00001d0) s=-0.00001d0   		!100 000 sec
	ssav=s	!point where this search started
c
	if(idebug1.ge.3) then
	   text=label//' search: s = '
	   print 83,text,slo,shi,np,ired
	   if(dprt) write(8,83) text,slo,shi,np,ired
83	   format(
     & 1x,a19,g13.6,' to ',g13.6,
     &     ' ( ',i6,' points; step red ',i2,')')
	endif
c
	det=DETWX(s)
	nds=ndscale		!scale from common/det4/
	ids=idsign		!sign of det from common/det4/
c dmin,dmax don't seem to be needed
c	if(det.lt.dmin) dmin=det
c	if(det.gt.dmax) dmax=det
c determ2 cannot overflow -returns ndscale
c	if(nerr.eq.10) then	!determinant overflowed
c	   goto 99		!deallocate and RETURN
c	endif
	s1=s/(10**dlogs)	!to get suitable step size for first estimate of slope
	delta=(s-s1)*0.1d0		!NB this will be negative
	det1=DETWX(s+delta)
	nds1=ndscale		!from common/det4/
	ids1=idsign		!sign of det from common/det4/
c	if(nerr.eq.10) then	!determinant overflowed
c	   goto 99		!deallocate and RETURN
c	endif
c  If det are scaled the det(true)=det*10.d0**(10*ndscale)
c The slope itself might overflow when multiplied by 10.d0**(10*nds1),
c but only sign of slope is needed anyway
	if(nds.eq.0.and.nds1.eq.0) then	!det are correct -no scale facs
	   slope=(det1-det)/delta
	   nss=0		!scale factor for slope (like nds for determinant)
	else
	   slope=(det1 - det*10.d0**(10*(nds-nds1)))	!scaled
	   iss=int4(dsign(1.d0,slope))	!sign of slope
	   if(slope.eq.0.d0) iss=0
c       now scale slope correctly (needed? -overflow?)
	   nss=nds1		!scale factor for slope (like nds for determinant)
c as suspected, next line may overflow so keep scale fac for slope,
c as for determinant true slope= scaled slope*10.d0**(10*nss)
c=	   slope=10.d0**(10*nds1)*(det1 - det*10.d0**(10*(nds-nds1)))
	endif
c
c Now the loop
2	continue	!return here for next step in s value
	istep=istep+1
	slast=s
	s=slo/(10**(dfloat(istep)*dlogs))
	if(s.gt.-0.00001d0) s=-0.00001d0   		!100 000 sec
	if(istep.eq.np.or.s.gt.0.d0) then
	   goto 99
c  End search and restart with smaller steps if all roots not found
	endif
c
	detlast=det
	ndslast=nds		!scale from common/det4/
	idslast=ids		!sign of det from common/det4/
	det=DETWX(s)
	nds=ndscale		!scale from common/det4/
	ids=idsign		!sign of det from common/det4/
c	if(nerr.eq.10) then	!determinant overflowed
c	   goto 99		!deallocate and RETURN
c	endif
c NB step size for estimation of slope must be small compared
c with step between slast and s
	delta=(s-slast)*0.1d0		!NB this will be negative
	detlast1=det1
	ndslast1=nds1		!from common/det4/
	idslast1=ids1		!sign of det from common/det4/
	det1=DETWX(s+delta)
	nds1=ndscale		!from common/det4/
	ids1=idsign		!sign of det from common/det4/
c	if(nerr.eq.10) then	!determinant overflowed
c	   goto 99		!deallocate and RETURN
c	endif
c	if(det1.lt.dmin) dmin=det1
c	if(det1.gt.dmax) dmax=det1
	sloplast=slope
	nsslast=nss		!scale fac for slope
	isslast=iss		!sign of slope
c	slope=(det1-det)/delta
	if(nds.eq.0.and.nds1.eq.0) then	!det are correct -no scale facs
	   slope=(det1-det)/delta
	   nss=0		!scale factor for slope (like nds for determinant)
	   iss=int4(dsign(1.d0,slope))	!sign of slope
	   if(slope.eq.0.d0) iss=0
	else
	   slope=(det1 - det*10.d0**(10*(nds-nds1)))/delta	!scaled
	   iss=int4(dsign(1.d0,slope))	!sign of slope
	   if(slope.eq.0.d0) iss=0
c       now scale slope correctly (needed? -overflow?)
	   nss=nds1		!scale factor for slope (like nds for determinant)
c=	   slope=slope*(10.d0**(10*nds1))	
	endif
c Check for change in sign of detw(s)
c If sign changes must have odd number of roots since last s value (and poss
c some pairs of identical roots too). So do fine search between these s values
c -just use signs now
c	zero=1.d0
c	if((detlast.lt.zero.and.det.gt.zero).OR.
c     &   (detlast.gt.zero.and.det.lt.zero)) then
	if((idslast.lt.0.and.ids.gt.0).OR.
     &   (idslast.gt.0.and.ids.lt.0)) then
	   slast1=s	!value of s after last sign change in detw(s)
	   end=.true.	!leave to act on this in rootsch
	   if(idebug1.ge.3) then
		print 80,slast,s,detlast,det
		if(dprt) write(8,80) slast,s,detlast,det
80		format(
     & '   detW(s) changed sign between s = ',g13.6,' and ',g13.6,/,
     & '       detW(slast) = ',g13.6,', detW(s) = ',g13.6)
	   endif
	endif
c
c Check for change in sign of slope
c If sign changes must have even number of roots since last s value (and poss
c some pairs of identical roots too). So do fine search between these s values
c Now use only sign of slope
c	if((sloplast.lt.zero.and.slope.gt.zero).OR.
c     &   (sloplast.gt.zero.and.slope.lt.zero)) then
	if((isslast.lt.0.and.iss.gt.0).OR.
     &   (isslast.gt.0.and.iss.lt.0)) then
	   slast2=s	!value of s after last sign change in slope
	   end=.true.	!leave to act on this in rootsch
c    Rest is for debug -print slope change only in the case
c    in which it implies exisitence of (an even number of) roots
c now use only signs
c	   bothpos=(detlast.gt.zero).and.(det.gt.zero)	!det(slast) and det(s) both pos
c	   bothneg=(detlast.lt.zero).and.(det.lt.zero)	!det(slast) and det(s) both pos
c	   min=(sloplast.lt.zero).and.(slope.gt.zero)
c	   max=(sloplast.gt.zero).and.(slope.lt.zero)
	   bothpos=(idslast.gt.0).and.(ids.gt.0)	!det(slast) and det(s) both pos
	   bothneg=(idslast.lt.0).and.(ids.lt.0)	!det(slast) and det(s) both pos
	   min=(isslast.lt.0).and.(iss.gt.0)
	   max=(isslast.gt.0).and.(iss.lt.0)
	   even=(bothpos.and.min).or.(bothneg.and.max)
	   if(even.and.idebug1.ge.3) then
		print 81,slast,s,sloplast,slope,s-slast,delta,
     &	 detlast,detlast1,det,det1
		if(dprt) write(8,81) slast,s,sloplast,slope,s-slast,delta,
     &	   detlast,detlast1,det,det1
81		format(
     & '     Slope changed sign between s = ',g13.6,' and ',g13.6,/,
     & '       slope1 = ',g13.6,', slope2 = ',g13.6,/,
     & '       s-slast = ',g13.6,' delta = ',g13.6,/,
     & '       detW(slast) = ',g13.6,' detW(slast+delta) = ',g13.6,/,
     & '       detW(s) = ',g13.6,'     detW(s+delta) = ',g13.6)
	   endif
	endif
c
	if(.not.end) goto 2	!next s value
c
	if(idebug1.ge.3) then
	   print 82,ssav,s
	   if(dprt) write(8,82) ssav,s
82	   format(
     & '     (Searched from s =  ',g13.6,' to ',g13.6,')')
	endif
c
99	continue
	RETURN
	end


