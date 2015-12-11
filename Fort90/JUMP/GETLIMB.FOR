	subroutine GETLIMB(nplot,i1,jv,iv1,iv2,il1,il2,vmin,vmax,
     &	Yval,ndv1,ndimd)
	real*4 YVAL(ndv1,ndimd)
	integer*2 iv1,iv2
c
c To return il1,il2 =index in Yval of start of 2nd and 3rd limbs of ramp
c and also vmin,vmax
	   vmin=1.e37
	   vmax=-1.e37
	   do i=1,nplot
		ii=i1+i-1
		v=Yval(ii,jv)
		if(v.gt.vmax) then
		   vmax=v
		   imaxv=ii		!keep as index in original Yval(*,jv)
		endif
		if(v.lt.vmin) then
		   vmin=v
		   iminv=ii		!keep as index in original Yval(*,jv)
		endif
	   enddo
	   if(iv1.gt.iv2) then
		il1=imaxv	!index of point at end of 1st, beginning of 2nd limb
		il2=iminv	!index of point at end of 2nd, beginning of 3rd limb
	   else
		il1=iminv	!index of point at end of 1st, beginning of 2nd limb
		il2=imaxv	!index of point at end of 2nd, beginning of 3rd limb
	   endif
c
	RETURN
	end

