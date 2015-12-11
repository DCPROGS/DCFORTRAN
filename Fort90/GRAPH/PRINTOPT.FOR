	subroutine printopt(xlo,xhi,ylo,yhi,xpp,ypp,scalf)
	character*11 cnumx1,cnumx2,cnumy1,cnumy2
	character ans,UC
	character defolt*30,cans*30		!to hold text & result of QDIALOG


	   dxr=abs(xpp-297.)
	   dyr=abs(ypp-210.)
	   a=2.
	   ixlov=1
	   iylov=1
	   ixhiv=595
	   iyhiv=421
	   if(xpp.lt.ypp) then
	   	dxr=abs(xpp-210.)
	   	dyr=abs(ypp-297.)
		a=1.4
	   	ixlov=1
	   	iylov=1
	   	ixhiv=295
	   	iyhiv=416
	   endif
	   xlor=xlo+dxr/2.
	   xhir=xhi+dxr/2.
	   ylor=ylo+dyr/2.
	   yhir=yhi+dyr/2.

	   ixlo=a*xlor
	   iylo=a*ylor
	   ixhi=a*xhir
	   iyhi=a*yhir
	   ibk=15
         m=6
	   n=2
	   if(xpp.gt.ypp) then
	      call fillwin(1,1,595,421,15)
	   else
	      call fillwin(1,1,295,416,15)
	   endif
	   call dhline(ixlo,ixhi,iylo,1,0)
	   call dhline(ixlo,ixhi,iyhi,1,0)
	   call dvline(ixlo,iylo,iyhi,1,0)
	   call dvline(ixhi,iylo,iyhi,1,0)
	   if (scalf.ne.-1.) then
11	      call dhline(ixlo,ixhi,iylo,15,0)
	      call dhline(ixlo,ixhi,iyhi,15,0)
	      call dvline(ixlo,iylo,iyhi,15,0)
	      call dvline(ixhi,iylo,iyhi,15,0)
		idx=ixhi-ixlo
		idy=iyhi-iylo
		ixlo=ixlo-float(idx)*(scalf-1)/2.
		iylo=iylo-float(idy)*(scalf-1)/2.
		ixhi=ixhi+float(idx)*(scalf-1)/2.
		iyhi=iyhi+float(idy)*(scalf-1)/2.
	      call dhline(ixlo,ixhi,iylo,1,0)
	      call dhline(ixlo,ixhi,iyhi,1,0)
	      call dvline(ixlo,iylo,iyhi,1,0)
	      call dvline(ixhi,iylo,iyhi,1,0)
	      xlor=float(ixlo)/a
	      ylor=float(iylo)/a
	      xhir=float(ixhi)/a
	      yhir=float(iyhi)/a
	      xlo=xlor-dxr/2.
	      xhi=xhir-dxr/2.
	      ylo=ylor-dyr/2.
	      yhi=yhir-dyr/2.
	      call DCFORMAT(xlo,m,n,cnumx1)
	      call DCFORMAT(ylo,m,n,cnumy1)
	      call DCFORMAT(xhi,m,n,cnumx2)
	      call DCFORMAT(yhi,m,n,cnumy2)
		call WDIALOG(1,
     &       'Margins (mm from left/bottom of paper; A4=210x297)',11)
		call WDIALOG(1,'Xleft='//cnumx1//'Xright='//cnumx2,11)
		call WDIALOG(1,'Ybottom='//cnumy1//'Ytop='//cnumy2,11)
	      ans='Y'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Are these values OK'
     &	,defolt,11,cans)
	      call GETINPa(cans,ans)
	      if(UC(ans).eq.'N') then
	         call DEFOLTr(scalf,defolt)
	         call QDIALOG(1,'Scale factor [0.8 to 1.1]',
     &         defolt,11,cans)
	         call GETINPr(cans,scalf)
	         if(scalf.lt.0.8) scalf=0.8
	         if(scalf.gt.1.2) scalf=1.1
		   call DEFOLT2r(xlo,xhi,defolt)
	         call QDIALOG(1,'Margins (mm): Xleft, Xright',
     &         defolt,11,cans)
	         call GETINP2r(cans,xlo,xhi)
		   call DEFOLT2r(ylo,yhi,defolt)
	         call QDIALOG(1,'Margins (mm): Ybottom, Ytop',
     &         defolt,11,cans)
	         call GETINP2r(cans,ylo,yhi)
		   xlor=xlo+dxr/2.	!redefine things that depend on xlo etc
		   xhir=xhi+dxr/2.
		   ylor=ylo+dyr/2.
		   yhir=yhi+dyr/2.
		   ixlo=a*xlor
		   iylo=a*ylor
		   ixhi=a*xhir
		   iyhi=a*yhir
		   goto 11
		endif
	   else
	   imode=0
	   call dragbox(ixlo,iylo,ixhi,iyhi,ixlov,iylov,ixhiv,iyhiv,
     &	imode,ibk,icf)
	   endif
	   xlor=float(ixlo)/a
	   ylor=float(iylo)/a
	   xhir=float(ixhi)/a
	   yhir=float(iyhi)/a
	   xlo=xlor-dxr/2.
	   xhi=xhir-dxr/2.
	   ylo=ylor-dyr/2.
	   yhi=yhir-dyr/2.

	end
