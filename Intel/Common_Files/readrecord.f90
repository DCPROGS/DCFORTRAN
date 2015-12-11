
	subroutine read_record(istrec,Main,iData_list,iplot,ifiltype,iplotype,njset,nset,nsfit,&
	           xnum,nrows,niobs,njobs,stitle,hdisp,xobs,yobs,w,nj,titlex,titley,ilabel,&
			   title1,iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,ncurvd,ncurvc,dfile,&
			   nval1,useconsam,filtered,istr1,colseq,nhline,adcfil,&
			   itit,ndev,ib,istart,iend,srate,calfac,ioff,calfac2,&
			   cDATEW,adctimew,adcf1,isdfst,ndisp1,nrec1,jdat,base,&
			   ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,&
               ndelt,ffilt,fcz,fczoom,njump,idest,krn,titled,setx)

	use menu_f90

	integer :: iData_list
	ALLOCATABLE datcop	
	real datcop(:)
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer*4 nj(njset),njbase(njset)
	real setx(njset)
	character*60 :: dfile
	character*20 TITLEP(100)
	character*10 titlep1(100)
	character*64 title1
	character*60 stitle(10,200)
    CHARACTER*60 TITLEF,TITLED(20),TITMOD(40)
    CHARACTER*40 TITLEDS(10)
	character*40 titlex,titley
	character*76 title
	real*4 Xnum(20,3)
    INTEGER*4 jstrec(100),lstrec(100),jstrec1(100),lstrec1(100),nsets(20)
	integer*2 jstrec2(200),lstrec2(200),ititw(5)		!for old queue
	CHARACTER*22 CNUM(10),CNUM1
	logical hdisp,doframe
	real*4 stepamp(50),dt(49),filt(640)
	character ndev*2,adcf1*30,cDATEW*11,adctimew*8
	character*33 adcfil
	character*44 title2	!for queues from SCAN
	integer*4 irecfst(1000)	
	logical useconsam,colseq,noconsam,cjdat
	logical consamdef,usecons,filtered,newform
	logical discprt
	common/dp/discprt
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
     	xmin,xmax,ymin,ymax
	if(ifiltype.eq.1) then
	    read(11,rec=1) nfile,jstrec,lstrec,iver
		if(iplot.lt.1) iplot=1
		if(iver.le.1002) then
	         nrect=lstrec(IPLOT)-jstrec(IPLOT)+1		!total number of records
	         irec=jstrec(IPLOT)
	         IREC=((JSTREC(iplot))-1)*1024 + 1
		else
	         nrect=lstrec(IPLOT)-jstrec(IPLOT)+1		!total number of records
	         irec=jstrec(IPLOT)
		endif
	    nrecd=nrect-1				!number of records for Xval etc
		istrec=irec
	    if(iver.le.1002) then

		    if(iver.eq.1001) then
	            read(11,rec=irec) (titled(K),K=1,10),nset,(nj(k),k=1,10),&
     	      (titlep1(k),k=1,20),setvar,&
     	      (setx(k),k=1,10),iw,titlex,titley,ilabel,hdisp
		    else
	          read(11,rec=irec) titlef,titleds,nset,(nj(k),k=1,10),&
     	      (titlep1(k),k=1,20),setvar,&
     	      (setx(k),k=1,10),iw,titlex,titley,ilabel,hdisp
			title1=titlef
	            call SWAP(titled,titleds,nset,njset,1)  !copy titleds into titled
		    endif
		    do k=1,10
				njbase(k)=nj(k)
		    enddo
	   		ALLOCATE(datcop(3000))
	        n=1
	        do j=1,nrecd
		         m=n+255
		         irec=irec+1024
		         read(11,rec=irec) (DATCOP(i),i=n,m)
		         n=n+256
	        enddo
	    else
	       read(11,rec=irec) nset,titlef,(titled(j),j=1,nset),&
      	    (nj(j),j=1,nset),(setx(j),j=1,nset),setvar,iw,&
            titlex,titley,ilabel,hdisp,ioffset,kA
	            ntot=0
		   nsetbase=nset
		   do k=1,nset
				njbase(k)=nj(k)
		   enddo
	       do j=1,nset
		         ntot=ntot+nj(j)
	       enddo
	   	   ALLOCATE(datcop(3*ntot))
	       read(11,rec=irec+ioffset) (DATCOP(i),i=1,kA)
			title1=titlef
	    endif
	    k=0		!index for datcop
	    do j=1,nset
	          call INTCONV(J,cnum(1))
	          call INTCONV(nJ(j),cnum(2))
	          Stitle(J,IPLOT)='SET '//cnum(1)(1:3)//': '//cnum(2)(1:3)&
     	    //' obs. '//titled(J)
	          n=nj(j)
	          do i=1,n
	           k=k+1
	           Xobs(i,j)=datcop(k)
		      enddo
	          if(.not.hdisp) then	!X only for histograms
	            do i=1,n
	              k=k+1
	              Yobs(i,j)=datcop(k)
		      enddo
	   	      do i=1,n
	              k=k+1
	              w(i,j)=datcop(k)
		      enddo
	          endif
		enddo
	    DEALLOCATE(datcop)
		kwi=niobs   !dimensions of weight()
		kwj=njset
		ncurvc=0		!none here
	 
		
	       
	else if(ifiltype.eq.2) then
	      
		  
		  read(11,rec=1) nfile,jstrec2,lstrec2,iver
				
	      istrec=int4((jstrec2(iplot)))	!1st record no for plot # iPLOT
		  istrec=(jstrec2(iplot)-1)*1024+1	!1st record no for plot # iPLOT
		  read(11,rec=istrec) iptype
	      nrect=(lstrec2(iplot))-istrec+1		!total number of records
	      nrect=nrect-2				!number of records for Xval etc
	      
		  j=iabs(iptype)
		  if(iptype.lt.0) then
	 			isign=-1
		  else
	 			isign=1
		  endif
	      if(j.gt.1000.and.j.lt.2000) then
			j=j-1000
			cjdat=.true.
		  else
			cjdat=.false.
	      endif
	      iptype=j*isign		!1000 removed
		  if(j.eq.3.or.j.eq.31.or.j.eq.32.or.j.eq.30.or.j.eq.301.or.j.eq.302.or.j.eq.33) then
		    iplotype=3	!single channel
			ndimd=1
			if(j.eq.33) then
				read(11,rec=istrec)iptype,title1,titlex,titley,&
				nval1,consamdef,nhline,ltype,jbff1,jbff2,colseq,ic,icp,&
				xmin,xmax,ymin,ymax,XLO,XHI,YLO,YHI,ioffs1
				istr1=istrec+ioffs1
			else
				useconsam=iptype.gt.0
				scplots=.true.
				filtered=j.eq.30.or.j.eq.301.or.j.eq.302
				iptype=j
				call READSCQ(krn,istrec,iptype,useconsam,itit,title1,&
				ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,&
				cDATEW,adctimew,adcf1,isdfst,ndisp1,nrec1,jdat,base,&
				ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,&
				ndelt,ffilt,fcz,fczoom,njump)
			    if(useconsam) then
					idest=99
					imesg=gmdisplaymessagebox('','Not available for this version',gexclamation,gok)
					if(adcf1(2:2).eq.':') adcfil(1:30)=adcf1	!path for consam in queue
			
					call GETCONS(adcfil,noconsam,title,cDATEW,adctimew,nsam,&
					srate1,cjdat,irecfst,newform,ioffset,cfac,fflt,idest)
					if(noconsam) then
						consamdef=.false.
					else
						consamdef=.true.
						if(.not.cjdat.and.(srate.ne.srate1)) then	!srate1 not def for cjdat
						endif
					endif
					if(idest.eq.99) goto 99
				endif
			endif
			goto 99
          endif
		  if(iptype.eq.1.or.(iptype.ge.11.and.iptype.le.16)) then
		  	iplotype=1  ! vplot
			hdisp=.false.
		  else if(iptype.eq.21.or.iptype.eq.2.or.iptype.eq.22.or.iptype.eq.23) then
			iplotype=2	!vhist
			hdisp=.true.
		  else if(iptype.eq.40.or.iptype.eq.41.or.iptype.eq.42) then ! 3d
			iplotype=4
		  endif
		  if(iptype.ge.15.and.iptype.le.25) then
			
			istrec=(int4(jstrec2(iplot))-1)*1024+1
		  endif
	      if(iplotype.eq.1) then
		   if(iptype.eq.14) then
			read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,&
			itit,title1(1:44),&
			xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,&
			csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd
			kwi=100	!dimensions for weight()
			kwj=10
			kmax=20
			if(ndv1.eq.0) ndv1=1
			if(ndimd.eq.0) ndimd=1
			if(ndc1.eq.0) ndc1=1
			if(ndimc.eq.0) ndimc=1
		   else if(iptype.eq.15.or.iptype.eq.16) then
			if(iver.lt.1100) then
				read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,&
     			itit,title1(1:44),xmin,xmax,ymin,ymax,xcross,ycross,&
      			xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,iscal,doframe,&
     			titlex,titley,ilabel,ncurvd,ncurvc,inumx
				xlo=float(ixlo)
				xhi=float(ixhi)
				ylo=float(iylo)
				yhi=float(iyhi)
			else
				read(11,rec=istrec)iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,&
     			kmax,itit,title1,xmin,xmax,ymin,ymax,xcross,ycross,&
     			xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,iscal,doframe,&
     			titlex,titley,ilabel,ncurvd,ncurvc
			endif
			if(ndimd.gt.ncurvd) ndimd=ncurvd
			if(ndimc.gt.ncurvc) ndimc=ncurvc

		   else
			ndv1=2048	!dimensions as for earlier versions
			ndimd=10
			ndc1=2048
			ndimc=10
			kwi=100	!dimensions for weight()
			kwj=10
			kmax=20
	       endif
	       if(ndv1.le.0) ndv1=1		!in case no data curves
	       if(ndimd.le.0) ndimd=1		!in case no data curves
	       if(ndc1.le.0) ndc1=1		!in case no calc curves
	       if(ndimc.le.0) ndimc=1		!in case no calc curves
	       if(kwi.le.0) kwi=1
	       if(kwj.le.0) kwj=1
		   
		  else if(iplotype.eq.2) then
			if(iptype.eq.2) then
				ndv1=2048	!dimensions as for earlier versions
				ndimd=10
				ndc1=2048
				ndimc=10
				kmax=20	!for theta
			else if(iptype.eq.21.or.iptype.eq.22.or.iptype.eq.23) then
				read(11,rec=istrec) iptype,ndimd,ndv1,ndimc,ndc1,kmax
				ndimd=10		!still fixed=use queud value when alloc
				ndimc=10		!still fixed=use queud value when alloc
				if(iptype.eq.23) read(11,rec=istrec) iptype,ndimd,ndv1,ndimc,ndc1,kmax
			endif
			 
		  				
		  endif
		  do j=1,10
	          call INTCONV(J,cnum(1))
	          
	          Stitle(J,IPLOT)='SET '//cnum(1)(1:3)
			 enddo	 
          
	ELSE IF(IFiLTYPE.EQ.3) THEN
	    
	endif   
	 
99    end