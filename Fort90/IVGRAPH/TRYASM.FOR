	program scale

	integer*2	data(2000),ix(2000)


	do i=1,320
		ix(i)=i
		data(i)=100
		ix(320+i)=320+i
		data(320+i)=-100
		ix(640+i)=i
		data(640+i)=100
		ix(960+i)=320+i
		data(960+i)=-100
		ix(1280+i)=i
		data(1280+i)=100
		ix(1600+i)=320+i
		data(1600+i)=-100
	end do
	do i=1921,2000
	   ix(i)=i-1920
	   data(i)=0
	enddo

	do i=1,2000
	   ix(i)=i
	enddo

	ifirst=1
	ndisp=639    	!NDISPLAY
	ndisp1=ndisp	!normally, but ndisp1 smaller for truncated display

	call mode(18)
	call clears(0,480,1)
	call aivpix5(ix,data(1),639,data(1),14,ifirst,1,1,300,300,
     & 639)
	pause
	call aivpix5(ix,data(160),639,data(1),14,ifirst,1,1,300,300,
     & 639)
	 pause
c========================================
	call clears(0,480,1)
	ifirst=0
	call aivpix6(ix,data(1),639,data(1),14,ifirst,1,1,300,300,
     & 639,1)
	pause
	call aivpix6(ix,data(160),639,data(1),14,ifirst,1,1,300,300,
     & 639,1)
	pause
c==========================================
	call clears(0,480,1)
	ifirst=0
	call aivpix6(ix,data(1),ndisp1,data(1),14,ifirst,1,1,300,300,
     & ndisp,2)
	pause
	nlast=2000
	nd=nlast/2-160
	if(nd.lt.640) ndisp1=nd-1
	call aivpix6(ix,data(320),ndisp1,data(1),14,ifirst,1,1,300,300,
     & ndisp,2)
	pause
c=============================================
	call clears(0,480,1)
	 ndisp1=ndisp
	ifirst=0
	call aivpix6(ix,data(1),639,data(1),14,ifirst,1,1,300,300,
     & 639,3)
	pause
	nd=(nlast/3-160)
	ndisp1=ndisp
	if(nd.lt.640) ndisp1=nd-1
	call aivpix6(ix,data(480),ndisp1,data(1),14,ifirst,1,1,300,300,
     & ndisp,3)
	pause
c=============================================
	call clears(0,480,1)
	ifirst=0
	nd=(nlast/4)
	ndisp1=ndisp
	if(nd.lt.640) ndisp1=nd-1
	ndisp=ndisp1
	call aivpix6(ix,data(1),ndisp1,data(1),14,ifirst,1,1,300,300,
     & ndisp,4)
	pause
	nd=(nlast/4-160)
	ndisp1=ndisp
	if(nd.lt.640) ndisp1=nd-1
	call aivpix6(ix,data(640),ndisp1,data(1),14,ifirst,1,1,300,300,
     & ndisp,4)
	pause
	call mode(3)
	end
