	subroutine SETFILT(zoomed,filtzoom,ffilt,fczoom,tf1,tf50,tf99,
     & npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     & smult,iscrit,ampfac,avamp,avampsav,avampz,
     & filt1,nfilt1,dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
	real*4 filt1(1024),filt1n(1024),filt1z(1024)
	logical zoomed,filtzoom
c To set/reset values of tf1,tf50,tf99,npfilt,npfilt1,nshutfit,
c scrit,iampcrit, according to which filter is in use
c INPUT
c zoomed=true sets values for heavily-filtered display (fc=fczoom, kHz)
c zoomed=false resets regular value for normal display (fc=ffilt, kHz)
c Also need to input values of finter,facjump,avampsav,avampz,smult,ampfac.
c Define npfilt1= number of data points for filter to go from 1% to 99%
c and npfilt=number of points to jump forward after a transition, to start
c search for next transition
c
	if(.not.filtzoom) RETURN	!no filtering done when zoomed
c
	if(zoomed) then		!SET FILTERED VALUES
	   tf1=101.2187/fczoom
	   tf50=409.4718/fczoom
	   tf99=717.7249/fczoom
	   avamp=avampz
	   nfilt1=nfilt1z
	   dtf1=dtf1z
	   do i=1,nfilt1
		filt1(i)=filt1z(i)	!copy filtered step response to filt1()
	   enddo
	else				!RESET NORMAL VALUES
	   tf1=101.2187/ffilt	!values for filtering
	   tf50=409.4718/ffilt
	   tf99=717.7249/ffilt
	   avamp=avampsav
	   nfilt1=nfilt1n
	   dtf1=dtf1n
	   do i=1,nfilt1
		filt1(i)=filt1n(i)	!copy normal step response to filt1()
	   enddo
	endif
	npfilt1=ifixr((tf99-tf1)/finter)
	npfilt=ifixr(float(npfilt1)*facjump)
c==	nshutfit=8*npfilt		!default value -do not change value set in inscan
	iampcrit=iabs(ifixr(ampfac*avamp))
	if(iscrit.eq.1) then
	   scrit=smult*abs(avamp)		!in intermed units
	endif
c===should rms1 change, if so then scrit changes when iscrit=2 also
c
	RETURN
	end


