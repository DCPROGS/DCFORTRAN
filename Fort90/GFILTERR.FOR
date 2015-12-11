	subroutine GFILTERR(datin,datout,np1,np2,srate,fc,nd1,nd2,nerr)
c Gaussian filter, as in C & Sigworth (1994) Fig A3.1
c
c GFILTERR is version of GFILTER that
c  (1) Has real input and output arrays
c  (2) Upper and lower dimensions of input array specified as arguments
c      (nd1,nd2) and filtering done from elements np1 to np2
c  NB datin() and datout() cannot be the same array!
c
c Modif 06/19/97 12:51pm so that coefficient array is allocatable, to remove
c limit on the extent to which a record can be filtered
c
c Modif 04/04/97 04:32pm so returns nerr=0 if OK, nerr=1 if fc too small
c so error can be printed in calling prog, not here
c idatin,idatout=integer*2 input and output arrays.
c Data are in idatin(1) to idatin(npnt)
c
c NB maximum of 54/2=27 data points at each end are affected by edge
c effects, so read in arrays that overlap by at least this number
c of points, and discard overlaps.
c
c fc= -3 dB frequency in Hz
cc finter=microsec between sample points
c srate=sample rate (Hz)
c
	real*4 datin(nd1:nd2),datout(nd1:nd2)
c=	integer*2 idatin(npnt),idatout(npnt)
c=	real*4 a(54)
	allocatable::a
	real*4 a(:)
c
c fc1=fc/sample freq = fc/srate = fc*finter*1.e-6
c eg srate=10 kHz, finter=100 microsec: fc=1150 Hz  fc1=1150*100*1.e-6 =0.115
c sample freq (Hz) = srate = 1/(finter*1.e-6) =1.e6/finter
	nerr=0
	fc1=fc/srate
c	if(fc1.lt.0.01) then
c	   call BELL(1)
c	   nerr=1
cc	   print 1, 0.01*srate
cc1	   format(' Minimum fc in this version = ',g13.6,' Hz')
c	   RETURN
c	endif
c
c Calculate the coefficients in a()
c nc=number of coefficients, not counting the central one, a(0)
	sigma=0.132505/fc1
	if(sigma.lt.0.62) then	!narrow impulse -three terms only
	   ALLOCATE(a(2))
	   a(2)=sigma*sigma/2.0
	   a(1)=1.0 - 2.0*a(2)
	   nc=1
	else
	   nc=ifix(4.0*sigma)
	   ALLOCATE(a(nc+1))
c	   if(nc.gt.53) nc=53
	   b=-0.5/(sigma*sigma)
	   a(1)=1.0
	   sum=0.5
	   do i=1,nc
		fi=float(i)
		temp=exp(fi*fi*b)
		a(i+1)=temp
		sum=sum + temp
	   enddo
c    Normalise the coefficients
	   sum=2.0*sum
	   do i=1,nc+1
		a(i)=a(i)/sum
	   enddo
	endif
c Now do the filtering
c=	do i=1,npnt
	do i=np1,np2
	   jl=i - nc
	   if(jl.lt.np1) jl=np1
	   ju=i + nc
	   if(ju.gt.np2) ju=np2
	   sum=0.0
c
	   do j=jl,ju
		k=iabs(j-i) + 1
		sum=sum + datin(j)*a(k)
	   enddo
c
	   datout(i)=sum
	enddo
c
	DEALLOCATE(a)
	RETURN
	end


