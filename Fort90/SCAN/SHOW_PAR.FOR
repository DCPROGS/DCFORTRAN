	subroutine show_pars(srate,ilen,indfst,isec,nsec)

c	Soubroutine to show some parameters at the special request of users!
c     Enter:
c	srate  = sample rate
c     ilen   = sample length in bytes
c     indfst = current point on screen
c     isec   = current section
c     nsec   = nr sections
c	On the screen:
c	srate  = sample rate
c     ilen   = sample length in points
c     indfst = current point on screen
c     isec   = current section
c     nsec   = nr sections
c     stime  = current time
c     ttime  = sample time
c
	character*10	chsrate,chilen,chindfst,chisec,chstime,chttime
	character*10	chnsec
	character*35	strings(10)

	stime=indfst/srate
	ttime=float(ilen/2)/srate

	call intconv(int(ilen/2),chilen)
	call intconv(indfst,chindfst)
	call intconv(isec,chisec)
	call intconv(nsec,chnsec)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!
c Something funny here: if you call first realtoch and then intconv
c crazy things happen! So beware!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!
	call realtoch(srate,chsrate,10)
	call realtoch(ttime,chttime,10)
	call realtoch(stime,chstime,10)
	strings(1)='Sample rate   (Hz) = '//chsrate
	strings(2)='Sample length      = '//chilen
	strings(3)='Current point      = '//chindfst
	strings(4)='Total sections     = '//chnsec
	strings(5)='Current section    = '//chisec
	strings(6)='Current time  (s)  = '//chstime
	strings(7)='Sample time   (s)  = '//chttime
	n=7
	ibk=14
	icf=6
	ixlo=320
	iylo=-1
	iyhi=400
	call POPTEXT(ixlo,iylo,iyhi,strings,n,ic,ibk,icf)
	end
