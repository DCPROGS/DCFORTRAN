	subroutine RANDSKV(ix,iy,iz,iwrite,repeat)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from D:\
c (1) Read: (a) Asks whether to repeat last sequence. If NO then output
c		ix,iy,iz and set REPEAT=false. If YES output ixlast,
c		iylast,izlast and set REPEAT=true.
c (2) Write: If REPEAT is true on entry then write nothing to disc
c		in case another repeat is needed. If REPEAT false on
c		entry then (a) replace current ixlast etc by ix etc from
c		disc, and (b) replace ix etc by input values
c MODIFIED 05/17/91 04:16pm so
c (3) iwrite=-1 skips question and does NOT repeat previous sequence
c
	logical repeat,present
	character*30 ranseed
	SAVE ranseed

	n=nblank1(ranseed)
c NB string when undefined is all ASCII 0, and nblank returns position of
c the first ascii 0
	if(n.ge.10) then
	   if(ranseed(n-9:n).ne.'d:\random.dat') then
		ranseed='d:\random.dat'  !file for seeds
	   endif
	else
	   ranseed='d:\random.dat'  !file for seeds
	endif
22	INQUIRE(file=ranseed,exist=present)
	INQUIRE(IOLENGTH=len) RANSEED
      
	if(.not.present) then
	   ranseed='d:\random.dat'		!file for seeds
	   INQUIRE(file=ranseed,exist=present)
	   INQUIRE(IOLENGTH=len) RANSEED
	   if(.not.present) then
		 call guimsg(' ',
     &       'File RANDOM.DAT not found in local directory or root ',3,
     &       0,istatg)
	       RETURN
	   endif
	endif
      OPEN(unit=25,file=ranseed,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=256)
	if(iwrite.eq.1) goto 1

c 	READ
	read(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
30	if(iwrite.eq.0) then
	   call guimsg(' ','Repeat preceding random number sequence ?',
     &   1,3,istatus)
	   if(istatus.eq.6) repeat=.true.
	else if(iwrite.eq.-1) then
	   if(ix.eq.0.or.iy.eq.0.or.iz.eq.0) then
		iwrite=1
		goto 30
	   endif
	   repeat=.false.
	   goto 2
	endif
	if(repeat) then
	   ix=ixlast		!if repeat
	   iy=iylast		!if repeat
	   iz=izlast		!if repeat
	endif
	goto 2

c 	Write
1	continue
	if(repeat) goto 2	!no write
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	ixlast=ix1
	iylast=iy1
	izlast=iz1
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
2	close(unit=25)
	RETURN
	end


