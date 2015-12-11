	subroutine READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,imode,defname,npatch,temp)
c
c To read the header record for CJUMP.DAT, and get corrected values if nec.
c (1) Reads record number =IREC (input)
c (2) Reads ikeep1(4,2) as integer*2 for versions up to CJUMP3 and as
c	integer*4 for CJUMP4 onwards, and returns it to calling program
c	as integer*4.
c (3) If ivhclamp,ipatch, ffilt are missing then asks for them, and writes
c	them back to disk (for CJUMP3 onwards)
c (4) Normally call with imode=0 to read header
c	If imode=1 then reads only as far as iver1 to establish value for
c	iver1, and returns it.  With imode=0 this version is used as input
c	and check made that value read from disc is the same
c
c NB iver1=-1001 for CJUMP2 data (values as far as swtime recorded) and
c iver1=-1003 for CJUMP3 data, for which all values up to errfac are recorded
c
      character cdate*11,ctime*11
	character title1*79
	logical vjump,sampv,control,setpatch,sampatch
c	logical discprt,pon,slock,debug,caplock
	integer*2 iver1,iver2
	integer*4 ikeep1(4,2)
	integer*2 ikeep12(4,2)
	integer*2 kstep(5)
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch
	integer*2 ivhclam1,ipatch1
	character*6 defname
c
c Must get iver1 anyway, as value is needed for full read
c	if(imode.eq.1) then	!get iver1 only
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &   vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1
	if(imode.eq.1) RETURN	!get iver1 only
c	endif
c Main part (imode=0)
c NB for iver1=-1001 (CJUMP2) only values up to swtime are defined
1	if(iver1.eq.-1001) then
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime
	   if(iver1.ne.iver2) then
		iver1=iver2		!the real one
		goto 1
	   endif
	else if(iver1.eq.-1003) then
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep12,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclam1,ipatch1,ffilt1
	   if(iver1.ne.iver2) then
		iver1=iver2		!the real one
		goto 1
	   endif
	   do i1=1,4	!convert to integer*4
	     do j=1,2
		 ikeep1(i1,j)=int4(ikeep12(i1,j))
	     enddo
	   enddo
	else if(iver.eq.-1005) then
	    read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclam1,ipatch1,ffilt1,defname,npatch,temp
	else
	    read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclam1,ipatch1,ffilt1
	   if(iver1.ne.iver2) then
		iver1=iver2		!the real one
		goto 1
	   endif
	endif
c Now check values for ivhclamp,ipatch,ffilt (NB read as ivhclam1,ipatch1,
c ffilt1 to avoid overwriting values specified on previous call
	call GETPATCH(ivhclamp,ipatch,ffilt,ivhclam1,ipatch1,ffilt1,
     &	setpatch,sampatch)
c If not correct then write back to disk
	if(setpatch) then
	  if(iver1.ge.-1003) then	!CJUMP3 or earlier
	    write(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep12,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  else
	    write(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  endif
	endif
c
	RETURN
	end

