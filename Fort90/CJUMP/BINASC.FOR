
c=======================================================================
	program consfile
c=======================================================================
	integer*2		olddata(24000),data(10)
	character*24      oldfil,newfil
	character*20      oldfile,newfile
	character*2		ndevo,ndevn
	character*1		ans,getch,UC
	character    cans*30,def*30
	logical	present
c=======================================================================
	ibk=0
	iclo=2
	ncol=72
	irhi=3
	nrow=10
	icqd=14
	icwd=11
	icb1=1
	oldfile='CONSAM.DAT'
	newfile='PERSIST.DAT'
	ndevo='C:'
	ndevn='C:'
	oldfil=ndevo//'\'//oldfile
	newfil=ndevn//'\'//newfile
	call mode(18)
	call clears(0,480,0)
	ibox=1
      call DEFDIALOG(1,irhi,iclo,nrow,ncol,ibk)
	call opendialog(ibox,icb1,.true.)
	CALL WDIALOG(1,'OPTIONS FOR FILES NAMES AND DISC PARTITIONS ',
     &ICWD)
	call wdialog(1,'YOU HAVE TO ENTER DISC PARTITION, FILE NAME'//
     & ' AND FILE EXTENTION',icwd)
	call wdialog(1,'IF YOU PRESS ENTER, THE DEFAULT WILL BE CHOOSED',
     &ICWD)
765	continue
	nc=nblank1(oldfil)
	call qdialog(1,'Name for Data file ',
     &' ['//oldfil(1:nc)//'] = ',icqd,cans)
	if(cans.eq.' ') goto 654
	nc=nblank1(cans)
	if(nc.le.24.and.cans(2:3).eq.':\') then
		oldfil=cans(1:nc)
		oldfile=cans(4:nc)
		ndevo=cans(1:2)
		goto 654
	endif
	call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',icwd)
	goto 765
654	continue
	np=nblank1(newfil)
	call qdialog(1,'Name for New file ',
     &' ['//newfil(1:np)//
     &'] = ',icqd,cans)
	if(cans.eq.' ') goto 653
	np=nblank1(cans)
	if(np.le.24.and.cans(2:3).eq.':\') then
	      newfil=cans(1:np)
		newfile=cans(4:nc)
		ndevn=cans(1:2)
		goto 653
	endif
	call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',icwd)
	goto 654
653	continue
	ans=getch(b)
	call wdialog(1,'Files in use : '//OLDfil(1:nc)//' and '//
     & newfil(1:np),icwd)
	INQUIRE(file=oldfil,exist=present,flen=ilen)
	if(present) then
	call wdialog(1,'START TRANSFER',icwd)
	   ilen1=ilen-512
	   irec=513
	   j=1
	   OPEN(unit=14,file=oldfil,status='UNKNOWN',
     &   access='TRANSPARENT')
	   OPEN(unit=15,file=newfil,status='unknown',
     &	access='SEQUENTIAL',form='formatted',recl=240)
33	   read(14,rec=irec) (olddata(i),i=j,j+39)
	   write(unit=15,fmt=25) (olddata(i),i=j,j+39)
25       FORMAT(40I6)
	   j=j+40
	   irec=irec+80
	   if(j+40.le.24000) goto 33
	   CLOSE(unit=12)
	   CLOSE(unit=15)
	else
	    ans='Y'
	    call defolta(ans,def)
	    call qdialog(1,'File not present; Another go ',def,icqd,cans)
	    call getinpa(cans,ans)
	    if(UC(ans).eq.'N') goto 99
	    goto 765
	endif
	call wdialog(1,'END TRANSFER',icwd)
	ans=getch(ktype)
99    continue
	call clrdialog(1,0)
	call enddialog(1,0)
	call mode(3)
	   OPEN(unit=15,file=newfil,status='unknown',
     &	access='SEQUENTIAL',form='formatted',recl=240)
	   read(unit=15,fmt=25) (data(i),i=1,10)
	   CLOSE(unit=15)
	print*,'nr = ',j
	print 26, data
26    FORMAT(10I6)

	ans=getch(ktype)
	end
