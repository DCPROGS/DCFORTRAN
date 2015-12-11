	subroutine DOPEN(idata,ndev)
c 05/26/89 10:31pm DOPEN is Lahey version of DOPEN2
c
c DOPEN2 is modif of DOPEN1 that uses NDEV (read in from DISCNM.FOR) to
c specify disc, and
c IDATA=0 to open directory on this disc
c IDATA=1 to open data file on this disc
c
c Subroutine for DISKIN to open files conveniently so can shut
c and open them many times to avoid having too many open at once
c	unit=12 data on winchester
c	unit=13 data on floppy
c	unit=14 directory on winchester
c	unit=16 directory on floppy
c
	logical floppy
	character ndev*2,infil*14
c
	floppy=ndev.eq.'A:'
	if(idata.eq.0) infil=ndev//'\SCDIRm.DAT'
	if(idata.eq.1) infil=ndev//'\SCDATm.DAT'
	if(floppy) goto 13
c
C Open data file on winchester
      if(idata.eq.1) OPEN(unit=12,file=INFIL,status='unknown',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
C NOTE: RECSIZE=256 real=512 INTEGERS=1024 bytes
c Open directory on winchester
      if(idata.eq.0) OPEN(unit=14,file=INFIL,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
	RETURN
c
c Open data file on floppy
13    if(idata.eq.1) OPEN(unit=13,file=INFIL,status='unknown',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
c Open directory on floppy
      if(idata.eq.0) OPEN(unit=16,file=INFIL,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
c
	RETURN
	END

