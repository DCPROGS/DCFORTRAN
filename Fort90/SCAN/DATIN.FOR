	SUBROUTINE DATIN(NBUF,NBLCK,NBS,NLAST,NSECT,NTMAX,DGAIN,IBOFF,
     & IBFLAG,NTRIG,IDISK,ISCRIT,FINTER,RATE,SMULT,RMS,calfac,
     & INVERT,OPENUP,DEBUG,NEWPAR,NWRIT,ICURS,tlast,iexp,ibuf,dgsav,
     & ibassv,iavamp,iampsv,tmaxf,nmaxf,tsect,nfilt1,dtf1,nbread,
     & warned,sdone,ifop,disp,restrt,idest,isect,icsect,ibdisp,isub)
C
	integer ibuf(10240)
	character*1 ans,UC,HELP
	character*1 ndev(5),confil(16),title(36)
c	character*1 CONSAM(16)
	LOGICAL INVERT,OPENUP,DEBUG,NEWPAR,warned,sdone,disp,restrt
	DATA YES,NO,HELP/'Y ','N ','H '/
C
C SUBROUTINE (DERIVED FROM INDAT) FOR OVERLAY IN SCAN1
c Modif. Oct 86 so that sections always numbered from block 0 even
c if we specify to start at a later block
C
101	format(a1)
	IBFLAG=0	!TO SIGNAL IBOFF CHANGE VIA NEWPAR
	IF(NEWPAR) GOTO 310
	if(restrt) goto 362
c
c	NSECT=0		!INITIALISE FOR FIRST SAMPLE
362	continue
c	print 2831
c2831	FORMAT( ' Debug? ')
c	input 101,ans
c	DEBUG=.FALSE.
c	if(UC(ans).EQ.'Y') DEBUG=.TRUE.
c	NDEV='DL1:'		!assume consam on DL1
c	CONSAM=ndev//'CONSAM.DAT'
c	if(idisk.eq.0) CONSAM='DL0:SCNSAV.DAT'
c	if(idisk.eq.1) CONSAM='DL1:SCNSAV.DAT'
	if(ifop.ne.0) goto 10
	if(idisk.eq.0)call rdfile('DL0:CONSAM.DAT',nblck,ierr)
	if(idisk.eq.1)call rdfile('DL1:CONSAM.DAT',nblck,ierr)
	if(idisk.eq.2)call rdfile('DL2:CONSAM.DAT',nblck,ierr)
	if(idisk.eq.3)call rdfile('DL3:CONSAM.DAT',nblck,ierr)
	IF(ierr.eq.0) GOTO 10		!Open file O.K.
	print 11,ierr,idisk,nblck
11	FORMAT(
     & ' PROBLEM IN OPENING CONSAM. ierr,idisk,nblck= ',2i4,i8,/)
	CALL BELL(5)
302	CONTINUE
c NB NBLCK is output (file size in blocks)
10	ifop=1
	if(restrt) goto 361
C Get sample rate etc from SAMDAT.DAT on same disc as data
	if(idisk.eq.0) NDEV='DL0:'
	if(idisk.eq.1) NDEV='DL1:'
	if(idisk.eq.2) NDEV='DL2:'
	if(idisk.eq.3) NDEV='DL3:'
	CONFIL=ndev//'SAMDAT.DAT'
cD	print 21,confil
cd21	format(1x,16a1)
	OPEN (unit=12,name=CONFIL,type='OLD',
     & access='DIRECT',form='UNFORMATTED',recordsize=128,
     & maxrec=1)
C NOTE: RECSIZE=128 real =256 integers= 1 BLOCK
c RATE=effective sample rate (Hz); FILT=effective filter
c setting (as on box if damped) (Hz); CALFAC in pa/ADC unit in SAMDAT
c
	read(12,rec=1) nchan,nblck1,rate,filt,calfac,title
	CLOSE(UNIT=12)
	print 161,title,filt
161	format(' Sample title= ',36a1,/,'  filter (Hz)= ',g13.6)
cd	calfac=1.0
cd	goto 61
	if(calfac.le.0.) goto 104
	print 103,calfac*2048./5.
103	FORMAT(
     & ' Calibration factor (pA per volt in computer)= ',g12.5,
     & ' O.K.? ')
	input 101,ans
	if(UC(ans).eq.'Y') goto 102
104	print 105
105	FORMAT(' Calibration factor (pA per volt in computer)= ')
	input 106,cal
cd61	continue
	calfac=cal*5.0/2048.0		!pA per ADC unit
c	write(12,rec=1) nchan,nblck1,rate,filt,calfac,title
106	format(g13.6)
102	continue
C CONVERT CALFAC TO OUTPUT UNITS PER ADC UNIT (NOTE CALFAC IS
C DIV BY DGAIN IN MAIN PROG SINCE ORIG ADC UNITS HAVE BEEN
C MULTIPLIED BY DGAIN)
C
c
cd	rate=66.66667e3
cd	goto 60
	print 200,rate
200	format(' Sample rate (Hz)= ',g13.6)
	print 206
206	format(' O.K.? ')
	input 101,ans
	if(UC(ans).eq.'Y') goto 208
	print 207
207	format(' Sample rate (Hz)= ')
	input 21,rate
208	continue
21	format(g13.6)
cd60	continue
	finter=1.e6/rate
C FINTER=NO OF MICROSEC BETWEEN DATA POINTS
12	FORMAT(I8)
C
C SET DEFAULT PARAMETER VALUES
c New variable added (DEC 86)
c ISUB=0: Returns from SUBLEV every time i.e. never
c stay in sublev for more than one fitting; if last amp was not shut
c then assumes next is shut
c ISUB=1: Returns from sublevel only if last amp was shut
c ISUB=2: Returns from SUBLEV if last amp was shut OR last amp was
c   within 10% of mean 'full' amp
	OPENUP=.TRUE.	!OPENINGS UPWARDS IN ORIG DATA
	INVERT=.FALSE.	!DO NOT INVERT IN PROG
	SMULT=4.0
	NTRIG=2		!NO OF POINTS ABOVE TRIG LEVEL
	DGAIN=1.0	!GAIN FACTOR FOR DISPLAY
	IBOFF=0		!BASELINE OFFSET FOR DISPLAY (ADC)
	NWRIT=60	!WRITE TINT ETC TO DISC EVERY 60 TRANSITIONS
	IEXP=8		!INITIAL DISPLAY EXPANSION FACTOR FOR SCROL7
	ibdisp=1024	!display baseline (if openings upwards)
	isub=0
	NEWPAR=.FALSE.
C TYPE DEFAULT VALUES
cd	openup=.false.
cd	basoff=0.
cd	goto 243
	print 303
303	FORMAT( ' Openings downwards in data? ')
	input 101,ans
	if(UC(ans).EQ.'Y') OPENUP=.FALSE.
310	continue
c	GOTO 400
51	BASOFF=FLOAT(IBOFF)*CALFAC/ABS(DGAIN)	!OFFSET IN PA
	print 242,INVERT,SMULT,NTRIG,BASOFF,IEXP,NWRIT,dgain,ibdisp
242	FORMAT( ' DEFAULT SETTINGS:',/
     & ,' (1)Invert for display= ',L1,/
     & ,' (2)No of S.D. for critical level= ',F6.2,/
     & ,' (3)No of points above critical level= ',I8,/
     & ,' (4)Baseline offset for display (pA)= ',F6.2,/
     & ,' (5)Initial display expansion factor= ',I6,/
     & ,' (6)Write to disc every ',I3,'th transition '/
     & ,' (7)Display gain factor= ',f6.2,/
     & ,' (8)Display baseline (when openings downwards)= ',i6,/
     & ,' (9)Returns from sublevel mode after each fitting ')
400	print 401
401	FORMAT( ' Alter any defaults (Y,N,H=HELP)? ')
	input 101,ans
	if(UC(ans).EQ.'N') GOTO 243
	if(UC(ans).EQ.HELP) GOTO 51
	print 2421
2421	FORMAT( ' Alter parameter no= ')
	input 12,IPAR
	GOTO (1,2,3,4,5,6,7,8,9),IPAR
1	IF(NEWPAR) GOTO 400	!CANNOT RESET INVERT WITH NEWPAR
	print 202
202	FORMAT( ' Invert events? ')
	input 101,ans
	INVERT=.FALSE.
	if(UC(ans).EQ.'Y') INVERT=.TRUE.
	GOTO 400
4	print 71
71	FORMAT( ' Baseline offset (pA)= ')
	input 21,BASOFF
	IBOFF=IFIXR(BASOFF*ABS(DGAIN)/CALFAC) !OFFSET IN ADC UNITS
	IF(NEWPAR) IBFLAG=1	!SET IF DGAIN OR IBOFF ALTERED BY NEWPAR
	GOTO 400
5	print 81
81	FORMAT( ' Initial display expansion factor (integer)= ')
	input 12,IEXP
	GOTO 400
6	print 91
91	FORMAT( ' Write to disc every Nth transition. N= ')
	input 12,NWRIT
	GOTO 400
2	print 245
245	FORMAT( ' No of S.D. for critical level (REAL)= ')
	input 21,SMULT
	IF(NEWPAR) ISCRIT=IFIX(SMULT*RMS)	!IN ADC UNITS
	GOTO 400
3	print 241
241	FORMAT( ' No of points above critical level= ')
	input 12,NTRIG
	GOTO 400
7	print 304
304	FORMAT( ' Display gain factor (REAL)= ')
	input 21,DGAIN
	IF(NEWPAR) IBFLAG=1	!SET IF DGAIN OR IBOFF ALTERED BY NEWPAR
	goto 400
8	print 305
305	FORMAT( ' Display baseline (integer,-2047 to +2047)= ')
	input 12,ibdisp
	goto 400
9	print 306
306	format(
     & ' ISUB=0: Returns from SUBLEV every time i.e. never',/,
     & ' stay in sublev for more than one fitting; if last amp was',/,
     & ' not shut then assumes next is shut.',/,
     & ' ISUB=1: Returns from sublevel only if last amp was shut.',/,
     & ' ISUB=2: Returns from SUBLEV if last amp was shut OR last',/,
     & ' amp was within 10% of mean ''full'' amp.',/,
     & ' ISUB= ')
	input 12,isub
	goto 400
C
243	CONTINUE
	IF(INVERT) DGAIN=-ABS(DGAIN)		!NEG IF INVERTED
c	IF(NEWPAR) RETURN
	IF(NEWPAR) goto 2741
	IF(INVERT) OPENUP=.NOT.OPENUP	!SO OPENUP=DIRECTION ON DISPLAY
C
C
c500	CONTINUE
cd	nblckt=160	!for consam.tst
cd	nblck=160	!for consam.tst
cd	iblck=0
cd	goto 62
	print 13,nblck
13	FORMAT( ' Total number of blocks of data,N= ',i8)
c	input 12,NBLCK
C
312	print 211
211	FORMAT( ' Starting section number (-1 to specify block)= ')
	input 12,isect
	icsect=0		!unless cursor posn specified
	if(isect.ge.1) goto 212
	print 210
210	FORMAT( ' Starting block number (1,..,N)= ')
	input 12,IBLCK0
cd62	continue
	IBLCK0=IBLCK0-1
	IX=MOD(IBLCK0,NBS)
	IBLCK0=IBLCK0-IX
	IF(IBLCK0.EQ.0) GOTO 312
	isect=iblck0/nbs
	print 3161,isect,IBLCK0+1
3161	FORMAT(' Start section no, block no= ',2I8)
212	print 111
111	FORMAT( ' Specify cursor position? ')
	input 101,ans
	if(UC(ans).EQ.'N') GOTO 110
	print 112
112	FORMAT( ' ICURS= ')
	input 12,ICURS
	ICURS=-ICURS
	icsect=isect
C NEG ICURS PREVENTS RESET OF ICURS TO ZERO IN ROLDAT. IT CAUSES
C DASHED CURSOR IN SCROL8 (WHICH ALSO RESETS ICURS POSITIVE)
110	CONTINUE
C NOTE BLOCKS ARE ACTUALLY NUMBERED 0,1,..,NBLCKT-1.
C IBLCK0 USED TO CALC ISECT FROM START BLOCK WHEN GOING BACK.
C CALC NO OF BLOCKS TO BE PROCESSED=NBLCK. THIS IS LESS THAN NBLCKT IF
C WE DO NOT START AT BEGINNING (IBLCK=0)-no- numbered from start now
c^^^	NBLCK=NBLCKT-IBLCK0
c	NSECT1=1+NBLCK/NBS
	NSECT=1+NBLCK/NBS
	NLAST=NBLCK-(NSECT-1)*NBS !NO OF BLOCKS FOR DISP IN LAST SECTION
	IF(NLAST.GT.0) GOTO 300
	IF(NLAST.LT.0) print 301
301	FORMAT( ' NLAST IS NEGATIVE!')
	NSECT=NSECT-1	!IF NLAST=0
	NLAST=NBS
300	CONTINUE
c	NSECT=NSECT+NSECT1
	print 3001,NSECT
3001	FORMAT( ' Number of sections= ',I8)
C NSECT=TOTAL NO OF SECTIONS TO BE ANALYSED. INITIALLY SET TO ZERO
c
c Now bit moved here from main prog
C NEXT BIT DONE ONLY IF NEWPAR ^^needs check??
	IF(DISP) GOTO 802
	IF(NEWPAR) GOTO 2741
	GOTO 361
2741	IF(IBFLAG.EQ.0) GOTO 259	!IBOFF AND DGAIN NOT RESET
	DGSAV=DGAIN/DGSAV	!FACTOR TO CHANGE OLD TO NEW GAIN
	DO 2045 I=1,NBUF
	X=DGSAV*FLOAT(IBUF(I)-IBSAV)
2045	IBUF(I)=IFIX(X+SIGN(0.5,X))+IBOFF	!CORRECT ROUNDING IF NEG
C NEXT BIT FOR SCALING BASELINE ETC NOT GOOD SO REDO RMS TO GET NEW
C YBASE ETC
	IBASSV=-9999		!NOT YET DETERMINED
	X=DGSAV*FLOAT(IAVAMP)
	IAVAMP=IFIX(X+SIGN(0.5,X))
	IF(IAMPSV.NE.-9999)IAMPSV=IFIXR(DGSAV*FLOAT(IAMPSV))
	GOTO 278	!NEW RMS THEN RE-SCROLL AFTER NEWPAR
C
361	TMAXF=FLOAT(NFILT1-1)*DTF1	!TOTAL LENGTH OF FILT IN MUS
C CALC NMAXF=NO OF DATA POINTS,ROUNDED UP,CORRESPONDING TO TMAXF,I.E.
C # OF POINTS AFTER A TRIGGER FOR TRANSITION TO BE COMPLETE.
	NMAXF=IFIX(TMAXF/FINTER)
d	print 260,TMAXF,NMAXF
d260	FORMAT( ' TMAXF,NMAXF= ',G13.6,I8)
C NB SCROL8.MAC is ALWAYS IN AUTO MODE UNLESS ITRIG
C IN CALL SET TO ZERO
802	CONTINUE
C
	TSECT=FLOAT(NBUF)*FINTER
C TSECT=TIME (MICROSEC) FROM FIRST POINT OF ONE SECTION
C TO FIRST POINT OF THE NEXT
C
C PREPARE TO READ FIRST SECTION OF NBUF POINTS
	NBREAD=NBS	!NO OF BLOCKS TO BE READ EACH TIME (EX LAST)
	WARNED=.FALSE.
	sdone=.false.	!no baseline SD yet
	IFOP=0		!set after file opened
	idest=0
	if(.not.restrt) idest=2151  !to indicate 1st read in DATGET
c
	RETURN
278	idest=278
	RETURN
259	idest=259
	END


                                                                                                        
