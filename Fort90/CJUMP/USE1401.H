/****************************************************************************/
/*                                                                          */
/* (C) Copyright Cambridge Electronic Design Ltd, 1992                      */
/*                                                                          */
/* Title:      USE1401.H                                                    */
/*                                                                          */
/* Version:    1.0                                                          */
/*                                                                          */
/* Date:       23/6/92                                                      */
/*                                                                          */
/* Author:     Paul Cox                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Change log:                                                              */
/*                                                                          */
/*    DATE     REV                 DESCRIPTION                              */
/* ----------- --- ---------------------------------------------------------*/
/*  21/Jul/92  PNC  First version intended to support Mac and DOS/Windows   */
/*                  calls with same interface.                              */
/*  17/Dec/92  TDB  Converted to new version for HIGHC use                  */
/*                                                                          */
/****************************************************************************/

#ifdef __HIGHC__

    #define PASCAL
    #define FAR _Far
    #define F_memcpy memcpy
    #define F_strcat strcat
    #define F_strcmp strcmp
    #define F_strcmpi strcmpi
    #define F_strcpy strcpy
    #define F_strlen strlen
    #define FDBL_DIG LDBL_DIG

    typedef  _Far char * LPSTR;
    typedef unsigned short WORD;
    typedef  _Far WORD * LPWORD;
    typedef unsigned long  DWORD;
    typedef long double fdouble;

    typedef unsigned char BYTE;

    typedef short HANDLE;
    typedef short HWND;

    typedef short BOOLEAN;
    #ifndef TRUE
        #define TRUE 1
        #define FALSE 0
    #endif


/*     #define PASCAL _CC(_CALLING_CONVENTION | _CALLEE_POPS_STACK) */

    #define U14API(retType) PASCAL retType

    #define wsprintf sprintf

    #define HIWORD(l)    ((WORD)((((DWORD)(l)) >> 16) & 0xFFFF))

    #define MSDOS

    #define GMEM_MOVEABLE 0 /***/

#endif

#ifndef __HIGHC__
#ifdef MSDOS
#define U14API(retType) retType
#endif
#endif


/****************************************************************************/
/*                                                                          */
/* Return codes from functions                                              */
/*                                                                          */
/****************************************************************************/

#define U14ERR_NOERROR        0           /* no problems                    */

#define U14ERR_OFF            -500        /* 1401 there but switched off    */
#define U14ERR_NC             -501        /* 1401 not connected             */
#define U14ERR_ILL            -502        /* if present it is ill           */
#define U14ERR_NOIF           -503        /* I/F card missing               */
#define U14ERR_TIME           -504        /* 1401 failed to come ready      */
#define U14ERR_BADSW          -505        /* I/F card bad switches          */
#define U14ERR_PTIME          -506        /* 1401+ failed to come ready     */
#define U14ERR_NOINT          -507        /* couldn't grab the int vector   */
#define U14ERR_INUSE          -508        /* 1401 is already in use         */
#define U14ERR_NODMA          -509        /* couldn't get DMA channel       */

#define U14ERR_NO_SUCH_FN     -520        /* no such function               */
#define U14ERR_NO_SUCH_SUBFN  -521        /* no such sub function           */
#define U14ERR_ERR_NOOUT      -522        /* no room in output buffer       */
#define U14ERR_ERR_NOIN       -523        /* no input in buffer             */
#define U14ERR_ERR_STRLEN     -524        /* string longer than buffer      */
#define U14ERR_LOCKFAIL       -525        /* failed to lock memory          */
#define U14ERR_UNLOCKFAIL     -526        /* failed to unlock memory        */
#define U14ERR_ALREADYSET     -527        /* area already set up            */
#define U14ERR_NOTSET         -528        /* area not set up                */
#define U14ERR_BADAREA        -529        /* illegal area number            */
                                 
#define U14ERR_NOFILE         -540        /* command file not found         */
#define U14ERR_READERR        -541        /* error reading command file     */
#define U14ERR_UNKNOWN        -542        /* unknown command                */
#define U14ERR_HOSTSPACE      -543        /* not enough host space to load  */
#define U14ERR_LOCKERR        -544        /* could not lock resource/command*/
#define U14ERR_CLOADERR       -545        /* CLOAD command failed           */

#define U14ERR_TOXXXERR       -560        /* tohost/1401 failed             */

#define U14ERR_NO386ENH       -580        /* not 386 enhanced mode          */
#define U14ERR_NO1401DRIV     -581        /* no device driver               */
#define U14ERR_DRIVTOOOLD     -582        /* device driver too old          */

#define U14ERR_TIMEOUT        -590        /* timeout occurred               */

#define U14ERR_BUFF_SMALL     -600        /* buffer for getstring too small */
#define U14ERR_CBALREADY      -601        /* there is already a callback    */
#define U14ERR_BADDEREG       -602        /* bad parameter to deregcallback */

#define U14TYPE1401           0           /* standard 1401                  */
#define U14TYPEPLUS           1           /* 1401 plus                      */
#define U14TYPEUNKNOWN        -1          /* dont know                      */

/****************************************************************************/
/*                                                                          */
/* TypeDefs                                                                 */
/*                                                                          */
/****************************************************************************/

typedef enum
{
   tszBytes=0,
   tszWords,
   tszLongs
} TSZ;                                    /* data types for block transfers */

typedef unsigned short TBLOCKENTRY;       /*index the blk transfer table 0-7*/

typedef struct TransferDesc
{
   TBLOCKENTRY wAreaNum;         /* number of transfer area to set up       */
   void FAR *  lpvBuff;          /* address of transfer area                */
   DWORD       dwLength;         /* length of area to set up                */
   short       eSize;            /* size to move (for swapping on MAC)      */
} TRANSFERDESC;

typedef TRANSFERDESC FAR *    LPTRANSFERDESC;

typedef struct                  /* used for get/set standard 1401 registers */
{
   short   sPC;
   char    A;
   char    X;
   char    Y;
   char    stat;
   char    rubbish;
} T1401REGISTERS;

typedef union   /* to communicate with 1401 driver status & control funcs */
{
   short          ints[11];
   long           longs[5];
   T1401REGISTERS registers;
} TCSBLOCK;

typedef TCSBLOCK FAR *  LPTCSBLOCK;

U14API(short) U14LastErrCode(void);

U14API(short) U14Open1401(void);
U14API(short) U14Close1401(void);
U14API(short) U14Reset1401(void);
U14API(short) U14TypeOf1401(void);

U14API(short) U14Stat1401(void);
U14API(short) U14CharCount(void);
U14API(short) U14LineCount(void);

U14API(short) U14SendString(LPSTR lpString);
U14API(short) U14GetString(LPSTR lpBuffer, WORD wMaxLen);
U14API(short) U14SendChar(char cChar);
U14API(short) U14GetChar(LPSTR lpcChar);

U14API(short) U14LdCmd(LPSTR command);
U14API(unsigned long) U14Ld(LPSTR vl,LPSTR str);

U14API(short) U14SetTransfer(LPTRANSFERDESC lpTransDesc);
U14API(short) U14UnSetTransfer(WORD wAreaNum);
U14API(short) U14GetTransfer(LPTRANSFERDESC lpTransDesc);
U14API(short) U14ToHost(LPSTR lpAddrHost,DWORD dwSize,long lAddr1401,
                                                            short eSz);
U14API(short) U14To1401(LPSTR lpAddrHost,DWORD dwSize,long lAddr1401,
                                                            short eSz);

U14API(short) U14StrToLongs(LPSTR lpszBuff,long FAR *lpalNums,short sMaxLongs);
U14API(short) U14LongsFrom1401(long FAR * lpalBuff,short sMaxLongs);

U14API(void)  U14SetTimeout(long lTimeout);
U14API(long)  U14GetTimeout(void);
U14API(short) U14OutBufSpace(void);
U14API(long)  U14BaseAddr1401(void);
U14API(long)  U14DriverVersion(void);
U14API(short) U14GetUserMemorySize (long FAR * lpMemorySize);
U14API(short) U14KillIO1401(void);

U14API(short) U14ZeroBlockCount(void);
U14API(short) U14ByteCount(long FAR * lpHowMany);
U14API(short) U14BlkTransState(void);
U14API(short) U14StopCircular(void);
U14API(short) U14StateOf1401(void);

U14API(short) U14Grab1401(void);
U14API(short) U14Free1401(void);
U14API(short) U14Step1401(void);
U14API(short) U14StepTill1401(long steps, long targetAddress);
U14API(short) U14Registers1401(LPTCSBLOCK lpBlock);
U14API(short) U14Set1401Registers(LPTCSBLOCK lpBlock);
U14API(short) U14SetOrin(short sOrIn);


/****************************************************************************/
/*                                                                          */
/* Windows/Dos Specifics                                                    */
/*                                                                          */
/****************************************************************************/
#ifdef MSDOS

#define  MINDRIVERMAJREV   1     /* minimum driver revision level we need   */

#define  CED_1401_Device_ID   0x2952            /* VxD ID for CED_1401.386  */

/* defines for type of function to ask driver to perform                    */

#define  U14_OPEN1401         0
#define  U14_CLOSE1401        1
#define  U14_SENDSTRING       2
#define  U14_RESET1401        3
#define  U14_GETCHAR          4
#define  U14_SENDCHAR         5
#define  U14_STAT1401         6
#define  U14_LINECOUNT        7
#define  U14_GETSTRING        8
#define  U14_REGCALLBACK      9
#define  U14_GETMONITORBUF    10
#define  U14_SETTRANSFER      11
#define  U14_UNSETTRANSFER    12
#define  U14_GETCIRCSELECTOR  13
#define  U14_GETOUTBUFSPACE   14
#define  U14_GETBASEADDRESS   15
#define  U14_GETDRIVERREVISION   16
#define  U14_GETTRANSFER      17
#define  U14_KILLIO1401       18
#define  U14_BLKTRANSSTATE    19
#define  U14_BYTECOUNT        20
#define  U14_ZEROBLOCKCOUNT   21
#define  U14_STOPCIRCULAR     22
#define  U14_STATEOF1401      23

/* not yet implemented from here */

#define  U14_TYPEOF1401       24
#define  U14_REGISTERS1401    25
#define  U14_GRAB1401         26
#define  U14_FREE1401         27
#define  U14_STEP1401         28
#define  U14_SET1401REGISTERS 29
#define  U14_STEPTILL1401     30
#define  U14_SETORIN          31

/* end of unimplemented from here */

#define  U14_NOSUBFN       0

#ifndef __HIGHC__

#define  WM_CEDCALLBACK_STR   "CEDCALLBACK"

BOOL FAR PASCAL LibMain(HANDLE hMod,WORD wDSeg,
                        WORD wHeapSz,LPSTR lpszCmdLine);
BOOL FAR PASCAL WEP(int nArgument);
U14API(short) U14RegCallBackWnd(HWND hWnd, LPWORD lpwMessCode);
U14API(short) U14DeRegCallBackWnd(HWND hWnd);
U14API(short) U14GetMonBuff(LPSTR lpBuffer, WORD wMaxLen);
U14API(short) U14GetCircSelector(void);

#else

U14API(BOOLEAN) GetDriverReady(void);     /* Equivalent of LibMain for DOSX */

#endif

typedef  void FAR *        U14PARAM;

U14API(short) U14CallDriver(BYTE bMainFn, BYTE bSubFn, U14PARAM lpParams);

#define  U14Status1401(X,Y)   U14CallDriver(X,U14_NOSUBFN,(U14PARAM)Y)
#define  U14Control1401(X,Y)  U14CallDriver(X,U14_NOSUBFN,(U14PARAM)Y)
#endif




/****************************************************************************/
/*                                                                          */
/* Macintosh Specifics                                                      */
/*                                                                          */
/****************************************************************************/
#ifdef macintosh        

#define  MINDRIVERMAJREV   2     /* minimum driver revision level we need   */

#define U14_RES1401COMMAND     '1401'
#define U14_RESPLUSCOMMAND     '1402'

#define k1401CommandFile    "\p1401Commands"
#define k1401DriverName     "\p.Driver1401"

/* Status codes */
    
#define  U14_STAT1401            0   
#define  U14_GETOUTBUFSPACE      2
#define  U14_GETBASEADDRESS      3
#define  U14_STATEOF1401         4     /* These use different elements of   */
#define  U14_TYPEOF1401          4     /* the param block to return info    */
#define  U14_GETDRIVERREVISION   5
#define  U14_REGISTERS1401       6
#define  U14_BYTECOUNT           7
#define  U14_LINECOUNT           8
#define  U14_GETTRANSFER         10  
#define  U14_BLKTRANSSTATE       13
    
/* Control codes */

#define  U14_STOPCIRCULAR        0
#define  U14_SETTRANSFER         2
#define  U14_UNSETTRANSFER       3
#define  U14_KILLIO1401          6
#define  U14_GRAB1401            7
#define  U14_FREE1401            8
#define  U14_STEP1401            9
#define  U14_SET1401REGISTERS    10
#define  U14_STEPTILL1401        11
#define  U14_SETORIN             12
#define  U14_RESET1401           16
#define  U14_ZEROBLOCKCOUNT      17
                                   

extern short ref1401;           /* reference number for 1401 driver, for   */
                                /* status and control                      */
extern short appResFile;        /* reference number for the application    */
extern short cmdResFile;        /* ref num for 1401 command resource files */

U14API(short) U14Status1401(short csCode, LPTCSBLOCK pBlock);
U14API(short) U14Control1401(short csCode, LPTCSBLOCK pBlock);

#endif




