/****************************************************************************/
/*                                                                          */
/* (C) Copyright Cambridge Electronic Design Ltd, 1992                      */
/*                                                                          */
/* Title:      USE1401.H                                                    */
/*                                                                          */
/* Version:    3.20                                                          */
/*                                                                          */
/* Date:       29/07/97                                                     */
/*                                                                          */
/* Author:     Paul Cox, Tim Bergel                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Change log:                                                              */
/*                                                                          */
/*    DATE     REV                 DESCRIPTION                              */
/* ----------- --- ---------------------------------------------------------*/
/*  21/Jul/92  PNC  First version intended to support Mac and DOS/Windows   */
/*                  calls with same interface.                              */
/*   8/Feb/93  TDB  Fixed up GetTransfer - MSDOS version needs separate     */
/*                  structure to hold setup results from VXD.               */
/*  24/Feb/93  TDB  USE1401.H protected against multiple includes           */
/*  14/May/93  TDB  Multiple 1401 support added, extra handle parameter     */
/*                  added to almost every function. Required driver level   */
/*                  increased by 1 for both platforms.                      */
/*  01/Jun/93  PNC  Modified to support Windows NT kernel mode driver,      */
/*                  NT compile requires "winioctl.h" to be included before  */
/*                  use1401.h.                                              */
/*   4/Oct/93  TDB  Converted SetTransfer to SetTransArea with separate     */
/*                  parameters - better for Visual Basic use.               */
/*  19/Jan/94  MAE  Added U14 prefix to U14WhenToTimeOut & U14PassedTime,   */
/*                  and now exported in Use1401.h                           */
/*  14/Jun/94  TDB  Added functions to support self-test.                   */
/*  29/Mar/95  TDB  Added MAXAREAS so that USE1401 knows about areas.       */
/*   9/Jun/95  TDB  Added code for 16-bit TYPEOF1401, adjusted codes        */
/*                  for 32-bit build so that it matches.                    */
/*  23/Jun/95  TDB  Added code for TransferFlags enquiry, all modes.        */
/*  30/Oct/95  TDB  Added GetErrorString function to help applications.     */
/*  15/Apr/96  TDB  Changed LPSTR parameters to LPCSTR wherever             */
/*                  possible to allow easier use.                           */
/*  18/Jul/96  SMP  Added support for cross development system.             */
/*   5/Nov/96  TDB  Tidied-up parameters - Mac work had introduced char*    */
/*  10/Jun/97  TDB  Fixed up GetTransfer for 32-bit use, won't work on NT.  */
/*  29/Jul/97  TDB  Added U14API define for Borland C++ Builder.            */
/*   4/Nov/97  TDB  Added self-test codes for Power Mac                     */
/*   3/Jun/98  TDB  Added support for the power1401. V3.20                  */
/*   4/Dec/98  TDB  Transfer table size enlarged to cope with 1M areas.     */
/*  18/Mar/99  TDB  U14MonitorRev function added.                           */
/*  17/Nov/99  TDB  Dignostic functions added to support setup and debug.   */
/*  23/Feb/01  TDB  Added support for the micro1401 mkII.                   */
/*                                                                          */
/****************************************************************************/

#ifndef __USE1401_H__                  /* Protect against multiple includes */
#define __USE1401_H__

#ifndef RC_INVOKED

#include "machine.h"

#ifdef macintosh
#define U14API(retType) pascal retType
#ifndef __TYPES__
#include <types.h>
#endif
#endif

#if (defined(_IS_MSDOS_) || defined(_IS_WINDOWS_)) && !defined(_MAC)
#define U14API(retType) retType FAR PASCAL
#endif

#ifdef _MAC
#undef U14API
#define U14API(retType) retType
#endif

/* This sets up for Borland C++ Builder, may not work with earlier Borland */
/* C/C++ compilers. We undefine as the normal Windows form is present. */
#ifdef __BCPLUSPLUS__
#undef U14API
#define U14API(retType) retType __stdcall
#endif

#endif                                          /* End of ifndef RC_INVOKED */

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
#define U14ERR_PTIME          -506        /* 1401+ didn't come ready UNUSED */
#define U14ERR_NOINT          -507        /* couldn't grab the int vector   */
#define U14ERR_INUSE          -508        /* 1401 is already in use         */
#define U14ERR_NODMA          -509        /* couldn't get DMA channel       */
#define U14ERR_BADHAND        -510        /* handle provided was bad        */
#define U14ERR_BAD1401NUM     -511        /* 1401 number provided was bad   */

#define U14ERR_NO_SUCH_FN     -520        /* no such function               */
#define U14ERR_NO_SUCH_SUBFN  -521        /* no such sub function           */
#define U14ERR_ERR_NOOUT      -522        /* no room in output buffer       */
#define U14ERR_ERR_NOIN       -523        /* no input in buffer             */
#define U14ERR_ERR_STRLEN     -524        /* string longer than buffer      */
#define U14ERR_LOCKFAIL       -525        /* failed to lock memory          */
#define U14ERR_UNLOCKFAIL     -526        /* failed to unlock memory        */
#define U14ERR_ALREADYSET     -527        /* area or grab1401 already set   */
#define U14ERR_NOTSET         -528        /* area or grab1401 not set up    */
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

#define U14ERR_DRIVCOMMS      -610        /* failed talking to driver       */
#define U14ERR_OUTOFMEMORY    -611        /* neede memory and couldnt get it*/

#define U14TYPE1401           0           /* standard 1401                  */
#define U14TYPEPLUS           1           /* 1401 plus                      */
#define U14TYPEU1401          2           /* u1401                          */
#define U14TYPEPOWER          3           /* power1401                      */
#define U14TYPEU14012         4           /* u1401 mkII                     */
#define U14TYPEUNKNOWN        -1          /* dont know                      */

// Transfer flags to allow driver capabilities to be interrogated
#define U14TF_USEDMA          1           /* Transfer flag for use DMA      */
#define U14TF_MULTIA          2           /* Transfer flag for multi areas  */
#define U14TF_FIFO            4           /* for FIFO interface card        */

#define U14TF_DIAG            256         /* Diagnostics\debug functions    */

// Definitions of element sizes for DMA transfers - to allow byte-swapping
#define ESZBYTES              0           /* BYTE element size value        */
#define ESZWORDS              1           /* WORD element size value        */
#define ESZLONGS              2           /* long element size value        */
#define ESZUNKNOWN            0           /* unknown element size value     */

// These define required access types for the debug\diagnostiscs function
#define BYTE_SIZE             1           /* 8-bit access                   */
#define WORD_SIZE             2           /* 16-bit access                  */
#define LONG_SIZE             3           /* 32-bit access                  */

/****************************************************************************/
/*                                                                          */
/* TypeDefs                                                                 */
/*                                                                          */
/****************************************************************************/

#if (defined(_IS_MSDOS_) || defined(_IS_WINDOWS_)) && !defined(_MAC)
#ifndef RC_INVOKED
#pragma pack(1)
#endif
#endif

/* This is the structure used to set up a transfer area */
typedef struct VXTransferDesc    /* use1401.c and use1432x.x use only       */
{
   WORD        wArea;            /* number of transfer area to set up       */
   WORD        wAddrSel;         /* 16 bit selector for area                */
   DWORD       dwAddrOfs;        /* 32 bit offset for area start            */
   DWORD       dwLength;         /* length of area to set up                */
} VXTRANSFERDESC;

typedef  void FAR *        U14PARAM;

/* 
** This lot defines the structure used to retrieve information about a
**  transfer area and the blocks into which it is split
*/
#define  MAXAREAS   8   /* The number of transfer areas supported by driver */

#define  GET_TX_MAXENTRIES   257       /* Array size for GetTransfer struct */

typedef struct                          /* used for U14_GetTransfer results */
{                                          /* Info on a single mapped block */
   long     physical;
   long     size;
} TXENTRY;

typedef struct TGetTxBlock              /* used for U14_GetTransfer results */
{                                               /* matches structure in VXD */
   long     size;
   long     linear;
   short    seg;
   short    reserved;
   short    avail;
   short    used;
   TXENTRY  entries[GET_TX_MAXENTRIES];       /* Array of mapped block info */
} TGET_TX_BLOCK;

typedef TGET_TX_BLOCK FAR *    LPGET_TX_BLOCK;

typedef struct                  /* used for get/set standard 1401 registers */
{
   short   sPC;
   char    A;
   char    X;
   char    Y;
   char    stat;
   char    rubbish;
} T1401REGISTERS;

typedef union     /* to communicate with 1401 driver status & control funcs */
{
   char           chrs[22];
   short          ints[11];
   long           longs[5];
   T1401REGISTERS registers;
} TCSBLOCK;

#if defined(WIN32) || defined(_MAC)
typedef TCSBLOCK*  LPTCSBLOCK;
#else
typedef TCSBLOCK FAR *  LPTCSBLOCK;

#endif

#if (defined(_IS_MSDOS_) || defined(_IS_WINDOWS_)) && !defined(_MAC)
#ifndef RC_INVOKED
#pragma pack()
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

U14API(long) U14WhenToTimeOut(short hand);
        /* Returns the time to time out in time units suitable for the */
        /* machine we are running on  ie millsecs for pc, 1/60th for Mac */

U14API(short) U14PassedTime(long lCheckTime);
        /* Returns non zero if the timed passed in has been passed 0 if not */

U14API(short) U14LastErrCode(short hand);

U14API(short) U14Open1401(short n1401);
U14API(short) U14Close1401(short hand);
U14API(short) U14Reset1401(short hand);
U14API(short) U14TypeOf1401(short hand);

U14API(short) U14Stat1401(short hand);
U14API(short) U14CharCount(short hand);
U14API(short) U14LineCount(short hand);

U14API(short) U14SendString(short hand, LPCSTR lpString);
U14API(short) U14GetString(short hand, LPSTR lpBuffer, WORD wMaxLen);
U14API(short) U14SendChar(short hand, char cChar);
U14API(short) U14GetChar(short hand, LPSTR lpcChar);

U14API(short) U14LdCmd(short hand, LPCSTR command);
U14API(DWORD) U14Ld(short hand, LPCSTR vl, LPCSTR str);

U14API(short) U14SetTransArea(short hand, WORD wArea, void FAR * lpvBuff,
                                            DWORD dwLength, short eSz);
U14API(short) U14UnSetTransfer(short hand, WORD wArea);

U14API(short) U14ToHost(short hand, LPSTR lpAddrHost,DWORD dwSize,DWORD dw1401,
                                                            short eSz);
U14API(short) U14To1401(short hand, LPCSTR lpAddrHost,DWORD dwSize,DWORD dw1401,
                                                            short eSz);

U14API(short) U14StrToLongs(LPCSTR lpszBuff,long FAR *lpalNums,short sMaxLongs);
U14API(short) U14LongsFrom1401(short hand, long FAR * lpalBuff,short sMaxLongs);

U14API(void)  U14SetTimeout(short hand, long lTimeout);
U14API(long)  U14GetTimeout(short hand);
U14API(short) U14OutBufSpace(short hand);
U14API(long)  U14BaseAddr1401(short hand);
U14API(long)  U14DriverVersion(void);
U14API(long)  U14DriverType(void);
U14API(short) U14GetUserMemorySize (short hand, long FAR * lpMemorySize);
U14API(short) U14KillIO1401(short hand);

U14API(short) U14ZeroBlockCount(short hand);
U14API(short) U14ByteCount(short hand, long FAR * lpHowMany);
U14API(short) U14BlkTransState(short hand);
U14API(short) U14StopCircular(short hand);
U14API(short) U14StateOf1401(short hand);

U14API(short) U14Grab1401(short hand);
U14API(short) U14Free1401(short hand);
U14API(short) U14Step1401(short hand);
U14API(short) U14StepTill1401(short hand, long steps, long targetAddress);
U14API(short) U14Registers1401(short hand, LPTCSBLOCK lpBlock);
U14API(short) U14Set1401Registers(short hand, LPTCSBLOCK lpBlock);
U14API(short) U14SetOrin(short hand, short sOrIn);

U14API(short) U14Peek1401(short hand, long lAddr, long lSize,
                                      long lRepeats);
U14API(short) U14Poke1401(short hand, long lAddr, long lValue,
                                      long lSize, long lRepeats);
U14API(short) U14Ramp1401(short hand, long lAddr, long lDef, long lEnable,
                                      long lSize, long lRepeats);
U14API(short) U14RampAddr(short hand, long lDef, long lEnable,
                                      long lSize, long lRepeats);
U14API(short) U14StopDebugLoop(short hand);
U14API(short) U14GetDebugData(short hand, long* plValue);

U14API(short) U14StartSelfTest(short hand);
U14API(short) U14CheckSelfTest(short hand, long FAR * lpData);
U14API(short) U14TransferFlags(short hand);
U14API(void)  U14GetErrorString(short nErr, LPSTR lpStr, WORD wMax);
U14API(long)  U14MonitorRev(short hand);

#ifdef __cplusplus
}
#endif

/****************************************************************************/
/*                                                                          */
/* Windows 3.1 /Dos Specifics                                               */
/*                                                                          */
/****************************************************************************/
#if ((defined(_IS_MSDOS_) || defined(_INC_WINDOWS)) && !defined(WIN32))

#define  MINDRIVERMAJREV   1    /* minimum driver revision level we need    */

#define  CED_1401_Device_ID   0x2952            /* VxD ID for CED_1401.386  */

/* defines for type of function to ask driver to perform                    */
/* NOTE: any changes here must be reflected in WIN16 constants in NT section*/

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

#define  U14_REGISTERS1401    24
#define  U14_GRAB1401         25
#define  U14_FREE1401         26
#define  U14_STEP1401         27
#define  U14_SET1401REGISTERS 28
#define  U14_STEPTILL1401     29
#define  U14_SETORIN          30
#define  U14_STARTSELFTEST    31
#define  U14_CHECKSELFTEST    32
#define  U14_TYPEOF1401       33
#define  U14_TRANSFERFLAGS    34
#define  U14_DBGPEEK          35
#define  U14_DBGPOKE          36
#define  U14_DBGRAMPDATA      37
#define  U14_DBGRAMPADDR      38
#define  U14_DBGGETDATA       39
#define  U14_DBGSTOPLOOP      40


#define  U14_NOSUBFN       0

#define  WM_CEDCALLBACK_STR   "CEDCALLBACK"

BOOL FAR PASCAL LibMain(HANDLE hMod,WORD wDSeg,
                        WORD wHeapSz,LPSTR lpszCmdLine);
BOOL FAR PASCAL WEP(int nArgument);

U14API(short) U14CallDriver(short hand, BYTE bMainFn, BYTE bSubFn, U14PARAM lpParams);

U14API(short) U14RegCallBackWnd(short n1401, HWND hWnd, LPWORD lpwMessCode);
U14API(short) U14DeRegCallBackWnd(HWND hWnd);
U14API(short) U14GetMonBuff(LPSTR lpBuffer, WORD wMaxLen);
U14API(short) U14GetCircSelector(void);

U14API(short) U14GetTransfer(short hand, LPGET_TX_BLOCK lpTransBlock);

#define  U14Status1401(H,X,Y)   U14CallDriver(H,X,U14_NOSUBFN,(U14PARAM)Y)
#define  U14Control1401(H,X,Y)  U14CallDriver(H,X,U14_NOSUBFN,(U14PARAM)Y)
#endif

/****************************************************************************/
/*                                                                          */
/* Windows NT Specifics                                                     */
/*                                                                          */
/****************************************************************************/
#if defined(WIN32) && !defined(_MAC)
				   /* if we are in NT/Win95/Win32s we have extra bits       */

#define  MINDRIVERMAJREV   1    /* minimum driver revision level we need    */

#define  FILE_DEVICE_CED1401    0x8001

#define  FNNUMBASE              0x800

/* defines for type of function to ask driver to perform                    */
/* these must be matched in the Use1401 DLL which will call us              */
/* Define the IOCTL codes we will use.  The IOCTL code contains a command   */
/* identifier, plus other information about the device, the type of access  */
/* with which the file must have been opened, and the type of buffering.    */
/*
/* The IOCTL function codes from 0x80 to 0xFF are for developer use.        */

#define  U14_OPEN1401            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE,               \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_CLOSE1401           CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+1,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_SENDSTRING          CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+2,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_RESET1401           CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+3,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETCHAR             CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+4,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_SENDCHAR            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+5,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STAT1401            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+6,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_LINECOUNT           CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+7,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETSTRING           CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+8,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_REGCALLBACK         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+9,             \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETMONITORBUF       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+10,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_SETTRANSFER         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+11,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_UNSETTRANSFER       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+12,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETCIRCSELECTOR     CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+13,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETOUTBUFSPACE      CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+14,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETBASEADDRESS      CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+15,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETDRIVERREVISION   CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+16,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GETTRANSFER         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+17,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_KILLIO1401          CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+18,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_BLKTRANSSTATE       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+19,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_BYTECOUNT           CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+20,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_ZEROBLOCKCOUNT      CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+21,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STOPCIRCULAR        CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+22,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STATEOF1401         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+23,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_REGISTERS1401       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+24,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_GRAB1401            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+25,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_FREE1401            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+26,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STEP1401            CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+27,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_SET1401REGISTERS    CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+28,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STEPTILL1401        CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+29,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_SETORIN             CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+30,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_STARTSELFTEST       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+31,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_CHECKSELFTEST       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+32,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_TYPEOF1401          CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+33,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_TRANSFERFLAGS       CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+34,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGPEEK             CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+35,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGPOKE             CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+36,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGRAMPDATA         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+37,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGRAMPADDR         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+38,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGGETDATA          CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+39,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

#define  U14_DBGSTOPLOOP         CTL_CODE( FILE_DEVICE_CED1401,     \
                                           FNNUMBASE+40,            \
                                           METHOD_BUFFERED,         \
                                           FILE_ANY_ACCESS)

/*
** 16-bit function codes for re-mapping between systems.
*/
#define  WIN16_OPEN1401             0
#define  WIN16_CLOSE1401            1
#define  WIN16_SENDSTRING           2
#define  WIN16_RESET1401            3
#define  WIN16_GETCHAR              4
#define  WIN16_SENDCHAR             5
#define  WIN16_STAT1401             6
#define  WIN16_LINECOUNT            7
#define  WIN16_GETSTRING            8
#define  WIN16_REGCALLBACK          9
#define  WIN16_GETMONITORBUF        10
#define  WIN16_SETTRANSFER          11
#define  WIN16_UNSETTRANSFER        12
#define  WIN16_GETCIRCSELECTOR      13
#define  WIN16_GETOUTBUFSPACE       14
#define  WIN16_GETBASEADDRESS       15
#define  WIN16_GETDRIVERREVISION    16
#define  WIN16_GETTRANSFER          17
#define  WIN16_KILLIO1401           18
#define  WIN16_BLKTRANSSTATE        19
#define  WIN16_BYTECOUNT            20
#define  WIN16_ZEROBLOCKCOUNT       21
#define  WIN16_STOPCIRCULAR         22
#define  WIN16_STATEOF1401          23
#define  WIN16_REGISTERS1401        24
#define  WIN16_GRAB1401             25
#define  WIN16_FREE1401             26
#define  WIN16_STEP1401             27
#define  WIN16_SET1401REGISTERS     28
#define  WIN16_STEPTILL1401         29
#define  WIN16_SETORIN              30
#define  WIN16_STARTSELFTEST        31
#define  WIN16_CHECKSELFTEST        32
#define  WIN16_TYPEOF1401           33
#define  WIN16_TRANSFERFLAGS        34
#define  WIN16_DBGPEEK              35
#define  WIN16_DBGPOKE              36
#define  WIN16_DBGRAMPDATA          37
#define  WIN16_DBGRAMPADDR          38
#define  WIN16_DBGGETDATA           39
#define  WIN16_DBGSTOPLOOP          40


#ifndef RC_INVOKED
#pragma pack(1)
#endif

typedef struct paramBlk
{
    short       sState;
    TCSBLOCK    csBlock;
} PARAMBLK;

typedef PARAMBLK*   PPARAMBLK;


#ifndef RC_INVOKED
#pragma pack()
#endif

U14API(short) U14GetTransfer(short hand, LPGET_TX_BLOCK lpTransBlock);

U14API(short) U14Status1401(short sHand, LONG lCode, TCSBLOCK* pBlk);
U14API(short) U14Control1401(short sHand, LONG lCode, TCSBLOCK* pBlk);

#endif

/****************************************************************************/
/*                                                                          */
/* Macintosh Specifics                                                      */
/*                                                                          */
/****************************************************************************/
#if defined(macintosh) || defined(_MAC)

#define  MINDRIVERMAJREV   2     /* minimum driver revision level we need   */

/* The basic resource ID, modified by 1401 type (plus is '1402 etc.) */
#define U14_RES1401COMMAND     '1401'      /* We should calculate these ... */
#define U14_RESPLUSCOMMAND     '1402'
#define U14_RESU1401COMMAND    '1403'
#define U14_RESPOWERCOMMAND    '1404'
#define U14_RESU14012COMMAND   '1405'

#define k1401CommandFile    "\p1401Commands"
#define k1401DriverName     "\p.Driver1401"

#define  MAXAREAS   8   /* The number of transfer areas supported by driver */

/* Structure for GetTransfer results(NB also used internally by NT version) */
typedef struct TransferDesc
{
   WORD        wArea;            /* number of transfer area to set up       */
   void FAR *  lpvBuff;          /* address of transfer area                */
   DWORD       dwLength;         /* length of area to set up                */
   short       eSize;            /* size to move (for swapping on MAC)      */
} TRANSFERDESC;

typedef TRANSFERDESC FAR *    LPTRANSFERDESC;


/* Status codes */
#define  U14_STAT1401            0   
#define  U14_GETOUTBUFSPACE      2
#define  U14_GETBASEADDRESS      3
#define  U14_STATEOF1401         4     /* These use different elements of */
#define  U14_TYPEOF1401          4     /* the param block to return info */
#define  U14_GETDRIVERREVISION   5
#define  U14_REGISTERS1401       6
#define  U14_BYTECOUNT           7
#define  U14_LINECOUNT           8
#define  U14_GETTRANSFER         10  
#define  U14_BLKTRANSSTATE       13
#define  U14_CHECKSELFTEST       14
#define  U14_DBGGETDATA          15
    
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
#define  U14_STARTSELFTEST       19
#define  U14_DBGPEEK             20
#define  U14_DBGPOKE             21
#define  U14_DBGRAMPDATA         22
#define  U14_DBGRAMPADDR         23
#define  U14_DBGSTOPLOOP         24
                                   

U14API(short) U14GetTransfer(short hand, LPTRANSFERDESC lpTransDesc);

U14API(short) U14Status1401(short hand, short csCode, LPTCSBLOCK pBlock);
U14API(short) U14Control1401(short hand, short csCode, LPTCSBLOCK pBlock);

#endif

#endif                                       /* End of ifndef __USE1401_H__ */
