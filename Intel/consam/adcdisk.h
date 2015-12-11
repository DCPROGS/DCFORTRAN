/*****************************************************************************
**
** AdcDisk.h
**
** Definitions of the AdcDisk function
**
** 09/Nov/00    TDB Created first version
**
*/

#ifndef __ADCDISK__
#define __ADCDISK__

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes, the USE1401 error codes can also be returned */
#define ADE_CREATE_FAILED   -1000
#define ADE_BAD_WRITE       -1001
#define ADE_BAD_SEEK        -1002
#define ADE_TOO_FAST        -1003
#define ADE_ABORTED         -1004
#define ADE_BAD_PARAM       -1005

/*
** This is a prototype of the callback function used to indicate progress
** and allow cancel. The parameter is the bytes sampled so far. The result
** is zero to continue, -ve to abort and delete data, +ve to stop now but
** keep data already collected.
*/
typedef int TCancelFunc(long);

/*
** Now the definition of the AdcDisk function
*/
int AdcDisk(const char* szFile, int nHead, long nBytes, const char* szChans,
                      const char* szClk, int div1, int div2, 
					  TCancelFunc* pfCancel, const char* szDir, int nBuff, 
					  int counter, int *button, float sec);

#ifdef __cplusplus
}
#endif

#endif /* __ADCDISK__ */
