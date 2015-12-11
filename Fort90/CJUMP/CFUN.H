/*-----------------------------------------------------------------------

	 															CLIB.H

 -----------------------------------------------------------------------*/

short  Open1401(short *);
void  LastErrCode(short *);
void  Stat1401(short *);
void  TypeOf1401(short *);
void  CharCount(short *);
void  LineCount(short *);
void  Reset1401(short *);
void  OutBufSpace(short *);
void  StateOf1401(short *);
void  KillIO1401(short *);
void  ZeroBlockCount(short *);
void  BlkTransState(short *);
void  StopCircular(short *);

void  DriverVersion(long *);
void  SetTimeout(long *);
void  GetTimeout(long *);
void  BaseAddr1401(long *);

void  GetUserMemorySize(long *,short *);
void  ByteCount(long *,short *);
void  UnSetTransfer(long *,short *);
void	SendChar(char , short *);
void  GetChar(char *,short *);
void  To1401(short [],long *,long *,short *,short *);
void  ToHost(short [],long *,long *,short *,short *);
void  Ld(char *[],char *[], short *,short *);
void  SendString(char *[],int *,short *);
void  GetString(char *[] , long *,short *);
void  LdCmd(char *, short *);
void  LongsFrom1401(long [], short *,short *);
void  StrToLongs(char *,long [],short *,short *);
void  SetTransfer(int *,short [],long *,short *,short *);

struct	string
				{
					char text;
					int leng;
				};
struct string *nString;

/*------------------------------------------------------------------------*/
