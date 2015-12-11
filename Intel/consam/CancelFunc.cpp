//
#include "StdAfx.h"
#include "conio.h"

long lLast = -1;

int __stdcall CancelFunc(long lDone)
{
    int iCancel = 0;

    if (lLast != lDone)
    {
       
        lLast = lDone;
        if (_kbhit())
        {
            _getch();
            iCancel = 1;
        }
    }
    return iCancel;
}