/*
TUSE.C    Tests USE1401.C unit

MetaWare High C: hc386 -g -w4 tuse.c use1401.obj calldrv.obj dosx32.lib

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "use1401.h"

main()
{
    int    flag;
    int    status,point;
    char   buff[20];
    short  test[80000];
    long    lval[4];

    flag = U14Open1401();
    printf("Open flag was %d\n", flag);
    if (flag == U14ERR_NOERROR)
    {
        flag = U14SendString("KILL;EEE;ERR;");
        flag = U14GetString((LPSTR)buff,15);
        printf("Return string %s\n", buff);
        test[0] = 12;
        test[1] = 15;
        test[2] = 18;
        test[3] = 15;
        test[4] = 12;
        flag = U14SendString("WRADR,2,2,0;");
        flag = U14SendString("RDADR,2,2;");
        flag = U14GetString((LPSTR)buff,15);
        printf("RDADR before To1401 %s\n", buff);
        flag = U14To1401((LPSTR)test,80000,0,2);
        printf("Return value from To1401 %d\n",flag);
        flag = U14SendString("RDADR,2,2;");
        flag = U14GetString((LPSTR)buff,15);
        printf("RDADR after To1401 %s\n", buff);
        flag = U14Ld("","ADCMEM,ADCBST");
        printf("Return value from Ld %d\n",flag);

        if (flag == 0)
        {
            flag = U14SendString("ADCMEM,I,2,0,80000,0,100,C,20,5;");
            do
            {
                flag  = U14SendString("ADCMEM,P;");
                flag  = U14LongsFrom1401(lval, 2);
                point = lval[0];
                flag  = U14SendString("ADCMEM,?;");
                flag  = U14LongsFrom1401(lval, 2);
                status = lval[0];
                flag = U14ToHost((LPSTR)test,80000,0,2);
                printf("ADCMEM status %4d, pointer %8d (%8d %8d)\r",status,point,test[0],test[1]);
            }while((status != 0) | (point != 0));
        }
        U14Close1401();
    }
    return 0;
}
