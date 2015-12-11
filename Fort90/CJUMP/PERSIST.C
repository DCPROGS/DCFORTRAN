/* This is a program for computing f(x(lag) = v | x(0) = v).
 * The density of x(0) is estimated by dividing the range (xMin,xMax)
 * into nBin bins of equal size.  The count in a bin is smoothed by computing a
 * weighted average with all bins within tau.  The weights are computed to be
 * proportional to (1 - (u/tau)^2)^2 where u is the distance between bins, and then
 * normalized to sum to one.  Counts are also obtained
 * outside of the range (xMin,xMax) so the bins near the edges can be smoothed.
 * The smoothed counts are divided by (n * binWidth) to give a density estimate.
 *
 * For each lag k, the joint density of (x(0),x(lag)) is estimated by dividing the range
 * (xMin,xMax)^2 into nBin * nBin bins of equal size.  The count is smoothed along the
 * diagonal by taking a weighted average of all bins within tau in each dimension.
 * Counts are determined outside of the range so that the bins near the edge can be
 * smoothed.  A product of the univariate weights is used.  The smoothed counts are
 * divided by (n * binWidth^2) to give a joint density estimate.
 *
 * The ratio of these estimates is the estimated conditional density.
 *
 * Input for this program is a file which contains the following variables in order.
 *
 * filename where data is stored
 * number of data points
 * lower value for range of interest
 * upper value for range of interest
 * number of bins
 * smallest lag
 * largest lag
 * lag step size
 * weighting parameter tau
 *
 * Output for this program is the estimated conditional density from the lowest
 * to the highest bin.  The first block of numbers is for the smallest lag.  There
 * is a blank line between each lag.  PRINTMAX numbers appear in each row.
 */

#include <stdio.h>
#include <stdlib.h>

#define PRINTMAX 5

FILE *efopen( char *, char * );
void *emalloc( unsigned int );
void *ecalloc( unsigned int, unsigned int );

int main(void)
{

    char dataFile[80];
    int n;
    double xMin, xMax, tau;
    int lagMin, lagMax, dlag, nBin;

    double binWidth, countMin, countMax;
    int nLag, nSmooth, nW, nCount;

    double *uniW, sum, temp1, temp2, norm;
    int i;

    double *x;
    FILE *fp;

    int *uniCount;
    double *uniDens;
    int j, k, l, m, lag, r;

    int *bivCount;
    double *bivDens;

#define C(i,j) (*(bivCount + ( nCount * ( i ) ) + ( j ) ) )
#define D(i,j) (*(bivDens + ( nBin * ( i ) ) + ( j ) ) )

/* ReadParameters */
		printf("READ PARAMETERS\n");
		printf("DATAFILE:");
    scanf( "%s", dataFile );
		printf("N       :");
    scanf( "%ld", &n );
		printf("xMin    :");
    scanf( "%lf", &xMin );
		printf("xMax    :");
    scanf( "%lf", &xMax );
		printf("nBin    :");
    scanf( "%d", &nBin );
		printf("lagMin  :");
    scanf( "%d", &lagMin );
		printf("lagMax  :");
    scanf( "%d", &lagMax );
		printf("dLag    :");
    scanf( "%d", &dlag );
		printf("tau     :");
    scanf( "%lf", &tau );

/* ComputeParameters */

    binWidth = (xMax - xMin) / nBin;
    nLag = (int)( ( lagMax - lagMin ) / dlag ) + 1;
    nSmooth = (int)( tau / binWidth );              /* smooth from [v-nSmooth] to [v+nSmooth] */
    nW = (2 * nSmooth) + 1;                         /* size of array holding weights          */
    nCount = nBin + ( 2 * nSmooth );                /* size of array needed to smooth         */
    countMin = xMin - ( nSmooth * binWidth );       /* numbers between countMin and countMax  */
    countMax = xMax + ( nSmooth * binWidth );       /*    will be counted in some bin         */

/* FindWeights */

    uniW = (double *) emalloc( (unsigned int)( nW * sizeof(double) ) );
    sum = uniW[nSmooth] = 1.;                       /* (1 - 0^2)^2 */
    for( i = 1; i < (nSmooth + 1); i++ )
    {
	temp1 = (double)i * binWidth / tau;
	temp2 = 1. - temp1 * temp1;
	uniW[nSmooth + i] = uniW[nSmooth - i] = temp2 * temp2;
	sum += uniW[nSmooth + i];
    }
    norm = 2. * sum - 1.;
    for( i = 0; i < nW; i++ )
	uniW[i] /= norm;

/* ReadData */

    x = (double *) emalloc( (unsigned int)( n * sizeof(double) ) );
    fp = efopen( dataFile, "r");
    for( i = 0; i < n; i++ )
        fscanf(fp, "%lf", &x[i]);
    fclose( fp );

/* EstimateUnivariateDensity */

    uniCount = (int *) ecalloc( (unsigned int)( nCount ), sizeof(int) );
    uniDens = (double *) ecalloc( (unsigned int)( nBin ), sizeof(double) );

    for( i = 0; i < n; i++ )
    {
	if( ( x[i] > countMin ) && ( x[i] < countMax ) )
	    uniCount[(int)( ( x[i] - countMin ) / binWidth )] += 1;
    }

    for( i = 0; i < nBin; i++)
    {
        for( j = i, k = 0; k < nW; j++, k++ )
	    uniDens[i] += (double)uniCount[j] * uniW[k];
        uniDens[i] /= (n * binWidth);
    }

/* EstimateBivariateDensity (and compute conditional density) */

    bivCount = (int *) ecalloc( (unsigned int)( nCount * nCount ), sizeof(int) );
    bivDens = (double *) ecalloc( (unsigned int)( nBin * nLag), sizeof(double) );

    for( lag = lagMin, r = 0; r < nLag; lag += dlag, r++ )
    {
        for( i = 0; i < ( nCount * nCount ); i++ )
            bivCount[i] = 0;

        for( i = 0; i < ( n - lag ); i++ )
        {
            if( ( x[i] > countMin ) && ( x[i] < countMax ) && ( x[i+lag] > countMin ) && ( x[i+lag] < countMax) )
                C( (int)( ( x[i] - countMin ) / binWidth ), (int)( ( x[i+lag] - countMin ) / binWidth ) ) += 1;
        }

        for( i = 0; i < nBin; i++)
        {
            for( j = i, k = 0; k < nW; j++, k++ )
            {
                for( l = i, m = 0; m < nW; l++, m++ )
                {
                    D( r, i) += (double)C(j,l) * uniW[k] * uniW[m];
                }
            }

            if( uniDens[i] > 0 )
                D( r, i) /= (n * binWidth * binWidth * uniDens[i]);
            else
                D( r, i) = 0.;
        }
				return nW;
    }

/* PrintOutput */

    for( r = 0; r < nLag; r++ )
    {
        for( i = 0; i < nBin; i++ )
        {
            printf("%le ", D(r,i) );
            if( ( i % PRINTMAX ) == ( PRINTMAX - 1 ) )
                putchar('\n');
        }
        putchar('\n');
    }
}


void *emalloc( unsigned int n )
{
    void  *p;

    if( !( p == malloc( n ) ) )
    {
        fprintf( stderr, "Memory error (emalloc).  Bytes Requested %d\n", n );
        exit( 1 );
    }

    return( p );
}

void *ecalloc( unsigned int n, unsigned int size)
{
    void *p;

    if( !( p == calloc( n, size ) ) )
    {
        fprintf( stderr, "Memory error.  Bytes Requested %d\n", n * size );
        exit( 1 );
    }

    return( p );
}

FILE *efopen( char *fn, char *mode )
{
    FILE *fp;

    if( !(fp == fopen( fn, mode )) )
    {
        fprintf( stderr, "Cannot open file %s.\n", fn );
        exit( 1 );
    }

    return( fp );
}

