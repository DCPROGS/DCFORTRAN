CCC
C  This file contains global constants for HGRAPH
C  This file should be included in any routine containing HGRAPH routines
C  This file must be included after the last variable declaration statement 
C  and before the first data statement

      INTEGER LEFT, CENTER, RIGHT, BASE
      INTEGER BELOW, BOTTOM, ABOVE, TOP
      INTEGER LINEAR, LOG
      INTEGER NONE, MINOR, SUBMAJOR, MAJOR
      INTEGER DRAFT, DUPLEX, COMPLEX, BOLD, SCRIPT, GREEK
      DATA LEFT, CENTER, RIGHT, BASE /0, 2, 1, 3/
      DATA BELOW, BOTTOM, ABOVE, TOP /0, 0, 1, 1/
      DATA LINEAR, LOG /0, 1/
      DATA MINOR, SUBMAJOR, MAJOR, NONE /0, 1, 2, 3/
      DATA DRAFT, DUPLEX, COMPLEX, BOLD, SCRIPT, GREEK/0, 1, 2, 3, 4, 5/
