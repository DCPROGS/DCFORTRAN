CCC
C  This program will zomm and pan a graph
C
C  Pressing - Z will zoom region
C               select opposite corners using graphics cursor
C             L will pan left and down
C             R will pan right and up
C             D will reset default scale
C             Q will exit program (must press an additional key)
C
C  Cursor may be moved in any of 8 directions by pressing the keypad 1-4 & 6-9 
C  keys
C                ------ ------ ------
C               |  up  |  up  |  up  |
C               | left |      | right|
C                ------ ------ ------
C               |      |      |      |
C               | left |      |right |
C                ------ ------ ------
C               | down | down | down |
C               | left |      | right|
C                ------ ------ ------
C               | toggle speed|
C               | between 1&8 |
C                ------------- 

      character*1 ch
      integer ittinr, fndcur
      real x(1001), s(1001), pi, x1, x2, y1, y2, xworld, yworld
      integer i, ix, iy
      data pi /3.141592654/

      do 10 i = 1, 1001
        x(i) = 0.002 * (i - 501)
        s(i) = sin(8*pi*x(i)) * sin(100*pi*x(i)) * exp(-3-3*x(i))
10    continue
      x1 = -1.0
      x2 = 1.0
      y1 = -1.0
      y2 = 1.0

      call INIPLT(0, .false., 1.0)
100   call ERASCR
      call SCALE(x1, x2, y1, y2)
      call AXIS((x2-x1)/2, '10.3', 'x', 3, (y2-y1)/2, '10.3', 'y', 3)
      call POLYLINE(x, s, 1001, 0, 0, 0, 0, 0)

C  Read a character from keyboard, key pressed is not echoed
      ch = char(ITTINR())

      if ((ch .ge. 'a') .and. (ch .le. 'z')) ch = char(ichar(ch)-32)

C  Zoom image
      if (ch .eq. 'Z') then
        call GETSCALE(x1, x2, y1, y2)

C  Pan right and up
      else if (ch .eq. 'R') then 
        ch = char(FNDCUR(ix, iy))
        x2 = x2 + XWORLD(ix) - x1
        y2 = y2 + YWORLD(iy) - y1
        x1 = XWORLD(ix)
        y1 = YWORLD(iy)

C  Pan left and down
      else if (ch .eq. 'L') then 
        ch = char(FNDCUR(ix, iy))
        x1 = x1 + XWORLD(ix) - x2
        y1 = y1 + YWORLD(iy) - y2
        x2 = XWORLD(ix)
        y2 = YWORLD(iy)

C  Reset to default scale
      else if (ch .eq. 'D') then
        x1 = -1.0
        y1 = -1.0
        x2 = 1.0
        y2 = 1.0

C  Quit - note must press an additional key return
      else if (ch .eq. 'Q') then 
        goto 200
      endif
      goto 100
200   call ENDPLT

      End
