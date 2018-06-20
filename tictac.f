      module blk
      implicit none
      integer :: LINE(76,4), PTLIST(64,7)
      integer :: PTXYZ(4,4,4)
      integer :: PT(64),LINLEV(22),LINCNT(76),OPLIN(22,13),MOVES(22,2)
      
      end module blk


      PROGRAM TICTAC
      use blk, only: ptxyz, pt, moves
      
      use, intrinsic:: iso_fortran_env, only: IOUT=>output_unit, 
     &          input_unit
     
      implicit none
     
      INTEGER VAL(64)
      INTEGER TLEV, FIRST, i2, ibest, icall, ireply, idfend, mm, n,
     &   next, ibest2, irply2, isave, j2, k2, level
      
      integer, parameter :: MINE=1,HIS=4
      character(8) :: buf
      integer :: argc, i,j,k,m,idff, ios
      real :: r
      
      ptxyz = reshape( 
     &  [1, 17,18,3,
     &   19,20,21,22,
     &   23,24,25,26,
     &   5,27,28,7,

     & 29,30,31,32,
     & 33,2,4,34, 
     & 35,6,8,36,
     & 37,38,39,40,

     & 41,42,43,44,
     & 45,10,12,46,
     & 47,14,16,48,
     & 49,50,51,52,

     & 9,53,54,11,
     & 55,56,57,58,
     & 59,60,61,62,
     & 13,63,64,15], shape(ptxyz))






C          PTLIST  -  LIST OF LINES FOR EACH POINT
C          LINE -      LIST OF POINTS IN EACH LINE
C          PT  -   STATE OF EACH  POINT  - 0,MINE,HIS
C          MOVES  -  LIST OF MOVES  IN ORDER PLAYED
C          OPLIN    -   LIST OF WINNING LINES AT EACH LEVEL
C          LINLEV   -  NUMBER OF ENTIES IN OPLIN AT EACH LEVEL
C          PTXYZ  -   USED TO CONVERT FROM INTERNAL TO EXTERNAL FORM
C          LEVEL -  COUNTER FOR NUMBER OF TURNS PLAYED
C          TLEV -   COUNTER FOR TEMPORARY LOOK-AHEAD MOVES
C          VAL -   VALUE ASSOCIATED WITH EACH OPEN SQUARE,
C                   USED TO COMPUTE 'BEST' POSSIBLE MOVE
C          M  -   MY CURRENT MOVE
C          N -   HIS LAST MOVE
C   INITIALIZATION OF LINES AND POINTS
      CALL SETUP(N)
      M=0
C    THIS IS THE RESTART LOCATION FOR SUBSEQUENT GAMES
C
301   LEVEL=1
      NEXT=3
      DO I=1,64
        PT(I)=0
      enddo
      ! difficulty
      IDFF = 1
      argc = command_argument_count()
      do j = 1,argc
        call get_command_argument(j,buf)
        if (buf == '-d') then
          call get_command_argument(j+1,buf)
          read(buf,'(I1)') IDFF
          exit
        endif
      enddo
      
      print '(A,I1)','difficulty level: ',IDFF
      
      ! who goes first
      call random_number(r)
      
      if (r>0.5) GOTO 401
      goto 408
C
C    OPPONENTS MOVES ARE READ HERE
C
400   print *, 'SORRY, THAT SPACE IS OCCUPIED'
401   print *, 'WHAT IS YOUR MOVE? '
      i=0; j=0; k=0
      READ(input_unit,'(3I1)', iostat=ios) I,J,K
      if (ios/=0) goto 401
      
      select case (i)
        case (1,2,3,4)
          if (any([j,k]<1).or.any([j,k]>4)) goto 401
          GO TO 404 ! normal
        case (6) 
          if (M==0) then ! very first move of game
            print *,'surprise me'
            goto 401
          endif
          GOTO 402 ! hint
        case (7)
          stop     ! quit
        case default
          goto 401 ! bad input
       end select

402   GO TO (850,820,830,840,850),NEXT
800   M=IREPLY
      ICALL=4
      GO TO 592
880   print '(A,3I1)', "I RECOMMEND ", I,J,K

      IF(IDFF < 6) GO TO 401
      PT(M)=HIS
      GO TO 408
820   print *, 'I RECOMMEND YOU RESIGN.'
      IF(IDFF < 6)GO TO 401
      stop
      
830   IBEST=IDFEND(M,IDFF,IREPLY)
      PT(M)=MINE
      IF(IBEST)820,800,800
840   IREPLY=ISAVE
      GO TO 800
850   CALL SCORES(IREPLY,NEXT,HIS)
      GO TO 800
404   N=PTXYZ(I,J,K)
      IF(PT(N).NE.0) GOTO 400
405   PT(N)=HIS
408   LEVEL=LEVEL+1
C
C     MY MOVES ARE OUTPUT HERE
C
409   IF (LEVEL.GT.2) GOTO 500
C
C    MY FIRST MOVE IS COMPUTED HERE
C
410   IF(N.GT.16.OR.N/2*2.LT.N)GO TO 450
      N=N-2
450   M=N-N/16*16+1
440   PT(M)=MINE
      ICALL=3
      GOTO 592
460   print '(A,3I1)', ' MY MOVE IS ',I,J,K
      
      CALL BOARD
      IF(IDFF.EQ.6)GO TO 402
      GO TO 401
C
C    THE GAME'S  STATUS IS COMPUTED BY SCORES
C
500   CALL SCORES( M ,NEXT,MINE)
      GOTO(5015,585,610,630,670,640),NEXT
5015  IF(IDFF.LT.3)GOTO 700
C
502   CALL FORCE(MINE,IDFF ,TLEV,M,NEXT)
      GOTO(700,590,680),NEXT
C
C        NO FORCED WIN FOUND
C
C        DETERMINE HIGHEST VALUE MOVE
C
700   CALL VALUE(VAL,MINE)
C
4010   FIRST=1
4050  MM=1
      IBEST=700
      DO 4100 I=1,64
      IF(VAL(I))4100,4101,4101
4101  IF(VAL(I)-VAL(MM))4100,4102,4106
4102  IBEST2=IDFEND(I,IDFF,IRPLY2)
4105  IF(IBEST.LE.IBEST2)GO TO 4100
      IBEST=IBEST2
      IREPLY=IRPLY2
      GO TO 4107
4106  IBEST=IDFEND(I,IDFF,IREPLY)
4107  MM=I
      IF(IBEST)4130,4100,4100
4100  CONTINUE
      GO TO (4140,4120,4140,4120,4120,4120),IDFF
4120  IF(VAL(MM))620,4125,4125
4125  CALL LOSE(HIS,MM,VAL,NEXT,IDFF)
      GO TO (4140,4051,680),NEXT
4051  IF(FIRST.EQ.2)GO TO 4050
      FIRST=2
      M=MM
      ISAVE=MOVES(1,1)
      GO TO 4050
C
C     PREPARE WINNING REMARKS
C
4130  WRITE(IOUT,100)
      NEXT=2
4140  M=MM
      GO TO 440
620   WRITE(IOUT,1041)
      NEXT=4
      GO TO 440
585   TLEV=1
590   PT(M)=MINE
      MOVES(TLEV,1)=M
      LEVEL=1
591   ICALL=1
589   M=MOVES(LEVEL,ICALL)
592   DO 595 K=1,4
      DO 595 J=1,4
      DO 595 I=1,4
      IF(PTXYZ(I,J,K).EQ.M)GOTO 596
595   CONTINUE
596   GOTO(597,598,460,880),ICALL
597   IF(LEVEL.EQ.TLEV)GOTO 660
      I2=I
      J2=J
      K2=K
      ICALL=2
      GOTO 589
598   WRITE(IOUT,1014)I2,J2,K2,I,J,K
      LEVEL=LEVEL+1
      GOTO 591
610   print *, "I'LL BLOCK YOU."
      GO TO 440
630   print *, 'THIS GAME IS A DRAW.'
      GOTO 650
660   WRITE(IOUT,1008) I,J,K
      GOTO 650
670   WRITE(IOUT,1040)
      GOTO 440
680   WRITE(IOUT,1060)
      GOTO 650
640   print *, 'CONGRATULATIONS, YOU WIN.'
650   CALL BOARD

      STOP

1014  FORMAT(' MY MOVE IS ',3I1,', FORCING YOU TO ',3I1)

1008  FORMAT(' THAT WAS A GOOD GAME, BUT I WIN BY PLAYING ',3I1)
1040  FORMAT(' I''M IN BIG TROUBLE.')
1060  FORMAT(' INSUFFICIENT MEMORY.')
100   FORMAT(' YOU''RE IN BIG TROUBLE.')
1041  FORMAT(' I MAY BE IN TROUBLE.')
      END PROGRAM
      
C   QUBIC  -   SCORES SUBROUTINE                        HEADER  SCORES
      SUBROUTINE SCORES(WIN,NEXT,MINE)
      use blk, only: pt, lincnt, line
      implicit none
      
      integer, intent(out) :: win, next
      integer, intent(in) :: mine

      INTEGER SCORE,WON , i, j, ix, ibad, lose, none, nscore
C
C   EVALUATES STATUS OF ALL LINES FROM THE VIEWPOINT OF THE CALLING
C   PLAYER, AND ALARMS ANY CRITICAL CONDITIONS.
C    MULTIPLE RETURNS -      NEXT = 1, USUAL CONDITION
C                     -      NEXT = 2,  EXIT WITH WINNING MOVE
C                     -      NEXT = 3,  EXIT WITH BLOCKING MOVE
C                     -      NEXT = 4,  GAME IS A DRAW
C                     -      NEXT = 5,  -  GAME WILL BE LOST
C                     -      NEXT = 6,  GAME IS LOST
      WON=0
      LOSE=0
      NONE =0
      IBAD=0
         DO 100 I=1,76
      SCORE =0
         DO 5 J=1,4
      NSCORE=0
      IX=LINE(I,J)
      IF(PT(IX).EQ.1)NSCORE=MINE
      IF(PT(IX).EQ.4)NSCORE= 4/MINE
5     SCORE=SCORE+NSCORE
         IF(SCORE .EQ. 0) GO TO 20
         GOTO (20,20,15,40,10,10,10,40,10,10,10,45,10,10,10,300),SCORE
10       LINCNT(I)=5
      NONE=NONE+1
         GO TO 100
15    WON=I
20       LINCNT(I)=SCORE
         GO TO 100
45    IBAD=IBAD+1
      LOSE=I
40       LINCNT(I)=-SCORE/4
100      CONTINUE
      IF(WON.NE.0)GOTO 50
      IF (LOSE.NE.0)GO TO 60
      NEXT=1
      IF(NONE.EQ.76)NEXT=4
         RETURN
50       DO 150 J=1,4
      IX=LINE(WON,J)
      IF(PT(IX).EQ.0) WIN=IX
150      CONTINUE
      NEXT=2
      RETURN
60       DO 200 J= 1,4
      IX=LINE(LOSE,J)
      IF(PT(IX).EQ.0) WIN=IX
200      CONTINUE
      NEXT=3
      IF(IBAD.GT.1)NEXT=5
      RETURN
300   NEXT=6

      END SUBROUTINE SCORES
      
C   QUBIC  -   FORCE  SUBROUTINE                        HEADER  FORCE
      SUBROUTINE FORCE(MINE, IDFF,TLEV,M,NEXT)
C
C   DETERMINES IF CURRENT BOARD PRESENTS A FORCING OPPORTUNITY FOR
C   THE CALLING PLAYER.
C   NOTE: IF FOUND, THE MOVES LEADING TO THE FORCING WIN ARE NOT
C   REMOVED FROM THE BOARD, BUT ARE LISTED IN THE MOVES ARRAY.
C   AN ARBITRARY LIMIT OF 26 FORCING MOVES AT ONE TIME, AND/OR
C   20 FORCING MOVES IN SEQUENCE, HAS BEEN SPECIFIED.    EXCEEDING THESE
C   WILL CUASE AN ABORT FOR INSUFFICIENT MEMORY
      use blk, only: linlev, moves, pt, oplin, line, ptlist, lincnt
      implicit none
      integer, intent(in) :: mine, idff
      integer, intent(out) :: tlev, M
      integer, intent(inout) :: next
      
      INTEGER :: HIS,TPT,MODE(22),TEMPT(2),TLINE, I, I2, IX, IY, IZ, K,
     &   NUMLIN

      HIS=5-MINE
      TLEV=1
C     FORCED WIN SEARCH BEGINS  HERE
      LINLEV(1)=0
      IF(NEXT.EQ.3)CALL ADDLIN(M,1)
      IF(NEXT.NE. 3)CALL TWOCNT(1)
      IF(LINLEV(1))590,590,510
510   IF(LINLEV(TLEV).GT.13.OR.TLEV.GT.22)GO TO 610
      IX=LINLEV(TLEV)
      TLINE=OPLIN(TLEV,IX)
C
C        TLINE IS CURRENT OPEN LINE
C
      IX=0
         DO 520I = 1,4
         TPT = LINE(TLINE,I)
         IF (PT(TPT).EQ.MINE)GO TO 520
      IX=IX+1
      TEMPT(IX)=TPT
520   CONTINUE
         MODE(TLEV)=1
      IX=TEMPT(1)
      PT(IX)=MINE
      IY=TEMPT(2)
      PT(IY)=HIS
         MOVES(TLEV,1)=IX
         MOVES(TLEV,2)=IY
      GOTO 530
525      MODE(TLEV)=2
         TEMPT(1)=MOVES(TLEV,1)
         TEMPT(2)=MOVES(TLEV,2)
      IX=TEMPT(1)
      IY=TEMPT(2)
      PT(IX)=HIS
      PT(IY)=MINE
         MOVES(TLEV,1)=IY
         MOVES(TLEV,2)=IX
530      TLEV = TLEV+1
      LINLEV(TLEV)=0
      CALL SCORES( M ,NEXT,MINE)
      GOTO(540,600,535,560,560,560),NEXT
C
C        DETERMINE IF ANY POINTS SHOULD BE TRIED
535   CALL ADDLIN(M,TLEV)
      GOTO 555
C
540   IF( IDFF.NE.5)GO TO 541
      CALL TWOCNT(TLEV)
      GO TO 555
541   IX=MODE(TLEV-1)
      IY=TEMPT(IX)
         NUMLIN=4
      IF(TEMPT(IX).LE.16)NUMLIN=7
         DO550 I=1,NUMLIN
      K=PTLIST(IY,I)
C
C        K IS A LINE NUMBER
C
         IF(LINCNT(K).NE. 2 )GO TO 542
         LINLEV(TLEV)=LINLEV(TLEV)+1
      IX=LINLEV(TLEV)
      OPLIN(TLEV,IX)=K
      GOTO 550
542   IF(LINCNT(K).NE.1)GOTO 550
      DO 543 I2=1,4
      IZ=LINE(K,I2)
      IF(PT(IZ).EQ.MINE)GOTO 543
      CALL ADDLIN(IZ,TLEV)
543   CONTINUE
550      CONTINUE
555      IF(LINLEV(TLEV).NE.0) GO TO  510
C
C        HERE WE MUST BACK UP
C
560      TLEV=TLEV-1
         IF(MODE(TLEV).EQ.1)GO TO 525
570   IX=MOVES(TLEV,1)
      PT(IX)=0
      IY=MOVES(TLEV,2)
      PT(IY)=0
         LINLEV(TLEV)=LINLEV(TLEV)-1
         IF(LINLEV(TLEV).GT.0)GO TO 510
580      IF(TLEV.GT.1)GO TO 560
C
590   NEXT=1
600   RETURN
610   NEXT=3

      END SUBROUTINE FORCE
      
C   QUBIC  -   TWOCNT SUBROUTINE                        HEADER  TWOCNT
      SUBROUTINE TWOCNT(LEVEL)
C
C   MAKES LIST OF ALL 2-IN-A-ROWS FOR CALLING PLAYER.
      use blk, only: linlev, oplin, lincnt
      implicit none
      integer, intent(in) :: level
      integer :: i, ix
      IX=0
      DO I=1,76
         IF(LINCNT(I).NE.  2) cycle
        IX=IX+1
        OPLIN(LEVEL,IX)=I
      enddo
      LINLEV(LEVEL)=IX
      END SUBROUTINE TWOCNT
      
      
      SUBROUTINE ADDLIN(IZ,TLEV)
C   FOR A SPECIFIED POINT, ADD ALL LINES THRU THE POINT, WHICH ARE 2-IN-
      use blk, only: linlev, oplin, ptlist, lincnt
      implicit none
      INTEGER TLEV,iz,i3,ix,k1,mmln2,nmln2

      NMLN2=4
      IF(IZ.LE.16)NMLN2=7
      DO I3=1,NMLN2
        K1=PTLIST(IZ,I3)
        IF(LINCNT(K1) /= 2) cycle
        LINLEV(TLEV)=LINLEV(TLEV)+1
        IX=LINLEV(TLEV)
        OPLIN(TLEV,IX)=K1
      enddo
      END SUBROUTINE ADDLIN
      
C   QUBIC  -   VALUE  SUBROUTINE                        HEADER  VALUE
      SUBROUTINE VALUE(VAL,MINE)
      use blk, only: pt, ptlist, lincnt
        implicit none
        integer, intent(out) :: VAL(64)
        integer, intent(in) :: mine

      integer :: i, ii, ix, nodif, numlin
      
      CALL SCORES(NODIF,NODIF,MINE)
      
      do I=1,64
         VAL(I)=0
         IF(PT(I).EQ.0)GO TO 710
        VAL(I)=-200
        cycle
710      NUMLIN=4
         IF(I.LE.16)NUMLIN=7
        do II=1,NUMLIN
          IX=PTLIST(I,II)
          IF(LINCNT(IX).EQ. 0) VAL(I)=VAL(I)+9
          IF(LINCNT(IX).EQ. 1) VAL(I)=VAL(I)+91
          IF(LINCNT(IX).EQ.-1) VAL(I)=VAL(I)+16
          IF(LINCNT(IX).EQ.-2) VAL(I)=VAL(I)+110
        enddo
      end do
      END SUBROUTINE VALUE
      
C   QUBIC  -   IDFEND FUNCTION                          HEADER  IDFEND
      Integer FUNCTION IDFEND(N,IDFF,K)
      use blk, only: pt
      implicit none
C   FOR ANY LEGAL MOVE,WHAT IS THE VALUE OF THE BEST NON-LOSING REPLY.
      integer, intent(in) :: N, IDFF
      integer, intent(out) :: K
      integer ::DVAL(64)
      integer :: i, next

      
      PT(N)=1
      CALL VALUE(DVAL,4)
20    K=1
      DO I=2,64
        IF(DVAL(I) > DVAL(K)) K=I
      enddo
      IF(IDFF < 4)GO TO 30
      IF(DVAL(K))30,40,40
40    CALL LOSE(1,K,DVAL,NEXT,4)
      IF(NEXT.EQ.3)CALL BOARD
      IF(NEXT.EQ.2)GO TO 20
30    PT(N)=0
      IDFEND=DVAL(K)
      END FUNCTION IDFEND
      
C   QUBIC  -   LOSE   SUBROUTINE                        HEADER  LOSE
      SUBROUTINE LOSE(HIS,MM,VAL,NEXT,IDFF)
C   IS THIS A LOSING MOVE FOR THE CALLING PLAYER ?
      use blk, only: pt, moves
      implicit none
      integer, intent(in) :: his, mm, idff
      integer, intent(inout) :: val(64)
      integer, intent(out) :: next
      INTEGER tlev, i2, j2, k2, mine, nodif

      MINE =5-HIS
      PT(MM)=MINE
      CALL SCORES(NODIF,NEXT,HIS)
      IF(NEXT.EQ.2)GO TO 4215
      CALL FORCE(HIS,IDFF,TLEV,NODIF,NEXT)
      GOTO(4300,4200,680),NEXT
4200  DO 4210 I2=2,TLEV
      J2=MOVES(I2-1,1)
      K2=MOVES(I2-1,2)
      PT(J2)=0
4210  PT(K2)=0
4215  VAL(MM)=-100
4300  PT(MM)=0
680   END SUBROUTINE LOSE
      
C   QUBIC  -   BOARD  SUBROUTINE                        HEADER  BOARD
      SUBROUTINE BOARD
      use blk, only: pt, line
      use, intrinsic:: iso_fortran_env, only: IOUT=>output_unit
      IMPLICIT none
      
      integer :: i, j, k
      character :: IOBUF(16)
      character, parameter :: LETDSH="-",LETEKS="O",LETOH="X"
      
      
      DO I=1,4
        DO J=1,16
          K=LINE(J,I)
          IOBUF(J)=LETDSH
          IF(PT(K).EQ.1 )IOBUF(J)=LETEKS
          IF(PT(K).EQ.4 )IOBUF(J)=LETOH
        enddo
        WRITE(IOUT,1100)(IOBUF(K),K=1,16)
      enddo
1100  FORMAT(1H ,4(4A2,4X))
      END SUBROUTINE BOARD
      
C   QUBIC  -   SETUP  SUBROUTINE                        HEADER  SETUP
      SUBROUTINE SETUP(N)
      use blk, only: ptlist, line, ptxyz
      implicit none
      integer :: i,j,k,n,m, argc
      logical :: help=.false.
      character(8) :: buf
C     THIS SECTION RUNS ONLY ONCE.
C     ALL OF THE TABLES THAT ARE NEEDED ARE COMPUTED FROM THE
C     CONFIGURATION SHOWN ABOVE
      N=1
      DO 10 K=1,4
      DO 20 J=1,4
      DO 30 I   =1,4
30    LINE(N,I)=PTXYZ(I,J,K)
20    N=N+1
10    CONTINUE
      DO 40 K=1,4
      DO 50 I=1,4
      DO 60 J=1,4
60    LINE(N,J)=PTXYZ(I,J,K)
50    N=N+1
40    CONTINUE
      DO 70 I=1,4
      DO 80 J=1,4
      DO 90 K=1,4
90    LINE(N,K)=PTXYZ(I,J,K)
80    N=N+1
70    CONTINUE
      DO 100 J=1,4
      DO 110 I=1,4
110   LINE(N,I)=PTXYZ(I,J,I)
100   N=N+1
      DO 120 I=1,4
      DO 130 J=1,4
130   LINE(N,J)=PTXYZ(I,J,J)
120   N=N+1
      DO 140 J=1,4
      DO 150 I=1,4
      K=5-I
150   LINE(N,I)=PTXYZ(I,J,K)
140   N=N+1
      DO 160 I= 1,4
      DO 170 J=1,4
      K=5-J
170   LINE(N,J)=PTXYZ(I,J,K)
160   N=N+1
      DO 180 K=1,4
      DO 190 I=1,4
190   LINE(N,I)=PTXYZ(I,I,K)
180   N=N+1
      DO 200 K=1,4
      DO 210 I= 1,4
      J=5-I
210   LINE(N,I)=PTXYZ(I,J,K)
200   N=N+1
      DO 220 I=1,4
220   LINE(N,I)=PTXYZ(I,I,I)
      N=N+1
      DO 230 I=1,4
      J=5-I
230   LINE(N,I)=PTXYZ(I,J,I)
      N=N+1
      DO 240 J=1,4
      I=5-J
240   LINE(N,J)=PTXYZ(I,J,J)
      N=N+1
      DO 250 K=1,4
      I=5-K
250   LINE(N,K)=PTXYZ(I,I,K)
C    AT THIS POINT, ALL OF THE LINES HAVE BEEN DEFINED.
C     NEXT THE  LINES FOR EACH POINT ARE DETERMINED.
      DO 300 I=1,64
      N=1
      DO 310 J=1,76
      DO 320 K=1,4
      IF(LINE(J,K).NE.I) GO TO 320
       PTLIST(I,N)=J
      N=N+1
320   CONTINUE
310   CONTINUE
300   CONTINUE
      print *, 'HELLO,I AM THE APOLLO/GOULD TIC-TAC-TOE PLAYER.'

      argc = command_argument_count()
      do m = 1,argc
        call get_command_argument(m,buf)
        if (buf == '-h'.or. buf== '--help') then
          help=.true.
          exit
        endif
      enddo

      IF (.not.help) RETURN
      
      print *,"THE GAME IS PLAYED IN A 4 X 4 X 4 CUBE."
      print *,"EACH MOVE IS INDICATED BY A 3 DIGIT NUMBER."
      print *, "THE DIGITS CORRESPOND TO THE ROW, COLUMN, AND LEVEL."
      print *,''
      
      print *, "Difficulty selection at command line: -d 1 and so on:"
      print *, "1. AVERAGE GAME"
      print *, "2. STRONG DEFENSE"
      print *, "3. STRONG OFFENSE"
      print *, "4. BOTH 2 AND 3"
      print *, "5. MAXIMUM GAME"
      print *, "6. ON AUTOMATIC"
      print *,''
      
      print *, 'During gameplay:'
      print *, " X is You"
      print *, " O is Computer."
      print *,''
      
      print *, "During the game, special inputs:"
      print *, "6: GET ADVICE"
      print *, "7: RESIGN"

      END SUBROUTINE SETUP



