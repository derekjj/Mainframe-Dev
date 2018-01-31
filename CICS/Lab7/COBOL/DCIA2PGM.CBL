       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCIA2PGM.
       AUTHOR. DEREK JOHNSTON.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY 'DCIA2MM'.

       01 WS-COMMAREA       PIC X(3)
                        VALUE 'XXX'.
       01 WS-COMM-LEN       PIC S9(4) COMP
                        VALUE 3.

       LINKAGE SECTION.

       01 DFHCOMMAREA       PIC X(3).

       PROCEDURE DIVISION.

       000-START-LOGIC.

      * IF COMING BACK TO THE MENU
           IF EIBCALEN = 3 THEN
               GO TO 100-FIRST-TIME
           END-IF.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF1(300-CHOICE-1)
                PF2(400-CHOICE-2)
                PF3(500-CHOICE-3)
                PF4(600-CHOICE-4)
                PF9(700-CHOICE-9)
           END-EXEC.

           EXEC CICS RECEIVE
                MAP('MENU')
                MAPSET('DCIA2MM')
           END-EXEC.


           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MENUO.
           EXEC CICS SEND
               MAP('MENU')
               MAPSET('DCIA2MM')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('IA20')
           END-EXEC.

       200-MAIN-LOGIC.

           IF CHOICEI IS NOT NUMERIC
                GO TO 999-SEND-ERROR-MSG
           ELSE
           IF CHOICEI IS EQUAL TO '1'
                GO TO 300-CHOICE-1
           ELSE
           IF CHOICEI IS EQUAL TO '2'
                GO TO 400-CHOICE-2
           ELSE
           IF CHOICEI IS EQUAL TO '3'
                GO TO 500-CHOICE-3
           ELSE
           IF CHOICEI IS EQUAL TO '4'
                GO TO 600-CHOICE-4
           ELSE
           IF CHOICEI IS EQUAL TO '9'
                GO TO 700-CHOICE-9
           ELSE
                GO TO 999-SEND-ERROR-MSG
           END-IF.

       300-CHOICE-1.

           MOVE LOW-VALUES TO MENUO.

           EXEC CICS XCTL
                PROGRAM('DCIA2PGR')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMM-LEN)
           END-EXEC.

       400-CHOICE-2.
           MOVE LOW-VALUES TO MENUO.

           EXEC CICS XCTL
                PROGRAM('DCIA2PGC')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMM-LEN)
           END-EXEC.

       500-CHOICE-3.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'CHOICE 3: CHANGE OR DELETE UNAVAIALBLE' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCIA2MM') END-EXEC.
           EXEC CICS RETURN TRANSID('IA20') END-EXEC.

       600-CHOICE-4.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'CHOICE 4: SEARCH BY ACCOUNT UNAVAIALBLE' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCIA2MM') END-EXEC.
           EXEC CICS RETURN TRANSID('IA20') END-EXEC.

       700-CHOICE-9.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'CHOICE 9: EXITING THIS SCREEN' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCIA2MM') END-EXEC.
           EXEC CICS RETURN END-EXEC.

       999-SEND-ERROR-MSG.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'INVAID ENTRY : PLEASE ENTER 1 - 4 OR 9' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCIA2MM') END-EXEC.
           EXEC CICS RETURN TRANSID('IA20') END-EXEC.

       END PROGRAM DCIA2PGM.
