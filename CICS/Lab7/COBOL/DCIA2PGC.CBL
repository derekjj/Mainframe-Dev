       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCIA2PGC.
       AUTHOR. DEREK JOHNSTON.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCIA2MC'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE        PIC X(60) VALUE SPACES.

      * VARIABLES USED TO SEND XCTL BACK TO MENU.
       01 WS-COMMAREA       PIC X(3)
                        VALUE 'XXX'.
       01 WS-COMM-LEN       PIC S9(4) COMP
                        VALUE 3.

       LINKAGE SECTION.

       01 DFHCOMMAREA       PIC X(3).

       PROCEDURE DIVISION.

       000-START-LOGIC.

      * IF COMING FROM THE MENU
           IF EIBCALEN = 3 THEN
               GO TO 100-FIRST-TIME
           END-IF.
      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                DUPREC(300-ACCTFILE-DUPREC)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF9(999-EXIT)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCIA2MC')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCIA2MC')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('IA22')
           END-EXEC.

       200-MAIN-LOGIC.

      * IMPLEMENT VALIDATION LOGIC

           IF ACCTNOI = "XXXXX" THEN
                GO TO 999-EXIT
           ELSE
           IF ACCTNOL < 5 THEN
                MOVE "ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG"
                    TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF ACCTNOI IS NOT NUMERIC THEN
                GO TO 400-NOT-NUMERIC
           ELSE
           IF FNAMEL = 0 THEN
                MOVE "FIRST NAME REQUIRED" TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF SNAMEL = 0 THEN
                MOVE "SIR NAME REQUIRED" TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF ADDR1L = 0 THEN
                MOVE "ADDRESS REQUIRED" TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF ADDR2L = 0 THEN
                MOVE "ADDRESS REQUIRED" TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF STATI NOT EQUAL "A" AND "B" AND "X" AND "Z" THEN
                MOVE "ERROR. STATUS OPTIONS ARE A, B, X, OR Z."
                    TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF CRLIMITI(1:CRLIMITL) IS NOT NUMERIC THEN
                MOVE "LIMIT MUST BE NUMERIC"
                    TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
           IF CRLIMITL < 3 THEN
                MOVE "LIMIT MUST BE 100 TO 99999999 INCLUSIVE"
                    TO WS-MESSAGE
                GO TO 500-REPORT-ERROR
           ELSE
                GO TO 600-CREATE-ACCOUNT
           END-IF.

       300-ACCTFILE-DUPREC.

           MOVE LOW-VALUES TO MAP1O.
           MOVE 'ACCT EXISTS, PLEASE ENTER ANOTHER ACCTNO' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA22')
           END-EXEC.

       400-NOT-NUMERIC.
           MOVE LOW-VALUES TO MAP1O.

           MOVE 'ACCOUNT NUMBERS MUST BE NUMERIC' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA22')
           END-EXEC.

       500-REPORT-ERROR.
           MOVE LOW-VALUES TO MAP1O.

           MOVE WS-MESSAGE TO MSGO.
           MOVE SPACES TO WS-MESSAGE.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA22')
           END-EXEC.

       600-CREATE-ACCOUNT.


      * MOVE VALUES FROM I FEILDS TO ACCTREC
           MOVE ACCTNOI  TO ACCTNO.
           MOVE TITLI    TO TITL.
           MOVE FNAMEI   TO FNAME.
           MOVE SNAMEI   TO SNAME.
           MOVE ADDR1I   TO ADDR1.
           MOVE ADDR2I   TO ADDR2.
           MOVE STATI    TO STAT.

      * TODO: fix limit, random number in right char
      * PROF WORK AROUND USED, CHANGED LIMIT TO PIC X IN ACCTREC
           MOVE CRLIMITI TO CRLIMIT.

           EXEC CICS WRITE
                FILE('ACCTFILE')
                FROM(ACCTREC)
                LENGTH(ACCTREC-LEN)
                RIDFLD(ACCTKEY)
           END-EXEC.

      * RECORD FOUND, MOVE VALUES TO MAP OUTPUTS
           MOVE LOW-VALUES TO MAP1O.
           MOVE "RECORD SUCCESSFULLY WRITTEN!" TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MC')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA22')
           END-EXEC.

       999-EXIT.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS XCTL
                PROGRAM('DCIA2PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMM-LEN)
           END-EXEC.

      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCIA2MC') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCIA2PGC.
