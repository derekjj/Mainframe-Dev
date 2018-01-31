       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCIA2PGR.
       AUTHOR. DEREK JOHNSTON.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCIA2MR'.

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
      * ADD CONDITION FOR RECORD NOT FOUND IN ACCTFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(300-ACCTFILE-NOTFND)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF9(999-EXIT)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCIA2MR')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCIA2MR')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('IA21')
           END-EXEC.

       200-MAIN-LOGIC.

      * IMPLEMENT VALIDATION LOGIC

           IF ACCTNOL < 5 THEN
                GO TO 500-NOT-FIVE-LONG
           ELSE
           IF ACCTNOI = "XXXXX" THEN
                GO TO 999-EXIT
           ELSE
           IF ACCTNOI IS NOT NUMERIC THEN
                GO TO 400-NOT-NUMERIC
           ELSE
                GO TO 600-FIND-ACCOUNT
           END-IF.

       300-ACCTFILE-NOTFND.

           MOVE ACCTNOI TO ACCTNO.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'ACCOUNT NOT FOUND' TO MSGO.
           MOVE ACCTNO  TO ACCTNOO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA21')
           END-EXEC.

       400-NOT-NUMERIC.
           MOVE ACCTNOI TO ACCTNO.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'ACCOUNT NUMBERS MUST BE NUMERIC' TO MSGO.
           MOVE ACCTNO  TO ACCTNOO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA21')
           END-EXEC.


       500-NOT-FIVE-LONG.
           MOVE ACCTNOI TO ACCTNO.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG' TO MSGO.
           MOVE ACCTNO TO ACCTNOO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA21')
           END-EXEC.

       600-FIND-ACCOUNT.

      * ATTEMPT TO FIND AN ACCOUND RECORD IN ACCTFILE
      * FROM USER INPUT ACCOUNTNO
           MOVE ACCTNOI TO ACCTNO.
           EXEC CICS READ
                FILE('ACCTFILE')
                INTO(ACCTREC)
                LENGTH(ACCTREC-LEN)
                RIDFLD(ACCTKEY)
           END-EXEC.


      * RECORD FOUND, MOVE VALUES TO MAP OUTPUTS
           MOVE LOW-VALUES TO MAP1O.
           MOVE "RECORD FOUND!" TO MSGO.

      * MOVE VALUES FROM ACCTREC TO O FIELDS
           MOVE ACCTNO  TO ACCTNOO.
           MOVE TITL    TO TITLO.
           MOVE FNAME   TO FNAMEO.
           MOVE SNAME   TO SNAMEO.
           MOVE ADDR1   TO ADDR1O.
           MOVE ADDR2   TO ADDR2O.
           MOVE STAT    TO STATO.
           MOVE CRLIMIT TO CRLIMITO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCIA2MR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('IA21')
           END-EXEC.

       999-EXIT.
           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS XCTL
                PROGRAM('DCIA2PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMM-LEN)
           END-EXEC.

      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCIA2MR') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCIA2PGR.
