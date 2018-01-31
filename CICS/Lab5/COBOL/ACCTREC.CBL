       01 ACCTREC.
           05 ACCTKEY.
                10 ACCTCODE     PIC X(3) VALUE 'XXX'.
                10 ACCTNO       PIC 9(5).
           05 FNAME             PIC X(15).
           05 SNAME             PIC x(15).
           05 TITL              PIC X(4).
           05 ADDR1             PIC X(20).
           05 ADDR2             PIC X(20).
           05 CRLIMIT           PIC 9(8).
           05 STAT              PIC X.
       01 ACCTREC-LEN           PIC S9(4) COMP VALUE 91.