       01  MAP1I.
           02  FILLER PIC X(12).
           02  ACCTNOL    COMP  PIC  S9(4).
           02  ACCTNOF    PICTURE X.
           02  FILLER REDEFINES ACCTNOF.
             03 ACCTNOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ACCTNOI  PIC X(5).
           02  TITLL    COMP  PIC  S9(4).
           02  TITLF    PICTURE X.
           02  FILLER REDEFINES TITLF.
             03 TITLA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TITLI  PIC X(4).
           02  FNAMEL    COMP  PIC  S9(4).
           02  FNAMEF    PICTURE X.
           02  FILLER REDEFINES FNAMEF.
             03 FNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  FNAMEI  PIC X(15).
           02  SNAMEL    COMP  PIC  S9(4).
           02  SNAMEF    PICTURE X.
           02  FILLER REDEFINES SNAMEF.
             03 SNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SNAMEI  PIC X(15).
           02  ADDR1L    COMP  PIC  S9(4).
           02  ADDR1F    PICTURE X.
           02  FILLER REDEFINES ADDR1F.
             03 ADDR1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADDR1I  PIC X(20).
           02  ADDR2L    COMP  PIC  S9(4).
           02  ADDR2F    PICTURE X.
           02  FILLER REDEFINES ADDR2F.
             03 ADDR2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADDR2I  PIC X(20).
           02  STATL    COMP  PIC  S9(4).
           02  STATF    PICTURE X.
           02  FILLER REDEFINES STATF.
             03 STATA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  STATI  PIC X(1).
           02  CRLIMITL    COMP  PIC  S9(4).
           02  CRLIMITF    PICTURE X.
           02  FILLER REDEFINES CRLIMITF.
             03 CRLIMITA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CRLIMITI  PIC X(8).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(40).
       01  MAP1O REDEFINES MAP1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ACCTNOC    PICTURE X.
           02  ACCTNOH    PICTURE X.
           02  ACCTNOO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  TITLC    PICTURE X.
           02  TITLH    PICTURE X.
           02  TITLO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  FNAMEC    PICTURE X.
           02  FNAMEH    PICTURE X.
           02  FNAMEO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  SNAMEC    PICTURE X.
           02  SNAMEH    PICTURE X.
           02  SNAMEO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ADDR1C    PICTURE X.
           02  ADDR1H    PICTURE X.
           02  ADDR1O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ADDR2C    PICTURE X.
           02  ADDR2H    PICTURE X.
           02  ADDR2O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  STATC    PICTURE X.
           02  STATH    PICTURE X.
           02  STATO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRLIMITC    PICTURE X.
           02  CRLIMITH    PICTURE X.
           02  CRLIMITO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(40).