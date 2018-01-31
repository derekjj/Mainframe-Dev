       01  MENUI.
           02  FILLER PIC X(12).
           02  CHOICEL    COMP  PIC  S9(4).
           02  CHOICEF    PICTURE X.
           02  FILLER REDEFINES CHOICEF.
             03 CHOICEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHOICEI  PIC X(1).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(40).
       01  MENUO REDEFINES MENUI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CHOICEC    PICTURE X.
           02  CHOICEH    PICTURE X.
           02  CHOICEO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(40).