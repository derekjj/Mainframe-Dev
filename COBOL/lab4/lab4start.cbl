       identification division.
       program-id. lab4.
       author.
           your name.         <- change to your name...

       environment division.

       input-output section.
       file-control.
           select sales-file  assign to "lab4.dat"
                    organization is line sequential.
                    
           select report-file assign to "lab4.out"
                    organization is line sequential.

       data division.
       file section.
       fd  sales-file 
           data record is sales-rec.

       01  sales-rec.
           05  sr-sman-num     pic xx.
           05  sr-name         pic x(8).
           05  sr-sales        pic 9(4).
           05  sr-rate         pic 99.
           05  sr-min          pic 999.
           05  sr-max          pic 9999.

       fd  report-file 
           data record is prt-line.

       01  prt-line.
      * i have not added the required pic clauses on the prt-line fields 
      * you will need to add these based on info in the lab write up
       
           05  filler           pic x(1).
           05  sdl-sman-num     pic
           05  filler           pic x(2).
           05  sdl-name         pic
           05  filler           pic x(2).
           05  sdl-sales        pic
           05  filler           pic x(2).
           05  sdl-min          pic
           05  filler           pic x(2).
           05  sdl-max          pic
           05  filler           pic x(3).
           05  sdl-rate         pic
           05  sdl-perc-sign    pic x.
           05  filler           pic x(3).
           05  sdl-earned       pic
           05  filler           pic x(4).
           05  sdl-paid         pic
           05  filler           pic x(20).

       working-storage section.

       01  sw-eof               pic x value 'n'.
      
      * add the pic values in the following total lines.
       01  total-line.
           05  filler           pic x(24).
           05  filler           pic x(6)
                   value "totals".
           05  filler           pic x(9).
           05  tl-tot-earned    pic
           05  filler           pic x(2).
           05  tl-tot-paid      pic

       01  num-max-line.
           05  filler          pic x(40)
                value "     number with bonus more than max    ".
           05  maxl-num-mor-max  pic

       01  num-min-line.
           05  filler          pic x(40)
                value "     number with no bonus less than min ".
           05  minl-num-less-min  pic

       01  rpt-heading.
           05 filler  pic x(28).
           05 filler  pic x(23)
                   value "sales commission report".

       01  heading-line1.
           05  filler          pic x(1)  value spaces.
           05  filler          pic x(2)  value "no".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(4)  value "name".
           05  filler          pic x(6)  value spaces.
           05  filler          pic x(5)  value "sales".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(3)  value "min".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(3)  value "max".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(4)  value "rate".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(6)  value "earned".
           05  filler          pic x(5)  value spaces.
           05  filler          pic x(4)  value "paid".

       01  underlines.
           05  filler          pic x(1)  value spaces.
           05  filler          pic x(2)  value "--".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(4)  value "----".
           05  filler          pic x(6)  value spaces.
           05  filler          pic x(5)  value "-----".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(3)  value "---".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(3)  value "---".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(4)  value "----".
           05  filler          pic x(3)  value spaces.
           05  filler          pic x(6)  value "------".
           05  filler          pic x(5)  value spaces.
           05  filler          pic x(4)  value "----".

       01  name-line.
           05  filler          pic x(5) value spaces.
           05  filler          pic x(28)
                   value "your name, lab 4".                <============ change name here too
           05  filler          pic x(5)  value spaces.
           05  nl-date         pic 9(6).
           05  filler          pic x(5)  value spaces.
           05  nl-time         pic 9(8).

       procedure division.
           open input sales-file,
                output report-file.
                
           accept nl-date from date.
           accept nl-time from time.
                     
           perform 100-print-headings.
           
           read sales-file at end move 'y' to sw-eof.

           perform 000-process-records until  sw-eof = 'y'.

           perform 200-print-totals.

           close   sales-file
                   report-file.
           stop run.

       000-process-records.

           if sr-sales > 5000
		   
               compute ws-earned rounded =
		   
		   
		   
		   
		   
		   
		   
		   
           else
              compute ws-earned rounded = sr-sales * sr-rate
		   
		   
		   
           end-if.




           read.....

       100-print-headings.

           write prt-line from name-line after advancing page.

      *    add other write's for headings here....
		   
 
