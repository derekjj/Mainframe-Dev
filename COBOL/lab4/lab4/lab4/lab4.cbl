       identification division.
       program-id. lab4.
       author. Derek Johnston.

       environment division.

       input-output section.
       file-control.
           select sales-file  assign to "../../../data/lab4.dat"
                    organization is line sequential.
                    
           select report-file
           assign to "../../../data/lab4.out"
      *    assign to display
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
           05  sdl-sman-num     pic 99.
           05  filler           pic x(2).
           05  sdl-name         pic x(5).
           05  filler           pic x(2).
           05  sdl-sales        pic z,zzz,zz9.
           05  filler           pic x(2).
           05  sdl-min          pic zz9.
           05  filler           pic x(2).
           05  sdl-max          pic z,zz9.
           05  filler           pic x(3).
           05  sdl-rate         pic z9.
           05  sdl-perc-sign    pic x.
           05  filler           pic x(3).
           05  sdl-earned       pic z,zz9.
           05  filler           pic x(4).
           05  sdl-paid         pic $*,*99.
           05  filler           pic x(20).

       working-storage section.

       01  sw-eof               pic x value 'n'.
      
      * add the pic values in the following total lines.
       01  total-line.
           05  filler           pic x(24).
           05  filler           pic x(6)
                   value "totals".
           05  filler           pic x(9).
           05  tl-tot-earned    pic $$$$,$$9.
           05  filler           pic x(2).
           05  tl-tot-paid      pic $$$$,$$9.

       01  num-max-line.
           05  filler          pic x(40)
                value "     number with bonus more than max    ".
           05  maxl-num-mor-max  pic zz9.

       01  num-min-line.
           05  filler          pic x(40)
                value "     number with no bonus less than min ".
           05  minl-num-less-min  pic zz9.

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
                   value "Derek Johnston, lab 4".
           05  filler          pic x(5)  value spaces.
           05  nl-date         pic 9(6).
           05  filler          pic x(5)  value spaces.
           05  nl-time         pic 9(8).
           
       77 ws-earned            pic 9999 value 0.
       77 ws-paid              pic 9999 value 0.
       
       77 ws-num-less-min      pic 999 value 0.
       77 ws-num-more-max      pic 999 value 0.
       77 ws-tot-earned        pic 99999 value 0.
       77 ws-tot-paid          pic 99999 value 0.

       procedure division.
           open input sales-file,
                output report-file.
                
           accept nl-date from date.
           accept nl-time from time.
                     
           perform 100-print-headings.
           
           read sales-file at end move 'y' to sw-eof.

           perform 000-process-records until  sw-eof = 'y'.

           perform 200-print-totals.
           
      *    accept return-code.
           
           close   sales-file
                   report-file.
           stop run.

       000-process-records.

           move spaces to prt-line.
           
           if sr-sales > 5000
      
      *        Find the amount earned with bonus
               compute ws-earned rounded =
                   (sr-sales * (sr-rate/100)) +
                   ((sr-sales - 5000) * 0.125)
      
      *        The amount paid, can't be more then the max            
	           if sr-max < ws-earned
                   move sr-max to ws-paid
                   add 1 to ws-num-more-max
               else
                   move ws-earned to ws-paid
               end-if
		   
           else
      *        Find the amount earned     
               compute ws-earned rounded = (sr-sales * (sr-rate/100))
	  
      *        The the amount paid, can't be less then min
	           if sr-min > ws-earned
                   move sr-min to ws-paid
                   add 1 to ws-num-less-min
               else
                   move ws-earned to ws-paid
               end-if
		   
		   
           end-if.
      
      *    Move all the data to the sub data line    
           move sr-sman-num to sdl-sman-num.
           move sr-name to sdl-name.
           move sr-sales to sdl-sales.
           move sr-min to sdl-min.
           move sr-max to sdl-max.
           move sr-rate to sdl-rate.
           move "%" to sdl-perc-sign.
           move ws-earned to sdl-earned.
           move ws-paid to sdl-paid.
           
      *    Print that line after more line    
           write prt-line after advancing 1 line.

      *    Add values to the totals
           add ws-earned to ws-tot-earned.
           add ws-paid to ws-tot-paid.
           
           read sales-file at end move 'y' to sw-eof.

       100-print-headings.
      
      *    Print all the heading lines with line spacing
           write prt-line from name-line after advancing page.
           write prt-line from rpt-heading after advancing 2 lines.
           write prt-line from heading-line1 after advancing 2 lines.
           write prt-line from underlines after advancing 1 line.
		   
       200-print-totals.
     
      *    Move all the total data in WS to their line output storage
           move ws-tot-earned to tl-tot-earned.
           move ws-tot-paid to tl-tot-paid.
           move ws-num-less-min to minl-num-less-min.
           move ws-num-more-max to maxl-num-mor-max.
      
      *    Print all the total lines using WS.    
           write prt-line from total-line after advancing 2 lines.
           write prt-line from num-max-line after advancing 2 lines.
           write prt-line from num-min-line after advancing 2 lines.