       identification division.
       program-id. lab5.
       author. Derek Johnston.

       environment division.

       input-output section.
       file-control.
           select sales-file  assign to "../../../data/lab5.dat"
                    organization is line sequential.
                    
           select report-file
           assign to "../../../data/lab5.out"
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
       
           05  filler           pic x(1).
           05  sdl-sman-num     pic 99.
           05  filler           pic x(2).
           05  sdl-name         pic x(8).
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
           05  filler           pic x(3).
           05  sdl-notes        pic x(16).

       working-storage section.
       
      *---------------Headers----------------------------------
       
       01  heading-line1.
           05  filler          pic x(1)  value spaces.
           05  filler          pic x(2)  value "no".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(4)  value "name".
           05  filler          pic x(9)  value spaces.
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
           05  filler          pic x(4)  value spaces.
           05  filler          pic x(5)  value "notes".

       01  underlines.
           05  filler          pic x(1)  value spaces.
           05  filler          pic x(2)  value "--".
           05  filler          pic x(2)  value spaces.
           05  filler          pic x(4)  value "----".
           05  filler          pic x(9)  value spaces.
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
           05  filler          pic x(4)  value spaces.
           05  filler          pic x(6)  value "------".

       01  name-line.
           05  filler          pic x(5) value spaces.
           05  filler          pic x(28)
                   value "Derek Johnston, lab 5".
           05  filler          pic x(5)  value spaces.
           05  nl-date         pic 9(6).
           05  filler          pic x(5)  value spaces.
           05  nl-time         pic 9(8).
      
      *----------------Totals-------------------------------
       
       01  total-line.
           05  filler           pic x(27).
           05  filler           pic x(6)
                   value "totals".
           05  filler           pic x(9).
           05  tl-tot-earned    pic $$$$,$$9.
           05  filler           pic x(2).
           05  tl-tot-paid      pic $$$$,$$9.

       01  num-max-line.
           05  filler          pic x(5).
           05  filler          pic x(35)
                value "number with bonus more than max ".
           05  maxl-num-mor-max  pic zz9.
           
       01  num-min-line.
           05  filler          pic x(5).
           05  filler          pic x(35)
                value "number with no bonus less than min ".
           05  minl-num-less-min  pic zz9.
           
       01  earned-it-percent-line.
           05  filler          pic x(5).
           05  filler          pic x(35)
                value "percent paid what they earned ".
           05  earnedl-percent pic zz9.
           05  filler          pic x
               value "%".
               
       01  percent-greater-than-5000-line.
           05  filler              pic x(5).
           05  filler              pic x(16)
                value "Percent > 5000: ".
           05  greaterl-percent    pic zz9.99.
           05  filler              pic x
               value "%".
               
       01  percent-less-than-5001-line.
           05  filler              pic x(5).
           05  filler              pic x(16)
               value "Percent <= 5000: ".
           05  lessl-percent       pic zz9.99.
           05  filler              pic x
               value "%".

       01  rpt-heading.
           05 filler  pic x(28).
           05 filler  pic x(23)
                   value "sales commission report".
      
      *---------------Variables------------------------------           
       01 ws-line-calculations.  
           05 ws-earned            pic 99999 value 0.
           05 ws-paid              pic 99999 value 0.
      
       01 ws-totals-and-counts.
           05 ws-line-count        pic 99 value 0.
           05 ws-num-less-min      pic 999 value 0.
           05 ws-num-more-max      pic 999 value 0.
           05 ws-num-earned-it     pic 999 value 0.
           05 ws-tot-earned        pic 99999 value 0.
           05 ws-tot-paid          pic 99999 value 0.
       
       01 ws-constants.
           05 ws-bonus             pic 9999 value 5000.
           05 ws-bonus-rate        pic 9v999 value 0.125.
           05 ws-lines-per-page    pic 99 value 10.
       
      *--------------Loop Switch-------------------------------
       01  sw-eof               pic x value 'n'.

       procedure division.
           open input sales-file,
                output report-file.
                
           accept nl-date from date.
           accept nl-time from time.
                     
           perform 100-print-headings.
           
           read sales-file at end move 'y' to sw-eof.

           perform 000-process-records until  sw-eof = 'y'.
     
           perform 700-compute-totals.
           
           perform 200-print-totals.
           
      *    accept return-code.
           
           close   sales-file
                   report-file.
           stop run.

       000-process-records.

           move spaces to prt-line.
           add 1 to ws-line-count.
           
           if sr-sales > ws-bonus
      
               perform 300-greater-bonus
		   
           else
		   
               perform 400-less-equal-bonus
		   
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
           
           if ws-line-count = ws-lines-per-page
               perform 100-print-headings
               move 0 to ws-line-count
           end-if.
           
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
           write prt-line from total-line
               after advancing 2 lines.
           write prt-line from num-max-line
               after advancing 2 lines.
           write prt-line from num-min-line
               after advancing 2 lines.
           write prt-line from earned-it-percent-line
               after advancing 2 lines.
           write prt-line from percent-greater-than-5000-line
               after advancing 2 lines.
           write prt-line from percent-less-than-5001-line
               after advancing 1 lines.
       
       300-greater-bonus.
           
      *     Find the amount earned with bonus
            compute ws-earned rounded =
                (sr-sales * (sr-rate/100)) +
                ((sr-sales - ws-bonus) * ws-bonus-rate).
                   
            perform 600-max-logic.
      
       400-less-equal-bonus.
       
      *     Find the amount earned     
            compute ws-earned rounded = (sr-sales * (sr-rate/100)).
               
            perform 500-min-logic.
	  
               
       500-min-logic.
       
      *     The the amount paid, can't be less then min
	        if sr-min > ws-earned
                move sr-min to ws-paid
                add 1 to ws-num-less-min
                move "earned under min" to sdl-notes
            else
                move ws-earned to ws-paid
                add 1 to ws-num-earned-it
            end-if.
               
       600-max-logic.
       
      *     The amount paid, can't be more then the max            
	        if sr-max < ws-earned
                move sr-max to ws-paid
                add 1 to ws-num-more-max
                move "earned over max" to sdl-notes
            else
                move ws-earned to ws-paid
                add 1 to ws-num-earned-it
            end-if.
               
       700-compute-totals.
           
      *    sales people that got paid what then earned
      *    total lines divied by peole that got paid what they earned.
           compute earnedl-percent rounded =
               ws-num-earned-it /
               (ws-num-less-min + ws-num-more-max + ws-num-earned-it) *
               100.

      *    less than equal to 5000 sales people        
           compute lessl-percent rounded =
               ws-num-less-min /
               (ws-num-less-min + ws-num-more-max) *
               100.
               
      *    greater than 5000 sales people  
           compute greaterl-percent rounded =
               ws-num-more-max /
               (ws-num-less-min + ws-num-more-max) *
               100.