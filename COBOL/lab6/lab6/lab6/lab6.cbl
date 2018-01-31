       identification division.
       program-id. Program1.
       author. Derek Johnston.
       date-compiled. 2016-02-17.
       date-written. 2016-02-17.

       environment division.
       input-output section.
       file-control.
       
           select input-file
           assign to "../../../data/lab6.dat"
           organization is line sequential.
           
           select output-file
           assign to "../../../data/lab6.out"
      *    assign to display
           organization is line sequential.
           
       configuration section.

       data division.
       file section.
	   
       fd  input-file
           data record is read-line
           record contains 29 characters.
		   
       01  input-line.
           05 input-employee-number        pic xxx.
           05 input-name                   pic x(15).
           05 input-years-service          pic 99.
           05 input-education-code         pic x.
           05 input-present-salary         pic 9(5)v99.

       FD  output-file
           data record is print-line
           record contains 90 characters.
           
       01  print-line.
           05 filler                       pic x(90).
           
       working-storage section.
       01  name-line.
           05  filler                      pic x(5) value spaces.
           05  filler                      pic x(21)
               value "Derek Johnston, LAB 6".
           05  filler                      pic x(5) value spaces.
           05  nl-date                     pic 9(6).
           05  filler                      pic x(5) value spaces.
           05  nl-time                     pic 9(8).
           
       01  rpt-heading.
           05 filler                       pic x(28).
           05 filler                       pic x(22)
               value "EMPLOYEE SALARY REPORT".
           05 filler                       pic x(20).
           05 filler                       pic x(6) value "PAGE ".
           05 rpt-heading-page             pic z9 value 0.
                   
       01  heading-line1.
           05 filler                       pic x(3).
           05 filler                       pic x(3)
               value "EMP".
           05 filler                       pic x(3).
           05 filler                       pic x(3)
               value "EMP".
           05 filler                       pic x(31).
           05 filler                       pic x(7)
               value "PRESENT".
           05 filler                       pic x(4).
           05 filler                       pic x(8)
               value "INCREASE".
           05 filler                       pic x(7).
           05 filler                       pic x(3)
               value "PAY".
           05 filler                       pic x(10).
           05 filler                       pic x(3)
               value "NEW".
               
       01  heading-line2.
           05 filler                       pic x(3).
           05 filler                       pic x(3)
               value "NUM".
           05 filler                       pic x(3).
           05 filler                       pic x(4)
               value "NAME".
           05 filler                       pic x(11).
           05 filler                       pic x(5)
               value "YEARS".
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "POSITION".
           05 filler                       pic x(3).
           05 filler                       pic x(6)
               value "SALARY".
           05 filler                       pic x(8).
           05 filler                       pic x(1)
               value "%".
           05 filler                       pic x(8).
           05 filler                       pic x(8)
               value "INCREASE".
           05 filler                       pic x(6).
           05 filler                       pic x(6)
               value "SALARY".
               
       01 data-line.
           05 filler                       pic x(3).
           05 print-employee-number        pic xxx.
           05 filler                       pic x(3).
           05 print-employee-name          pic x(15).
           05 filler                       pic x(3).
           05 print-years-service          pic z9.
           05 filler                       pic x(3).
           05 print-class                  pic x(8).
           05 filler                       pic x(3).
           05 print-present-salary         pic zz,zz9.99.
           05 filler                       pic x(3).
           05 print-increase-percent       pic z9.9.
           05 print-percent-symbol         pic x.
           05 filler                       pic x(3).
           05 print-pay-increase           pic $$$$,$$9.99.
           05 print-pay-symbol             pic x value "+".
           05 filler                       pic x(3).
           05 print-new-salary             pic $zzz,zz9.99.
           
       01 average-line1.
           05 filler                       pic x(3).
           05 filler                       pic x(18)
               value "AVERAGE INCREASES: ".
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "ANALYST=".
           05 filler                       pic x(3).
           05 avg-line-analyst             pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(9)
               value "SEN PROG=".
           05 filler                       pic x(3).
           05 avg-line-sen-prog            pic zz,zz9.99.
           
       01 average-line2.
           05 filler                       pic x(24).
           05 filler                       pic x(8)
               value "PROG=".
           05 filler                       pic x(3).
           05 avg-line-prog                pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(9)
               value "JR PROG=".
           05 filler                       pic x(3).
           05 avg-line-jr-prog             pic zz,zz9.99.
           
      *--------------Loop Switch-------------------------------
       01  sw-eof               pic x value 'n'.
      *--------------Constants-------------------------------
       77 ws-lines-per-page                    pic 99 value 15.
       
       01 ws-min-years.
           05 ws-min-years-grad-analyst        pic 99 value 15.
           05 ws-min-years-grad-sen-prog       pic 9 value 7.
           05 ws-min-years-grad-prog           pic 9 value 2.
           05 ws-min-years-prog                pic 99 value 10.
           05 ws-min-years-jr-prog             pic 9 value 4.
           
       01 ws-pay-increases.
           05 ws-pay-inc-analyst               pic 9v999 value 0.119.
           05 ws-pay-inc-sen-prog              pic 9v999 value 0.093.
           05 ws-pay-inc-prog                  pic 9v999 value 0.067.
           05 ws-pay-inc-jr-prog               pic 9v999 value 0.037.
           05 ws-pay-inc-un-class              pic 9v9 value 0.0.
           
      *------counts----------------------------------------
       01 ws-counts.
           05 ws-count-line                    pic 99 value 0.
           05 ws-count-page                    pic 99 value 1.
           05 ws-count-analyst                 pic 99 value 0.
           05 ws-count-sen-prog                pic 99 value 0.
           05 ws-count-prog                    pic 99 value 0.
           05 ws-count-jr-prog                 pic 99 value 0.
       
      *------totals----------------------------------------
       01 ws-totals.
           05 ws-total-analyst                 pic 9(6)v99 value 0.
           05 ws-total-sen-prog                pic 9(6)v99 value 0.
           05 ws-total-prog                    pic 9(6)v99 value 0.
           05 ws-total-jr-prog                 pic 9(6)v99 value 0.
           
      *------Varables--------------------------------------
       77 ws-pay-inc                           pic 9v999 value 0.0.
       77 ws-pay-increase                      pic 9(5)v99 value 0.
       
       procedure division.
           open input input-file,
                output output-file.
                
           accept nl-date from date.
           accept nl-time from time.
           
      * Header for the first page    
           perform 100-print-headings.
      
      * Main loop
           read input-file at end move 'y' to sw-eof.
           perform 000-process-records until  sw-eof = 'y'.
      
      * Footer with averages    
           perform 900-averages.
           
      *    accept return-code.
           close   input-file
                   output-file.
           stop run.
           
       000-process-records.
      
      *If number of lines at max for a page
      *-Reset line count and add 1 to page count
      *-Print the headers
           if ws-count-line = ws-lines-per-page
               add 1 to ws-count-page
               move 0 to ws-count-line
               perform 100-print-headings
           end-if.
      
      *new line, add to line count    
           add 1 to ws-count-line.
      
      *clear the data in the working storage for ouput.    
           move spaces to data-line.
      
      *if a grad or non grad    
           if input-education-code = "G"
               if input-years-service > ws-min-years-grad-analyst
                   perform 400-analyst
               else if input-years-service >= ws-min-years-grad-sen-prog
                   perform 500-sen-prog
               else if input-years-service >= ws-min-years-grad-prog
                   perform 600-prog
               else
                   perform 800-unclass
               end-if
           else if input-education-code = "N"
               if input-years-service > ws-min-years-prog
                   perform 600-prog
               else if input-years-service >= ws-min-years-jr-prog
                   perform 700-jr-prog
               else
                   perform 800-unclass
               end-if
           end-if.
     
      *if there is a pay increase, compute the increase and new salary  
           if ws-pay-inc > 0
               compute print-increase-percent rounded =
                   100 * ws-pay-inc
               move "%" to print-percent-symbol
               
               compute ws-pay-increase rounded =
                   input-present-salary * ws-pay-inc
               
               compute print-new-salary rounded =
                   (input-present-salary * ws-pay-inc) +
                   input-present-salary
      *otherwise load default values.            
           else
               move input-present-salary to print-new-salary
               move zero to ws-pay-increase
           end-if.
      
      *dependent on class, add pay increase to respective class total
      *and add one to the repective class counter
           if "ANALYST" = print-class
               add ws-pay-increase to ws-total-analyst
               add 1 to ws-count-analyst
           else if "SEN PROG" = print-class
               add ws-pay-increase to ws-total-sen-prog
               add 1 to ws-count-sen-prog
           else if "PROG" = print-class
               add ws-pay-increase to ws-total-prog
               add 1 to ws-count-prog
           else if "JR PROG" = print-class
               add ws-pay-increase to ws-total-jr-prog
               add 1 to ws-count-jr-prog
           end-if.
      
      *prep all the reset of the data for printing    
           move input-employee-number to print-employee-number.
           move input-name to print-employee-name.
           move input-years-service to print-years-service.
           move input-present-salary to print-present-salary.
           move ws-pay-increase to print-pay-increase.
           move "+" to print-pay-symbol.
     
      *print the next line    
           write print-line from data-line after advancing 2 lines.
      
      *try to read the next line in the data file
           read input-file at end move 'y' to sw-eof.
           
       100-print-headings.
           move spaces to print-line.
           move ws-count-page to rpt-heading-page.
           write print-line from name-line after advancing page.
           write print-line from rpt-heading after advancing 2 lines.
           write print-line from heading-line1 after advancing 2 lines.
           write print-line from heading-line2 after advancing 1 line.
           
       400-analyst.
           move "ANALYST" to print-class.
           move ws-pay-inc-analyst to ws-pay-inc.
               
       500-sen-prog.
           move "SEN PROG" to print-class.
           move ws-pay-inc-sen-prog to ws-pay-inc.
           
       600-prog.
           move "PROG" to print-class.
           move ws-pay-inc-prog to ws-pay-inc.
           
       700-jr-prog.
           move "JR PROG" to print-class.
           move ws-pay-inc-jr-prog to ws-pay-inc.
           
       800-unclass.
           move spaces to print-class.
           move ws-pay-inc-un-class to ws-pay-inc.
       
       900-averages.
           compute avg-line-analyst rounded =
               ws-total-analyst / ws-count-analyst.
           compute avg-line-sen-prog rounded =
               ws-total-sen-prog / ws-count-sen-prog.
           compute avg-line-prog rounded =
               ws-total-prog / ws-count-prog.
           compute avg-line-jr-prog rounded =
               ws-total-jr-prog / ws-count-jr-prog.
           
           write print-line from average-line1 after advancing 2 lines.
           write print-line from average-line2 after advancing 1 line.
       end program Program1.
       