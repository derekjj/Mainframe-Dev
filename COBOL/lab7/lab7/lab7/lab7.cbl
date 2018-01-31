       identification division.
       program-id. lab7.
       author. Derek Johnston.
       date-compiled. 2016-03-21.
       date-written. 2016-03-21.

       environment division.
       input-output section.
       file-control.
       
           select input-file
           assign to "../../../data/lab7.dat"
           organization is line sequential.
           
           select grad-output-file
           assign to "../../../data/lab7GradOut.out"
      *    assign to display
           organization is line sequential.
           
           select non-grad-output-file
           assign to "../../../data/lab7NonGradOut.out"
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
           05 input-budget-estimate        pic 9(6)v99.
           
       FD  grad-output-file
           data record is grad-output-print-line
           record contains 117 characters.
           
       01  grad-print-line.
           05 filler                       pic x(117).
           
       FD  non-grad-output-file
           data record is non-grad-print-line
           record contains 117 characters.
           
       01  non-grad-print-line.
           05 filler                       pic x(117).
           
       working-storage section.
       
       01  name-line.
           05  filler                      pic x(5) value spaces.
           05  filler                      pic x(21)
               value "Derek Johnston, LAB 7".
           05  filler                      pic x(5) value spaces.
           05  nl-date                     pic 9(6).
           05  filler                      pic x(5) value spaces.
           05  nl-time                     pic 9(8).
           
       01  rpt-heading.
           05 filler                       pic x(28).
           05 heading-title                pic x(19).
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
           05 filler                       pic x(9).
           05 filler                       pic x(6)
               value "BUDGET".
           05 filler                       pic x(9).
           05 filler                       pic x(6)
               value "BUDGET".
               
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
           05 filler                       pic x(9).
           05 filler                       pic x(6)
               value "EST".
           05 filler                       pic x(9).
           05 filler                       pic x(6)
               value "DIFF".
               
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
           05 filler                       pic x(3).
           05 print-budget-est             pic $zzz,zz9.99.
           05 filler                       pic x(3).
           05 print-budget-diff            pic ---,--9.99.
           
       01 grad-avg-line.
           05 filler                       pic x(3).
           05 filler                       pic x(18)
               value "AVERAGE INCREASES:".
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "ANALYST=".
           05 filler                       pic x(3).
           05 grad-avg-line-analyst        pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(9)
               value "SEN PROG=".
           05 filler                       pic x(3).
           05 grad-avg-line-sen-prog       pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "PROG=".
           05 filler                       pic x(3).
           05 grad-avg-line-prog           pic zz,zz9.99.
           
       01 non-grad-avg-line.
           05 filler                       pic x(3).
           05 filler                       pic x(17)
               value "AVERAGE INCREASES:".
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "PROG=".
           05 filler                       pic x(3).
           05 non-grad-avg-line-prog       pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(9)
               value "JR PROG=".
           05 filler                       pic x(3).
           05 non-grad-avg-line-jr-prog    pic zz,zz9.99.
           
      *--------------Loop Switch-------------------------------
       01  sw-eof               pic x value 'n'.
      *--------------Constants-------------------------------
       77 ws-lines-per-page                    pic 99 value 20.
       
       01 ws-min-years.
           05 ws-min-years-grad-analyst        pic 99 value 15.
           05 ws-min-years-grad-sen-prog       pic 9 value 7.
           05 ws-min-years-grad-prog           pic 9 value 2.
           05 ws-min-years-non-grad-prog       pic 99 value 10.
           05 ws-min-years-non-grad-jr-prog    pic 9 value 4.
           
       01 ws-pay-increases.
           05 ws-pay-inc-analyst               pic 9v999 value 0.119.
           05 ws-pay-inc-sen-prog              pic 9v999 value 0.093.
           05 ws-pay-inc-prog                  pic 9v999 value 0.067.
           05 ws-pay-inc-jr-prog               pic 9v999 value 0.037.
           05 ws-pay-inc-un-class              pic 9v9 value 0.0.
           
      *------counts----------------------------------------
       01 ws-counts.
           05 ws-count-grad-line               pic 99 value 0.
           05 ws-count-non-grad-line           pic 99 value 0.
           05 ws-count-grad-page               pic 99 value 1.
           05 ws-count-non-grad-page           pic 99 value 1.
           05 ws-count-grad-analyst            pic 99 value 0.
           05 ws-count-grad-sen-prog           pic 99 value 0.
           05 ws-count-grad-prog               pic 99 value 0.
           05 ws-count-non-grad-prog           pic 99 value 0.
           05 ws-count-non-grad-jr-prog        pic 99 value 0.
       
      *------totals----------------------------------------
       01 ws-totals.
           05 ws-total-grad-analyst            pic 9(6)v99 value 0.
           05 ws-total-grad-sen-prog           pic 9(6)v99 value 0.
           05 ws-total-grad-prog               pic 9(6)v99 value 0.
           05 ws-total-non-grad-prog           pic 9(6)v99 value 0.
           05 ws-total-non-grad-jr-prog        pic 9(6)v99 value 0.
           
      *------Varables--------------------------------------
       77 ws-pay-inc                           pic 9v999 value 0.
       77 ws-pay-increase                      pic 9(5)v99 value 0.
       77 ws-new-salary                        pic 9(6)v99 value 0.
       
       procedure division.
           open input input-file,
                output grad-output-file,
                output non-grad-output-file.
                
           accept nl-date from date.
           accept nl-time from time.
           
      * Header for the first page    
           perform 100-print-grad-headings.
           perform 150-print-non-grad-headings.
      
      * Main loop
           read input-file at end move 'y' to sw-eof.
           perform 000-process-records until  sw-eof = 'y'.
      
      * Footer with averages    
           perform 900-averages.
           
      *    accept return-code.
           close   input-file
                   grad-output-file
                   non-grad-output-file.
           stop run.
           
       000-process-records.
      
      
      *    clear the data in the working storage for ouput.    
           move spaces to data-line.
      
           move input-employee-number to print-employee-number.
           move input-name to print-employee-name.
           move input-years-service to print-years-service.
           move input-present-salary to print-present-salary.
           move "+" to print-pay-symbol.
           move input-budget-estimate to print-budget-est.
     
           
      *    if a grad or non grad    
           if input-education-code = "G"
               perform 1000-grad
           else if input-education-code = "N"
               perform 1100-non-grad
           end-if.
          
      
      *try to read the next line in the data file
           read input-file at end move 'y' to sw-eof.
           
       100-print-grad-headings.
           move spaces to grad-print-line.
      *    Move the correct title to the heading line
           move "GRADUATE REPORT" to heading-title.
           move ws-count-grad-page to rpt-heading-page.
           
           write grad-print-line from name-line
               after advancing page.
           write grad-print-line from rpt-heading
               after advancing 2 lines.
           write grad-print-line from heading-line1
               after advancing 2 lines.
           write grad-print-line from heading-line2
               after advancing 1 line.
           
       150-print-non-grad-headings.
           move spaces to non-grad-print-line.
      *    Move the correct title to the heading line
           move "NON-GRADUATE REPORT" to heading-title.
           move ws-count-non-grad-page to rpt-heading-page.
           
           write non-grad-print-line from name-line
               after advancing page.
           write non-grad-print-line from rpt-heading
               after advancing 2 lines.
           write non-grad-print-line from heading-line1
               after advancing 2 lines.
           write non-grad-print-line from heading-line2
               after advancing 1 line.
       
       200-pay-increase.
           
      *if there is a pay increase, compute the increase and new salary  
           if ws-pay-inc > 0
               compute print-increase-percent rounded =
                   100 * ws-pay-inc
               move "%" to print-percent-symbol
               
               compute ws-pay-increase rounded =
                   input-present-salary * ws-pay-inc
               
               compute ws-new-salary rounded =
                   (input-present-salary * ws-pay-inc) +
                   input-present-salary
      *    otherwise load default values.            
           else
               move input-present-salary to ws-new-salary
               move zero to ws-pay-increase
           end-if.
           move ws-new-salary to print-new-salary.
           
           compute print-budget-diff =
               input-budget-estimate - ws-new-salary.
       300-grad-pay-incrase.
       
      *dependent on class, add pay increase to respective class total
      *and add one to the repective class counter
           if "ANALYST" = print-class
               add ws-pay-increase to ws-total-grad-analyst
               add 1 to ws-count-grad-analyst
           else if "SEN PROG" = print-class
               add ws-pay-increase to ws-total-grad-sen-prog
               add 1 to ws-count-grad-sen-prog
           else if "PROG" = print-class
               add ws-pay-increase to ws-total-grad-prog
               add 1 to ws-count-grad-prog
           end-if.
           
       350-non-grad-pay-increase.
       
      *dependent on class, add pay increase to respective class total
      *and add one to the repective class counter
           if "PROG" = print-class
               add ws-pay-increase to ws-total-non-grad-prog
               add 1 to ws-count-non-grad-prog
           else if "JR PROG" = print-class
               add ws-pay-increase to ws-total-non-grad-jr-prog
               add 1 to ws-count-non-grad-jr-prog
           end-if.
       
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
           compute grad-avg-line-analyst rounded =
               ws-total-grad-analyst / ws-count-grad-analyst.
           compute grad-avg-line-sen-prog rounded =
               ws-total-grad-sen-prog / ws-count-grad-sen-prog.
           compute grad-avg-line-prog rounded =
               ws-total-grad-prog / ws-count-grad-prog.
               
           compute non-grad-avg-line-prog rounded =
               ws-total-non-grad-prog / ws-count-non-grad-prog.
           compute non-grad-avg-line-jr-prog rounded =
               ws-total-non-grad-jr-prog / ws-count-non-grad-jr-prog.
           
           write grad-print-line from grad-avg-line
               after advancing 2 lines.
               
           write non-grad-print-line from non-grad-avg-line
               after advancing 2 lines.
       1000-grad.
           
      *    If number of lines at max for a page
      *    -Reset line count and add 1 to page count
      *    -Print the headers
           if ws-count-grad-line = ws-lines-per-page
               add 1 to ws-count-grad-page
               move 0 to ws-count-grad-line
               perform 100-print-grad-headings
           end-if.
      *    new line, add to line count 
           add 1 to ws-count-grad-line
           
      
           evaluate input-years-service
               when > ws-min-years-grad-analyst
                   perform 400-analyst
               when >= ws-min-years-grad-sen-prog
                   perform 500-sen-prog
               when > ws-min-years-grad-prog
                   perform 600-prog
               when other
                   perform 800-unclass
           end-evaluate.
           
           perform 200-pay-increase.
           perform 300-grad-pay-incrase.
           
           move ws-pay-increase to print-pay-increase.
       
      *print the next line    
           write grad-print-line from data-line after advancing 2 lines.
               
       1100-non-grad.
      *    If number of lines at max for a page
      *    -Reset line count and add 1 to page count
      *    -Print the headers
           if ws-count-non-grad-line = ws-lines-per-page
               add 1 to ws-count-non-grad-page
               move 0 to ws-count-non-grad-line
               perform 150-print-non-grad-headings
           end-if.
       
      *    new line, add to line count    
           add 1 to ws-count-non-grad-line
               
           evaluate input-years-service
               when > ws-min-years-non-grad-prog
                   perform 600-prog
               when > ws-min-years-non-grad-jr-prog
                   perform 700-jr-prog
               when other
                   perform 800-unclass
           end-evaluate.
           
           perform 200-pay-increase.
           perform 350-non-grad-pay-increase.
           
           move ws-pay-increase to print-pay-increase.
     
      *print the next line    
           write non-grad-print-line from data-line
               after advancing 2 lines.
           
       end program lab7.