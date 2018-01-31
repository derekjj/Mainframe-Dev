       identification division.
       program-id. lab8.
       author. Derek Johnston.
       date-compiled. 2016-03-28.
       date-written. 2016-03-28.

       environment division.
       input-output section.
       file-control.
       
           select input-file
           assign to "../../../data/lab8.dat"
           organization is line sequential.
           
           select grad-output-file
           assign to "../../../data/lab8GradOut.out"
      *    assign to display
           organization is line sequential.
           
           select ngrad-output-file
           assign to "../../../data/lab8NonGradOut.out"
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
           record contains 120 characters.
           
       01  grad-print-line.
           05 filler                       pic x(120).
           
       FD  ngrad-output-file
           data record is ngrad-print-line
           record contains 120 characters.
           
       01  ngrad-print-line.
           05 filler                       pic x(120).
           
       working-storage section.
       
       01  name-line.
           05  filler                      pic x(5) value spaces.
           05  filler                      pic x(21)
               value "Derek Johnston, LAB 8".
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
           05 print-budget-diff            pic $$$$,$$9.99-.
           
       01 page-raise-line.
           05 filler                       pic x(50).
           05 filler                       pic x(17)
               value "RAISES THIS PAGE:".
           05 filler                       pic x(3).
           05 page-raise-line-amount       pic $$$,$$9.99.
           
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
           
       01 ngrad-avg-line.
           05 filler                       pic x(3).
           05 filler                       pic x(18)
               value "AVERAGE INCREASES:".
           05 filler                       pic x(3).
           05 filler                       pic x(8)
               value "PROG=".
           05 filler                       pic x(3).
           05 ngrad-avg-line-prog          pic zz,zz9.99.
           05 filler                       pic x(3).
           05 filler                       pic x(9)
               value "JR PROG=".
           05 filler                       pic x(3).
           05 ngrad-avg-line-jr-prog       pic zz,zz9.99.
           
       01 highest-pay-raise-line.
           05 filler                           pic x(35).
           05 highest-pay-raise-line-name      pic x(15).
           05 filler                           pic x(27)
               value " MADE THE HIGHEST RAISE AT ".
           05 filler                           pic x(3).
           05 highest-pay-raise-line-amount    pic $$$,$$9.99.
           
       01 non-grad-bonus-msg.
           05 filler                           pic x(35).
           05 filler                           pic x(48)
               value "****** NON-GRAD SALARY ADJUSTED BY $1500 *******".
            
       01 budget-diff-warning-msg.
           05 filler                           pic x(35).
           05 filler                           pic x(48)
               value "***** BUDGET DIFFERENCE GREATER THAN $500 ******".
               
           
      *--------------Loop Switch-------------------------------
       01  sw-eof               pic x value 'n'.
      *--------------Constants-------------------------------
       77 ws-lines-per-page                    pic 99 value 15.
       77 ws-lines-group-1                     pic 9 value 5.
       77 ws-lines-group-2                     pic 99 value 10.
       
       01 ws-min-years.
           05 ws-min-years-grad-analyst        pic 99 value 15.
           05 ws-min-years-grad-sen-prog       pic 9 value 7.
           05 ws-min-years-grad-prog           pic 9 value 2.
           05 ws-min-years-ngrad-prog          pic 99 value 10.
           05 ws-min-years-ngrad-jr-prog       pic 9 value 4.
           05 ws-min-years-ngrad-bonus         pic 99 value 12.
           
       01 ws-ngrad-bonus-amount                pic 9999 value 1500.
       01 ws-budget-diff-warning               pic 999 value 500.
           
       01 ws-pay-increases.
           05 ws-pay-inc-analyst               pic 9v999 value 0.119.
           05 ws-pay-inc-sen-prog              pic 9v999 value 0.093.
           05 ws-pay-inc-prog                  pic 9v999 value 0.067.
           05 ws-pay-inc-jr-prog               pic 9v999 value 0.037.
           05 ws-pay-inc-un-class              pic 9v9 value 0.0.
           
      *------counts----------------------------------------
       01 ws-counts.
           05 ws-count-grad-line               pic 99 value 0.
           05 ws-count-ngrad-line              pic 99 value 0.
           05 ws-count-grad-page               pic 99 value 1.
           05 ws-count-ngrad-page              pic 99 value 1.
           05 ws-count-grad-analyst            pic 99 value 0.
           05 ws-count-grad-sen-prog           pic 99 value 0.
           05 ws-count-grad-prog               pic 99 value 0.
           05 ws-count-ngrad-prog              pic 99 value 0.
           05 ws-count-ngrad-jr-prog           pic 99 value 0.
       
      *------totals----------------------------------------
       01 ws-totals.
           05 ws-total-grad-analyst            pic 9(6)v99 value 0.
           05 ws-total-grad-sen-prog           pic 9(6)v99 value 0.
           05 ws-total-grad-prog               pic 9(6)v99 value 0.
           05 ws-total-ngrad-prog              pic 9(6)v99 value 0.
           05 ws-total-ngrad-jr-prog           pic 9(6)v99 value 0.
           
      *------Varables--------------------------------------
       01 ws-variables.
           05 ws-pay-inc                       pic 9v999 value 0.
           05 ws-pay-increase                  pic 9(5)v99 value 0.
           05 ws-new-salary                    pic 9(6)v99 value 0.
           05 ws-budget-diff                   pic 9(6)v99 value 0.
           05 ws-grad-page-raise               pic 9(6)v99 value 0.
           05 ws-ngrad-page-raise              pic 9(6)v99 value 0.
           05 ws-grad-greatest-raise-name      pic x(15) value spaces.
           05 ws-grad-greatest-raise-amount    pic 9(6)v99 value 0.
           05 ws-ngrad-greatest-raise-name     pic x(15) value spaces.
           05 ws-ngrad-greatest-raise-amount   pic 9(6)v99 value 0.
           05 ws-bonus                         pic 9999 value 0.
       
       procedure division.
           open input input-file,
                output grad-output-file,
                output ngrad-output-file.
                
           accept nl-date from date.
           accept nl-time from time.
           
      * Header for the first page    
           perform 100-print-grad-headings.
           perform 150-print-ngrad-headings.
      
      * Main loop
           read input-file at end move 'y' to sw-eof.
           perform 000-process-records until  sw-eof = 'y'.
      
      * Footer with averages
           perform 180-print-grad-page-raise.
           perform 190-print-ngrad-page-raise.
           perform 900-averages.
           perform 170-print-greatest-raises.
           
      *    accept return-code.
           close   input-file
                   grad-output-file
                   ngrad-output-file.
           stop run.
           
       000-process-records.
      
      *    clear the data in the working storage for ouput.    
           move spaces to data-line.
           move zero to ws-bonus.
      
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
               perform 1100-ngrad
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
           
       150-print-ngrad-headings.
           move spaces to ngrad-print-line.
      *    Move the correct title to the heading line
           move "NON-GRADUATE REPORT" to heading-title.
           move ws-count-ngrad-page to rpt-heading-page.
           
           write ngrad-print-line from name-line
               after advancing page.
           write ngrad-print-line from rpt-heading
               after advancing 2 lines.
           write ngrad-print-line from heading-line1
               after advancing 2 lines.
           write ngrad-print-line from heading-line2
               after advancing 1 line.
               
       170-print-greatest-raises.
           move ws-grad-greatest-raise-amount
               to highest-pay-raise-line-amount.
           move ws-grad-greatest-raise-name
               to highest-pay-raise-line-name.
           write grad-print-line from highest-pay-raise-line
               after advancing 2 lines.
               
           move ws-ngrad-greatest-raise-amount
               to highest-pay-raise-line-amount.
           move ws-ngrad-greatest-raise-name
               to highest-pay-raise-line-name.
           write ngrad-print-line from highest-pay-raise-line
               after advancing 2 lines. 
               
       180-print-grad-page-raise.
           move ws-grad-page-raise to page-raise-line-amount.
           move zero to ws-grad-page-raise.
           write grad-print-line from page-raise-line
               after advancing 2 lines.
               
       190-print-ngrad-page-raise.
           move ws-ngrad-page-raise to page-raise-line-amount.
           move zero to ws-ngrad-page-raise.
           write ngrad-print-line from page-raise-line
               after advancing 2 lines.
       
       200-pay-increase.
           
      *if there is a pay increase, compute the increase and new salary  
           if ws-pay-inc > 0
               compute print-increase-percent rounded =
                   100 * ws-pay-inc
               move "%" to print-percent-symbol
               
               compute ws-pay-increase rounded =
                   ws-bonus + input-present-salary * ws-pay-inc
               
               compute ws-new-salary =
                   ws-pay-increase + input-present-salary
      *    otherwise load default values.            
           else
               move input-present-salary to ws-new-salary
               move zero to ws-pay-increase
           end-if.
           move ws-new-salary to print-new-salary.
           
      *finding the budget differance    
           compute ws-budget-diff =
               input-budget-estimate - ws-new-salary.
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
           
       350-ngrad-pay-increase.
       
      *dependent on class, add pay increase to respective class total
      *and add one to the repective class counter
           if "PROG" = print-class
               add ws-pay-increase to ws-total-ngrad-prog
               add 1 to ws-count-ngrad-prog
           else if "JR PROG" = print-class
               add ws-pay-increase to ws-total-ngrad-jr-prog
               add 1 to ws-count-ngrad-jr-prog
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
               
           compute ngrad-avg-line-prog rounded =
               ws-total-ngrad-prog / ws-count-ngrad-prog.
           compute ngrad-avg-line-jr-prog rounded =
               ws-total-ngrad-jr-prog / ws-count-ngrad-jr-prog.
           
           write grad-print-line from grad-avg-line
               after advancing 2 lines.
               
           write ngrad-print-line from ngrad-avg-line
               after advancing 2 lines.
       1000-grad.
           
      *    If number of lines at max for a page
      *    -Reset line count and add 1 to page count
      *    -Print the headers
           if ws-count-grad-line = ws-lines-per-page
               add 1 to ws-count-grad-page
               move 0 to ws-count-grad-line
               perform 180-print-grad-page-raise
               perform 100-print-grad-headings
           end-if.
           
           move spaces to grad-print-line.
           if ws-count-grad-line = ws-lines-group-1 or
               ws-count-grad-line = ws-lines-group-2
                   write grad-print-line after advancing 2 lines
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
           
           add ws-pay-increase to ws-grad-page-raise.
           move ws-pay-increase to print-pay-increase.
           
           if ws-pay-increase > ws-grad-greatest-raise-amount
               move ws-pay-increase to ws-grad-greatest-raise-amount
               move input-name to ws-grad-greatest-raise-name
           end-if
           
      *print the next line    
           write grad-print-line from data-line after advancing 1 line.
           
      *if budget differance + or - 500 then display warning        
           if ws-budget-diff > ws-budget-diff-warning
               write grad-print-line from budget-diff-warning-msg
                   after advancing 1 line
           end-if.
               
       1100-ngrad.
      *    If number of lines at max for a page
      *    -Reset line count and add 1 to page count
      *    -Print the headers
           if ws-count-ngrad-line = ws-lines-per-page
               add 1 to ws-count-ngrad-page
               move 0 to ws-count-ngrad-line
               perform 190-print-ngrad-page-raise
               perform 150-print-ngrad-headings
           end-if.
           
           move spaces to ngrad-print-line.
           if ws-count-ngrad-line = ws-lines-group-1
               or ws-count-ngrad-line = ws-lines-group-2
                   write ngrad-print-line after advancing 2 lines
           end-if.
               
       
      *    new line, add to line count    
           add 1 to ws-count-ngrad-line
               
           evaluate input-years-service
               when > ws-min-years-ngrad-prog
                   perform 600-prog
               when > ws-min-years-ngrad-jr-prog
                   perform 700-jr-prog
               when other
                   perform 800-unclass
           end-evaluate.
           
      *if non-grads work 12 or more years, they get a bonus, add it     
           if input-years-service >= ws-min-years-ngrad-bonus
               move ws-ngrad-bonus-amount to ws-bonus
           end-if.
           
           perform 200-pay-increase.
           perform 350-ngrad-pay-increase.
           
           add ws-pay-increase to ws-ngrad-page-raise.
           move ws-pay-increase to print-pay-increase.
           
           if ws-pay-increase > ws-ngrad-greatest-raise-amount
               move ws-pay-increase to ws-ngrad-greatest-raise-amount
               move input-name to ws-ngrad-greatest-raise-name
           end-if
     
           
      *print the next line    
           write ngrad-print-line from data-line
               after advancing 1 line.
      
      *if non-grads work 12 or more years, they get a bonus, write it   
           if input-years-service >= ws-min-years-ngrad-bonus
               write ngrad-print-line from non-grad-bonus-msg
                   after advancing 1 line
           end-if.
           
      *if budget differance + or - 500 then display warning        
           if ws-budget-diff > ws-budget-diff-warning
               write ngrad-print-line from budget-diff-warning-msg
                   after advancing 1 line
           end-if.
           
       end program lab8.