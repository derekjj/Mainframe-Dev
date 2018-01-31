       identification division.
       program-id. Lab9CallCenterOperatorReport.
       author. Derek Johnston.

       environment division.
       input-output section.
       file-control.

           select input-file assign to '../../../data/lab9.data'
               organization is line sequential.

           select report-file assign to '../../../data/lab9.out'
               organization is line sequential.

       data division.
       file section.

       fd input-file 
           data record is emp-rec.

       01 emp-rec.
           05 emp-rec-num               pic x(3).
           05 emp-rec-name              pic x(12).
           05 emp-rec-calls.
               10 emp-rec-calls-month   pic 9(3)
                   occurs 6 times.

       fd report-file 
           data record is print-line.

       01 print-line                    pic x(132).

       working-storage section.
       
      *create the necessary working storage variables for your code here
       01 ws-constants.
           05 ws-number-of-months     pic 99   value 6.
       
       01 ws-month-subscript          pic 9.
           
       01 ws-calculated-fields.
           05 ws-non-zero-month-count pic 9(2) value 0.
           
       
       01 found-eof                     pic x value 'n'.
           88 is-end-of-file                  value "y".

       01 ws-totals.
           05 ws-grand-total         pic 9(5) value 0.
           05 ws-emp-total           pic 9(4) value 0.
           05 ws-total-no-calls      pic 9(2) value 0.

       01 name-line.
           05 filler                 pic x(2) value spaces.
           05 filler                 pic x(29)
                                     value 'Derek Johnston, lab 9'.
           05 filler                 pic x(5)  value spaces.
           05 name-line-date         pic 9(6).
           05 filler                 pic x(5)  value spaces.
           05 name-line-time         pic 9(8).

       01 report-heading.
           05 filler                 pic x(20).
           05 filler                 pic x(39)
                        value 'call centre volumes for july - december'.

       01 heading-line1.
           05 filler                 pic x(2) value spaces.
           05 filler                 pic x(8) value 'operator'.
           05 filler                 pic x(2) value spaces.
           05 filler                 pic x(8) value 'operator'.
           05 filler                 pic x(6) value spaces.
           05 filler                 pic x(3) value 'jul'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'aug'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'sep'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'oct'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'nov'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'dec'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(5) value 'total'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'avg'.
           05 filler                 pic x(4) value spaces.
           05 filler                 pic x(3) value 'rem'.

       01 heading-line2.
           05 filler                 pic x(5) value spaces.
           05 filler                 pic x(1) value '#'.
           05 filler                 pic x(8) value spaces.
           05 filler                 pic x(4) value 'name'.

       01 detail-line.
           05 filler                    pic x(4) value spaces.
           05 detail-line-num           pic x(3).
           05 filler                    pic x(6) value spaces.
           05 detail-line-name          pic x(12).
           05 filler                    pic x(1) value spaces.
           05 detail-line-months.
               10 filler                occurs 6 times.
                   15 detail-line-month pic zz9.
                   15 filler            pic x(4) value spaces.
           05 filler                    pic x(1) value spaces.
           05 detail-line-total         pic zz9.
           05 filler                    pic x(5) value spaces.
           05 detail-line-avg           pic zzz9.
           05 detail-line-avg-text      redefines detail-line-avg
                                        pic x(4).
           05 filler                    pic x(4) value spaces.
           05 detail-line-rem           pic 9.
           05 detail-line-rem-text      redefines detail-line-rem
                                        pic x.

       01 total-line1.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(35)
                            value "number of operators with no calls: ".
           05 total-line-no-calls       pic z9.

       01 total-line2.
           05 filler                    pic x(6) value spaces.
           05 filler                    pic x(20)
                                           value "overall total calls:".
           05 filler                    pic x(15) value spaces.
           05 total-line-calls          pic zzz99.

       procedure division.
           *> open file handles
           open input input-file,
                output report-file.

           *> grab the current date & time
           accept name-line-date from date.
           accept name-line-time from time.

           *> output heading
           perform 000-print-headings.

           *> process input file & output results
           perform 100-read-input-file.
           perform 200-process-records until is-end-of-file.

           *> output total lines
           perform 500-print-totals.

           *> close file handles
           close input-file
                 report-file.
                 
           stop run.

       000-print-headings.
           write print-line from name-line before advancing 1 line.

           write print-line from report-heading after advancing 1 line.

           write print-line from heading-line1 after advancing 2 lines.
           write print-line from heading-line2 after advancing 1 line.


       100-read-input-file.
           *> reads a line from input file & stores it in emp-rec
           *> - unless eof is encountered in which case it sets found-eof to y
           read input-file at end move 'y' to found-eof.

       200-process-records.
       
      *iterate through monthly calls table to find total
      *and how many months were non-zero.          
           perform varying ws-month-subscript from 1 by 1 until
                ws-month-subscript > ws-number-of-months
      
      *Count the months that more then zero calls where made            
                if emp-rec-calls-month(ws-month-subscript) > 0 then
                    add 1 to ws-non-zero-month-count
                end-if
      
      *add the months call to the emp total months calls         
                add emp-rec-calls-month(ws-month-subscript) to
                    ws-emp-total
                    
      *move the months calls to the output array             
                move emp-rec-calls-month(ws-month-subscript) to
                    detail-line-month(ws-month-subscript)
      
           end-perform.
           
      *add the emp total calls to the total calls         
           add ws-emp-total to ws-grand-total
           
      *Implement average calculation logic as outlined in requirments
           if ws-non-zero-month-count > 0
               divide ws-emp-total by ws-non-zero-month-count
                   giving detail-line-avg rounded
                   remainder detail-line-rem
      *Other method to find the remainder            
      *        COMPUTE detail-line-rem = 
      *            FUNCTION MOD (ws-emp-total, ws-non-zero-month-count)
           else
      *If a record has 0 for all 6 values the average cannot be
      *calculated because it would be division by zero.  The program is
      *to print the word ZERO (use the REDEFINED variable for this) for
      *the average and leave the remainder blank.
               move "ZERO" to detail-line-avg-text
               move spaces to detail-line-rem-text
               add 1 to ws-total-no-calls
           end-if.
           
           
           move emp-rec-num to detail-line-num.
           move emp-rec-name to detail-line-name.
           move ws-emp-total to detail-line-total.
           
           *> print detail line
           write print-line from detail-line after advancing 2 lines.

           *> reset field for next record
           move 0 to ws-emp-total.
           move 0 to ws-non-zero-month-count.

           *> read next record (if any)
           perform 100-read-input-file.


       500-print-totals.
       
           move ws-total-no-calls to total-line-no-calls.
           move ws-grand-total to total-line-calls.
           
           write print-line from total-line1
               after advancing 2 lines.
           write print-line from total-line2
               after advancing 2 lines.

       end program Lab9CallCenterOperatorReport.