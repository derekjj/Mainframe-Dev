       identification division.
       program-id. Lab3Transportation.
       author. Derek Johnston.
       date-written. 2016-02-01.

       environment division.
       input-output section.
       file-control.
       
           select input-file
           assign to "../../../data/lab3.dat"
           organization is line sequential.
           
           select output-file
           assign to "../../../data/lab3.out"
      *    assign to display
           organization is line sequential.
           
       configuration section.

       data division.
       file section.
	   
       fd  input-file
           data record is read-line
           record contains 29 characters.
		   
       01  input-line.
           05 input-invoice-number          pic xxxx.
           05 input-quantity                pic 999.
           05 input-description             pic x(13).
           05 input-price                   pic 9(4)v99.
           05 input-class                   pic x.

       FD  output-file
           data record is print-line
           record contains 132 characters.
           
       01  print-line.
           05 filler                        pic x(3).
           05 print-invoice-number          pic xxxx.
           05 filler                        pic x(3).
           05 print-extended-price          pic zzz,zz9.99.
           05 filler                        pic x(6).
           05 print-discount-amount         pic zzz,zz9.99.
           05 filler                        pic x(5).
           05 print-net-price               pic zzz,zz9.99.
           05 filler                        pic x(5).
           05 print-class                   pic x(4).
           05 filler                        pic x(5).
           05 print-trans-percent           pic z9.9.
           05 filler                        pic x(9).
           05 print-trans-charge            pic zzz,zz9.99.
           
           
       working-storage section.
       
       01 ws-header-line.
           05 filler               pic x(3).
           05 filler               pic x(3) value "INV".
           05 filler               pic x(5).
           05 filler               pic x(8) value "ExTENDED".
           05 filler               pic x(8).
           05 filler               pic x(8) value "DISCOUNT".
           05 filler               pic x(5).
           05 filler               pic x(9) value "NET PRICE".
           05 filler               pic x(5).
           05 filler               pic x(5) value "CLASS".
           05 filler               pic x(5).
           05 filler               pic x(5) value "TRANS".
           05 filler               pic x(5).
           05 filler               pic x(14) value "TRANSPORTATION".
           
       01 ws-header-second-line.
           05 filler               pic x(3).
           05 filler               pic x(3) value "NUM".
           05 filler               pic x(7).
           05 filler               pic x(8) value "PRICE".
           05 filler               pic x(7).
           05 filler               pic x(8) value "AMOUNT".
           05 filler               pic x(6).
           05 filler               pic x(9).
           05 filler               pic x(5).
           05 filler               pic x(5).
           05 filler               pic x(6).
           05 filler               pic x(4) value "%".
           05 filler               pic x(6).
           05 filler               pic x(14) value "CHARGE".
           
       01 ws-total-line.
           05 filler                        pic x(6).
           05 total-extended-price          pic $$$,$$$,$$9.99.
           05 filler                        pic x(17).
           05 total-net-price               pic $$$,$$$,$$9.99.
           05 filler                        pic x(23).
           05 total-trans-charge            pic $$$,$$$,$$9.99.
       
       01 ws-total-line-no-discount.
           05 filler                        pic x(3).
           05 filler                        pic x(30)
               value "TOTAL ITEMS WITHOUT DISCOUNT = ".
           05 total-items-no-discount       pic z9.
           
       01 total-line-percent-discount.
           05 filler                        pic x(3).
           05 filler                        pic x(44)
               value "PERCENT OF ITEMS THAT RECEIVED A DISCOUNT = ".
           05 print-percent-discount        pic z9.9.
           05 filler                        pic x
               value "%".
           
       01 ws-end-of-file-flag               pic x
           value "N".
           
       01 ws-header.
           05 filler                        pic x(3).
           05 filler                        pic x(21)
           value "Derek Johnston, Lab 3".
       
       77 ws-extended-price                 pic 9(6)v99.
       77 ws-discount-percent               pic 99v99.
       77 ws-discount-amount                pic 9(6)v99.
       77 ws-net-price                      pic 9(6)v99.
       77 ws-trans-percent                  pic 9v999.
       77 ws-trans-charge                   pic 9(6)v99.
       
       77 ws-total-extended-price           pic 9(8)v99 value 0.
       77 ws-total-net-price                pic 9(8)v99 value 0.
       77 ws-total-trans-charge             pic 9(8)v99 value 0.
       
       77 ws-items-no-discount              pic 99 value 0.
       77 ws-items-discount                 pic 99 value 0.
       77 ws-total-items                    pic 99 value 0.
       77 ws-percent-discount               pic 9v999 value 0.
       
       77 total-perc-discount               pic 99v9 value 0.
       
       
       
       procedure division.
       
           open output output-file.
           open input input-file.
           move spaces to print-line.
      
      *Write the header being my name
           write print-line from ws-header
               after advancing 2 lines.
           write print-line from ws-header-line
               after advancing 2 line.
           write print-line from ws-header-second-line
               after advancing 1 lines.
               
           move spaces to print-line.
           write print-line after advancing 1 line.
               
           read input-file at end move "Y" to ws-end-of-file-flag.
               
           perform 100-compute until ws-end-of-file-flag equals "Y".
           
           perform 200-totals.
               
      *    accept return-code.
               
      *close the files.    
           close input-file, output-file.
           
           goback.

       
       100-compute.    
               add 1 to ws-total-items.
      
      *Find extended price        
               compute ws-extended-price = 
                   input-price * input-quantity.
                   
      *Discount            
      *if extendeded price greater then $200 then discount rate is 11%   
               if ws-extended-price > 200
               
                   move 0.11 to ws-discount-percent
                   
                   compute ws-discount-amount rounded = 
                       (ws-extended-price * ws-discount-percent)
                   
                   compute ws-net-price  =
                       (ws-extended-price - ws-discount-amount)
               else
      *otherwise no discount        
                   move zero to ws-discount-amount
                   add 1 to ws-items-no-discount
                   move ws-extended-price to ws-net-price
               end-if
      
      *Transportion charges        
               if input-class = "A"
                   move 0.29 to ws-trans-percent
               else if input-class = "B"
                   move 0.14 to ws-trans-percent
               else if input-quantity > 144
                   move 0.115 to ws-trans-percent
               else
                   move 0 to ws-trans-percent
               end-if.
               
               if ws-trans-percent > 0
                   compute ws-trans-charge rounded =
                       ws-trans-percent * ws-extended-price
               else
                   move 35 to ws-trans-charge
               end-if.
               
      *adding totals
               add ws-extended-price to ws-total-extended-price.
               add ws-net-price to ws-total-net-price.
               add ws-trans-charge to ws-total-trans-charge.
      
      *moving for printing
               move spaces to print-line
               move input-invoice-number to print-invoice-number
               move ws-extended-price to print-extended-price
               move ws-discount-amount to print-discount-amount
               move ws-net-price to print-net-price
               move input-class to print-class
               multiply ws-trans-percent by 100
                   giving print-trans-percent
               move ws-trans-charge to print-trans-charge
      
      *advance and print the line.
               write print-line from print-line
               after advancing 1 lines 
      
      *try to read the next line.        
               read input-file at end move "Y" to ws-end-of-file-flag. 
       
       200-totals.
           compute ws-items-discount =
               ws-items-no-discount - ws-total-items.
               
           compute ws-percent-discount =
               ws-items-discount / ws-total-items.
               
           multiply ws-percent-discount
           by 100 giving total-perc-discount.
      
      *working storage to print-totals   
           move ws-total-extended-price to total-extended-price.
           move ws-total-net-price to total-net-price.
           move ws-total-trans-charge to total-trans-charge.
           
           move ws-items-no-discount to total-items-no-discount.
           move total-perc-discount to print-percent-discount.
           
      *Printing the totals    
           write print-line from ws-total-line
               after advancing 2 lines.
           write print-line from ws-total-line-no-discount
               after advancing 2 lines.
           write print-line from total-line-percent-discount
               after advancing 2 lines.
           
       end program Lab3Transportation.