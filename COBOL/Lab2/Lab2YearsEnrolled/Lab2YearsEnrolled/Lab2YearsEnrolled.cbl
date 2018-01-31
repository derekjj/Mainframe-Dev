       identification division.
       program-id. Lab2YearsEnrolled.
       author. Derek Johnston.
       date-written. 2016-01-18.
       
       environment division.
       input-output section.
       file-control.
       
           select input-file
           assign to "../../../data/lab2.dat"
           organization is line sequential.
           
           select output-file
           assign to "../../../data/lab2.out"
           organization is line sequential.
           
       configuration section.

       data division.
       file section.
	   
       fd  input-file
           data record is read-line
           record contains 29 characters.
		   
       01  input-line.
           05 input-student-number          pic x(9).
           05 input-student-name            pic x(20).
           05 input-student-year            pic 9999.

       FD  output-file
           data record is print-line
           record contains 45 characters.
           
       01  print-line.
           05 filler                        pic x(5).
           05 print-student-number          pic x(9).
           05 filler                        pic x(5).
           05 print-student-name            pic x(20).
           05 filler                        pic x(5).
           05 print-student-year            pic 9(4).
           05 filler                        pic x(5).
           05 print-student-enrolled        pic z9.
       
       working-storage section.
       
       01 ws-end-year                       pic 9999
           value 2016.
       01 ws-student-years                  pic 99.
       
       01 ws-end-of-file-flag               pic x
           value "N".
       
       01 ws-header.
           05 filler                        pic x(25).
           05 ws-header-name                pic x(14)
           value "Derek Johnston".
       
       procedure division.
       
           open output output-file.
           open input input-file.
      
      *Write the header being my name    
           write print-line from ws-header
               after advancing 1 line.
      
      *Priming the input file       
           read input-file
               at end move "Y" to ws-end-of-file-flag.
      
      *Start of looping the input file
           perform until ws-end-of-file-flag equals "Y"
           
      *file processing goes here...
           subtract input-student-year from ws-end-year
               giving ws-student-years
           
      *clear output buffer.
           move spaces to print-line
      
      *manipulate output.
           move input-student-number to print-student-number
           move input-student-name to print-student-name
           move input-student-year to print-student-year
           move ws-student-years to print-student-enrolled
           
      *preform output.    
           write print-line after advancing 2 lines
      
      *try to get the next line in the input file    
           read input-file
               at end move "Y" to ws-end-of-file-flag
               
      *end of the looping input file        
           end-perform.
           
      *close the files.    
           close input-file, output-file.
           
           goback.
       end program Lab2YearsEnrolled.