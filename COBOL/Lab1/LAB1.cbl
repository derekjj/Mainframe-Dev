       identification division.
       program-id. Lab1-Contact-Info.
       author. DEREK JOHNSTON.
       date-written. 2016-01-13.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 ws-exit-key  pic x(3).
       01 ws-header.
           05 filler                           pic x(18).
           05 ws-header-info                   pic x(22)
               value "MAFD 4204 Contact List".
       01 ws-student-info.
           05 ws-name-student-info             pic x(16).
           05 filler                           pic x(3).
           05 ws-email-student-info            pic x(23).
           05 filler                           pic x(3).
           05 ws-phone-student-info            pic x(12).
       
       procedure division.
      *display header!
           display ws-header.
		   move spaces to ws-header.
		   display ws-header.
      
      *Display my contact info.    
           move "Derek Johnston" to ws-name-student-info.
           move "derek.johnston@dcmail.ca" to ws-email-student-info.
           move "905-926-8279" to ws-phone-student-info.
           display ws-student-info.
      
      *Display student 1 contact info.    
           move "Kyle Warner" to ws-name-student-info.
           move "kyle.warner@dcmail.ca" to ws-email-student-info.
           move "555-926-8277" to ws-phone-student-info.
           display ws-student-info.
      
      *Display student 2 contact info.    
           move "Rob Budd" to ws-name-student-info.
           move "robert.budd1@dcmail.ca" to ws-email-student-info.
           move "555-926-8276" to ws-phone-student-info.
           display ws-student-info.
           
      *pause the display, done the wrong way shown in class.    
           accept ws-exit-key.
           goback.
       
       end program Lab1-Contact-Info.