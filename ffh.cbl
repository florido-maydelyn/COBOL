       IDENTIFICATION DIVISION.
       PROGRAM-ID. TravelBooking.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BookingFile ASSIGN TO 'bookinglist.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FileStatus.

           SELECT TempBFile ASSIGN TO 'bookinglistTEMP.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.

       FD BookingFile.
       01  BookingRecord.
           05   BookingID         PIC X(13).
           05   FDestination        PIC X(25).
           05   TravelDate        PIC X(12).
           05   FStatus            PIC X(15).
       FD TempBFile.
       01  TempBRecord.
           05   BookingIDTemp     PIC X(10).
           05   DestinationTemp    PIC X(25).
           05   TravelDateTemp     PIC X(12).
           05   StatusTemp         PIC X(15).

       WORKING-STORAGE SECTION.
       01  UserChoice              PIC X.
       01  EndOfFile               PIC X        VALUE 'N'.
       01  FileStatus              PIC XX.
       01  SearchID                PIC X(10).
       01  Found                   PIC X        VALUE 'N'.
       01  WS-BookingID            PIC X(10).
       01  WS-COMMAND              PIC X(100).

       PROCEDURE DIVISION.
           PERFORM UNTIL UserChoice = '5'
               DISPLAY 'TRAVEL BOOKING SYSTEM'
               DISPLAY '1. ADD BOOKING'
               DISPLAY '2. VIEW BOOKINGS'
               DISPLAY '3. UPDATE STATUS'
               DISPLAY '4. DELETE BOOKING'
               DISPLAY '5. EXIT'
               DISPLAY 'ENTER YOUR CHOICE: ' WITH NO ADVANCING
               ACCEPT UserChoice

               EVALUATE UserChoice
                   WHEN '1'
                       DISPLAY SPACE
                       PERFORM WriteInput
                       DISPLAY SPACE
                   WHEN '2'
                       DISPLAY SPACE
                       PERFORM ViewBookings
                       DISPLAY SPACE
                   WHEN '3'
                       DISPLAY SPACE
                       PERFORM UpdateStatus
                       DISPLAY SPACE
                   WHEN '4'
                       DISPLAY SPACE
                       PERFORM DeleteBooking
                       DISPLAY SPACE
                   WHEN '5'
                       DISPLAY 'Exiting program.'
                   WHEN OTHER
                       DISPLAY 'Invalid choice. Please try again.'
               END-EVALUATE
           END-PERFORM

           STOP RUN.

       WriteInput.
           OPEN I-O BookingFile
           IF FileStatus = '35'
               DISPLAY 'Creating a file.'
               PERFORM Labels
               PERFORM AddBooking
           ELSE
               READ BookingFile
                   AT END
                       DISPLAY 'Adding first Booking.'
                       CLOSE BookingFile
                       PERFORM AddBooking
                   NOT AT END
                       CLOSE BookingFile
                       DISPLAY 'Incrementing booking list'
                       PERFORM AddBooking
               END-READ
           END-IF
           CLOSE BookingFile.

       Labels.
           OPEN OUTPUT BookingFile

           MOVE "Booking ID" TO BookingID
           MOVE "Destination" TO FDestination
           MOVE "Date" TO TravelDate
           MOVE "Status" TO FStatus
           WRITE BookingRecord

           CLOSE BookingFile.

       AddBooking.
           OPEN EXTEND BookingFile

           DISPLAY 'Enter Booking ID (10 characters): ' NO ADVANCING
           ACCEPT BookingID
           DISPLAY 'Enter Destination (30 characters): ' NO ADVANCING
           ACCEPT FDestination
           DISPLAY 'Enter Travel Date (YYYY-MM-DD): ' NO ADVANCING
           ACCEPT TravelDate
           DISPLAY 'Enter Status (Confirmed/Pending/Canceled): ' 
           NO ADVANCING
           ACCEPT FStatus
           WRITE BookingRecord

           CLOSE BookingFile
           DISPLAY 'Booking added successfully.'.

       ViewBookings.
           OPEN INPUT BookingFile
           IF FileStatus = '00'
               DISPLAY 'ERROR' FileStatus
           END-IF

           DISPLAY " BOOKING ID | DESTINATION             | TRAVEL DATE 
      -    "| STATUS"
           DISPLAY '----------------------------------------------------
      -    '---------'

           MOVE 'N' TO EndOfFile

           PERFORM UNTIL EndOfFile = 'Y'
               READ BookingFile INTO BookingRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END
                       DISPLAY BookingID SPACE FDestination SPACE 
                       TravelDate SPACE " "FStatus
               END-READ
           END-PERFORM
           CLOSE BookingFile
           DISPLAY SPACE
           DISPLAY 'Finished viewing bookings.'.

       UpdateStatus.
           DISPLAY 'Enter Booking ID to update status: ' NO ADVANCING
           ACCEPT SearchID

           OPEN I-O BookingFile
           MOVE 'N' TO Found
           MOVE 'N' TO EndOfFile

           PERFORM UNTIL EndOfFile = 'Y'
               READ BookingFile INTO BookingRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END
                       IF BookingID = SearchID
                          MOVE 'Y' TO Found
                          DISPLAY 'Current Status: ' FStatus
                          DISPLAY 'Enter new Status (Confirmed/Pending/C
      -                   'anceled): '
                          ACCEPT FStatus
                          REWRITE BookingRecord
                          DISPLAY 'Status updated successfully.'
                       END-IF
               END-READ
           END-PERFORM
           IF Found = 'N'
               DISPLAY 'Booking ID not found.'
           END-IF
           CLOSE BookingFile.

       DeleteBooking.
           DISPLAY 'Enter Booking ID to delete: ' NO ADVANCING
           ACCEPT WS-BookingID

           OPEN INPUT BookingFile
           OPEN OUTPUT TempBFile

           MOVE 'N' TO Found
           MOVE 'N' TO EndOfFile

           PERFORM UNTIL EndOfFile = 'Y'
               READ BookingFile INTO BookingRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END
                       IF WS-BookingID = BookingID
                          MOVE 'Y' TO Found
                          DISPLAY 'Deleting Booking: '
                          DISPLAY 'Booking ID: ' BookingID
                          DISPLAY 'Destination: ' FDestination
                          DISPLAY 'Travel Date: ' TravelDate
                          DISPLAY 'Status: ' FStatus
                          CONTINUE
                       ELSE
                          WRITE TempBRecord FROM BookingRecord
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BookingFile.
           CLOSE TempBFile.

           IF Found = 'Y'
               MOVE 'del bookinglist.dat' TO WS-COMMAND
               CALL 'SYSTEM' USING WS-COMMAND
               MOVE 'move bookinglistTEMP.dat bookinglist.dat' TO 
               WS-COMMAND
               CALL 'SYSTEM' USING WS-COMMAND

               DISPLAY 'Booking deleted successfully.'
           ELSE
               DISPLAY 'Booking not found.'
           END-IF.
