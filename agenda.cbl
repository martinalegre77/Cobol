      * Author: Martin Alegre
      * Date: 03/09/24
      * Purpose: Archivos secuenciales
      * Type: cbl
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASE-No-7-15.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AgendaFile
           ASSIGN TO "agendaDatos.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD AgendaFile.
       01 PersonDetails.
           88 EndOfAgendaFile    VALUE HIGH-VALUE.
           03 PerTitulo          PIC 9.
               88 Sr             VALUE 1.
               88 Sra            VALUE 2.
               88 Ing            VALUE 3.
               88 Inga           VALUE 4.
               88 Dr             VALUE 5.
               88 Dra            VALUE 6.
               88 Lic            VALUE 7.
               88 Licda          VALUE 8.
           03 PerName.
               05 PerSurname     PIC X(15).
               05 PerForname     PIC X(10).
           03 PerAddress.
               05 PerStreet      PIC X(15).
               05 PerNumber      PIC 9(4).
               05 PerNeigh       PIC X(10).
               05 PerCol         PIC X(10).
               05 PerCity        PIC X(10).
               05 PerCountry     PIC X(10).
           03 PerDateOfBirth.
               05 PerDOB       PIC 9(2).
               05 PerMOB       PIC 9(2).
               05 PerYOB       PIC 9(4).
       WORKING-STORAGE SECTION.
       77 WS-PerTitulo         PIC X(6).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT AgendaFile
           READ AgendaFile
               AT END SET EndOfAgendaFile TO TRUE
           END-READ.
           PERFORM UNTIL EndOfAgendaFile
               EVALUATE TRUE
                   WHEN Sr    MOVE "Sr."   TO WS-PerTitulo
                   WHEN Sra   MOVE "Sra."  TO WS-PerTitulo
                   WHEN Ing   MOVE "Ing."  TO WS-PerTitulo
                   WHEN Inga  MOVE "Inga." TO WS-PerTitulo
                   WHEN Dr    MOVE "Dr."   TO WS-PerTitulo
                   WHEN Dra   MOVE "Dra."  TO WS-PerTitulo
                   WHEN Lic   MOVE "Lic."  TO WS-PerTitulo
                   WHEN Licda MOVE "Licda." TO WS-PerTitulo
                   WHEN OTHER MOVE "---" TO WS-PerTitulo
               END-EVALUATE
               DISPLAY WS-PerTitulo SPACE PerForname SPACE PerSurname
                       " | " PerStreet " nro " PerNumber " | "
                       PerCity " - " PerCountry " | "
                       PerDOB "/" PerMOB "/" PerYOB
               READ AgendaFile
                   AT END SET EndOfAgendaFile TO TRUE
               END-READ
           END-PERFORM
           CLOSE AgendaFile
           STOP RUN.
       END PROGRAM CLASE-No-7-15.
