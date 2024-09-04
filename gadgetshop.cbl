      * Author: Martin Alegre
      * Date: 04/09/24
      * Purpose: GadgetShop.Com
      * Type: cbl
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASE-No-8-18.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT StockFile
           ASSIGN TO "F:\Documentos\Cobol\clase8\GadgetStock.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD StockFile.
       01 GadgetDetails.
           88 EndOfStockFile  VALUE HIGH-VALUE.
           02 GadgetID      PIC 9(6).
           02 GadgetName    PIC X(30).
           02 QtyInStock    PIC 9(4).
           02 Price         PIC 9(4)V99.
       WORKING-STORAGE SECTION.
       77 WS-Nombre         PIC X(30) VALUE "Nombre".
       77 WS-Preciototal    PIC X(22) VALUE "Precio Total" JUST RIGHT.
       77 WS-StockTotal     PIC X(30) VALUE "Stock Total:" JUST RIGHT.
       77 WS-GadgetTotal    PIC 9(10)V99.
       77 WS-TotalTotal     PIC 9(10)V99 VALUE ZERO.
       77 WS-NumFormat      PIC Z,ZZZ,ZZZ,ZZ9.99.
       77 WS-NumFinal       PIC X(16).
       77 WS-LINE           PIC X(55) VALUES ALL "-".
       PROCEDURE DIVISION.
       Main.
           OPEN INPUT StockFile
           READ StockFile
               AT END SET EndOfStockFile TO TRUE
           END-READ.
           DISPLAY WS-Nombre " | " WS-Preciototal
           DISPLAY WS-LINE
           PERFORM UNTIL EndOfStockFile
               MULTIPLY QtyInStock BY Price GIVING WS-GadgetTotal
               ADD WS-GadgetTotal  TO WS-TotalTotal
               MOVE WS-GadgetTotal TO WS-NumFormat
               MOVE WS-NumFormat   TO WS-NumFinal
                   INSPECT WS-NumFinal CONVERTING "." TO "*"
                   INSPECT WS-NumFinal CONVERTING "," TO "."
                   INSPECT WS-NumFinal CONVERTING "*" TO ","
               DISPLAY GadgetName " |     $ " WS-NumFinal
               READ StockFile
                   AT END SET EndOfStockFile TO TRUE
               END-READ
           END-PERFORM
           CLOSE StockFile
               DISPLAY WS-LINE
               MOVE WS-TotalTotal TO WS-NumFormat
               MOVE WS-NumFormat   TO WS-NumFinal
                   INSPECT WS-NumFinal CONVERTING "." TO "*"
                   INSPECT WS-NumFinal CONVERTING "," TO "."
                   INSPECT WS-NumFinal CONVERTING "*" TO ","
               DISPLAY WS-StockTotal " |     $ " WS-NumFinal
           STOP RUN.
       END PROGRAM CLASE-No-8-18.
