       IDENTIFICATION DIVISION.
       PROGRAM-ID. Parcial2.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT stock
         ASSIGN TO "stock.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD stock.
       01 archArticulos.
           88 EndOfArchFile    VALUE HIGH-VALUE.
           03 ArtCodigo         PIC 9(4).
           03 ArtDescrip        PIC X(30).
           03 ArtCantidad       PIC 9(6).
           03 ArtPrecio         PIC 9(3)V99.
       WORKING-STORAGE SECTION.
       01 tarea PIC X.
           88 registro          VALUE "1".
           88 consulta          VALUE "2".
           88 salir             VALUE "3".
       01 WS-articulos.
           03 WS-ArtCodigo      PIC 9(4).
           03 WS-ArtDescrip     PIC X(30).
           03 WS-ArtCantidad    PIC 9(6).
           03 WS-ArtPrecio      PIC 9(3)V99.
       01 continuar             PIC X.
       77 WS-precioTotal        PIC 9(9)V99.
       77 WS-totalGrl           PIC 9(10)V99 VALUE ZEROS.
       77 WS-linea              PIC 9(3) VALUE 10.
       01 Format-articulos.
           03 FT-ArtCantidad    PIC ZZZ.ZZ9.
           03 FT-ArtPrecio      PIC ZZ9,99.
           03 FT-precioTotal    PIC ZZ.ZZZ.ZZ9,99.
           03 FT-totalGrl       PIC Z.ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL "SYSTEM" USING "cls"
           DISPLAY "Ingrese el numero de la tarea a realizar:"
               LINE 2 
               POSITION 10
           DISPLAY "1 - Registrar Nuevo Articulo" 
               LINE 4 
               POSITION 10
           DISPLAY "2 - Consulta Valorizada de Mercaderia" 
               LINE 6 
               POSITION 10
           DISPLAY "3 - Salir del Sistema." 
           LINE 8 
           POSITION 10
           ACCEPT tarea 
               LINE 10 
               POSITION 10.
           EVALUATE TRUE
               WHEN registro PERFORM REGISTRO-PERFORM
               WHEN consulta PERFORM CONSULTA-PERFORM
               WHEN salir PERFORM SALIR-PERFORM
               WHEN OTHER
                   DISPLAY "Ingreso incorrecto"
                   LINE 10 
                   POSITION 10
                   DISPLAY "PRESIONE UNA TECLA PARA CONTINUAR"
                   LINE 12 
                   POSITION 10
                   ACCEPT continuar 
                   LINE 13 
                   POSITION 10
                   PERFORM MAIN-PROCEDURE
           END-EVALUATE.
       
       REGISTRO-PERFORM.
           CALL "SYSTEM" USING "cls"
           DISPLAY "Codigo de Articulo: " LINE 7  POSITION 10
           ACCEPT WS-ArtCodigo            LINE 7  POSITION 30 
           DISPLAY "Descripcion: "        LINE 9  POSITION 10
           ACCEPT WS-ArtDescrip           LINE 9  POSITION 30 
           DISPLAY "Cantidad: "           LINE 11 POSITION 10
           ACCEPT WS-ArtCantidad          LINE 11 POSITION 30 
           DISPLAY "Precio Unitario: $ "  LINE 13 POSITION 10
           ACCEPT WS-ArtPrecio            LINE 13 POSITION 30 
           OPEN EXTEND stock
               MOVE WS-articulos to archArticulos
               WRITE archArticulos
           CLOSE stock
           DISPLAY "INGRESO DE MERCADERIA EXITOSO"
           DISPLAY " "
           DISPLAY "PRESIONE UNA TECLA PARA CONTINUAR"
           ACCEPT continuar
           PERFORM MAIN-PROCEDURE.

       CONSULTA-PERFORM.
           PERFORM PANTALLA-PERFORM
           OPEN INPUT stock
               MOVE 10 TO WS-linea
               PERFORM UNTIL EndOfArchFile 
                   READ stock
                       AT END SET EndOfArchFile TO TRUE 
                       NOT AT END 
                           PERFORM DETAILS-PERFORM
                   END-READ 
               END-PERFORM 
           CLOSE stock
           ADD 1 TO WS-linea.
           DISPLAY "TOTAL GENERAL"     LINE WS-linea POSITION 47
           MOVE WS-totalGrl TO FT-totalGrl
           DISPLAY "$ "                LINE WS-linea POSITION 61
           DISPLAY FT-totalGrl         LINE WS-linea POSITION 63
           ADD 5 TO WS-linea.
           DISPLAY "PRESIONE UNA TECLA PARA CONTINUAR" 
               LINE WS-linea 
               POSITION 5
           ACCEPT continuar 
               LINE WS-linea 
               POSITION 40
           PERFORM MAIN-PROCEDURE.

       DETAILS-PERFORM.  
           DISPLAY ArtCodigo           LINE WS-linea POSITION 3
           DISPLAY ArtDescrip          LINE WS-linea POSITION 11
           MOVE ArtCantidad TO FT-ArtCantidad
           DISPLAY FT-ArtCantidad      LINE WS-linea POSITION 42
           MOVE ArtPrecio TO FT-ArtPrecio
           DISPLAY "$ "                LINE WS-linea POSITION 52
           DISPLAY FT-ArtPrecio        LINE WS-linea POSITION 54
           COMPUTE WS-precioTotal = ArtCantidad * ArtPrecio
           MOVE WS-precioTotal TO FT-precioTotal
           DISPLAY "$ "                LINE WS-linea POSITION 64
           DISPLAY FT-precioTotal      LINE WS-linea POSITION 66
           COMPUTE WS-totalGrl = WS-totalGrl + WS-precioTotal
           ADD 1 TO WS-linea.

       PANTALLA-PERFORM.
          CALL "SYSTEM" USING "cls"
          DISPLAY "CONSULTA VALORIZADA DE MERCADERIA"
               LINE 5 POSITION 20 
           DISPLAY "Cod.Art."         LINE 8 POSITION 2
           DISPLAY "Descripcion"      LINE 8 POSITION 11
           DISPLAY "Cantidad"         LINE 8 POSITION 42
           DISPLAY "Precio Un."       LINE 8 POSITION 52
           DISPLAY "Precio Total"     LINE 8 POSITION 64.

       SALIR-PERFORM.
           CALL "SYSTEM" USING "cls"
           STOP RUN.
           END PROGRAM Parcial2.
