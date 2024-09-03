      *
      * Author: Martin Alegre
      * Date: 02/09/24
      * Purpose: Archivos secuenciales
      * Type: cbl
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASE-No-7-01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO "employee.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EmployeeFile.
       01 EmployeeDetails.
           88 EndOfEmployeeFile  VALUE HIGH-VALUE.
           02 EmpSSN             PIC 9(9).
           02 EmpName.
               03 EmpSurname     PIC X(15).
               03 EmpForname     PIC X(10).
             02 EmpDateOfBirth.
                 03 EmpYOB       PIC 9(4).
                 03 EmpMOB       PIC 9(2).
                 03 EmpDOB       PIC 9(2).
             02 EmpGender        PIC X.
       PROCEDURE DIVISION.
       MAIN.
      *    Abrir el fichero para INPUT
           OPEN INPUT EmployeeFile
      *    Lectura del buffer
           READ EmployeeFile
      *    Si está vacío no hace nada
               AT END SET EndOfEmployeeFile TO TRUE
           END-READ.
           PERFORM UNTIL EndOfEmployeeFile
               DISPLAY EmpForname SPACE EmpSurname " - "
                       EmpMOB "/" EmpDOB "/" EmpYOB
               READ EmployeeFile
                   AT END SET EndOfEmployeeFile TO TRUE
               END-READ
           END-PERFORM
           CLOSE EmployeeFile
           STOP RUN.
       END PROGRAM CLASE-No-7-01.
