       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINTDEP.
      **************************************
      * RELATORIO DO CADASTRO DE DEPTOS    *
      **************************************
      *------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADDEPTO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS SEQUENTIAL
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO.

           SELECT ARQIMP ASSIGN TO DISK
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS  IS ST-ERRO.



      *-----------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADDEPTO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADDEPTO.DAT".
               01 REGDEPTO.
                03 CODIGO      PIC 9(04).
                03 DENOMINA    PIC X(20).

       FD ARQIMP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEPTO.TXT".
       01 REGIMP PIC X(80).
      *
      *------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(08) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 CONLIN       PIC 9(03) VALUE ZEROS.
       77 CONPAG     PIC 9(03) VALUE ZEROS.
       01 CABEC.
                03 FILLER PIC X(08) VALUE SPACES.
                03 FILLER PIC X(05) VALUE "DEPTO".
                03 FILLER PIC X(28) VALUE SPACES.
                03 FILLER PIC X(11) VALUE "DENOMINACAO".

       01 DETAL.
                03 FILLER PIC X(05) VALUE SPACES.
                03 IMPCOD PIC X(04).
                03 FILLER PIC X(10) VALUE SPACES.
                03 IMPDEN PIC X(40).
                03 FILLER PIC X(16) VALUE SPACES.
      *-------------------------------------------------------
       SCREEN SECTION.
             01  CADDEPT.
           05  BLANK SCREEN.

      *-------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN INPUT CADDEPTO.
           IF ST-ERRO NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. CADCEP" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-FIM.
           OPEN OUTPUT ARQIMP.

       ROT-CABEC.
                ADD 1 TO CONPAG
                WRITE REGIMP FROM CABEC.
                MOVE SPACES TO REGIMP.
                WRITE REGIMP.

       LER-DEPTO.

                READ CADDEPTO NEXT
                IF ST-ERRO = "10"
                     GO TO ROT-FIM.
                MOVE CODIGO TO IMPCOD.
                MOVE DENOMINA TO IMPDEN.
                WRITE REGIMP FROM DETAL.
                ADD 1 TO CONLIN
                IF CONLIN < 10
                   GO TO LER-DEPTO
                ELSE
                   MOVE ZEROS TO CONLIN
                   MOVE SPACES TO REGIMP
                   WRITE REGIMP
                   WRITE REGIMP
                   WRITE REGIMP
                   WRITE REGIMP
                   GO TO ROT-CABEC.


      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                CLOSE CADDEPTO.
                CLOSE ARQIMP.
      *          DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 1000000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.