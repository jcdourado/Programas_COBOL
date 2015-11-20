       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINTFOL.
      **************************************
      * RELATORIO DO CADASTRO DE FOLHA    *
      **************************************
      *------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FOLHA ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CHAVE
                    FILE STATUS  IS ST-ERROFOLHA.
           SELECT ARQIMP ASSIGN TO DISK
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS  IS ST-ERRO.
      *-----------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD FOLHA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "FOLHA.DAT".
       01 CADFOLHA.
                03 CHAVE.
                    05 CHAPAFUNC       PIC 9(06).
                    05 PERIODO         PIC 9(06).
                    05 PP REDEFINES PERIODO.
                        07 MES         PIC 9(02).
                        07 ANO         PIC 9(04). 
                03 SALBRUTO            PIC 9(06)V99. 
                03 INSS                PIC 9(06)V99. 
                03 IMPRENDA            PIC 9(06)V99.
       FD ARQIMP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOL.TXT".
       01 REGIMP PIC X(150).
      *
      *------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(08) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 ST-ERROFOLHA      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 CONLIN       PIC 9(03) VALUE ZEROS.
       77 CONPAG     PIC 9(03) VALUE ZEROS.
       01 CABEC.
                03 FILLER PIC X(05) VALUE "CHAPA".
                03 FILLER PIC X(10) VALUE SPACES.
                03 FILLER PIC X(11) VALUE "PERIODO".
                03 FILLER PIC X(05) VALUE SPACES.
                03 FILLER PIC X(11) VALUE "SALBRUTO".
                03 FILLER PIC X(05) VALUE SPACES.
                03 FILLER PIC X(11) VALUE "INSS".
                03 FILLER PIC X(05) VALUE SPACES.
                03 FILLER PIC X(11) VALUE "IMP. RENDA".
       01 DETAL.
                03 IMPCHAPA PIC 9(06).
                03 FILLER PIC X(06) VALUE SPACES.
                03 IMPPER PIC 9(06).
                03 FILLER PIC X(08) VALUE SPACES.
                03 IMPSAL PIC 9(06)V99.
                03 FILLER PIC X(08) VALUE SPACES.
                03 IMPINS PIC 9(06)V99.
                03 FILLER PIC X(08) VALUE SPACES.
                03 IMPIMP PIC 9(06)V99.
                03 FILLER PIC X(08) VALUE SPACES.
      *-------------------------------------------------------
       SCREEN SECTION.
             01  CADFOLHA.
             05  BLANK SCREEN.
      *-------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN INPUT FOLHA.
           IF ST-ERROFOLHA NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. FOLHA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-FIM.
           OPEN OUTPUT ARQIMP.
       ROT-CABEC.
                ADD 1 TO CONPAG
                WRITE REGIMP FROM CABEC.
                MOVE SPACES TO REGIMP.
                WRITE REGIMP.
       LER-DEPTO.
                READ FOLHA NEXT
                IF ST-ERROFOLHA = "10"
                     GO TO ROT-FIM.
                MOVE CHAPAFUNC TO IMPCHAPA.
                MOVE PERIODO TO IMPPER.
                MOVE SALBRUTO TO IMPSAL.
                MOVE INSS TO IMPINS.
                MOVE IMPRENDA TO IMPIMP.
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
                   WRITE REGIMP
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
                CLOSE FOLHA.
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
                IF W-CONT < 100
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.