       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
      *
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
           SELECT CARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CODIGO1
                    FILE STATUS  IS ST-ERROCARG
                ALTERNATE RECORD KEY IS CHAVE2 = DENOMINACAO1 CODIGO1
                                                      WITH DUPLICATES.
           SELECT FUNC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CHAPA
                    FILE STATUS  IS ST-ERROFUNC
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME CHAPA
                                                      WITH DUPLICATES.
      *---------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD FOLHA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "FOLHA.DAT".
       01 CADFOLHA.
                03 CHAVE.
                    05 CHAPAFUNC       PIC 9(06).
                    05 PERIODO.
                        07 ANO         PIC 9(04).
                        07 MES         PIC 9(02). 
                03 SALBRUTO            PIC 9(06)V99. 
                03 INSS                PIC 9(06)V99. 
                03 IMPRENDA            PIC 9(06)V99.
       FD CARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CARG.DAT".
       01 CADCARG.
                03 CODIGO1             PIC 9(04).
                03 DENOMINACAO1        PIC X(25).
                03 NIVELHIER           PIC 9(01).
                03 TIPSAL              PIC X(01).
                03 SALBAS              PIC 9(06)V99.
       FD FUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "FUNC.DAT".
       01 CADFUN.
                03 CHAPA               PIC 9(06).
                03 NOME                PIC X(35).
                03 DTNASC              PIC 9(08).
                03 DT-NASC REDEFINES DTNASC.
                    05 DIA                 PIC 9(02). 
                    05 MES                 PIC 9(02). 
                    05 ANO                 PIC 9(04). 
                03 SEXO                PIC X(01).
                03 OPCSEX              PIC X(01).
                03 CODPTO              PIC 9(04).
                03 CODCARG             PIC 9(04).
                03 DTADM               PIC 9(08).
                03 DT-ADM REDEFINES DTADM.
                    05 DIAADM              PIC 9(02). 
                    05 MESADM              PIC 9(02). 
                    05 ANOADM              PIC 9(04). 
                03 DTDEM               PIC 9(08).
                03 DT-DEM REDEFINES DTDEM.
                    05 DIADEM              PIC 9(02). 
                    05 MESDEM              PIC 9(02). 
                    05 ANODEM              PIC 9(04).
                03 STAT              PIC X(01).
       WORKING-STORAGE SECTION.   
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 A-SEX        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERROFUNC  PIC X(02) VALUE "00".
       77 ST-ERROFOLHA  PIC X(02) VALUE "00".
       77 ST-ERROCARG PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
       SCREEN SECTION.
       
       01  TELACALC.
           05  LINE 04  COLUMN 01 
               VALUE  "                         CALCULO DA FOLH".
           05  LINE 04  COLUMN 41 
               VALUE  "A".
           05  LINE 06  COLUMN 01 
               VALUE  "                     PERIODO:".
           05  LINE 06  COLUMN 34 
               VALUE  "/".
           05  LINE 07  COLUMN 01 
               VALUE  "                     CHAPA:".
           05  LINE 08  COLUMN 01 
               VALUE  "                     SAL BRUTO:".
           05  LINE 09  COLUMN 01 
               VALUE  "                     INSS:".
           05  LINE 10  COLUMN 01 
               VALUE  "                     IMP RENDA:".
           05  TPERIODO
               LINE 06  COLUMN 32  PIC 99/9999
               USING  PERIODO
               HIGHLIGHT.
           05  TCHAPA
               LINE 07  COLUMN 32  PIC 9(06)
               USING  CHAPA
               HIGHLIGHT.
           05  TSALBRUTO
               LINE 08  COLUMN 32  PIC 999999,99
               USING  SALBRUTO
               HIGHLIGHT.
           05  TINSS
               LINE 09  COLUMN 32  PIC 999999,99
               USING  INSS
               HIGHLIGHT.
           05  TIMPRENDA
               LINE 10  COLUMN 32  PIC 999999,99
               USING  IMPRENDA
               HIGHLIGHT.
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O FOLHA
           IF ST-ERROFOLHA NOT = "00"
               IF ST-ERROFOLHA = "30"
                      OPEN OUTPUT FOLHA
                      CLOSE FOLHA
                      MOVE "*** ARQUIVO FOLHA SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE ST-ERROFOLHA TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO DTNASC DTADM DTDEM CHAPAFUNC.
                MOVE SPACES TO NOME SEXO OPCSEX STAT TIPSAL DENOMINACAO1.
                MOVE ZEROS TO CHAPA CODCARG SALBAS NIVELHIER.
                MOVE ZEROS TO CODIGO1 INSS SALBRUTO IMPRENDA PERIODO.  
                DISPLAY TELACALC.
       INC-002.
                ACCEPT TPERIODO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE FOLHA
                   GO TO ROT-FIM.
                IF PERIODO  = 0
                   MOVE "*** PERIODO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       INC-003.
                ACCEPT TCHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   GO TO INC-002.
                IF CHAPA  = 0
                   MOVE "*** CHAPA  INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                MOVE CHAPA TO CHAPAFUNC.
       LER-FUNC.
               OPEN INPUT FUNC.
               READ FUNC.
               IF ST-ERROFUNC = "23"
                   MOVE "*** CHAPA  INEXSTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       LER-CARG.
               MOVE CODCARG TO CODIGO1.
               OPEN INPUT CARG.
               READ CARG.
               IF ST-ERROCARG = "23"
                   MOVE "CARGO INEXISTE, ATUALIZAR POR FAVOR" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       LER-FOLHA01.
                MOVE 0 TO W-SEL
                READ FOLHA
                IF ST-ERROFOLHA NOT = "23"
                   IF ST-ERROFOLHA = "00"
                      PERFORM CALCULOS
                      MOVE "***FOLHA JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO FOLHA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      * PEGAR O SALARIO BASE A PARTIR DO FUNCIONARIO 
       CALCULOS.
               COMPUTE SALBRUTO = SALBAS * 1.
               COMPUTE INSS = SALBRUTO * ( 8 / 100 ).
               COMPUTE IMPRENDA = ( SALBRUTO - INSS) * ( 5 / 100 ).
               DISPLAY TELACALC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF W-OPCAO = "N" OR "n"
                   MOVE "***DADOS RECUSADOS PELO OPERADOR **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADFOLHA
                IF ST-ERROFOLHA = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERROFOLHA = "22"
                      MOVE "*** FOLHA JA EXISTE *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE ST-ERROFOLHA TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE FOLHA RECORD
                IF ST-ERROFOLHA = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***  " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE CADFOLHA
                IF ST-ERROFOLHA = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FOLHA"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
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
                IF W-CONT < 200
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                CLOSE CARG FUNC.
                EXIT.
       FIM-ROT-TEMPO.