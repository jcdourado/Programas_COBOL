       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CODIGO1
                    FILE STATUS  IS ST-ERRO
                ALTERNATE RECORD KEY IS CHAVE2 = DENOMINACAO1 CODIGO1
                                                      WITH DUPLICATES.
      *---------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CARG.DAT".
       01 CADCARG.
                03 CODIGO1             PIC 9(04).
                03 DENOMINACAO1        PIC X(25).
                03 NIVELHIER           PIC 9(01).
                03 TIPSAL              PIC X(01).
                03 SALBAS              PIC 9(06)V99.
       WORKING-STORAGE SECTION.
       01 TABNIVEL.
                03 FILLER        PIC X(20) VALUE "DIRETORIA".
                03 FILLER        PIC X(20) VALUE "GERENCIA TATICA".
                03 FILLER      PIC X(20) VALUE "GERENCIA OPERACIONAL".
                03 FILLER        PIC X(20) VALUE "SUPERV. TATICA".
                03 FILLER       PIC X(20) VALUE "SUPERV. OPERACIONAL".
                03 FILLER        PIC X(20) VALUE "COORD. ADMI.".
                03 FILLER        PIC X(20) VALUE "COORD. INDUS.".            
       01 TBNIVEL REDEFINES TABNIVEL.
                03 VETNIVEL              PIC X(20) OCCURS 7 TIMES.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 A-SEX        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 TEXTOVET        PIC X(20) VALUE SPACES.
       77 TEXTOTIPSAL        PIC X(20) VALUE SPACES.
       SCREEN SECTION.
       01  TELAMANUCAR.
           05  LINE 02  COLUMN 01 
               VALUE  "                          MANUTENCAO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CARGOS".
           05  LINE 05  COLUMN 01 
               VALUE  "        CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  "        DENOMINACAO:".
           05  LINE 07  COLUMN 01 
               VALUE  "        NIVEL HIERARQUICO:".
           05  LINE 08  COLUMN 01 
               VALUE  "        TIPO SALARIO:".
           05  LINE 09  COLUMN 01 
               VALUE  "        SALARIO BASE:".
           05  TCODIGO1
               LINE 05  COLUMN 23  PIC 9(04)
               USING  CODIGO1
               HIGHLIGHT.
           05  TDENOMINACAO1
               LINE 06  COLUMN 23  PIC X(25)
               USING  DENOMINACAO1
               HIGHLIGHT.
           05  TNIVELHIER
               LINE 07  COLUMN 28  PIC 9(01)
               USING  NIVELHIER
               HIGHLIGHT.
           05  TTIPSAL
               LINE 08  COLUMN 23  PIC X(01)
               USING  TIPSAL
               HIGHLIGHT.
           05  TSALBAS
               LINE 09  COLUMN 23  PIC 999999,99
               USING  SALBAS
               HIGHLIGHT.        
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CARG
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CARG
                      CLOSE CARG
                      MOVE "*** ARQUIVO CARG SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CARG" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CODIGO1 SALBAS NIVELHIER
                MOVE SPACES TO DENOMINACAO1 TEXTOVET TIPSAL
                MOVE SPACES TO TEXTOTIPSAL
                DISPLAY TELAMANUCAR
                DISPLAY (07, 32) TEXTOVET.  
                DISPLAY (08, 32) TEXTOTIPSAL.
       INC-002.
                ACCEPT TCODIGO1.
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CARG
                   GO TO ROT-FIM.
                IF CODIGO1  = 0
                   MOVE "*** CODIGO1  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-CARG01.
                MOVE 0 TO W-SEL
                READ CARG
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELAMANUCAR
                      PERFORM VER-NIVEL
                      PERFORM VER-TIPO
                      MOVE "***CARGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CARG" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TDENOMINACAO1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF DENOMINACAO1  = SPACES
                   MOVE "*** DENOMINACAO1  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.                
       INC-004.
                ACCEPT TNIVELHIER
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.   
       VER-NIVEL.
                IF (NIVELHIER  < 1) OR (NIVELHIER > 7)
                   MOVE "*** NIVER HIERARQUICO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004
                ELSE
                   MOVE VETNIVEL(NIVELHIER) TO TEXTOVET
                   DISPLAY (07, 32) TEXTOVET.               
       INC-005.
                ACCEPT TTIPSAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       VER-TIPO.
                IF (TIPSAL = "H" OR TIPSAL = "h") AND NIVELHIER = 7
                  MOVE "HORISTA" TO TEXTOTIPSAL
                  DISPLAY(08, 32) TEXTOTIPSAL
                ELSE 
                IF (TIPSAL = "M" OR TIPSAL = "m") AND NIVELHIER = 7
                    MOVE "MENSALISTA" TO TEXTOTIPSAL
                    DISPLAY(08, 32) TEXTOTIPSAL
                  ELSE
                      IF TIPSAL = "D" OR TIPSAL = "d"
                        MOVE "DIARISTA" TO TEXTOTIPSAL
                        DISPLAY(08, 32) TEXTOTIPSAL
                      ELSE
                        MOVE "*** TIPO DE SALARIO INVALIDO **" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-005.                 
       INC-006.
                ACCEPT TSALBAS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.                  
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF W-OPCAO = "N" OR "n"
                   MOVE "***DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADCARG
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** DEPARTAMENTO JA EXISTE *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CARG"
                                                       TO MENS
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
                   GO TO INC-006.
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
                DELETE CARG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
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
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE CADCARG
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CARG"   TO MENS
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
                IF W-CONT < 300
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.