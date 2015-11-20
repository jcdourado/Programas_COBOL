       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DPTO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRODPTO
                    ALTERNATE RECORD KEY IS CHAVE2 = DENOMINACAO CODIGO
                                                      WITH DUPLICATES.
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
       FD DPTO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "DPTO.DAT".
       01 CADPTO.
                03 CODIGO              PIC 9(04).
                03 DENOMINACAO         PIC X(25).
                03 SUBORDINACAO        PIC 9(01).
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
       01 TABSUB.
                03 FILLER        PIC X(20) VALUE "PRESIDENCIA".
                03 FILLER        PIC X(20) VALUE "VICE PRESIDENCIA".
                03 FILLER        PIC X(20) VALUE "DIR. ADMINISTR.".
                03 FILLER        PIC X(20) VALUE "DIR. COMERCIAL.".
                03 FILLER        PIC X(20) VALUE "DIR. INDUSTRIAL".
                03 FILLER        PIC X(20) VALUE "DIR. REL. MERCADO".
       01 TBSUB REDEFINES TABSUB.
                03 VETSUB              PIC X(20) OCCURS 6 TIMES.     
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 A-SEX        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERROFUNC  PIC X(02) VALUE "00".
       77 ST-ERRODPTO  PIC X(02) VALUE "00".
       77 ST-ERROCARG PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
       77 TEXTOSUB        PIC X(20) VALUE SPACES.
       77 TEXTONIVEL        PIC X(20) VALUE SPACES.
       77 TEXTTIPO        PIC X(10) VALUE SPACES.
       77 TEXTSEXO        PIC X(09) VALUE SPACES.
       77 TEXTOPC        PIC X(09) VALUE SPACES.
       77 VERIFI         PIC X(02) VALUE "00".
      *
       SCREEN SECTION.
       01  TELAFUNC.
           05  LINE 02  COLUMN 01 
               VALUE  "                        MANUTENCAO DE FU".
           05  LINE 02  COLUMN 41 
               VALUE  "NCIONARIOS".
           05  LINE 05  COLUMN 01 
               VALUE  "      CHAPA:".
           05  LINE 06  COLUMN 01 
               VALUE  "      NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "      DATA NASCIMENTO:".
           05  LINE 08  COLUMN 01 
               VALUE  "      SEXO:".
           05  LINE 08  COLUMN 41 
               VALUE  "          OPC SEXUAL:".
           05  LINE 09  COLUMN 01 
               VALUE  "      COD DEPARTAMENTO:".
           05  LINE 10  COLUMN 01 
               VALUE  "      COD CARGO:".
           05  LINE 12  COLUMN 01 
               VALUE  "              TIPO SALARIO:".
           05  LINE 12  COLUMN 41 
               VALUE  "        SUBORDINACAO:".
           05  LINE 13  COLUMN 01 
               VALUE  "              SALARIO BASE:".
           05  LINE 13  COLUMN 41 
               VALUE  "   NIVEL HIERARQUICO:".
           05  LINE 15  COLUMN 01 
               VALUE  "      DATA ADMISSAO:".
           05  LINE 16  COLUMN 01 
               VALUE  "      DATA DEMISSAO:".
           05  LINE 17  COLUMN 01 
               VALUE  "      STATUS:".
           05  TCHAPA
               LINE 05  COLUMN 25  PIC 9(06)
               USING  CHAPA
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 25  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TDTNASC
               LINE 07  COLUMN 25  PIC 99/99/9999
               USING  DTNASC
               HIGHLIGHT.
           05  TSEXO
               LINE 08  COLUMN 25  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TOPCSEX
               LINE 08  COLUMN 63  PIC X(01)
               USING  OPCSEX
               HIGHLIGHT.
           05  TCODPTO
               LINE 09  COLUMN 25  PIC 9(04)
               USING  CODPTO
               HIGHLIGHT.
           05  TCODCARG
               LINE 10  COLUMN 25  PIC 9(04)
               USING  CODCARG
               HIGHLIGHT.
           05  TTIPSAL
               LINE 12  COLUMN 29  PIC X(01)
               USING  TIPSAL
               HIGHLIGHT.
           05  TSUBORDINACAO
               LINE 12  COLUMN 63  PIC 9(01)
               USING  SUBORDINACAO
               HIGHLIGHT.
           05  TSALBASE
               LINE 13  COLUMN 29  PIC 999999,99
               USING  SALBAS
               HIGHLIGHT.
           05  TNIVELHIER
               LINE 13  COLUMN 63  PIC 9(01)
               USING  NIVELHIER
               HIGHLIGHT.
           05  TDTADM
               LINE 15  COLUMN 25  PIC 99/99/9999
               USING  DTADM
               HIGHLIGHT.
           05  TDTDEM
               LINE 16  COLUMN 25  PIC 99/99/9999
               USING  DTDEM
               HIGHLIGHT.
           05  TSTAT
               LINE 17  COLUMN 25  PIC X(01)
               USING  STAT
               HIGHLIGHT.
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O FUNC
           IF ST-ERROFUNC NOT = "00"
               IF ST-ERROFUNC = "30"
                      OPEN OUTPUT FUNC
                      CLOSE FUNC
                      MOVE "*** ARQUIVO FUNC SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE ST-ERROFUNC TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO DTNASC DTADM DTDEM SUBORDINACAO.
                MOVE SPACES TO TEXTOSUB TEXTONIVEL TEXTTIPO TEXTOPC. 
                MOVE SPACES TO NOME SEXO OPCSEX STAT TIPSAL TEXTSEXO.
                MOVE ZEROS TO CHAPA CODCARG CODPTO SALBAS NIVELHIER.
                MOVE ZEROS TO CODIGO CODIGO1.  
                DISPLAY TELAFUNC.
                DISPLAY (12, 31) TEXTTIPO.  
                DISPLAY (12, 65) TEXTOSUB.
                DISPLAY (13, 65) TEXTONIVEL.
       INC-002.
                ACCEPT TCHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE FUNC
                   GO TO ROT-FIM.
                IF CHAPA  = 0
                   MOVE "*** CHAPA  INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-FUNC01.
                MOVE 0 TO W-SEL
                READ FUNC
                IF ST-ERROFUNC NOT = "23"
                   IF ST-ERROFUNC = "00"
                      PERFORM LER-DPTO
                      PERFORM LER-CARG
                      PERFORM MOSTRA
                      PERFORM MOSTRA2
                      PERFORM VER-SEXO
                      PERFORM VER-OPC
                      DISPLAY TELAFUNC
                      MOVE "***FUNCIONARIO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO FUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME  = SPACES
                   MOVE "*** NOME  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.                
       INC-004.
                ACCEPT TDTNASC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF DIA  = 0 OR MES  > 12 OR ANO  < 1900 OR MES = 0
                OR DIA > 31 OR ANO > 2015 OR (MES > 11 AND ANO = 2015)
                   MOVE "*** DATA NASC  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                ACCEPT TSEXO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       VER-SEXO.
                IF SEXO = "F" OR SEXO = "f"
                    MOVE "FEMININO" TO TEXTSEXO
                    DISPLAY (08, 27) TEXTSEXO
                ELSE
                    IF SEXO = "M" OR SEXO = "m"
                       MOVE "MASCULINO" TO TEXTSEXO
                       DISPLAY (08, 27) TEXTSEXO
                    ELSE
                     MOVE "*** SEXO  INVALIDO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-005.
       INC-006.
                ACCEPT TOPCSEX
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       VER-OPC.
                IF OPCSEX = "F" OR OPCSEX = "f"
                    MOVE "FEMININO" TO TEXTOPC
                    DISPLAY (08, 65) TEXTOPC
                ELSE
                    IF OPCSEX = "M" OR OPCSEX = "m"
                       MOVE "MASCULINO" TO TEXTOPC
                       DISPLAY (08, 65) TEXTOPC
                    ELSE
                     MOVE "*** OPÇÃO  INVALIDA ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-006.
       INC-007.
                ACCEPT TCODPTO.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       LER-DPTO.
                MOVE CODPTO TO CODIGO.
                OPEN INPUT DPTO.
                READ DPTO.
                IF ST-ERRODPTO = "23"
                   MOVE "***NÃO EXISTE TAL DEPARTAMENTO **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.
       MOSTRA.
                DISPLAY TELAFUNC.
                MOVE VETSUB(SUBORDINACAO) TO TEXTOSUB.
                IF TIPSAL = "H" OR TIPSAL = "h"
                  MOVE "HORISTA" TO TEXTTIPO
                ELSE 
                   IF TIPSAL = "M" OR TIPSAL = "m"
                    MOVE "MENSALISTA" TO TEXTTIPO
                   ELSE
                      IF TIPSAL = "D" OR TIPSAL = "d"
                        MOVE "DIARISTA" TO TEXTTIPO.
                DISPLAY (12, 31) TEXTTIPO.  
                DISPLAY (12, 65) TEXTOSUB.  
       INC-008.
                ACCEPT TCODCARG.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.  
       LER-CARG.
                MOVE CODCARG TO CODIGO1.
                OPEN INPUT CARG.
                READ CARG.
                IF ST-ERROCARG = "23"
                   MOVE "***NÃO EXISTE TAL CARGO **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-008.
       MOSTRA2.
                DISPLAY TELAFUNC.
                MOVE VETNIVEL(NIVELHIER) TO TEXTONIVEL
                DISPLAY (13, 65) TEXTONIVEL.
       INC-009.
                ACCEPT TDTADM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF DIAADM  = 0 OR MESADM  > 12 OR ANOADM  < 1900
                OR MESADM = 0 OR DIAADM > 31 OR ANOADM > 2015
                   MOVE "*** DATA ADMISSAO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009. 
       INC-010.
                ACCEPT TDTDEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF DIADEM  = 0 OR MESDEM  > 12 OR ANODEM  < 1900
                OR MESDEM = 0 OR DIADEM > 31
                   MOVE "*** DATA DEMISSAO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-010. 
                IF ANODEM NOT > ANOADM
                   IF ANODEM = ANOADM
                      IF MESDEM NOT > MESADM
                          IF MESDEM = MESADM
                              IF DIADEM NOT > DIAADM
                       MOVE "*** DATA DEMISSAO INVALIDA ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO INC-010
                              ELSE 

                          ELSE
                       MOVE "*** DATA DEMISSAO INVALIDA ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO INC-010
                     ELSE

                   ELSE
                       MOVE "*** DATA DEMISSAO INVALIDA ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO INC-010.
       INC-011.
                ACCEPT TSTAT
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
                IF VERIFI = 01 GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF W-OPCAO = "N" OR "n"
                   MOVE "***DADOS RECUSADOS PELO OPERADOR **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADFUN
                IF ST-ERROFUNC = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERROFUNC = "22"
                      MOVE "*** FUNCIONARIO JA EXISTE *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE ST-ERROFUNC TO MENS
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
                   MOVE 00 TO VERIFI                
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                MOVE 01 TO VERIFI
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
                DELETE FUNC RECORD
                IF ST-ERROFUNC = "00"
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
                MOVE 00 TO VERIFI.
                REWRITE CADFUN
                IF ST-ERROFUNC = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FUNC"   TO MENS
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
                IF W-CONT < 200000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           CLOSE DPTO CARG.
                EXIT.
       FIM-ROT-TEMPO.