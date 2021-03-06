       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADFUN.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FUNCI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC                
                    RECORD KEY   IS CHAPA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME CHAPA
                                                      WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD FUNCI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "FUNCI.DAT".
       01 REGFUN.
                03 CHAPA               PIC 9(06).
                03 NOME                PIC X(35).
               	03 DIA                 PIC 9(02). 
                03 MES                 PIC 9(02). 
                03 ANO                 PIC 9(04). 
                03 SEXO                PIC X(01).
                03 ESTCIVIL            PIC 9(01).
                03 TIPOSAL             PIC X(01).
                03 SALBASE             PIC 9(06)V99.
                03 FILLER              PIC X(04).
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
       77 SALBRUTO     PIC 9(06)V99 VALUE ZEROS.
       77 INSS         PIC 9(06)V99 VALUE ZEROS.
       77 IMPRENDA     PIC 9(06)V99 VALUE ZEROS.
       77 SALLIQ       PIC 9(06)V99 VALUE ZEROS.
       77 X            PIC 9(03) VALUE ZEROS.
       77 Y            PIC 9(02) VALUE ZEROS.
       77 Z            PIC 9(02) VALUE ZEROS.
       77 REDUCAO      PIC 9(03) VALUE ZEROS.
      *
       77 MASK-SALBASE PIC ZZZ.ZZ9,99.
       77 MASK-SALBRUTO PIC ZZZ.ZZ9,99.
       77 MASK-INSS PIC ZZZ.ZZ9,99.
       77 MASK-IMPRENDA PIC ZZZ.ZZ9,99.
       77 MASK-SALLIQ PIC ZZZ.ZZ9,99.
       SCREEN SECTION.
       01  TELACADFUN.
           05  BLANK SCREEN.
           05  FOREGROUND-COLOR  3  BACKGROUND-COLOR 0.
           05  LINE 02  COLUMN 01 
               VALUE  "                           Cadastro de F".
           05  LINE 02  COLUMN 41 
               VALUE  "uncionarios".
           05  LINE 03  COLUMN 01 
               VALUE  "                                        ".
           05  LINE 03  COLUMN 41 
               VALUE  "             ".
           05  LINE 04  COLUMN 01 
               VALUE  "     CHAPA:".
           05  LINE 05  COLUMN 01 
               VALUE  "     NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  "     DATA DE NASCIMENTO:".
           05  LINE 07  COLUMN 01 
               VALUE  "     SEXO:".
           05  LINE 08  COLUMN 01 
               VALUE  "     ESTADO CIVIL:".
           05  LINE 09  COLUMN 01 
               VALUE  "     TIPO SALARIO:".
           05  LINE 10  COLUMN 01 
               VALUE  "     SALARIO BASE:".
           05  LINE 13  COLUMN 01 
               VALUE  "     SALARIO BRUT0:".
           05  LINE 14  COLUMN 01 
               VALUE  "     INSS:".
           05  LINE 15  COLUMN 01 
               VALUE  "     IMPOSTO DE RENDA:".
           05  LINE 16  COLUMN 01 
               VALUE  "     SALARIO LIQUIDO:".
      *
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O FUNCI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT FUNCI
                      CLOSE FUNCI
                      MOVE "*** ARQUIVO FUNCI SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO FUNCI" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CHAPA ESTCIVIL SALBASE DIA MES ANO
      *PODE DAR ERRO NO MOVE ESPACES
                MOVE SPACES TO NOME SEXO TIPOSAL.
                DISPLAY TELACADFUN.
       INC-002.
                ACCEPT  (04, 27) CHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE FUNCI
                   GO TO ROT-FIM.
                IF CHAPA  = 0
                   MOVE "*** FUNCIONARIO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-FUNCI01.
                MOVE 0 TO W-SEL
                READ FUNCI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 27) NOME
                      DISPLAY (06, 27) DIA "/" MES "/" ANO
                      PERFORM VER-SEX
                      DISPLAY (07, 27) SEXO
                      PERFORM VER-EST
                      DISPLAY (08, 27) ESTCIVIL
                      PERFORM VER-TIPO
                      DISPLAY (09, 27) TIPOSAL
                      MOVE SALBASE TO MASK-SALBASE
                      DISPLAY (10, 27) MASK-SALBASE
                      PERFORM CALCULOS
                      MOVE "***FUNCIONARIO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO FUNCI" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 27) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME  = SPACES
                   MOVE "*** NOME  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.                
       INC-004.
                ACCEPT (06, 27) DIA WITH AUTO-SKIP
                DISPLAY (06, 29) "/"
                ACCEPT (06, 30) MES WITH AUTO-SKIP
                DISPLAY (06, 32) "/"
                ACCEPT (06, 33) ANO WITH AUTO-SKIP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.        
                IF DIA  = 0 OR MES  > 12 OR ANO  < 1900 OR MES = 0
                OR DIA > 31 OR ANO > 2015
                   MOVE "*** DATA NASC  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.                      
       INC-005.
                ACCEPT (07, 27) SEXO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       VER-SEX.
                IF SEXO  = SPACES OR SEXO = 0
                	MOVE "*** SEXO  INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001
                ELSE
                	IF SEXO  = "M" OR SEXO  = "m"
                		DISPLAY (07, 29) "MASCULINO"
                	ELSE
                		IF SEXO  = "F" OR SEXO  = "f"
                   		DISPLAY (07, 29) "FEMININO"
                    	ELSE
                    	MOVE "*** SEXO  INVALIDO ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001.                
       INC-006.
                ACCEPT (08, 27) ESTCIVIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       VER-EST.
                IF ESTCIVIL = 1 OR ESTCIVIL = 2 OR ESTCIVIL = 3
                OR ESTCIVIL = 4 OR ESTCIVIL = 5
                	IF ESTCIVIL = 1 DISPLAY (08, 29) "SOLTEIRO"
                	ELSE
                		IF ESTCIVIL = 2 DISPLAY (08, 29) "CASADO"
                		ELSE
                	   	IF ESTCIVIL = 3 DISPLAY (08, 29) "VIUVO"
                	   	ELSE
                		IF ESTCIVIL = 4 DISPLAY (08, 29) "DESQUITADO"
                		ELSE
                	 	IF ESTCIVIL = 5 DISPLAY (08, 29) "OUTROS"
                	 	ELSE
                ELSE
                	MOVE " ESTADO CIVIL  INVALIDO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
       INC-007.
                ACCEPT (09, 27) TIPOSAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       VER-TIPO.
                IF TIPOSAL = "H" OR TIPOSAL = "h" OR TIPOSAL = "D" OR 
                TIPOSAL = "d" OR TIPOSAL = "m" OR TIPOSAL = "M" 
                	IF TIPOSAL = "H" OR TIPOSAL = "h"
                	    DISPLAY (09, 29) "HORISTA"
                	    COMPUTE X = 0 + 220
                	ELSE
                	IF TIPOSAL = "D" OR TIPOSAL = "d"
                	    DISPLAY (09, 29) "DIARISTA"
                	    COMPUTE X = 0 + 30
                    ELSE
                	IF TIPOSAL = "M" OR TIPOSAL = "m"
                	    DISPLAY (09, 29) "MENSALISTA"
                	    COMPUTE X = 0 + 1
                	ELSE
                ELSE
                   MOVE "*** TIPO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.                
       INC-008.
                ACCEPT (10, 27) SALBASE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
       CALCULOS.
                COMPUTE SALBRUTO = SALBASE * X.
                IF SALBRUTO < 2400 
                	COMPUTE Z = 0 + 0
                        COMPUTE REDUCAO = 0 + 0
                ELSE
                   IF SALBRUTO < 5340
                	COMPUTE Z = 0 + 5
                        COMPUTE REDUCAO = 0 + 240
                   ELSE 
                	COMPUTE Z = 0 + 10
                        COMPUTE REDUCAO = 0 + 534.
                IF SALBRUTO < 1801
                        COMPUTE Y = 0 + 8
                ELSE
                   IF SALBRUTO < 3208
                        COMPUTE Y = 0 + 9
                   ELSE
                        COMPUTE Y = 0 + 10.
                COMPUTE INSS = (SALBRUTO * Y) / 100.
                COMPUTE IMPRENDA = (((SALBRUTO - INSS) * z ) / 100).
                COMPUTE IMPRENDA = IMPRENDA - REDUCAO.
                COMPUTE SALLIQ = SALBRUTO - INSS.
                COMPUTE SALLIQ = SALLIQ - IMPRENDA.
                MOVE SALBRUTO TO MASK-SALBRUTO.
                DISPLAY (13, 27) MASK-SALBRUTO.
                MOVE INSS TO MASK-INSS.
                DISPLAY (14, 27) MASK-INSS.
                MOVE IMPRENDA TO MASK-IMPRENDA.
                DISPLAY (15, 27) MASK-IMPRENDA.
                MOVE SALLIQ TO MASK-SALLIQ.
                DISPLAY (16, 27) MASK-SALLIQ.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF W-OPCAO = "N" OR "n"
                   MOVE "***DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFUN
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** FUNCIONARIO JA EXISTE *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE FUNCI"
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
                DELETE FUNCI RECORD
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
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFUN
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FUNCI"   TO MENS
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
                IF W-CONT < 500
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.