       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRALU.
*******
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ALUNO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
		    ACCESS MODE IS DYNAMIC
	 	    RECORD KEY IS RA
		    FILE STATUS IS ST-ERRO
		    ALTERNATE RECORD KEY IS CHAVE2 = NOME RA
                                             WITH DUPLICATES.
*******
       DATA DIVISION.
       FILE SECTION.
       FD ALUNO
               LABEL RECORD IS STANDARD
	       VALUE OF FILE-ID IS "ALUNO.DAT".
       01 REGALU.
                03 RA PIC 9(06).
		03 NOME PIC X(35).
		03 NOTA1 PIC 9(02)V99.
		03 NOTA2 PIC 9(02)V99.
		03 FALTA1 PIC 9(02).
		03 FALTA2 PIC 9(02).
*******
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 MEDIA        PIC 9(02) VALUE ZEROS.
       77 MASK         PIC Z9,9.
       77 MASK1        PIC Z9,9.
       77 MASK2        PIC Z9.
       77 MASK3        PIC Z9.
       77 TFALTAS      PIC 9(02) VALUE ZEROS.
*******
       SCREEN SECTION.
       01  TELACADALUNO.
           05  LINE 03  COLUMN 01 
               VALUE  "                      CADASTRO DE ALUNOS".
           05  LINE 05  COLUMN 01 
               VALUE  "      RA:".
           05  LINE 06  COLUMN 01 
               VALUE  "      NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "      NOTA1:".
           05  LINE 08  COLUMN 01 
               VALUE  "      NOTA2:".
           05  LINE 09  COLUMN 01 
               VALUE  "      FALTA1:".
           05  LINE 10  COLUMN 01 
               VALUE  "      FALTA2:".
           05  TRA
               LINE 05  COLUMN 11  PIC 9(06)
               USING  RA
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 14  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TNOTA1
               LINE 07  COLUMN 14  PIC 99,99
               USING  NOTA1
               HIGHLIGHT.
           05  TNOTA2
               LINE 08  COLUMN 14  PIC 99,99
               USING  NOTA2
               HIGHLIGHT.
           05  TFALTA1
               LINE 09  COLUMN 15  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TFALTA2
               LINE 10  COLUMN 15  PIC 9(02)
               USING  FALTA2.
       PROCEDURE DIVISION.
       INICIO.          
       INC-OP0.
           OPEN I-O ALUNO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ALUNO
                      CLOSE ALUNO
                      MOVE "*** ARQUIVO ALUNO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO ALUNO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO RA FALTA1 FALTA2 NOTA1 NOTA2
                MOVE SPACES TO NOME.
                DISPLAY TELACADALUNO.
       INC-002.
                ACCEPT TRA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE ALUNO
                   GO TO ROT-FIM.
                IF RA = 0
                   MOVE "*** ALUNO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-AMIGOS01.
                MOVE 0 TO W-SEL
                READ ALUNO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELACADALUNO
                      PERFORM CALC-MEDIA
                      MOVE "*** ALUNO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TNOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TNOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TFALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TFALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       CALC-MEDIA.
		COMPUTE MEDIA = (NOTA1 + NOTA2)/2.
                MOVE MEDIA TO MASK.
		DISPLAY (05 , 20) "MEDIA: " MASK.
                COMPUTE TFALTAS = FALTA1 + FALTA2.
                MOVE TFALTAS TO MASK2.
                DISPLAY (06, 20) "TOTAL DE FALTAS:" MASK2.
                IF MEDIA > 4,9 
                   IF TFALTAS < 11
                      DISPLAY (07, 20) "APROVADO"
                   ELSE 
                      DISPLAY (07, 20) "RECUPERACAO POR FALTA"
                ELSE
                   IF TFALTAS < 11 
                      DISPLAY (07, 20) "REPROVADO"
                   ELSE 
                      DISPLAY (07, 20) "RECUPERACAO POR NOTA".
      *
       FIM-CALC-MEDIA.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGALU
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** ALUNO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 10)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 10) MENS
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
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ALUNO RECORD
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
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGALU
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO ALUNO"   TO MENS
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
                IF W-CONT < 30000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.