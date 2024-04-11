      ******************************************************************
      * Author: JACKSON B MARQUES
      * Date: 20/03/2024
      * Purpose: CRIAR UM FORMULARIO DE CADASTRO COM VALIDA플O DE SENHA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAUSER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT USUARIO ASSIGN TO
                'D:\VIDA DE DEV\Desafios CAP\USER.DAT'
                ORGANISATION IS INDEXED
                ACCESS  MODE IS RANDOM
                RECORD KEY IS ID-USUARIO
                FILE STATUS IS WRK-FS.



       DATA DIVISION.
       FILE SECTION.

       FD USUARIO.
       01 REG-USUARIO.
         03 ID-USUARIO                 PIC 9(04).
         03 E-MAIL                     PIC X(100).
         03 NOME.
            05 FNAME                   PIC X(50).
            05 LNAME                   PIC X(50).
         03 PASSWORD                   PIC X(100).
         03 TELEFONE                   PIC X(100).

       WORKING-STORAGE SECTION.

       77 WRK-FS                         PIC 99.
         88 FS-OK                        VALUE 0.
       77 WRK-CONTA-ID                   PIC 9(4) VALUE ZEROS.
       77 WRK-GRAV                       PIC X.
          88 GRAV-OK                     VALUE 'S' FALSE 'N'.

       77 WRK-ID-USUARIO                 PIC 9(04) VALUE ZERO.
      * VARIAVEIS E-MAIL
       77 WRK-USER                       PIC X(86).
       77 WRK-DOMINIO                    PIC X(14).
       77 WRK-EMAIL                      PIC X(100).
       77 WRK-OPCAO                      PIC 9.
       77 WRK-USER-TAM                   PIC 99.
       77 WRK-EMAIL-OK                   PIC X.
          88 EMAIL-OK                    VALUE 'S' FALSE 'N'.
      * VARIAVEIS DE NOME
       01 WRK-NOME.
          03 WRK-FNAME                   PIC X(50).
          03 WRK-LNAME                   PIC X(50).
       77 WRK-FNAME-TAM                  PIC 99.
       77 WRK-LNAME-TAM                  PIC 99.
       77 WRK-FNAME-OK                   PIC X.
          88 FNAME-OK                    VALUE 'S' FALSE 'N'.
       77 WRK-LNAME-OK                   PIC X.
          88 LNAME-OK                    VALUE 'S' FALSE 'N'.
      *VARIAVEIS VERIFICACAO DE SENHA
       77 WRK-PASSWORD                   PIC X(100).
      *TAMANHO DA SENHA
       77 WRK-PASS-TAM                   PIC 99.
      *VERIFICA플O DE LETRAS MAIUSCULAS
       77 WRK-UPPER-PASS-OK                  PIC X VALUE 'N'.
       77 WRK-UPPER-CONT                     PIC 9(4) COMP VALUE 1.
      *VERIFICA플O DE LETRAS MINUSCULAS
       77 WRK-LOWER-PASS-OK                  PIC X VALUE 'N'.
       77 WRK-LOWER-CONT                     PIC 9(4) COMP VALUE 1.
      *VERIFICA플O CARACTERE ESPECIAL
       77 WRK-ESPECIAL-PASS-OK               PIC X VALUE 'N'.
       77 WRK-ESPECIAL-CONT                  PIC 9(4) COMP VALUE 1.
      *VERIFICA플O NUMERO
       77 WRK-NUMBER-PASS-OK                 PIC X VALUE 'N'.
       77 WRK-NUMBER-CONT                    PIC 9(4) COMP VALUE 1.
      *VERIFICA플O DE TELEFONE
       77 WRK-TELEFONE                      PIC X(100).
       77 WRK-TEL-TAM                       PIC 99 VALUE ZEROS.
       77 WRK-TEL-PASS-OK                   PIC X VALUE 'N'.
       77 WRK-TEL-CONT                      PIC 9(4) COMP VALUE 1.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY  '************************************************'
            DISPLAY  '*     *BEM VINDO AO CADASTRO DE USUARIOS*      *'
            DISPLAY  '************************************************'

            SET EMAIL-OK TO FALSE

            PERFORM P1-EMAIL    THRU P1-EMAIL-FIM UNTIL EMAIL-OK
            PERFORM P2-NOME     THRU P2-NOME-FIM
            PERFORM P3-SENHA    THRU P3-SENHA-FIM
            PERFORM P4-TELEFONE THRU P4-TELEFONE-FIM
            PERFORM P6-GRAVACAO THRU P6-GRAVACAO-FIM UNTIL GRAV-OK
            PERFORM P999-FINAL
       .

      *-------------------------------------------------------------------------
      *VALIDA플O E-MAIL
      *-------------------------------------------------------------------------
       P1-EMAIL.
            MOVE SPACES                        TO WRK-USER
            MOVE SPACES                        TO WRK-EMAIL
            MOVE SPACES                        TO WRK-DOMINIO
            MOVE ZEROS                         TO WRK-USER-TAM
            DISPLAY 'Selecione um dominio para o seu e-mail: '
            DISPLAY '1 - @CAPGEMINI.COM'
            DISPLAY '2 - @BRADESCO.COM'
            ACCEPT WRK-OPCAO

            EVALUATE WRK-OPCAO
              WHEN '1'
                 MOVE '@CAPGEMINI.COM'         TO WRK-DOMINIO
                 DISPLAY 'Sua opcao foi: ' WRK-DOMINIO
              WHEN '2'
                 MOVE '@BRADESCO.COM'          TO WRK-DOMINIO
                 DISPLAY 'Sua opcao foi: ' WRK-DOMINIO
              WHEN OTHER
                 DISPLAY 'Opcao invalida tente novamente!'
                 PERFORM P1-EMAIL
            END-EVALUATE
            .
       ESCOLHA-USER.
            DISPLAY 'Digite seu username: '
            DISPLAY '(parte do e-mail antes do @): '
            ACCEPT WRK-USER
            COMPUTE WRK-USER-TAM = FUNCTION LENGTH
                                 (FUNCTION TRIM(WRK-USER))
            IF WRK-USER-TAM < 1
               DISPLAY 'User muito curto, '
                       'precisa ter no minimo 1 digito'
               PERFORM ESCOLHA-USER
            ELSE
               MOVE 'S'                        TO WRK-EMAIL-OK
               DISPLAY 'Seu E-mail eh: '
               FUNCTION TRIM(WRK-USER) WRK-DOMINIO
            END-IF
            STRING FUNCTION TRIM(WRK-USER) WRK-DOMINIO
            INTO WRK-EMAIL
            END-STRING
            .
       P1-EMAIL-FIM.

      *-------------------------------------------------------------------------
      *VALIDA플O NOME
      *-------------------------------------------------------------------------
       P2-NOME.

            INITIALISE WRK-NOME
            .
       PRIMEIRO-NOME.
            DISPLAY 'Digite seu primeiro nome:'
            ACCEPT WRK-FNAME
            COMPUTE WRK-FNAME-TAM = FUNCTION LENGTH
                                 (FUNCTION TRIM(WRK-FNAME))
            IF WRK-FNAME-TAM < 1
              DISPLAY 'Seu nome deve conter pelo menos um digito: '
              PERFORM PRIMEIRO-NOME
            .
       SOBRENOME.
            DISPLAY 'Digite seu sobrenome:'
            ACCEPT WRK-LNAME
            COMPUTE WRK-LNAME-TAM = FUNCTION LENGTH
                                 (FUNCTION TRIM(WRK-LNAME))
            IF WRK-LNAME-TAM < 1
              DISPLAY 'Seu sobrenome deve conter pelo menos um digito: '
              PERFORM SOBRENOME

            .
       P2-NOME-FIM.
      *-------------------------------------------------------------------------
      * VALIDA플O DE SENHA
      *-------------------------------------------------------------------------
       P3-SENHA.
            INITIALISE WRK-PASSWORD
            DISPLAY 'Digite sua senha:'
            ACCEPT WRK-PASSWORD
      *-------------------------------------------------------------------------
      *VERFICA플O DE NUMERO DE DIGITOS

            COMPUTE WRK-PASS-TAM = FUNCTION LENGTH
                                 (FUNCTION TRIM(WRK-PASSWORD))
            IF WRK-PASS-TAM < 8
              DISPLAY 'Senha muito curta, sua senha '
                     'precisa ter no minimo 8 digitos, tente novamente!'
              PERFORM P3-SENHA
            END-IF
      *-------------------------------------------------------------------------
      *VERFICA플O DE LETRA MAIUSCULA
            MOVE 'N' TO WRK-UPPER-PASS-OK

            PERFORM VARYING WRK-UPPER-CONT FROM 1 BY 1
                         UNTIL WRK-UPPER-CONT > LENGTH OF WRK-PASSWORD
             IF WRK-PASSWORD(WRK-UPPER-CONT:1) >= 'A'
                               AND WRK-PASSWORD(WRK-UPPER-CONT:1) <= 'Z'
               MOVE 'S' TO WRK-UPPER-PASS-OK
             END-IF
            END-PERFORM

            IF WRK-UPPER-PASS-OK NOT EQUAL 'S'
              DISPLAY 'Sua senha deve ter pelo menos '
               'uma letra maiuscula, tente novamente!'
              PERFORM P3-SENHA

            END-IF
      *------------------------------------------------------------------------
      *VERFICA플O DE LETRA MINUSCULA

            MOVE 'N' TO WRK-LOWER-PASS-OK

            PERFORM VARYING WRK-LOWER-CONT FROM 1 BY 1
                          UNTIL WRK-LOWER-CONT > LENGTH OF WRK-PASSWORD
             IF WRK-PASSWORD(WRK-LOWER-CONT:1) >= 'a'
                               AND WRK-PASSWORD(WRK-LOWER-CONT:1) <= 'z'
               MOVE 'S' TO WRK-LOWER-PASS-OK
             END-IF
            END-PERFORM

            IF WRK-LOWER-PASS-OK NOT EQUAL 'S'
             DISPLAY 'Sua senha deve ter pelo menos '
              'uma letra minuscula, tente novamente!'
             PERFORM P3-SENHA

            END-IF
      *------------------------------------------------------------------------
      *VERFICA플O DE CARACTERE ESPECIAL

            MOVE 'N' TO WRK-ESPECIAL-PASS-OK

            PERFORM VARYING WRK-ESPECIAL-CONT FROM 1 BY 1
                       UNTIL WRK-ESPECIAL-CONT > LENGTH OF WRK-PASSWORD
            IF WRK-PASSWORD(WRK-ESPECIAL-CONT:1) NOT IS NUMERIC
                AND WRK-PASSWORD(WRK-ESPECIAL-CONT:1) NOT IS ALPHABETIC

            MOVE 'S' TO WRK-ESPECIAL-PASS-OK
            END-IF
            END-PERFORM

            IF WRK-ESPECIAL-PASS-OK NOT EQUAL 'S'
             DISPLAY 'Sua senha deve ter pelo menos '
              'um caractere especial ex: (@#$%),tente novamente! '
             PERFORM P3-SENHA

            END-IF

      *------------------------------------------------------------------------
      *VERFICA플O DE NUMERO

            MOVE 'N' TO WRK-NUMBER-PASS-OK

            PERFORM VARYING WRK-NUMBER-CONT FROM 1 BY 1
                         UNTIL WRK-NUMBER-CONT > LENGTH OF WRK-PASSWORD
             IF WRK-PASSWORD(WRK-NUMBER-CONT:1) IS NUMERIC

              MOVE 'S' TO WRK-NUMBER-PASS-OK
             END-IF
            END-PERFORM

            IF WRK-NUMBER-PASS-OK NOT EQUAL 'S'
             DISPLAY 'Sua senha deve ter pelo menos '
              'um numero, tente novamente!'
             PERFORM P3-SENHA
            END-IF
            .
       P3-SENHA-FIM.
      *------------------------------------------------------------------------
      *VERFICA플O DE TELEFONE
      *------------------------------------------------------------------------
       P4-TELEFONE.
            INITIALISE WRK-TELEFONE
            MOVE 'N' TO WRK-TEL-PASS-OK
            DISPLAY 'Digite seu telefone com o DDD: '
            ACCEPT WRK-TELEFONE


            PERFORM VARYING WRK-TEL-CONT FROM 1 BY 1
                       UNTIL WRK-TEL-CONT > LENGTH OF WRK-TELEFONE
            IF WRK-TELEFONE(WRK-TEL-CONT:1) NOT IS NUMERIC
                      AND WRK-TELEFONE(WRK-TEL-CONT:1) NOT IS ALPHABETIC

            MOVE 'S' TO WRK-TEL-PASS-OK
            END-IF
            END-PERFORM

            IF WRK-TEL-PASS-OK EQUAL 'S'
            DISPLAY 'O telefone deve conter apenas numeros '
            PERFORM P4-TELEFONE

            END-IF

             IF WRK-TELEFONE IS ALPHABETIC
               DISPLAY 'O telefone deve conter apenas numeros'
               PERFORM P4-TELEFONE
             END-IF

             COMPUTE WRK-TEL-TAM = FUNCTION LENGTH
                                          (FUNCTION TRIM(WRK-TELEFONE))
             IF WRK-TEL-TAM > 12 OR WRK-TEL-TAM < 11
              DISPLAY 'Seu telefone deve conter entre 11 e 12 digitos, '
              'tente novamente!'

              PERFORM P4-TELEFONE
             END-IF
             .
       P4-TELEFONE-FIM.



      *------------------------------------------------------------------------
      *P6-GRAVACAO DE DADOS
      *------------------------------------------------------------------------
       P6-GRAVACAO.
            ADD 1 TO WRK-ID-USUARIO

            INITIALISE WRK-FS

            OPEN I-O USUARIO
            IF WRK-FS EQUAL 35 THEN
                OPEN OUTPUT USUARIO
            END-IF

            IF FS-OK THEN
                     MOVE WRK-ID-USUARIO          TO ID-USUARIO
                     MOVE WRK-FNAME               TO FNAME
                     MOVE WRK-LNAME               TO LNAME
                     MOVE WRK-EMAIL               TO E-MAIL
                     MOVE WRK-PASSWORD            TO PASSWORD
                     MOVE WRK-TELEFONE            TO TELEFONE

                     WRITE REG-USUARIO
                     NOT INVALID KEY
                        DISPLAY '*********************************'
                        DISPLAY '*Usuario cadastrado com sucesso!*'
                        DISPLAY '*********************************'
                        MOVE 'S'   TO WRK-GRAV
                     END-WRITE
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE CONTATOS'
                DISPLAY 'FILE STATUS: ' WRK-FS
            END-IF
            CLOSE USUARIO
            .
       P6-GRAVACAO-FIM.

       P999-FINAL.
            DISPLAY '***************************************'
            DISPLAY 'Registro numero: ' WRK-ID-USUARIO
            DISPLAY 'Nome: ' FUNCTION TRIM(WRK-FNAME)' '
                             FUNCTION TRIM(WRK-LNAME)
            DISPLAY 'E-mail: ' WRK-EMAIL
            DISPLAY 'Telefone: ' WRK-TELEFONE
            DISPLAY 'Senha: ' WRK-PASSWORD
            DISPLAY '****************************************'




            STOP RUN.
       END PROGRAM CREAUSER.
