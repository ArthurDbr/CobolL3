       IDENTIFICATION DIVISION.
       PROGRAM-ID. ecole.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT feleves ASSIGN TO "eleves.dat"
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fe_ine
       FILE STATUS IS feleves_stat.

       SELECT fprof ASSIGN TO "professeur.dat"
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fp_id
       ALTERNATE RECORD KEY fp_matiere WITH DUPLICATES
       FILE STATUS IS fprof_stat.

       SELECT fmatiere ASSIGN TO "matiere.dat"
       ORGANIZATION IS sequential
       ACCESS IS sequential
       FILE STATUS IS fmatiere_stat.

       SELECT fclasse ASSIGN TO "classe.dat"
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fc_id
       ALTERNATE RECORD KEY fc_niveau WITH DUPLICATES
       FILE STATUS IS fclasse_stat.

       SELECT fnote ASSIGN TO "note.dat"
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fn_cle
       ALTERNATE RECORD KEY fn_idNote WITH DUPLICATES
       ALTERNATE RECORD KEY fn_matiere WITH DUPLICATES
       ALTERNATE RECORD KEY fn_ine WITH DUPLICATES
       ALTERNATE RECORD KEY fn_niveau WITH DUPLICATES
       FILE STATUS IS fnote_stat.

       SELECT fcours ASSIGN TO "cours.dat"
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fco_numS
       ALTERNATE RECORD KEY fco_classe WITH DUPLICATES
       FILE STATUS IS fcours_stat.

       DATA DIVISION.
       FILE SECTION.
       FD feleves.
       01 eleveTamp.
        02 fe_ine PIC X(10).
        02 fe_note PIC 9(2).
        02 fe_nom PIC A(15).
        02 fe_prenom PIC A(15).
        02 fe_dateNaiss PIC X(10).
        02 fe_classe PIC 9(2).

       FD fprof.
       01 profTamp.
        02 fp_id PIC 9(2).
        02 fp_nom PIC A(15).
        02 fp_prenom PIC A(15).
        02 fp_telephone PIC 9(10).
        02 fp_matiere PIC A(15).

       FD fcours.
       01 coursTamp.
        02 fco_numS PIC 9(2).
        02 fco_horaireD PIC 9(2).
        02 fco_horaireF PIC 9(2).
        02 fco_classe PIC X(3).
        02 fco_profId PIC 9(2).

       FD fclasse.
       01 classeTamp.
        02 fc_id PIC 9(2).
        02 fc_idProf PIC 9(2).
        02 fc_niveau PIC 9(1).
        02 fc_nbElevesMax PIC 9(2).
        02 fc_nbEleves PIC 9(2).

       FD fnote.
       01 noteTamp.
        02 fn_cle.
          03 fn_idNote PIC 9(2).
          03 fn_ine PIC X(10).
          03 fn_matiere PIC A(15).
        02 fn_note PIC 9(2).
        02 fn_niveau PIC 9(1).

       FD fmatiere.
       01 matiereTamp.
        02 fm_nom PIC A(15).
        02 fm_coef PIC 9(1).
        02 fm_niveau PIC 9(1).

       WORKING-STORAGE SECTION.
       77 fclasse_stat PIC 9(2).
       77 fmatiere_stat PIC 9(2).
       77 fcours_stat  PIC 9(2).
       77 fprof_stat PIC 9(2).
       77 feleves_stat PIC 9(2).
       77 fnote_stat PIC 9(2).

       77 Wrep PIC 9(1).

       77 Wnom PIC A(15).
       77 Wprenom PIC A(15).
       77 WjourNE PIC X(2).
       77 WMoisNE PIC X(2).
       77 WanneNE PIC X(4).
       77 WclasseE PIC 9(2).
       77 Wine PIC X(10).

       77 WNomMatiere PIC A(15).
       77 Wnote PIC 9(2).
       77 Wcoef PIC 9(1).
       77 WidNote PIC 9(2).

       77 Wfin PIC 9(1).
       77 Wtrouve PIC 9(1).
       77 Wchoix PIC 9(1).

       77 WidProf PIC 9(2).
       77 Wtelephone PIC 9(10).
       77 WmatiereProf PIC A(15).

       PROCEDURE DIVISION.
       OPEN EXTEND feleves
       IF feleves_stat =35 THEN
        OPEN OUTPUT feleves
       END-IF
       CLOSE feleves
       OPEN EXTEND fprof
       IF fprof_stat =35 THEN
        OPEN OUTPUT fprof
       END-IF
       CLOSE fprof
       OPEN EXTEND fcours
       IF fcours_stat =35 THEN
        OPEN OUTPUT fcours
       END-IF
       CLOSE fcours
       OPEN EXTEND fclasse
       IF fclasse_stat =35 THEN
        OPEN OUTPUT fclasse
       END-IF
       CLOSE fclasse
       OPEN EXTEND fnote
       IF fnote_stat =35 THEN
        OPEN OUTPUT fnote
       END-IF
       CLOSE fnote
       OPEN EXTEND fmatiere
       IF fmatiere_stat =35 THEN
        OPEN OUTPUT fmatiere
       END-IF.
       CLOSE fmatiere


       PERFORM WITH TEST AFTER UNTIL Wchoix = 0
           DISPLAY '----------------------------------'
           DISPLAY 'Quelle choix voulez vous faire :'
           DISPLAY ' 1 : AJOUT_ELEVES'
           DISPLAY ' 2 : AFFICHER_ELEVES'
           DISPLAY ' 3 : AJOUT_MATIERE'
           DISPLAY ' 4 : AFFICHER_MATIERE'
           DISPLAY ' 5 : AJOUT_NOTE'
           DISPLAY ' 0 : Sortir'
           ACCEPT Wchoix
           EVALUATE Wchoix
               WHEN 1
                   PERFORM AJOUT_ELEVES
               WHEN 2
                   PERFORM AFFICHER_ELEVES
               WHEN 3
                   PERFORM AJOUT_MATIERE
               WHEN 4
                   PERFORM AFFICHER_MATIERE
               WHEN 5
                   PERFORM AJOUT_NOTE
               WHEN OTHER
                   MOVE 0 TO Wchoix
       END-PERFORM
       STOP RUN.

       AJOUT_PROFESSEUR.
       MOVE 0 TO Wrep
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY 'Entrer l identifiant du professeur : '
        ACCEPT WidProf
        OPEN INPUT fprof
         MOVE WidProf TO fp_id
          READ fprof
          INVALID KEY
           DISPLAY 'Professeur deja present'
          NOT INVALID KEY
           MOVE 1 TO Wtrouve
          END-READ
         CLOSE fprof
         IF Wtrouve = 1
           DISPLAY 'Nom : '
           ACCEPT Wnom
           DISPLAY 'Prenom : '
           ACCEPT Wprenom
           DISPLAY 'Telephone : '
           ACCEPT Wtelephone
           DISPLAY 'Matiere de l enseignant :'
           ACCEPT WmatiereProf
           OPEN I-O fprof
               MOVE WidProf TO fp_id
               MOVE Wnom TO fp_nom
               MOVE Wprenom TO fp_prenom
               MOVE Wtelephone TO fp_telephone
               MOVE WmatiereProf TO fp_matiere
               WRITE eleveTamp
               END-WRITE
           CLOSE fprof
         END-IF

        PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
       END-PERFORM.

       AJOUT_ELEVES.
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        MOVE 0 TO Wrep
        MOVE 0 TO Wtrouve
         DISPLAY 'Veuillez entrer les informations suivantes :'
         DISPLAY 'INE'
         ACCEPT fe_ine
         DISPLAY 'Nom : '
         ACCEPT Wnom
         DISPLAY 'Prenom : '
         ACCEPT Wprenom
         OPEN INPUT feleves
         PERFORM WITH TEST AFTER UNTIL  Wfin = 1
           READ feleves NEXT
           AT END
            MOVE 1 TO Wfin
           NOT AT END
            IF Wprenom = fe_prenom AND Wnom = fe_nom
                DISPLAY 'eleves deja present'
                MOVE 1 TO Wtrouve
            END-IF
          END-READ
          END-PERFORM
         CLOSE feleves
         IF Wtrouve = 0
           DISPLAY 'Date de naissance : '
           DISPLAY 'annee : '
           ACCEPT WanneNE
           DISPLAY 'mois : '
           ACCEPT WmoisNE
           DISPLAY 'jour : '
           ACCEPT WjourNE
           DISPLAY 'id classe :'
           ACCEPT WclasseE
           OPEN I-O feleves
               MOVE Wnom TO fe_nom
               MOVE Wprenom TO fe_prenom
               STRING WanneNE "/" WmoisNE "/" WjourNE INTO fe_dateNaiss
               MOVE WclasseE TO fe_classe
               WRITE eleveTamp
               END-WRITE
           CLOSE feleves
        END-IF
         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
        END-PERFORM.

       AFFICHER_ELEVES.
       MOVE 0 TO Wfin
       OPEN INPUT feleves
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                    READ feleves NEXT
                    AT END
                        MOVE 1 TO Wfin
                    NOT AT END
                        DISPLAY 'Nom : '
                        DISPLAY fe_nom
                        DISPLAY 'Prenom'
                        DISPLAY fe_prenom
                        DISPLAY 'INE '
                        DISPLAY fe_ine
                    END-READ
                END-PERFORM
                CLOSE feleves.

        AFFICHER_MATIERE.
        MOVE 0 TO Wfin
        OPEN INPUT fmatiere
           PERFORM WITH TEST AFTER UNTIL Wfin = 1
               READ fmatiere
               AT END
                   MOVE 1 TO Wfin
               NOT AT END
                   DISPLAY 'Nom : '
                   DISPLAY fm_nom
               END-READ
           END-PERFORM
          CLOSE fmatiere.

       AJOUT_MATIERE.
       MOVE 0 TO Wfin
       MOVE 0 TO Wrep
       PERFORM WITH TEST AFTER UNTIL Wrep = 0
         DISPLAY 'Quelle est le nom de la matiere a ajouter ?'
         ACCEPT WNomMatiere
         DISPLAY 'Quelle est son coefficient ? '
         PERFORM WITH TEST AFTER UNTIL Wcoef > 0 AND Wcoef < 10
           DISPLAY 'Le coefficient ne peut etre inferieur a 1 ni'
           DISPLAY ' superieur a 9'
           ACCEPT Wcoef
         END-PERFORM
         OPEN EXTEND fmatiere
           MOVE WNomMatiere TO fm_nom
           MOVE Wcoef TO fm_coef
           WRITE matiereTamp
         END-WRITE
         CLOSE fmatiere
         DISPLAY 'Matiere cree !'
         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
       END-PERFORM.

       AJOUT_NOTE.
       MOVE 0 TO Wtrouve
       MOVE 0 TO Wfin
       MOVE 0 TO Wrep
       PERFORM WITH TEST AFTER UNTIL Wrep = 0
        OPEN INPUT feleves
        DISPLAY 'Veuillez rentrer le numero ine de l etudiant : '
        ACCEPT Wine
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ feleves NEXT
           AT END
            MOVE 1 TO Wfin
           NOT AT END
            IF Wine = fe_ine
                MOVE 1 TO Wtrouve
            END-IF
          END-READ
          END-PERFORM
         CLOSE feleves

         IF Wtrouve = 1
           MOVE 0 TO Wfin
           MOVE 0 TO Wtrouve
           DISPLAY 'Dans quelle matiere a-t-il eu cette note ?'
           ACCEPT WNomMatiere
           OPEN INPUT fmatiere
           PERFORM WITH TEST AFTER UNTIL Wfin = 1
             READ fmatiere
             AT END
               MOVE 1 TO Wfin
             NOT AT END
               IF WNomMatiere = fm_nom
                 MOVE 1 TO Wtrouve
               END-IF
             END-READ
           END-PERFORM
           CLOSE fmatiere
           IF Wtrouve = 1
             DISPLAY 'Quelle est le numero du devoir ?'
             ACCEPT WidNote
             OPEN INPUT fnote
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                READ fnote NEXT
                AT END
                  MOVE 1 TO Wfin
                NOT AT END
                  IF WNomMatiere = fn_matiere AND Wine = fn_ine
                  AND fn_idNote = WidNote
                    MOVE 0 TO Wtrouve
                  END-IF
                END-READ
                END-PERFORM
             CLOSE fnote
             IF Wtrouve = 1
              MOVE 50 TO Wnote
               PERFORM WITH TEST AFTER UNTIL Wnote > 0 AND Wnote < 21
                 DISPLAY 'Quelle note a eu l etudiant ? (< 0 et > 21)'
                 ACCEPT Wnote
               END-PERFORM
               OPEN I-O fnote
                 MOVE Wine TO fn_ine
                 MOVE WNomMatiere TO fn_matiere
                 MOVE WidNote TO fn_idNote
                 MOVE Wnote TO fn_note
                 WRITE noteTamp
                 END-WRITE
               CLOSE fnote
               DISPLAY 'Note ajoute !'
              ELSE
               DISPLAY 'Numero deja ajoute !'
              END-IF
             ELSE
               DISPLAY 'Matiere non reconnu !'
             END-IF
           ELSE
             DISPLAY 'Numero ine non reconnu !'
           END-IF
         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
       END-PERFORM.
