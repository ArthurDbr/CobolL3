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
       ORGANIZATION IS indexed
       ACCESS IS dynamic
       RECORD KEY fm_nom
       ALTERNATE RECORD KEY fm_type WITH DUPLICATES
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
       RECORD KEY fn_ine
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
        02 fn_ine PIC X(10).
        02 fn_matiere PIC A(15).
        02 fn_note PIC 9(2).

       FD fmatiere.
       01 matiereTamp.
        02 fm_nom PIC A(15).
        02 fm_coef PIC 9(1).
        02 fm_type PIC A(15).

       WORKING-STORAGE SECTION.
       77 fclasse_stat PIC 9(2).
       77 fmatiere_stat PIC 9(2).
       77 fcours_stat  PIC 9(2).
       77 fprof_stat PIC 9(2).
       77 feleves_stat PIC 9(2).
       77 fnote_stat PIC 9(2).

       77 Wrep PIC 9(1).

       77 WnomE PIC A(15).
       77 WprenomE PIC A(15).
       77 WjourNE PIC X(15).
       77 WMoisNE PIC X(15).
       77 WanneNE PIC X(15).
       77 WclasseE PIC 9(2).

       77 Wfin PIC 9(1).
       77 Wtrouve PIC 9(1).

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

       PERFORM AJOUT_ELEVES
       STOP RUN.

       AJOUT_ELEVES.
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        MOVE 0 TO Wrep
        MOVE 0 TO Wtrouve
         DISPLAY 'Veuillez entrer les informations suivantes :'
         DISPLAY 'INE'
         ACCEPT fe_ine
         DISPLAY 'Nom : '
         ACCEPT WnomE
         DISPLAY 'Prenom : '
         ACCEPT WprenomE
         OPEN INPUT feleves
         PERFORM WITH TEST AFTER UNTIL  Wfin = 0
           READ feleves
           AT END
            MOVE 1 TO Wfin
           NOT AT END
           DISPLAY 'hello'
           DISPLAY fe_nom
            IF WprenomE = fe_prenom AND WnomE = fe_nom
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
               MOVE WnomE TO fe_nom
               MOVE WprenomE TO fe_prenom
               STRING WanneNE "/" WmoisNE "/" WjourNE INTO fe_dateNaiss
               DISPLAY fe_dateNaiss
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
