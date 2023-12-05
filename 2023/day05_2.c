#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#define BUFF_LEN 500
#define RG_DEBUT(i) (liste_ranges[i].debut)
#define RG_FIN(i) (liste_ranges[i].debut + liste_ranges[i].longueur)
/* incrémente un index et écrit un message si la taille du buffer est atteinte */
#define INC_I(i) if (i < BUFF_LEN - 1) i++; else printf("buffer trop petit\n");
#define RG_FROM_BORNES(b1, b2) {b1, b2 - (b1)}
#define RIEN (-2)

struct range {
  long debut;
  long longueur;
};

struct fonction {
  long in_debut;
  long out_debut;
  long longueur;
};

struct range appliquer_fonction_range(struct range rg,
                                      struct fonction fn) {
  long diff = fn.out_debut - fn.in_debut;
  return (struct range) {rg.debut + diff, rg.longueur};
}

/* renvoie la nouvelle taille de la liste des ranges */
int appliquer_fonction_liste (struct range liste_ranges[BUFF_LEN],
                              int liste_taille,
                              bool deja_traites[BUFF_LEN],
                              struct fonction fonction) {
  for (int i = 0; i < liste_taille; i++) {
    if (!deja_traites[i]) {
      /* on découpe le range actuel pour matcher celui
       * défini par la fonction. On ajoute les nouveaux
       * ranges à la liste et on applique la fonction sur le
       * range courant si nécéssaire */

      if (RG_DEBUT(i) < fonction.in_debut) {
        if (RG_FIN(i) > (fonction.in_debut + fonction.longueur)) {
          /* le range de la fonction est strictement inclu
           * dans le range actuel : découpage en 3 */
          struct range rg1 = RG_FROM_BORNES(RG_DEBUT(i), fonction.in_debut - 1);
          struct range rg2 = RG_FROM_BORNES(fonction.in_debut, fonction.in_debut + fonction.longueur);
          struct range rg3 = RG_FROM_BORNES(fonction.in_debut + fonction.longueur + 1, RG_FIN(i));
          liste_ranges[i] = appliquer_fonction_range(rg2, fonction);
          deja_traites[i] = true;
          liste_ranges[liste_taille] = rg1;
          INC_I(liste_taille);
          liste_ranges[liste_taille] = rg3;
          INC_I(liste_taille);
        }
        else if (RG_FIN(i) >= fonction.in_debut) {
          /* intersection avec le range de la fonction : découpage en 2 */
          struct range rg1 = RG_FROM_BORNES(RG_DEBUT(i), fonction.in_debut - 1);
          struct range rg2 = RG_FROM_BORNES(fonction.in_debut, RG_FIN(i));
          liste_ranges[i] = appliquer_fonction_range(rg2, fonction);
          deja_traites[i] = true;
          liste_ranges[liste_taille] = rg1;
          INC_I(liste_taille);
        }
      }
      else if (RG_DEBUT(i) <= (fonction.in_debut + fonction.longueur)) {

        if (RG_FIN(i) > (fonction.in_debut + fonction.longueur)) {
          /* intersection avec le range de la fonction : découpage en 2 */
          struct range rg1 = RG_FROM_BORNES(RG_DEBUT(i), fonction.in_debut + fonction.longueur);
          struct range rg2 = RG_FROM_BORNES(fonction.in_debut + fonction.longueur + 1, RG_FIN(i));
          liste_ranges[i] = appliquer_fonction_range(rg1, fonction);
          deja_traites[i] = true;
          liste_ranges[liste_taille] = rg2;
          INC_I(liste_taille);
        }
        else {
          /* inclu dans le range de la fonction : pas de découpage */
          liste_ranges[i] = appliquer_fonction_range(liste_ranges[i], fonction);
          deja_traites[i] = true;
        }
      }
    }
  }
  return liste_taille;
}

/* fonction qui renvoie un tableau
 * avec tous les nombres de la ligne lue
 * retour = nombre de nombres lus ou EOF */
int lire_nombres_ligne_suivante(long buffer[BUFF_LEN], FILE* fd) {
  char lettre;
  long temp_nb = RIEN;
  int nb_nombres = 0;
  do {
    lettre = fgetc(fd);

    if (lettre == EOF)
      return EOF;
    else if (('0' <= lettre) && (lettre <= '9')) {
      if (temp_nb == RIEN)
        temp_nb = 0;
      temp_nb = 10 * temp_nb + (lettre - '0');
    } else {
      if (temp_nb != RIEN) {
        buffer[nb_nombres] = temp_nb;
        INC_I(nb_nombres);
        temp_nb = RIEN;
      }
    }

  } while (lettre != '\n');

  return nb_nombres;
}

int main() {
  FILE* fd = fopen("input.txt", "r");

  struct range liste_ranges[BUFF_LEN];
  /* permet de savoir qu'on a déjà appliqué
  * la fonction de la passe courante au range d'indice i */
  bool deja_traites[BUFF_LEN] = {false};
  int liste_taille = 0;

  int nb_nombres;
  long buff_nb[BUFF_LEN];
  do {
    nb_nombres = lire_nombres_ligne_suivante(buff_nb, fd);

    if (nb_nombres == 0) {
      memset(deja_traites, false, BUFF_LEN * sizeof(bool));
    }
    else if (nb_nombres == 3) {
      struct fonction fn = {buff_nb[1], buff_nb[0], buff_nb[2]};
      liste_taille = appliquer_fonction_liste(liste_ranges, liste_taille, deja_traites, fn);
    }
    else if (nb_nombres >= 4) {
      for (int i = 0; i < nb_nombres - 1; i += 2)
        liste_ranges[i / 2] = (struct range) {buff_nb[i], buff_nb[i + 1]};

      liste_taille = nb_nombres / 2;
    }

  }while (nb_nombres != EOF);

  for (int i = 0; i < liste_taille; i++) {
    printf("range = %ld; %ld\n", liste_ranges[i].debut, liste_ranges[i].longueur);
  }
}
