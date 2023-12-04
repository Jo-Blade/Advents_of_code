#include <stdio.h>
#include <string.h>
#define BUFF_LEN 255
#define NOTHING 0

enum etat {
  ATTENTE_LISTE,
  LISTE_1,
  LISTE_2
};

int editer_score_card(int liste_nombres[BUFF_LEN], int nb_en_lecture, int score_card) {
  if (nb_en_lecture == NOTHING)
    return score_card;

  for (int i = 0; i < BUFF_LEN; i++) {
    if (liste_nombres[i] == nb_en_lecture) {
      if (score_card == 0)
        return 1;
      else
        return score_card * 2;
    }
  }
  return score_card;
}

int main() {
  FILE* fd = fopen("./input.txt", "r");
  char lettre;
  int total = 0;
  int score_card = 0;
  int liste_nombres[BUFF_LEN] = {NOTHING};
  int indice_nb_courant = 0;
  int nb_en_lecture = 0;
  enum etat etat_lecture = ATTENTE_LISTE;

  do {
    lettre = fgetc(fd);

    switch (lettre) {
      case ' ':
      case '\n':
        switch (etat_lecture) {
          case ATTENTE_LISTE:
            break;
          case LISTE_1:
            liste_nombres[indice_nb_courant] = nb_en_lecture;
            if (liste_nombres[indice_nb_courant] != NOTHING)
              indice_nb_courant++;
            break;
          case LISTE_2:
            score_card = editer_score_card(liste_nombres, nb_en_lecture, score_card);
            break;
        }
        nb_en_lecture = 0;

        if (lettre == '\n') {
          total += score_card;
          score_card = 0;
          etat_lecture = ATTENTE_LISTE;
          indice_nb_courant = 0;
          memset(liste_nombres, NOTHING, BUFF_LEN * sizeof(int));
        }
        break;
      case ':':
        etat_lecture = LISTE_1;
        nb_en_lecture = 0;
        break;
      case '|':
        etat_lecture = LISTE_2;
        break;
    }

    if (('0' <= lettre) && (lettre <= '9'))
      nb_en_lecture = 10 * nb_en_lecture + (lettre - '0');
  } while (lettre != EOF);

  printf("%d\n", total);
}
