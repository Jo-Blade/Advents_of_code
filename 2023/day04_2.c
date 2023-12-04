#include <stdio.h>
#include <string.h>
#define BUFF_LEN 255
#define NOTHING 0

enum etat {
  ATTENTE_LISTE,
  LISTE_1,
  LISTE_2
};

int gagner_card_instances(int liste_nombres[BUFF_LEN], int nb_en_lecture,
                          int nb_instances_card[BUFF_LEN], int card_index,
                          int prochaine_instance_a_ajouter) {
  if (nb_en_lecture == NOTHING)
    return prochaine_instance_a_ajouter;

  for (int i = 0; i < BUFF_LEN; i++) {
    if (liste_nombres[i] == nb_en_lecture) {
      nb_instances_card[prochaine_instance_a_ajouter] += nb_instances_card[card_index];
      return prochaine_instance_a_ajouter + 1;
    }
  }
  return prochaine_instance_a_ajouter;
}

int main() {
  FILE* fd = fopen("./input.txt", "r");
  char lettre;
  int liste_nombres[BUFF_LEN] = {NOTHING};
  int indice_nb_courant = 0;
  int nb_en_lecture = 0;
  enum etat etat_lecture = ATTENTE_LISTE;

  int nb_instances_card[BUFF_LEN] = {0};
  int card_index = 0;
  int prochaine_instance_a_ajouter = 0;

  int total = 0;

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
            prochaine_instance_a_ajouter = gagner_card_instances(
              liste_nombres, nb_en_lecture, nb_instances_card,
              card_index, prochaine_instance_a_ajouter);
            break;
        }
        nb_en_lecture = 0;

        if (lettre == '\n') {
          etat_lecture = ATTENTE_LISTE;
          indice_nb_courant = 0;
          memset(liste_nombres, NOTHING, BUFF_LEN * sizeof(int));
        }
        break;
      case ':':
        card_index++;
        nb_instances_card[card_index]++;
        total += nb_instances_card[card_index];
        prochaine_instance_a_ajouter = card_index + 1;
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

  printf("flag = %d\n", total);
}
