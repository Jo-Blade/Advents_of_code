#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define BUFF_LEN 255
#define PRECEDENTE 0
#define COURANTE 1
#define SUIVANTE 2
#define RIEN (-1)
#define ETOILE (-2)

/* lit une ligne de façon safe; et parse les caractères pour obtenir
 * une ligne d'entiers de la forme :
 * - la valeur du nombre dont le chiffre courant est une décimale
 *   si le caractère d'origine était un chiffre
 * - -2 si le caractère d'origine était une étoile
 * - -1 sinon */
int parse_et_readline(int* buffer, int max_read, FILE* fd) {
  int i = 0;
  char lettre;

  /* comme on connait pas encore la valeur d'un nombre quand on lit,
   * on écrit dans le buffer son index dans tmp_nb puis on fait
   * un deuxième passage pour remplacer par la vraie valeur */
  int tmp_nb[BUFF_LEN] = {0};
  int tmp_nb_index = -1;
  bool lecture_nombre_en_cours = false;
  do {
    lettre = fgetc(fd);
    if (lettre == '*') {
      buffer[i] = ETOILE;
      lecture_nombre_en_cours = false;
    } else if (('0' <= lettre) && (lettre <= '9')) {
      if (!lecture_nombre_en_cours)
        tmp_nb_index++;
      tmp_nb[tmp_nb_index] = tmp_nb[tmp_nb_index] * 10 + (lettre - '0');
      lecture_nombre_en_cours = true;
      buffer[i] = tmp_nb_index;
    } else {
      buffer[i] = RIEN;
      lecture_nombre_en_cours = false;
    }
    i++;
  } while ((lettre != EOF) && (lettre != '\n') && (i < max_read));

  for (int j = 0; j < i - 1; j++) {
    if (buffer[j] >= 0)
      buffer[j] = tmp_nb[buffer[j]];
  }
  return i - 1;
}

/* indique si le caractère c est un symbole */
bool is_symbol(char c) {
  if (c == '.')
    return false;
  else if (('0' <= c) && (c <= '9'))
    return false;
  else
    return true;
}

/* calcule le gear ratio pour l'étoile à l'indice i de la ligne courante */
int gear_ratio(int lignes[3][BUFF_LEN], int i) {
  int nombre_part_numbers = 0;
  int ratio = 1;
  for (int l = PRECEDENTE; l <= SUIVANTE; l++) {
    bool lecture_nombre_en_cours = false;
    for (int x = i - 1; x <= i + 1; x++) {
      if (lignes[l][x] >= 0) {
        /* éviter de compter un même nombre plusieurs fois */
        if (!lecture_nombre_en_cours) {
          nombre_part_numbers++;
          ratio *= lignes[l][x];
        }
        lecture_nombre_en_cours = true;
      } else
      lecture_nombre_en_cours = false;
    }
  }

  if (nombre_part_numbers == 2)
    return ratio;
  else
    return 0;
}

int main() {
  FILE* fd = fopen("./input.txt", "r");
  int total = 0;
  /* On conserve 3 lignes plutot que tout le fichier dans la ram
    * pour éviter d'utiliser trop de mémoire :
    * 0. ligne précédente
    * 1. ligne courante
    * 2. ligne suivante
    * on fait de tableaux de int car on va stocker dans chaque case
    * - la valeur du nombre dont le chiffre courant est une décimale
    *   si le caractère d'origine était un chiffre
    * - -2 si le caractère d'origine était une étoile
    * - -1 sinon */
  int lignes[3][BUFF_LEN];

  /* toutes les lignes font la même longueur donc c'est une constante */
  const int ligne_len = parse_et_readline(lignes[COURANTE], BUFF_LEN, fd);

  /* pas de ligne précédente donc on met tous les nombres à RIEN */
  memset(lignes[PRECEDENTE], RIEN, ligne_len * sizeof(int));

  int nb_lus;
  do {
    /* Si on est à la fin, on met la ligne suivante à RIEN car pas de ligne suivante */
    if ((nb_lus = parse_et_readline(lignes[SUIVANTE] , BUFF_LEN, fd)) == 0)
      memset(lignes[SUIVANTE], RIEN, ligne_len * sizeof(int));

    for (int i = 0; i < ligne_len; i++) {
      if (lignes[COURANTE][i] == ETOILE)
        total += gear_ratio(lignes, i);
    }

    /* on fait glisser les lignes */
    memcpy(lignes[PRECEDENTE], lignes[COURANTE], ligne_len * sizeof(int));
    memcpy(lignes[COURANTE], lignes[SUIVANTE], ligne_len * sizeof(int));
  } while (nb_lus > 0);

  printf("flag = %d\n", total);
}
