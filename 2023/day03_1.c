#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define BUFF_LEN 255
#define PRECEDENTE 0
#define COURANTE 1
#define SUIVANTE 2

/* lit une ligne de façon safe; renvoie le nombre de caractères lus */
int readline(char* buffer, int max_read, FILE* fd) {
  int i = 0;
  char lettre;
  do {
    lettre = fgetc(fd);
    buffer[i] = lettre;
    i++;
  } while ((lettre != EOF) && (lettre != '\n') && (i < max_read));
  buffer[i - 1] = '\0';
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

int main() {
  FILE* fd = fopen("./input.txt", "r");
  int total = 0;
  /* On conserve 3 lignes plutot que tout le fichier dans la ram
    * pour éviter d'utiliser trop de mémoire :
    * 0. ligne précédente
    * 1. ligne courante
    * 2. ligne suivante */
  char lignes[3][BUFF_LEN];

  /* toutes les lignes font la même longueur donc c'est une constante */
  const int ligne_len = readline(lignes[COURANTE], BUFF_LEN, fd);

  /* pas de ligne précédente donc on met tous les caractères à des '.' */
  memset(lignes[PRECEDENTE], '.', ligne_len * sizeof(char));

  int nb_lus;
  do {
    /* Si on est à la fin, on met la ligne suivante à des '.' car pas de ligne suivante */
    if ((nb_lus = readline(lignes[SUIVANTE] , BUFF_LEN, fd)) == 0)
      memset(lignes[SUIVANTE], '.', ligne_len * sizeof(char));

    bool is_part_number = false;
    int number = 0;
    for (int i = 0; i < ligne_len; i++) {

      if (is_symbol(lignes[PRECEDENTE][i]) || is_symbol(lignes[COURANTE][i])
        || is_symbol(lignes[SUIVANTE][i])) {
        is_part_number = true;
      }

      if (('0' <= lignes[COURANTE][i]) && (lignes[COURANTE][i] <= '9'))
        number = 10 * number + (lignes[COURANTE][i] - '0');
      else {
        if (is_part_number)
          total += number;
        number = 0;

        /* on ne réinitialise is_part_number seulement si aucune des lignes ne contient
        * un symbole */
        if ((!is_symbol(lignes[PRECEDENTE][i])) && (!is_symbol(lignes[COURANTE][i]))
          && (!is_symbol(lignes[SUIVANTE][i])))
          is_part_number = false;
      }
    }
    if (is_part_number)
      total += number;

    /* on fait glisser les lignes */
    memcpy(lignes[PRECEDENTE], lignes[COURANTE], ligne_len);
    memcpy(lignes[COURANTE], lignes[SUIVANTE], ligne_len);
  } while (nb_lus > 0);

  printf("flag = %d\n", total);
}
