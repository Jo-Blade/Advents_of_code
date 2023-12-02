#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define BUFF_LEN 255
#define RED 0
#define GREEN 1
#define BLUE 2
#define NB_RED 12
#define NB_GREEN 13
#define NB_BLUE 14

char* tab[3] = {
  "red",
  "green",
  "blue"
};

/* renvoie le chiffre si trouvé, ou -1 si pas de chiffre */
int obtenir_chiffre(char* chaine, int chaine_len) {
  /* si ça contient un chiffre */
  if (('0' <= chaine[0]) && (chaine[0] <= '9'))
    return chaine[0] - '0';

  /* si c'est un chiffre écrit en toute lettres */
  for (int i = 0; i < 10; i++) {
    int n;
    if ((n = strlen(tab[i])) <= chaine_len) {
      if (strncmp(tab[i], chaine, n) == 0)
        return i;
    }
  }

  /* pas de chiffre en début de chaine */
  return -1;
}

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

int main() {
  FILE* fd = fopen("./input.txt", "r");
  int total = 0;
  char ligne[BUFF_LEN];
  int ligne_len;
  int tab_quantites[3];
  int id_jeu = 0;

  while((ligne_len = readline(ligne , BUFF_LEN, fd)) != 0) {
    id_jeu++;
    bool jeu_valide = true;
    int temp_number = 0;

    for (int i = 0; i < ligne_len; i++) {

      if ((ligne[i] == ';') || (ligne[i + 1] == '\0')) {
        if ((tab_quantites[RED] > NB_RED) || (tab_quantites[GREEN] > NB_GREEN) || (tab_quantites[BLUE] > NB_BLUE))
          jeu_valide = false;
      }

      if ((ligne[i] == ':') || (ligne[i] == ';')) {
        tab_quantites[RED] = 0;
        tab_quantites[GREEN] = 0;
        tab_quantites[BLUE] = 0;
      }

      if ((ligne[i] == ',') || (ligne[i] == ';') || (ligne[i] == ':'))
        temp_number = 0;

      if (('0' <= ligne[i]) && (ligne[i] <= '9'))
        temp_number = 10 * temp_number + (ligne[i] - '0');

      for (int couleur = 0; couleur < 3; couleur++) {
        if (strncmp(ligne + i, tab[couleur], strlen(tab[couleur])) == 0)
          tab_quantites[couleur] = temp_number;
      }
    }

    if (jeu_valide)
      total += id_jeu;
  }

  printf("flag = %d\n", total);
}
