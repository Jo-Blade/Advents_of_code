#include <stdio.h>
#include <string.h>
#define BUFF_LEN 255

char* tab[10] = {
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
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
  char lettre;
  int nombre_ligne_1 = -1;
  int nombre_ligne_2;
  int total = 0;
  char ligne[BUFF_LEN];
  int ligne_len;

  while((ligne_len = readline(ligne , BUFF_LEN, fd)) != 0) {

    for (int i = 0; i < ligne_len; i++) {
      int nb;
      if ((nb = obtenir_chiffre(ligne + i, ligne_len - i)) >= 0) {
        if (nombre_ligne_1 == -1)
          nombre_ligne_1 = nb;
        nombre_ligne_2 = nb;
      }
    }

    total += nombre_ligne_1 * 10 + nombre_ligne_2;
    nombre_ligne_1 = -1;
  }
  printf("total = %d\n", total);
}
