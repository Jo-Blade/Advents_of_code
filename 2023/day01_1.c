#include <stdio.h>

int main() {
  FILE* fd = fopen("./input.txt", "r");
  char lettre;
  int nombre_ligne_1 = -1;
  int nombre_ligne_2;
  int total = 0;
  while((lettre = fgetc(fd)) != EOF) {
    if (lettre == '\n') {
      total += nombre_ligne_1 * 10 + nombre_ligne_2;
      nombre_ligne_1 = -1;
    } else if ('0' <= lettre && lettre <= '9') {
      if (nombre_ligne_1 == -1)
        nombre_ligne_1 = lettre - '0';
      nombre_ligne_2 = lettre - '0';
    }
  }
  printf("%d\n", total);
}
