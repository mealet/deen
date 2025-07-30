#include <stdlib.h>
#include <string.h>

// NOTE: This function is AI generated
void __string_replace(char *src, char *from, char *to) {
  if (!src || !from || !to)
    return;

  int src_len = strlen(src);
  int from_len = strlen(from);
  int to_len = strlen(to);

  if (from_len == 0)
    return;

  char *temp = malloc(src_len * 2 + 1);
  if (!temp)
    return;

  char *src_ptr = src;
  char *temp_ptr = temp;

  while (*src_ptr) {
    if (strncmp(src_ptr, from, from_len) == 0) {
      strcpy(temp_ptr, to);
      temp_ptr += to_len;
      src_ptr += from_len;
    } else {
      *temp_ptr++ = *src_ptr++;
    }
  }
  *temp_ptr = '\0';

  strcpy(src, temp);
  free(temp);
}
