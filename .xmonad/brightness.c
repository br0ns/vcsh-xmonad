#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* Min brightness */
#define min 1

#define STEPS 10

/* Dell Latitude */
#define PATH "/sys/class/backlight/intel_backlight/"

#define MAX_STEP (STEPS - 1)

int main (int argc, char *argv[]) {
  int fd, ret, max, step, cur;
  char buf[64];

  if (argc != 2) {
    printf("usage: %s +/-\n", argv[0]);
    return EXIT_FAILURE;
  }

  /* Become root */
  setuid(0);
  setgid(0);

  /* Read max brightness */
  if ((fd = open(PATH "max_brightness", O_RDONLY)) == -1) {
    return EXIT_FAILURE;
  }

  if ((ret = read(fd, buf, sizeof(buf) - 1)) == -1) {
    return EXIT_FAILURE;
  }

  close(fd);

  buf[ret] = 0;
  max = atoi(buf);

  /* Read current brightness */
  if ((fd = open(PATH "brightness", O_RDWR)) == -1) {
    return EXIT_FAILURE;
  }

  if ((ret = read(fd, buf, sizeof(buf) - 1)) == -1) {
    return EXIT_FAILURE;
  }

  buf[ret] = 0;
  cur = atoi(buf);


  /* Current step, rounded down */
  step = (cur - min) * MAX_STEP / (max - min);

  switch (argv[1][0]) {
  case '+':
    step++;
    break;
  case '-':
    step--;
    break;
  }

  if (step > MAX_STEP) {
    step = MAX_STEP;
  } else if (step < 0) {
    step = 0;
  }

  /* Calculate brightness, rounded up */
  cur = min + ((max - min) * step + MAX_STEP - 1) / MAX_STEP;

  /* Clear file */
  ftruncate(fd, 0);
  lseek(fd, 0, SEEK_SET);

  /* Write new brightness */
  dprintf(fd, "%d\n", cur);

  close(fd);

  return EXIT_SUCCESS;
}
