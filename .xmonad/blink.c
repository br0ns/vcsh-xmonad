#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* Power button LED
 * Tested on:
 *   Lenovo Thinkpad T420s
 *   Lenovo Thinkpad T450s
 *   Lenovo Thinkpad X1 Carbon 4th gen
 */
#define PATH "/proc/acpi/ibm/led"
#define ON "0 on"
#define OFF "0 off"

/* Caps lock LED
 * Tested on:
 *   Dell Latitude E7450
 */
/* #define PATH "/sys/class/leds/input0::capslock/brightness" */
/* #define ON "1" */
/* #define OFF "0" */

#define DEFAULT OFF

int main () {
  int fd, i;
  setuid(0);
  setgid(0);
  if ((fd = open(PATH, O_WRONLY)) == -1) {
    return EXIT_FAILURE;
  }
  for (i = 0; i < 10; i++) {
    write(fd, OFF, strlen(OFF));
    usleep(200000);
    write(fd, ON, strlen(ON));
    usleep(200000);
  }
  write(fd, DEFAULT, strlen(DEFAULT));
  close(fd);
  return EXIT_SUCCESS;
}
