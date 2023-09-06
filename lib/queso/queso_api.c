#include "queso_api.h"
#include "string.h"
#include <stdlib.h>
#include <stdio.h>

void startVM () {
  startVM_ ();
}

void initialize (const char* eqset_fn_, const char* symb_eqset_fn_, unsigned char** td_store) {
  initialize_ (eqset_fn_, symb_eqset_fn_, td_store);
}

int opt_circuit (const char* cqasm, int timeout, char* buffer, int buff_size, unsigned char* td_) {
  return opt_circuit_ (cqasm, timeout, buffer, buff_size, td_);
}
