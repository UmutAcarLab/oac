#include "queso_api.h"
#include "string.h"
#include <stdlib.h>
#include <stdio.h>

void startVM () {
  return startVM_ ();
}

void initialize (const char* eqset_fn_, const char* symb_eqset_fn_, unsigned char** td_store) {
  initialize_ (eqset_fn_, symb_eqset_fn_, td_store);
}

int opt_circuit (const char* cqasm, int timeout, char* buffer, int buff_size, unsigned char* td_) {
  return opt_circuit_ (cqasm, timeout, buffer, buff_size, td_);
}


long unsigned int load_eqset (const char* eqset_fn_, unsigned char** store) {
  return load_eqset_(eqset_fn_, store);
}

long unsigned int load_greedy_xfers (const char* eqset_fn_, unsigned char** store) {
   return load_greedy_xfers_(eqset_fn_, store);
}

void load_xfers (const char* eqset_fn_,
  unsigned char** gstore, long unsigned int* glen,
  unsigned char** allstore, long unsigned int* alen)
{
  load_xfers_(eqset_fn_, gstore, glen, allstore, alen);
}
