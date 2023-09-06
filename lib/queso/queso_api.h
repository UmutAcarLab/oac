#ifndef QUESO_H
#define QUESO_H

#ifdef __cplusplus
extern "C" {
#endif

void startVM_ ();
void initialize_ (const char* eqset_fn_, const char* symb_eqset_fn_, unsigned char** td_store);
int opt_circuit_ (const char* cqasm, int timeout, char* buffer, int buff_size, unsigned char* td_);

#ifdef __cplusplus
}
#endif

#endif