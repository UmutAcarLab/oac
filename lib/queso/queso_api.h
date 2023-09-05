#ifndef QUESO_H
#define QUESO_H

#ifdef __cplusplus
extern "C" {
#endif


void startVM_ ();
void initialize_ (const char* eqset_fn_, const char* symb_eqset_fn_, unsigned char** td_store);
int opt_circuit_ (const char* cqasm, int timeout, char* buffer, int buff_size, unsigned char* td_);

// int preprocess_ (const char* cqasm, char* buffer, int buff_size);
// long unsigned int load_eqset_ (const char* eqset_fn_, unsigned char** store);
// long unsigned int load_greedy_xfers_ (const char* eqset_fn_, unsigned char** store);
// void load_xfers_ (const char* eqset_fn_,
//   unsigned char** gstore, long unsigned int* glen,
//   unsigned char** allstore, long unsigned int* alen);

#ifdef __cplusplus
}
#endif

#endif