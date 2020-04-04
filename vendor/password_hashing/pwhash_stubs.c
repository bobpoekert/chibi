#include <hydrogen.c>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define OPSLIMIT_MAX 50000
#define MEMLIMIT_MAX 0
#define THREADS_MAX  1

#define OPSLIMIT 10000
#define MEMLIMIT 0
#define THREADS  1

value call_generate_key() {
    CAMLparam0();
    value res = caml_alloc_string(hydro_pwhash_MASTERKEYBYTES);
    const char *res_string = String_val(res);
    hydro_pwhash_keygen(res_string);
    CAMLreturn(res);

}

value call_hash_password(value master_key, value password) {
    CAMLparam2(master_key, password);

    if (caml_string_length(master_key) != hydro_pwhash_MASTERKEYBYTES) {
        caml_failwith("invalid master key. wrong length");
    }

    const char *master_key_str = String_val(master_key);

    value res = caml_alloc_string(hydro_pwhash_STOREDBYTES);
    const char *password_str = String_val(password);
    size_t password_len = caml_string_length(password);
    const char *res_str = String_val(res);

    hydro_pwhash_create(res_str, password_str, password_len, master_key_str,
            OPSLIMIT, MEMLIMIT, THREADS);

    CAMLreturn(res);

}

value call_verify_password(value master_key, value password_hash, value password) {
    CAMLparam3(master_key, password_hash, password);
    
    if (caml_string_length(master_key) != hydro_pwhash_MASTERKEYBYTES) {
        caml_failwith("invalid master key. wrong length");
    }
    
    if (caml_string_length(password_hash) != hydro_pwhash_STOREDBYTES) {
        caml_failwith("invalid password hash. wrong length");
    }


    const char *master_key_str = String_val(master_key);
    char *password_hash_str = String_val(password_hash);

    const char *password_str = String_val(password);
    size_t password_length = caml_string_length(password);

    if (hydro_pwhash_verify(
                password_hash_str,
                password_str, password_length,
                master_key_str,
                OPSLIMIT_MAX, MEMLIMIT_MAX, THREADS_MAX) != 0) {

        /* bad password */

        return Val_false;

    } else {


        /* good password */

        return Val_true;

    }


}
