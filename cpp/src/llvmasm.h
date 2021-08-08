#pragma once

#ifdef __cplusplus
#include <cstdint>
#include <string>
#else
#include <stdint.h>
#endif

#if defined(AMBERCPP_EXPORTS)
#define AMBERCPP_API __attribute__((visibility("default")))
#else
#define AMBERCPP_API
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef void* llvmasm_t;

AMBERCPP_API llvmasm_t CreateLLVMAsm();
AMBERCPP_API void DestroyLLVMAsm(llvmasm_t la);

AMBERCPP_API bool InitLLVMAsm(llvmasm_t la, const char* triple, size_t triple_len, const char* chip, size_t chip_len);
AMBERCPP_API void GetAsmBuffer(llvmasm_t la, char** buffer, size_t* buffer_size);
AMBERCPP_API bool EmitBuffered(llvmasm_t la, size_t asm_len);
AMBERCPP_API bool EmitAsmLabel(llvmasm_t la, const char* name, size_t name_len, bool global);
AMBERCPP_API bool CreateTempSymbol(llvmasm_t la, const char* prefix, size_t prefix_len, const char** name, size_t* name_len);
AMBERCPP_API bool SetSymbolValue(llvmasm_t la, const char* sym, size_t sym_len, uint64_t val);
AMBERCPP_API bool EndProgram(llvmasm_t la, const char** elf_data, size_t* elf_data_len);

AMBERCPP_API void GetLLVMAsmError(llvmasm_t la, const char** error, size_t* error_len);

#ifdef __cplusplus
}
#endif
