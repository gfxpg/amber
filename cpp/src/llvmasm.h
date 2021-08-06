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

typedef struct llvmasm_strref
{
    const char* str;
    size_t len;
} llvmasm_strref_t;

typedef void* llvmasm_t;

AMBERCPP_API llvmasm_t CreateLLVMAsm();
AMBERCPP_API void DestroyLLVMAsm(llvmasm_t la);

AMBERCPP_API bool InitLLVMAsm(llvmasm_t la, llvmasm_strref_t triple, llvmasm_strref_t chip);
AMBERCPP_API bool EmitAsmInstruction(llvmasm_t la, llvmasm_strref_t inst);
AMBERCPP_API bool EmitAsmLabel(llvmasm_t la, llvmasm_strref_t name, bool global);
AMBERCPP_API bool CreateTempSymbol(llvmasm_t la, llvmasm_strref_t prefix, llvmasm_strref_t* name);
AMBERCPP_API bool SetSymbolValue(llvmasm_t la, llvmasm_strref_t sym, uint64_t val);
AMBERCPP_API bool EndProgram(llvmasm_t la, llvmasm_strref_t* out_bytes);

AMBERCPP_API llvmasm_strref_t GetLLVMAsmError(llvmasm_t la);

#ifdef __cplusplus
}
#endif

