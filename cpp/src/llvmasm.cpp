#include "llvmasm.h"

#include <optional>
#include <string>

#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCParser/AsmLexer.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"

#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"

#include "llvm-c/Target.h"

// Must be big enough to fit textually encoded PAL metadata
constexpr size_t AsmStringBufferSize = 16384;

class LLVMAsm
{
  public:
    bool Init(llvm::StringRef triple_str, llvm::StringRef chip_str);
    bool EmitBufferedAsm(size_t asm_len);
    bool EmitLabel(llvm::StringRef name, bool global);
    bool CreateTempSymbol(llvm::StringRef prefix, llvm::StringRef* name);
    bool SetSymbolValue(llvm::StringRef sym, uint64_t val);
    bool EndProgram();
    void SetAsmError(const llvm::SMDiagnostic& diag)
    {
        _error = std::string(diag.getMessage())
                     .append(":\n")
                     .append(diag.getLineContents())
                     .append("\n")
                     .append(diag.getColumnNo(), ' ')
                     .append("^");
    }
    const std::string& Error() const
    {
        return _error;
    }
    llvm::MutableArrayRef<char> AsmBuffer()
    {
        return _asm_buf;
    }
    llvm::ArrayRef<char> ELFBytes() const
    {
        return llvm::ArrayRef<char>(_elf_bytes);
    }

  private:
    std::string _error{};
    llvm::SmallVector<char, 0> _elf_bytes{};
    llvm::raw_svector_ostream _elf_bytes_os{_elf_bytes};
    llvm::MutableArrayRef<char> _asm_buf{};
    llvm::SourceMgr _src_mgr{};
    llvm::MCObjectFileInfo _mofi{};
    std::optional<llvm::MCContext> _mc_ctx{};
    std::unique_ptr<llvm::MCInstrInfo> _mc_instr_info{};
    std::unique_ptr<llvm::MCRegisterInfo> _mc_reg_info{};
    std::unique_ptr<llvm::MCAsmInfo> _mc_asm_info{};
    std::unique_ptr<llvm::MCSubtargetInfo> _mc_subtarget_info{};
    std::unique_ptr<llvm::MCStreamer> _mc_streamer{};
    std::unique_ptr<llvm::MCAsmParser> _mc_parser{};
    std::unique_ptr<llvm::MCTargetAsmParser> _mc_target_parser{};
};

llvmasm_t CreateLLVMAsm()
{
    LLVMInitializeAMDGPUAsmParser();
    LLVMInitializeAMDGPUTargetInfo();
    LLVMInitializeAMDGPUTargetMC();

    return new LLVMAsm();
}

void DestroyLLVMAsm(llvmasm_t la)
{
    delete reinterpret_cast<LLVMAsm*>(la);
}

bool InitLLVMAsm(llvmasm_t la, const char* triple, size_t triple_len, const char* chip, size_t chip_len)
{
    return reinterpret_cast<LLVMAsm*>(la)->Init(llvm::StringRef(triple, triple_len), llvm::StringRef(chip, chip_len));
}

void GetAsmBuffer(llvmasm_t la, char** buffer, size_t* buffer_size)
{
    auto buf = reinterpret_cast<LLVMAsm*>(la)->AsmBuffer();
    *buffer = buf.data();
    *buffer_size = buf.size();
}

bool EmitBuffered(llvmasm_t la, size_t asm_len)
{
    return reinterpret_cast<LLVMAsm*>(la)->EmitBufferedAsm(asm_len);
}

bool EmitAsmLabel(llvmasm_t la, const char* name, size_t name_len, bool global)
{
    return reinterpret_cast<LLVMAsm*>(la)->EmitLabel(llvm::StringRef(name, name_len), global);
}

bool CreateTempSymbol(llvmasm_t la, const char* prefix, size_t prefix_len, const char** name, size_t* name_len)
{
    llvm::StringRef name_ref;
    bool result = reinterpret_cast<LLVMAsm*>(la)->CreateTempSymbol(llvm::StringRef(prefix, prefix_len), &name_ref);
    *name = name_ref.data();
    *name_len = name_ref.size();
    return result;
}

bool SetSymbolValue(llvmasm_t la, const char* sym, size_t sym_len, uint64_t val)
{
    return reinterpret_cast<LLVMAsm*>(la)->SetSymbolValue(llvm::StringRef(sym, sym_len), val);
}

bool EndProgram(llvmasm_t la, const char** elf_data, size_t* elf_data_len)
{
    auto* la_ = reinterpret_cast<LLVMAsm*>(la);
    bool result = la_->EndProgram();
    if (result)
    {
        *elf_data = la_->ELFBytes().data();
        *elf_data_len = la_->ELFBytes().size();
    }
    return result;
}

void GetLLVMAsmError(llvmasm_t la, const char** error, size_t* error_len)
{
    const auto& error_str = reinterpret_cast<LLVMAsm*>(la)->Error();
    *error = error_str.data();
    *error_len = error_str.size();
}

bool LLVMAsm::EndProgram()
{
    _mc_streamer->Finish();
    return true;
}

bool LLVMAsm::SetSymbolValue(llvm::StringRef sym, uint64_t val)
{
    _mc_ctx->setSymbolValue(*_mc_streamer, sym, val);
    return true;
}

bool LLVMAsm::CreateTempSymbol(llvm::StringRef prefix, llvm::StringRef* name)
{
    *name = _mc_ctx->createTempSymbol(prefix, true, false)->getName();
    return true;
}

bool LLVMAsm::EmitBufferedAsm(size_t asm_len)
{
    assert(asm_len <= AsmStringBufferSize);
    _asm_buf[asm_len] = '\0';
    dynamic_cast<llvm::AsmLexer&>(_mc_parser->getLexer()).setBuffer(llvm::StringRef(_asm_buf.data(), asm_len));
    return _mc_parser->Run(true, true) == 0;
}

bool LLVMAsm::EmitLabel(llvm::StringRef name, bool global)
{
    llvm::MCSymbol* sym = _mc_ctx->getOrCreateSymbol(name);
    _mc_streamer->emitLabel(sym);
    if (global)
        _mc_streamer->emitSymbolAttribute(sym, llvm::MCSA_Global);
    return true;
}

bool LLVMAsm::Init(llvm::StringRef triple_str, llvm::StringRef chip_str)
{
    llvm::Triple triple(triple_str);
    llvm::MCTargetOptions mc_opts{};

    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(triple.str(), _error);
    if (!target)
        return false;

    _mc_instr_info.reset(target->createMCInstrInfo());
    if (!_mc_instr_info)
    {
        _error = "unable to create instruction info";
        return false;
    }
    _mc_reg_info.reset(target->createMCRegInfo(triple.str()));
    if (!_mc_reg_info)
    {
        _error = std::string("unable to create target register info for triple '").append(triple_str).append("'");
        return false;
    }
    _mc_asm_info.reset(target->createMCAsmInfo(*_mc_reg_info, triple.str(), mc_opts));
    if (!_mc_asm_info)
    {
        _error = std::string("unable to create target asm info for triple '").append(triple_str).append("'");
        return false;
    }

    _mc_subtarget_info.reset(target->createMCSubtargetInfo(triple.str(), chip_str, ""));
    if (!_mc_subtarget_info)
    {
        _error = std::string("unable to create subtarget info for chip '").append(chip_str).append("'");
        return false;
    }

    _mc_ctx.emplace(_mc_asm_info.get(), _mc_reg_info.get(), &_mofi, &_src_mgr, &mc_opts);
    _mofi.InitMCObjectFileInfo(triple, false, *_mc_ctx, false);

    {
        std::unique_ptr<llvm::MCCodeEmitter> mc_emitter(
            target->createMCCodeEmitter(*_mc_instr_info, *_mc_reg_info, *_mc_ctx));
        std::unique_ptr<llvm::MCAsmBackend> mc_backend(
            target->createMCAsmBackend(*_mc_subtarget_info, *_mc_reg_info, mc_opts));
        std::unique_ptr<llvm::MCObjectWriter> mc_obj_writer(mc_backend->createObjectWriter(_elf_bytes_os));
        _mc_streamer.reset(target->createMCObjectStreamer(
            triple, *_mc_ctx, std::move(mc_backend), std::move(mc_obj_writer), std::move(mc_emitter),
            *_mc_subtarget_info, mc_opts.MCRelaxAll, mc_opts.MCIncrementalLinkerCompatible,
            /*DWARFMustBeAtTheEnd*/ false));
    }

    auto buf = llvm::WritableMemoryBuffer::getNewMemBuffer(AsmStringBufferSize);
    _asm_buf = buf->getBuffer();
    _src_mgr.AddNewSourceBuffer(std::move(buf), llvm::SMLoc());
    _src_mgr.setDiagHandler(
        [](const llvm::SMDiagnostic& diag, void* this_ptr) {
            auto this_ = reinterpret_cast<LLVMAsm*>(this_ptr);
            this_->SetAsmError(diag);
        },
        this);

    _mc_parser.reset(llvm::createMCAsmParser(_src_mgr, *_mc_ctx, *_mc_streamer, *_mc_asm_info));
    _mc_target_parser.reset(target->createMCAsmParser(*_mc_subtarget_info, *_mc_parser, *_mc_instr_info, mc_opts));
    if (!_mc_target_parser)
    {
        _error = std::string("unable to create target parser for triple '").append(triple_str).append("'");
        return false;
    }

    _mc_parser->setTargetParser(*_mc_target_parser);
    _mc_streamer->InitSections(false);
    return true;
}
