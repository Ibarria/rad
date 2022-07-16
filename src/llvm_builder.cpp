#pragma warning(disable: 4530)

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/IR/DIBuilder.h"

#include "FileObject.h"
#include "llvm_builder.h"
#include "assert.h"
#include "AST.h"
#include "Timer.h"
#include "Profiler.h"
#include "os.h"

#ifdef PLATFORM_WINDOWS
 #include <filesystem>
 namespace fs = std::filesystem;
#else
 #include <experimental/filesystem>
 namespace fs = std::experimental::filesystem;
#endif

// from Parser.cpp
DirectTypeAST* getBuiltInType(TOKEN_TYPE tktype);


using namespace llvm;

// function on llvm_backend.cpp
int link_object(FileObject &obj_file, ImportsHash &imports, const char* output_name, bool option_debug_info);

typedef Hash<TextType, DIFile*, 21, TextTypeHashFunc<21>, TextTypeComp> DIFilesHash;

// These should be wrapped in some kinda class... some day
// Will be needed for multithreading
struct LLVMInfo {
    std::unique_ptr<LLVMContext> TheContext;
    std::unique_ptr<IRBuilder<>> Builder;
    std::unique_ptr<Module> TheModule;
    std::unique_ptr<DIBuilder> DBuilder;
    std::unique_ptr<legacy::FunctionPassManager> fpass;
    legacy::PassManager pass;
    PassManagerBuilder PMBuilder;
    DICompileUnit* RootCU = nullptr;
    Function* llvm_function = nullptr;
    DIFilesHash filesHash;
    std::vector<DIScope*>dscope_vec;

    DIFile* getOrCreateDFile(BaseAST* ast);
    DIScope* getDebugScope();
    void pushDebugScope(DIScope* ds);
    void popDebugScope();
    DIScope* getGlobalDebugScope() { return RootCU; }
    void emitLocation(BaseAST* ast);
};

static LLVMInfo llinfo;

// Statics for the llvm type, to make code more readable
static Type *llvm_u32;
static Type *llvm_u64;
static Type* llvm_f32;
static Type* llvm_f64;

// These helpers are for new and delete to know what to call
// this could be done in the Interpreter
static VariableDeclarationAST* decl_malloc = nullptr;
static VariableDeclarationAST* decl_free = nullptr;

static void generateCode(BaseAST *ast);

void LLVMInfo::pushDebugScope(DIScope* ds) 
{
    dscope_vec.push_back(ds);
}

void LLVMInfo::popDebugScope()
{
    dscope_vec.pop_back();
}

DIFile* LLVMInfo::getOrCreateDFile(BaseAST* ast)
{
    if (!DBuilder) return nullptr;

    DIFile* root_file = nullptr;
    TextType filename = ast->filename;

    if (ast->filename == nullptr) {
        filename = (TextType)"./internal.rad";
    }

    if (filesHash.get(filename, root_file)) {
        return root_file;
    }

    fs::path p = filename;
    auto folder = p.parent_path();
    auto fname = p.filename();

    root_file = DBuilder->createFile(fname.string(), folder.string());
    filesHash.put(filename, root_file);

    return root_file;
}

DIScope* LLVMInfo::getDebugScope() {
    if (dscope_vec.empty()) return nullptr;
    return dscope_vec.back();
}

void LLVMInfo::emitLocation(BaseAST* ast)
{
    if (!DBuilder) return;

    if (!ast) return Builder->SetCurrentDebugLocation(DebugLoc());
    if (ast->line_num == 0) return Builder->SetCurrentDebugLocation(DebugLoc());
    DIScope* dscope = getDebugScope();
    if (dscope == nullptr) dscope = getOrCreateDFile(ast);
    unsigned int col = 0;
#ifdef PLATFORM_LINUX
    col = ast->char_num;
#endif    
    Builder->SetCurrentDebugLocation(llvm::DILocation::get(dscope->getContext(), ast->line_num, col, dscope));
}

static void printBasicBlock(BasicBlock *bb)
{    
    printf("  Block %s has %d instructions\n", bb->getName().str().c_str(), (int)bb->size());
}

static void printFunction(Function *f)
{
    for (auto it = f->getBasicBlockList().begin(); it != f->getBasicBlockList().end(); it++) {
        printBasicBlock(&(*it));
    }
    f->print(outs());
    outs().flush();
}

static PointerType *getLlvmPointer(TypeAST *type)
{
    if (isVoidType(type)) {
        return Type::getInt8PtrTy(*llinfo.TheContext);
    } else {
        return type->llvm_type->getPointerTo();
    }
}

static llvm::DIType* getLlvmDebugPointer(TypeAST* type)
{
    if (isVoidType(type)) {
        return llinfo.DBuilder->createPointerType(llinfo.DBuilder->createBasicType("u8", 8, dwarf::DW_ATE_unsigned), 64);
    } else {
        assert(type->debug_type != nullptr);
        return llinfo.DBuilder->createPointerType(type->debug_type, 64);
    }
}

static void generateFunctionPrototype(VariableDeclarationAST *decl)
{
    if (decl->codegen) return;

    // I am not sure this is needed
    // This could be just a function pointer
    assert(isFunctionDeclaration(decl));

    auto ft = (FunctionTypeAST *)decl->specified_type;
    // we need to define the function prototype here first
    // and then proceed with the body if we have it
    generateCode(decl->specified_type);
    auto llvm_type = (llvm::FunctionType *)decl->specified_type->llvm_type;
    Function *llvm_func = Function::Create(llvm_type, Function::ExternalLinkage,
        decl->varname, *llinfo.TheModule);
    // Got these for main from looking at clang
    if (!llinfo.fpass) {
        llvm_func->addFnAttr(llvm::Attribute::AttrKind::NoInline);
        llvm_func->addFnAttr(llvm::Attribute::AttrKind::NoRecurse);
        llvm_func->addFnAttr(llvm::Attribute::AttrKind::OptimizeNone);
    }
    llvm_func->addFnAttr(llvm::Attribute::AttrKind::NoUnwind);

    // ensure llvm knows the name of arguments
    u32 idx = 0;
    for (auto &llvm_arg : llvm_func->args()) {
        llvm_arg.setName(ft->arguments[idx]->varname);
        idx++;
    }

    decl->codegen = llvm_func;
}

static void allocateVariable(VariableDeclarationAST *decl)
{
    if (isFunctionDeclaration(decl)) {
        generateFunctionPrototype(decl);
    }

    // declared functions need no variable allocation
    if (isFunctionDefinition(decl)) return;
    if (isFunctionForeign(decl)) return;

    // we need the type of the variable to be part of llvm
    generateCode(decl->specified_type);

    // This catches structs and functions, might be redundant with the above
    if (isTypeDeclaration(decl)) return;

    if (isGlobalDeclaration(decl)) {
        bool isConstant = isConstantDeclaration(decl);
        llvm::Constant *initializer = nullptr;
        if (isConstant || decl->definition) {
            generateCode(decl->definition);
            initializer = (llvm::Constant *) decl->definition->codegen;
        } else {
            if (isTypeStruct(decl->specified_type) || isTypeArray(decl->specified_type)) {
                initializer = ConstantAggregateZero::get(decl->specified_type->llvm_type);
            } else {
                initializer = Constant::getNullValue(decl->specified_type->llvm_type);
            }
        }
        auto gv = new GlobalVariable(*llinfo.TheModule, decl->specified_type->llvm_type, isConstant,
            GlobalValue::LinkOnceAnyLinkage, initializer, decl->varname);
        decl->codegen = gv;
        // The last true parameter indicates if this is a static (local to unit or not)
        if (llinfo.DBuilder) {
            auto* dgv = llinfo.DBuilder->createGlobalVariableExpression(llinfo.getGlobalDebugScope(), decl->varname, decl->varname,
                llinfo.getOrCreateDFile(decl), decl->line_num, decl->specified_type->debug_type, false);
            gv->addDebugInfo(dgv);
        }
    } else {
        IRBuilder<> TmpB(&llinfo.llvm_function->getEntryBlock(),
            llinfo.llvm_function->getEntryBlock().begin());
        Value *arraySize = nullptr;
        auto AllocA = TmpB.CreateAlloca(decl->specified_type->llvm_type, arraySize, decl->varname);

		if (decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT) {
			// codegen should be here because it is generated in the function definition generation
			assert(decl->codegen);
			if (decl->codegen->getType()->isPointerTy() &&
				(isTypeArray(decl->specified_type) || isTypeStruct(decl->specified_type))) {
				// if it is an argument and a pointer, there is no need to do a local copy from arguments
				// for structs and arrays
				return;
			}
			TmpB.CreateStore(decl->codegen, AllocA);
		}
        decl->codegen = AllocA;
        if (llinfo.DBuilder) {
            llvm::DILocalVariable* dlv;
            auto* dbg_scope = llinfo.getDebugScope();
            if (decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT) {
                dlv = llinfo.DBuilder->createParameterVariable(dbg_scope, decl->varname, decl->llvm_index,
                    llinfo.getOrCreateDFile(decl), decl->line_num, decl->specified_type->debug_type);
            } else {
                // The last true parameter indicates if this is a static (local to unit or not)
                dlv = llinfo.DBuilder->createAutoVariable(dbg_scope, decl->varname,
                    llinfo.getOrCreateDFile(decl), decl->line_num, decl->specified_type->debug_type);
            }
            llinfo.DBuilder->insertDeclare(decl->codegen, dlv, llinfo.DBuilder->createExpression(),
                llvm::DILocation::get(dbg_scope->getContext(), decl->line_num, decl->char_num, dbg_scope),
                llinfo.Builder->GetInsertBlock());
        }
    }
}

static llvm::Type* getIdentifierType(IdentifierAST* id) 
{
    if (id->next != nullptr) {
        // this is the case of an ARRAY or STRUCT access, we need to return the type inside
        BaseAST* loop_ast = id->next;
        llvm::Type* rType = nullptr;
        while (loop_ast != nullptr) {
            switch (loop_ast->ast_type) {
                case AST_ARRAY_ACCESS: {
                    auto loop_aa = (ArrayAccessAST*)loop_ast;
                    ArrayTypeAST* atype = (ArrayTypeAST*)getDefinedType(loop_aa->prev);
                    assert(atype->ast_type == AST_ARRAY_TYPE);
                    rType = loop_aa->access_type->llvm_type;
                    loop_ast = loop_aa->next;
                    break;
                }
                case AST_STRUCT_ACCESS: {
                    auto loop_sac = (StructAccessAST*)loop_ast;
                    rType = loop_sac->decl->specified_type->llvm_type;
                    loop_ast = loop_sac->next;
                    break;
                }
                default:
                    assert(!"We should never get here!");
            }
        }
        return rType;
    } else {
        return id->decl->specified_type->llvm_type;
    }
}

static Value *generateIdentifierCode(IdentifierAST *id)
{
    if (id->next != nullptr) {
        // this is the case of an ARRAY or STRUCT access, we need GEP
        std::vector<Value *> idx;
        auto decl = id->decl;
        BaseAST *loop_ast = id->next;
        Value *variable = decl->codegen;
        TypeAST* currentType = decl->specified_type;
        Value *val = ConstantInt::get(llvm_u32, 0);
        llvm::Type* ltype = decl->specified_type->llvm_type;
        idx.push_back(val);
        Value* gep = nullptr;

        if (isTypePointer(id->decl->specified_type)) {
            // if our AST says we have a pointer, in llvm we have a pointer to pointer.
            // before we can compute anything, we need to dereference the pointer
            Value *gep = llinfo.Builder->CreateGEP(decl->specified_type->llvm_type, variable, idx);
            Value *deref_ptr = llinfo.Builder->CreateLoad(decl->specified_type->llvm_type, gep);
            variable = deref_ptr;
            PointerTypeAST* ptype = (PointerTypeAST*)id->decl->specified_type;
            currentType = ptype->points_to_type;
            generateCode(currentType);
            // We do not clear indices here, as indices should just have a single deref
            // idx.clear();
        }
        
        while(loop_ast != nullptr) {
            switch(loop_ast->ast_type) {
                case AST_ARRAY_ACCESS: {
                    auto loop_aa = (ArrayAccessAST *)loop_ast;
                    ArrayTypeAST *atype = (ArrayTypeAST *)getDefinedType(loop_aa->prev);
                    assert(atype->ast_type == AST_ARRAY_TYPE);
                    if (isSizedArray(atype) || isDynamicArray(atype)) {
                        // Sized and Dynamic arrays are implemented as a struct, meaning there
                        // is a pointer. Whenever there is a pointer, llvm requires to load it
                        // and then compute the GEP. Thus, one more level of indirection
                        idx.push_back(ConstantInt::get(Type::getInt32Ty(*llinfo.TheContext), 0));
                        // In this situation, llvm asks we load the pointer
                        Value *gep = llinfo.Builder->CreateGEP(currentType->llvm_type, variable, idx);
                        Value *array_pointer = llinfo.Builder->CreateLoad(getLlvmPointer(loop_aa->access_type), gep);
                        variable = array_pointer;
                        ltype = loop_aa->access_type->llvm_type;
                        idx.clear();
                    }
                    generateCode(loop_aa->array_exp);
                    idx.push_back(loop_aa->array_exp->codegen);
                    loop_ast = loop_aa->next;
                    break;
                }
                case AST_STRUCT_ACCESS: {
                    auto loop_sac = (StructAccessAST *)loop_ast;
                    val = ConstantInt::get(llvm_u32, loop_sac->decl->llvm_index);
                    ltype = currentType->llvm_type;
                    currentType = loop_sac->decl->specified_type;
                    idx.push_back(val);
                    loop_ast = loop_sac->next;
                    break;
                }
                default:
                    assert(!"We should never get here!");
            }

#if 0
            printf("\nGetIdentifier code for %s on %d:%d", id->name, id->line_num, id->char_num);
            printf("\nIndices: ");
            for (auto v : idx) {
                v->print(outs(), true);
            }
            printf("\ndeclaration: ");
            variable->print(outs(), true);
            printf("\ncurrentType: ");
            currentType->llvm_type->print(outs(), true);
            printf("\nltype: ");
            ltype->print(outs(), true);
            printf("\n");
#endif 

            gep = llinfo.Builder->CreateGEP(
                ltype,
                variable, idx);
            idx.clear();
            idx.push_back(ConstantInt::get(llvm_u32, 0));
            variable = gep;
        }

        return gep;
    } else {
        return id->decl->codegen;
    }
}

static Value *generateAssignLHSCode(ExpressionAST *expr)
{
    // Only unary operations can yield the correct type (and being a pointer)
    assert(expr->ast_type = AST_UNARY_OPERATION);
    auto unop = (UnaryOperationAST *)expr;
    assert(unop->op == TK_LSHIFT);
    // This should work, unop->expr has to be of pointer type 
    // (otherwise this operator would have failed), so we can just generate code
    // and return that. 
    generateCode(unop->expr);
    return unop->expr->codegen;
}

static Value *computeArrayDataPtr(IdentifierAST * array)
{
    assert(array->expr_type->ast_type == AST_ARRAY_TYPE);
    auto arType = (ArrayTypeAST *)array->expr_type;

    Value *array_val = generateIdentifierCode(array);

    if (isStaticArray(arType)) {
        // The address of `id` is the first field, the .data pointer already
        return array_val;
    } else {
        // For other array types, we need a dereference to get to .data
        std::vector<Value *> idx;
        Value *llvm_zero = ConstantInt::get(llvm_u32, 0);

        // If array is a simple identifier, it is a pointer in llvm that needs an extra index for GEP
        if (array->next == nullptr) idx.push_back(llvm_zero);
        idx.push_back(llvm_zero);
        Value *gep = llinfo.Builder->CreateGEP(arType->array_of_type->llvm_type, array_val, idx);
        return gep;
    }
}

static Value *computeArrayCount(IdentifierAST * array)
{
    assert(array->expr_type->ast_type == AST_ARRAY_TYPE);
    auto arType = (ArrayTypeAST *)array->expr_type;

    if (isStaticArray(arType)) {
        return ConstantInt::get(llvm_u64, arType->num_elems);
    }

    Value *array_val = generateIdentifierCode(array);

    std::vector<Value *> idx;
    Value *llvm_zero = ConstantInt::get(llvm_u32, 0);

    // If array is a simple identifier, it is a pointer in llvm that needs an extra index for GEP
    if (array->next == nullptr) idx.push_back(llvm_zero);
    Value *llvm_one = ConstantInt::get(llvm_u32, 1);
    idx.push_back(llvm_one);
    Value *gep = llinfo.Builder->CreateGEP(arType->array_of_type->llvm_type, array_val, idx);
    return gep;
}

static void generateVariableDeclaration(VariableDeclarationAST *decl)
{
    // Type variables do not need any further work
    if (decl->definition && decl->definition->ast_type == AST_STRUCT_DEFINITION) return;
    // all variables should have their alloc (or prototype) done already
    assert(decl->codegen);

    switch (decl->specified_type->ast_type) {
    case AST_FUNCTION_TYPE: {
        // @TODO: differentiate between top level functions
        // and lambda (these need rename)
        BasicBlock *oldBlock = nullptr;

        auto func_decl = (FunctionDefinitionAST *)decl->definition;
        auto ft = (FunctionTypeAST *)decl->specified_type;

        // If it is a Foreign function, nothing else to do;
        if (ft->isForeign) return;

        assert(func_decl);
        Function *llvm_func = (Function *)decl->codegen;

        if (!isGlobalDeclaration(decl)) {
            // save the current block where we are inserting
            oldBlock = llinfo.Builder->GetInsertBlock();
        }

        u32 arg_index = 0;
        auto& arg_decls = func_decl->function_body->block_scope.decls;
        for (auto& llvm_arg : llvm_func->args()) {
            arg_decls[arg_index++]->codegen = &llvm_arg;
        }
        // If it is not foreign, we should do the body here
        // Create a new basic block to start insertion into.
        BasicBlock *BB = BasicBlock::Create(*llinfo.TheContext, "entry", llvm_func);
        llinfo.Builder->SetInsertPoint(BB);

        Function *old_func = llinfo.llvm_function;
        llinfo.llvm_function = llvm_func;
        func_decl->llret_block = BasicBlock::Create(*llinfo.TheContext, "retblock");

        if (llinfo.DBuilder) {
            DISubroutineType* llvm_type = (DISubroutineType*)decl->specified_type->debug_type;
            DISubprogram* dsp = llinfo.DBuilder->createFunction(llinfo.getDebugScope(), decl->varname, StringRef(),
                llinfo.getOrCreateDFile(decl), decl->line_num, llvm_type, func_decl->function_body->line_num,
                DINode::FlagPrototyped, DISubprogram::SPFlagDefinition);
            llinfo.llvm_function->setSubprogram(dsp);

            // We copy this so the function body has the scope of the function
            func_decl->function_body->block_scope.debug_scope = dsp;
            llinfo.Builder->SetCurrentDebugLocation(llvm::DILocation::get(*llinfo.TheContext, func_decl->line_num, func_decl->char_num, dsp));
            llinfo.pushDebugScope(dsp);
            //        printf("Creating DebugLexical block %p from parent scope %p, For function %s scope %d:%d\n",
            //            dsp, decl->scope->debug_scope, decl->varname, decl->line_num, decl->char_num);
        }

        if (!isVoidType(ft->return_type)) {
            // We need to declare a local variable of the return type
            generateCode(ft->return_type);
            Value* arraySize = nullptr;
            auto retalloc = llinfo.Builder->CreateAlloca(ft->return_type->llvm_type, arraySize, "retval");
            func_decl->llret_alloc = retalloc;

            if (llinfo.DBuilder) {
                // The last true parameter indicates if this is a static (local to unit or not)
                auto* dlv = llinfo.DBuilder->createAutoVariable(llinfo.getDebugScope(), "retval",
                    llinfo.getOrCreateDFile(decl), decl->line_num, ft->return_type->debug_type);
                llinfo.DBuilder->insertDeclare(retalloc, dlv, llinfo.DBuilder->createExpression(),
                    llvm::DILocation::get(*llinfo.TheContext, decl->line_num, decl->char_num, llinfo.getDebugScope()),
                    llinfo.Builder->GetInsertBlock());
            }
        }

        generateCode(func_decl->function_body);

        if (func_decl->num_return_statements > 0) {
            llinfo.Builder->CreateBr(func_decl->llret_block);
            llvm_func->getBasicBlockList().push_back(func_decl->llret_block);
            llinfo.Builder->SetInsertPoint(func_decl->llret_block);
        }
        if (isVoidType(ft->return_type)) {
            llinfo.Builder->CreateRetVoid();
        } else {
            auto local_ret = llinfo.Builder->CreateLoad(ft->return_type->llvm_type, func_decl->llret_alloc);
            llinfo.Builder->CreateRet(local_ret);
        }
        
        if (llinfo.DBuilder) llinfo.popDebugScope();

        // Debugging
        // printf("\n***************** FUNC %s ****************\n", decl->varname);
        // printFunction(llvm_func);
        // printf("\n***************** FUNC ****************\n");

        verifyFunction(*llvm_func);
        if (llinfo.fpass) {
//            llinfo.fpass->doInitialization();
            llinfo.fpass->run(*llvm_func);
//            llinfo.fpass->doFinalization();
        }

        llinfo.llvm_function = old_func;

        if (oldBlock) {
            llinfo.Builder->SetInsertPoint(oldBlock);
        }

        break;
    }
    case AST_DIRECT_TYPE: {
        // Global variables do not require the initialization, it is done in another section.
        if (isGlobalDeclaration(decl)) return;

        generateCode(decl->specified_type);
        // The variable has already been allocated, now we might have to initialize it
        if (!decl->definition) return;

        generateCode(decl->definition);
        llinfo.Builder->CreateStore(decl->definition->codegen, decl->codegen);

        break;
    }
    case AST_POINTER_TYPE: {
        // @TODO: check if we can merge POINTER TYPE with DIRECT_TYPE
        // Global variables do not require the initialization, it is done in another section.
        if (isGlobalDeclaration(decl)) return;

        generateCode(decl->specified_type);
        // The variable has already been allocated, now we might have to initialize it
        if (!decl->definition) return;

        generateCode(decl->definition);
        llinfo.Builder->CreateStore(decl->definition->codegen, decl->codegen);

        break;
    }
    case AST_ARRAY_TYPE: {
        // Global variables do not require the initialization, it is done in another section.
        if (isGlobalDeclaration(decl)) return;

        generateCode(decl->specified_type);
        // The variable has already been allocated, now we might have to initialize it
        if (isStaticArray(decl->specified_type)) {
            // Static arrays have their allocation created when looping over declarations
            return;
            /*
            std::vector<Value *>idx;
            Value *val = ConstantInt::get(llvm_u32, 0);
            idx.push_back(val);
//            idx.push_back(val);
            Value *gep = llinfo.Builder->CreateGEP(decl->llvm_storage, idx);
            llinfo.Builder->CreateStore(gep, decl->codegen);
            */
        }

        if (!decl->definition) return;

        generateCode(decl->definition);
        llinfo.Builder->CreateStore(decl->definition->codegen, decl->codegen);

        break;
    }
    default:
        assert(!"Type of variable declaration on llvm is not supported!");
    }
}

static void generateIfStatement(IfStatementAST *ifst)
{
    // This code is taken from http://llvm.org/docs/tutorial/LangImpl05.html
    generateCode(ifst->condition);

    Value *cond = ifst->condition->codegen;
    Function *lfunc = llinfo.Builder->GetInsertBlock()->getParent();

    BasicBlock *then_block = BasicBlock::Create(*llinfo.TheContext, "if.then", lfunc);
    BasicBlock *merge_block = BasicBlock::Create(*llinfo.TheContext, "if.end");
    BasicBlock* else_block = merge_block;
    if (ifst->else_branch) {
        else_block = BasicBlock::Create(*llinfo.TheContext, "if.else");
    }

    llinfo.Builder->CreateCondBr(cond, then_block, else_block);

    llinfo.Builder->SetInsertPoint(then_block);

    generateCode(ifst->then_branch);
    Value *then_val = ifst->then_branch->codegen;

    if (!ifst->then_branch_return) {
        // Only jump to the merge block if we do not have a return that does it
        llinfo.Builder->CreateBr(merge_block);
    }
    then_block = llinfo.Builder->GetInsertBlock();

    if (ifst->else_branch) {
        llinfo.llvm_function->getBasicBlockList().push_back(else_block);
        llinfo.Builder->SetInsertPoint(else_block);
        generateCode(ifst->else_branch);
        if (!ifst->then_branch_return) {
            llinfo.Builder->CreateBr(merge_block);
        }
        else_block = llinfo.Builder->GetInsertBlock();
    }

    llinfo.llvm_function->getBasicBlockList().push_back(merge_block);
    llinfo.Builder->SetInsertPoint(merge_block);
}

static void generateForStatement(ForStatementAST *forst)
{
    if (llinfo.DBuilder) {
        forst->for_scope.debug_scope = llinfo.DBuilder->createLexicalBlock(llinfo.getDebugScope(),
            llinfo.getOrCreateDFile(forst), forst->line_num, forst->char_num);
        llinfo.pushDebugScope(forst->for_scope.debug_scope);
    }
//    printf("Creating DebugLexical block %p from parent scope %p, For scope %d:%d\n",
//        forst->for_scope.debug_scope, forst->scope->debug_scope, forst->line_num, forst->char_num);

    for (auto decl : forst->for_scope.decls) {
        allocateVariable(decl);
    }

    Function *lfunc = llinfo.Builder->GetInsertBlock()->getParent();
    BasicBlock *for_entry = BasicBlock::Create(*llinfo.TheContext, "for_entry", lfunc);
    BasicBlock *for_block = BasicBlock::Create(*llinfo.TheContext, "for_block", lfunc);
    BasicBlock *for_after = BasicBlock::Create(*llinfo.TheContext, "for_after", lfunc);

	if (forst->it->decl->definition) {
		generateCode(forst->it->decl->definition);
		llinfo.Builder->CreateStore(forst->it->decl->definition->codegen, forst->it->decl->codegen);
	}
	if (forst->it_index->decl->definition) {
		generateCode(forst->it_index->decl->definition);
		llinfo.Builder->CreateStore(forst->it_index->decl->definition->codegen, forst->it_index->decl->codegen);
	}

    if (forst->is_array) {
        llinfo.Builder->CreateBr(for_entry);
        llinfo.Builder->SetInsertPoint(for_entry);

        Value *array_count = computeArrayCount(forst->arr);
        Value *it_index1 = llinfo.Builder->CreateLoad(forst->it_index->decl->specified_type->llvm_type, forst->it_index->decl->codegen, forst->it_index->name);
        Value *for_check = llinfo.Builder->CreateICmpUGE(it_index1, array_count);

        llinfo.Builder->CreateCondBr(for_check, for_after, for_block);
        llinfo.Builder->SetInsertPoint(for_block);

        // load it with the correct value
        Value *array_ptr = computeArrayDataPtr(forst->arr);
        auto arType = (ArrayTypeAST*)forst->arr->expr_type;

        std::vector<Value *> idx;
        Value* it_index2 = llinfo.Builder->CreateLoad(forst->it_index->decl->specified_type->llvm_type, forst->it_index->decl->codegen, forst->it_index->name);
        Value *llvm_zero = ConstantInt::get(llvm_u64, 0);
        idx.push_back(llvm_zero);
        idx.push_back(it_index2);
        Value *it_ptr = llinfo.Builder->CreateGEP(arType->llvm_type, array_ptr, idx, forst->it->name);
        if (forst->is_it_ptr) {
            llinfo.Builder->CreateStore(it_ptr, forst->it->decl->codegen);
        } else {
            Value *it_val = llinfo.Builder->CreateLoad(forst->it->decl->specified_type->llvm_type, it_ptr);
            llinfo.Builder->CreateStore(it_val, forst->it->decl->codegen);
        }

        // Do the actual loop code
        generateCode(forst->loop_block);

        if (!forst->for_block_has_return) {
            BasicBlock* for_incr = BasicBlock::Create(*llinfo.TheContext, "for_incr", lfunc);

            llinfo.Builder->CreateBr(for_incr);
            llinfo.Builder->SetInsertPoint(for_incr);

            // Do the loop increment, and then inconditional jump
            Value* llvm_one = ConstantInt::get(llvm_u64, 1);
            Value* it_index_val = llinfo.Builder->CreateLoad(getIdentifierType(forst->it_index), generateIdentifierCode(forst->it_index), forst->it_index->name);
            Value* it_index_inc = llinfo.Builder->CreateAdd(it_index_val, llvm_one, "inc");
            llinfo.Builder->CreateStore(it_index_inc, forst->it_index->decl->codegen);

            llinfo.Builder->CreateBr(for_entry);
        }
    } else {
        generateCode(forst->end);
        Value *end_val = forst->end->codegen;

        llinfo.Builder->CreateBr(for_entry);
        llinfo.Builder->SetInsertPoint(for_entry);

        Value *it_val = llinfo.Builder->CreateLoad(getIdentifierType(forst->it), generateIdentifierCode(forst->it), forst->it->name);
        Value *for_check;
        assert(forst->end->expr_type->ast_type == AST_DIRECT_TYPE);
        auto dtype = (DirectTypeAST *)forst->end->expr_type;
        if (dtype->isSigned) {
            for_check = llinfo.Builder->CreateICmpSGT(it_val, end_val);
        } else {
            for_check = llinfo.Builder->CreateICmpUGT(it_val, end_val);
        }

        llinfo.Builder->CreateCondBr(for_check, for_after, for_block);
        llinfo.Builder->SetInsertPoint(for_block);
        // Do the actual loop code
        generateCode(forst->loop_block);

        if (!forst->for_block_has_return) {
            BasicBlock* for_incr = BasicBlock::Create(*llinfo.TheContext, "for_incr", lfunc);
            llinfo.Builder->CreateBr(for_incr);
            llinfo.Builder->SetInsertPoint(for_incr);

            // increment the it, it_index. It could have changed
            it_val = llinfo.Builder->CreateLoad(getIdentifierType(forst->it), generateIdentifierCode(forst->it), forst->it->name);
            Value* llvm_one = ConstantInt::get(llvm_u64, 1);
            Value* it_inc = llinfo.Builder->CreateAdd(it_val, llvm_one, "inc");
            llinfo.Builder->CreateStore(it_inc, forst->it->decl->codegen);

            Value* it_index_val = llinfo.Builder->CreateLoad(getIdentifierType(forst->it_index), generateIdentifierCode(forst->it_index), forst->it_index->name);
            Value* it_index_inc = llinfo.Builder->CreateAdd(it_index_val, llvm_one, "inc");
            llinfo.Builder->CreateStore(it_index_inc, forst->it_index->decl->codegen);

            llinfo.Builder->CreateBr(for_entry);
        }
    }
    llinfo.Builder->SetInsertPoint(for_after);

    if (llinfo.DBuilder) {
        llinfo.popDebugScope();
    }
}

static void generateLiteral(LiteralAST *lit)
{
    generateCode(lit->typeAST);
    switch (lit->typeAST->basic_type) {
    case BASIC_TYPE_INTEGER: {
        auto llvm_val = ConstantInt::get(
            lit->typeAST->llvm_type,
            APInt(lit->typeAST->size_in_bytes * 8,
            (lit->typeAST->isSigned ? lit->_s64 : lit->_u64),
                lit->typeAST->isSigned));
        lit->codegen = llvm_val;
        break;
    }
    case BASIC_TYPE_FLOATING: {
        auto llvm_val = ConstantFP::get(lit->typeAST->llvm_type, lit->_f64);
        lit->codegen = llvm_val;
        break;
    }
    case BASIC_TYPE_BOOL: {
        auto llvm_val = ConstantInt::get(lit->typeAST->llvm_type, APInt(1, lit->_bool));
        lit->codegen = llvm_val;
        break;
    }
    case BASIC_TYPE_STRING: {
        auto llvm_str = (Constant *)llinfo.Builder->CreateGlobalStringPtr(lit->str);
        auto llvm_sz = ConstantInt::get(llvm_u64, strlen(lit->str));
        std::vector<Constant *> members;
        members.push_back(llvm_str);
        members.push_back(llvm_sz);
        auto llvm_struct = ConstantStruct::get((StructType *)lit->typeAST->llvm_type, members);
        lit->codegen = llvm_struct;
        break;
    }
    default:
        assert(!"Unknonw literal type!");
    }
}

static void generateBinaryOperation(BinaryOperationAST *binop)
{
    generateCode(binop->lhs);
    generateCode(binop->rhs);
    switch (binop->op) {
    case TK_EQ: {
        if (isTypeInteger(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type) ||
            isTypePointer(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpEQ(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpOEQ(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for EQ comparison!");
        }
        break;
    }
    case TK_LEQ: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeInteger(binop->lhs->expr_type)) {
            auto dt = (DirectTypeAST *)binop->lhs->expr_type;
            if (dt->isSigned) {
                binop->codegen = llinfo.Builder->CreateICmpSLE(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                binop->codegen = llinfo.Builder->CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
            }
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpOLE(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for LEQ comparison!");
        }
        break;
    }
    case TK_GEQ: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeInteger(binop->lhs->expr_type)) {
            auto dt = (DirectTypeAST *)binop->lhs->expr_type;
            if (dt->isSigned) {
                binop->codegen = llinfo.Builder->CreateICmpSGE(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                binop->codegen = llinfo.Builder->CreateICmpUGE(binop->lhs->codegen, binop->rhs->codegen);
            }
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpOGE(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for GEQ comparison!");
        }
        break;
    }
    case TK_NEQ: {
        if (isTypeInteger(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type) ||
            isTypePointer(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpNE(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpONE(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for NEQ comparison!");
        }
        break;
    }
    case TK_LT: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpULT(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeInteger(binop->lhs->expr_type)) {
            auto dt = (DirectTypeAST *)binop->lhs->expr_type;
            if (dt->isSigned) {
                binop->codegen = llinfo.Builder->CreateICmpSLT(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                binop->codegen = llinfo.Builder->CreateICmpULT(binop->lhs->codegen, binop->rhs->codegen);
            }
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpOLT(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for LT comparison!");
        }
        break;
    }
    case TK_GT: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateICmpUGT(binop->lhs->codegen, binop->rhs->codegen);
        } else if (isTypeInteger(binop->lhs->expr_type)) {
            auto dt = (DirectTypeAST *)binop->lhs->expr_type;
            if (dt->isSigned) {
                binop->codegen = llinfo.Builder->CreateICmpSGT(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                binop->codegen = llinfo.Builder->CreateICmpUGT(binop->lhs->codegen, binop->rhs->codegen);
            }
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFCmpOGT(binop->lhs->codegen, binop->rhs->codegen);
        } else {
            assert(!"Type not supported for GT comparison!");
        }
        break;
    }
    case TK_STAR: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            assert(!"Pointers and booleans cannot be multiplied!");
        } else if (isTypeInteger(binop->lhs->expr_type) || isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateMul(binop->lhs->codegen, binop->rhs->codegen, "mul");
        } else {
            assert(!"Type not supported for division!");
        }
        break;
    }
    case TK_DIV: {
        if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
            assert(!"Pointers and booleans cannot be divided!");
        } else if (isTypeInteger(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateUDiv(binop->lhs->codegen, binop->rhs->codegen, "udiv");
        } else if (isTypeFloating(binop->lhs->expr_type)) {
            binop->codegen = llinfo.Builder->CreateFDiv(binop->lhs->codegen, binop->rhs->codegen, "fdiv");
        } else {
            assert(!"Type not supported for division!");
        }
        break;
    }
    case TK_MOD: {
        assert(!"TK_MOD has not been implemented for llvm!");
        break;
    }
    case TK_PLUS: {
        binop->codegen = llinfo.Builder->CreateAdd(binop->lhs->codegen, binop->rhs->codegen, "add");
        break;
    }
    case TK_MINUS: {
        binop->codegen = llinfo.Builder->CreateSub(binop->lhs->codegen, binop->rhs->codegen, "sub");
        break;
    }
    default:
        assert(!"Token operation is not recognized/supported!");
    }
}

static void generateCode(BaseAST *ast)
{
    llinfo.emitLocation(ast);

    switch (ast->ast_type)
    {
    case AST_FILE: {
        auto f = (FileAST *)ast;

        for (auto decl : f->global_scope.decls) {
            allocateVariable(decl);
        }

        for (auto item : f->items) {
            switch (item->ast_type) {
            case AST_VARIABLE_DECLARATION: {
                generateCode(item);
                break;
            }
            case AST_RUN_DIRECTIVE: {
                // Run directives either have already been executed, or in top level they do nothing
                break;
            }
            default:
                assert(!"Unsupported type on top level expression");
            }
        }
        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto stmt_block = (StatementBlockAST *)ast;

        if (stmt_block->codegen) return;

        bool need_pop_scope = false;

        if (llinfo.DBuilder) {
            if (stmt_block->block_scope.debug_scope == nullptr) {
                // A normal block would not have a debug scope, but this block is the body of a function, 
                // then we want to use the function debug scope
                stmt_block->block_scope.debug_scope = llinfo.DBuilder->createLexicalBlock(llinfo.getDebugScope(),
                    llinfo.getOrCreateDFile(stmt_block), stmt_block->line_num, stmt_block->char_num);
                llinfo.pushDebugScope(stmt_block->block_scope.debug_scope);
                need_pop_scope = true;
                //            printf("Creating DebugLexical block %p from parent scope %p, Statement scope %d:%d\n",
                //                stmt_block->block_scope.debug_scope, stmt_block->scope->debug_scope, stmt_block->line_num, stmt_block->char_num);
            }
            // It is important that we set the debug location to that of this statement block (could be a function)
            // Otherwise, it would use the parent and that is not valid
            llinfo.emitLocation(ast);
        }

        for (auto decl : stmt_block->block_scope.decls) {
            allocateVariable(decl);
        }

        for (auto item : stmt_block->statements) {
            generateCode(item);
        }

        if (need_pop_scope) llinfo.popDebugScope();
        break;
    }
    case AST_IF_STATEMENT: {
        auto ifst = (IfStatementAST *)ast;
        generateIfStatement(ifst);
        break;
    }
    case AST_FOR_STATEMENT: {
        auto forst = (ForStatementAST *)ast;
        generateForStatement(forst);
        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret_stmt = (ReturnStatementAST *)ast;
        auto* fundef = findEnclosingFunction(ret_stmt);
        if (!ret_stmt->isFunctionLevel) fundef->num_return_statements++;
        if (ret_stmt->ret != nullptr) {
            generateCode(ret_stmt->ret);
            llinfo.emitLocation(ret_stmt);
            llinfo.Builder->CreateStore(ret_stmt->ret->codegen, fundef->llret_alloc);
        } 
        if (!ret_stmt->isFunctionLevel) llinfo.Builder->CreateBr(findEnclosingFunction(ret_stmt)->llret_block);
        break;
    }
    case AST_FUNCTION_DEFINITION: {
        auto fundef = (FunctionDefinitionAST *)ast;
        assert(!"LLVM function definition is not implemented!");
        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        // Print in C expects all floats to be printed as double... 
        bool needs_print_ext = !strcmp(funcall->function_name, "print");
        std::vector<Value *> ArgsV;
        for (auto arg : funcall->args) {
            generateCode(arg);
            auto* topush = arg->codegen;
            assert(arg->codegen);
            if (needs_print_ext && arg->expr_type->ast_type == AST_DIRECT_TYPE) {
                auto dtype = (DirectTypeAST*)arg->expr_type;
                if (dtype->basic_type == BASIC_TYPE_FLOATING && dtype->size_in_bytes == 4) {
                    topush = llinfo.Builder->CreateFPExt(arg->codegen, llvm_f64);
                } else if (dtype->basic_type == BASIC_TYPE_INTEGER && (dtype->size_in_bytes < 4)) {
                    if (dtype->isSigned) {
                        topush = llinfo.Builder->CreateSExt(arg->codegen, llvm_u32);
                    } else {
                        topush = llinfo.Builder->CreateZExt(arg->codegen, llvm_u32);
                    }
                }
            }
            ArgsV.push_back(topush);
        }
        // The function call should not have a name if the return value is void
        auto llvm_call = llinfo.Builder->CreateCall((llvm::Function *)funcall->fundef->var_decl->codegen, ArgsV);
        funcall->codegen = llvm_call;
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;
        id->codegen = llinfo.Builder->CreateLoad(getIdentifierType(id), generateIdentifierCode(id), id->name);
        break;
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)ast;
        generateLiteral(lit);
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        generateBinaryOperation(binop);
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        switch (unop->op) {
        case TK_LSHIFT: {
            generateCode(unop->expr);
            assert(unop->expr->expr_type->ast_type == AST_POINTER_TYPE);
            PointerTypeAST* pt = (PointerTypeAST*)unop->expr->expr_type;
            unop->codegen = llinfo.Builder->CreateLoad(pt->points_to_type->llvm_type, unop->expr->codegen);
            break;
        }
        case TK_BANG: {
            generateCode(unop->expr);
            unop->codegen = llinfo.Builder->CreateNot(unop->expr->codegen, "not");
            break;
        }
        case TK_STAR: {
            assert(unop->expr->ast_type == AST_IDENTIFIER);
            auto id = (IdentifierAST *)unop->expr;
            unop->codegen = generateIdentifierCode(id);
            break;
        }
        default:            
            assert(!"Unary operator not implemented in llvm");
            exit(1);
        }
        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)ast;
        // This can work for identifiers but not for arrays or structs
        generateCode(assign->rhs);
        llvm::Value* store_val = nullptr;
        if (assign->lhs->ast_type == AST_IDENTIFIER) {
            auto id = (IdentifierAST *)assign->lhs;
            store_val = generateIdentifierCode(id);
        } else {
            store_val = generateAssignLHSCode(assign->lhs);
        }
        llinfo.emitLocation(assign);
        llinfo.Builder->CreateStore(assign->rhs->codegen, store_val);
        break;
    }
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;
        generateVariableDeclaration(decl);
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto defn = (StructDefinitionAST *)ast;
        assert(!"Struct definition is not supported on llvm yet");
        break;
    }
    case AST_ARRAY_ACCESS: {
        auto aa = (ArrayAccessAST *)ast;        
        assert(!"Array access should never be processed like this, it comes from Identifier");
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;
        assert(!"Struct access should never be processed like this, it comes from Identifier");
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto ftype = (FunctionTypeAST *)ast;
        std::vector<Type *> func_args;
        generateCode(ftype->return_type);
        // Extend this to support functions that return arrays or structs
        assert(!isTypeArray(ftype->return_type) && !isTypeStruct(ftype->return_type));

        for (auto arg : ftype->arguments) {
            generateCode(arg->specified_type);
            // Complex data structures are pointers in llvm
            if (isTypeArray(arg->specified_type) || isTypeStruct(arg->specified_type)) {
                func_args.push_back(arg->specified_type->llvm_type->getPointerTo());
            } else {
                func_args.push_back(arg->specified_type->llvm_type);
            }
        }
        generateCode(ftype->return_type);
        auto llvm_ft = FunctionType::get(ftype->return_type->llvm_type, // return type
            func_args,
            ftype->hasVariableArguments);

        ftype->llvm_type = llvm_ft;

        if (llinfo.DBuilder) {
            std::vector<Metadata*> debug_args;
           // debug_args.push_back(ftype->return_type->debug_type);
            if (isVoidType(ftype->return_type)) {
                debug_args.push_back(nullptr);
            } else {
                debug_args.push_back(ftype->return_type->debug_type);
            }
            for (auto arg : ftype->arguments) {
                // Complex data structures are pointers in llvm
                if (isTypeArray(arg->specified_type) || isTypeStruct(arg->specified_type)) {
                    debug_args.push_back(getLlvmDebugPointer(arg->specified_type));
                } else {
                    debug_args.push_back(arg->specified_type->debug_type);
                }
            }

            ftype->debug_type = llinfo.DBuilder->createSubroutineType(llinfo.DBuilder->getOrCreateTypeArray(debug_args));
        }
        break;
    }
    case AST_DIRECT_TYPE: {
        auto dtype = (DirectTypeAST *)ast;
        if (dtype->llvm_type) return;

        switch (dtype->basic_type) {
        case BASIC_TYPE_BOOL: {
            dtype->llvm_type = Type::getInt1Ty(*llinfo.TheContext);
            if (llinfo.DBuilder) dtype->debug_type = llinfo.DBuilder->createBasicType("bool", 1, dwarf::DW_ATE_boolean);
            break;
        }
        case BASIC_TYPE_CUSTOM: {
            assert(dtype->custom_type != nullptr);
            if (dtype->custom_type->llvm_type == nullptr) generateCode(dtype->custom_type);
            dtype->llvm_type = dtype->custom_type->llvm_type;
            if (llinfo.DBuilder) dtype->debug_type = dtype->custom_type->debug_type;
            break;
        }
        case BASIC_TYPE_FLOATING: {
            switch (dtype->size_in_bytes) {
            case 4: {
                dtype->llvm_type = llvm_f32;
                if (llinfo.DBuilder) dtype->debug_type = llinfo.DBuilder->createBasicType("f32", 32, dwarf::DW_ATE_float);
                break;
            }
            case 8: {
                dtype->llvm_type = llvm_f64;
                if (llinfo.DBuilder) dtype->debug_type = llinfo.DBuilder->createBasicType("f64", 64, dwarf::DW_ATE_float);
                break;
            }
            default:
                assert(!"Unsupported byte number for floating point");
            }
            break;
        }
        case BASIC_TYPE_INTEGER: {
            switch (dtype->size_in_bytes) {
            case 1: {
                dtype->llvm_type = Type::getInt8Ty(*llinfo.TheContext);
                if (llinfo.DBuilder) {
                    if (dtype->isSigned) {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("s8", 8, dwarf::DW_ATE_signed);
                    } else {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("u8", 8, dwarf::DW_ATE_unsigned);
                    }
                }
                break;
            }
            case 2: {
                dtype->llvm_type = Type::getInt16Ty(*llinfo.TheContext);
                if (llinfo.DBuilder) {
                    if (dtype->isSigned) {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("s16", 16, dwarf::DW_ATE_signed);
                    } else {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("u16", 16, dwarf::DW_ATE_unsigned);
                    }
                }
                break;
            }
            case 4: {
                dtype->llvm_type = Type::getInt32Ty(*llinfo.TheContext);
                if (llinfo.DBuilder) {
                    if (dtype->isSigned) {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("s32", 32, dwarf::DW_ATE_signed);
                    } else {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("u32", 32, dwarf::DW_ATE_unsigned);
                    }
                }
                break;
            }
            case 8: {
                dtype->llvm_type = Type::getInt64Ty(*llinfo.TheContext);
                if (llinfo.DBuilder) {
                    if (dtype->isSigned) {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("s64", 64, dwarf::DW_ATE_signed);
                    } else {
                        dtype->debug_type = llinfo.DBuilder->createBasicType("u64", 64, dwarf::DW_ATE_unsigned);
                    }
                }
                break;
            }
            default:
                assert(!"Unsupported byte number for integer");
            }
            break;
        }
        case BASIC_TYPE_STRING: {
            // A string is a special case of a struct type
            std::vector<Type *> struct_members;
            struct_members.push_back(Type::getInt8PtrTy(*llinfo.TheContext)); // the char *
            struct_members.push_back(Type::getInt64Ty(*llinfo.TheContext)); // the size
            dtype->llvm_type = StructType::create(*llinfo.TheContext, struct_members, "Rad_string");
            if (llinfo.DBuilder) {

                // Ok, this is rather complicated. For debug struct types in llvm, is needed to 
                // first create the container object, but without members. 
                auto* composite_type = llinfo.DBuilder->createStructType(llinfo.getDebugScope(), "Rad_string",
                    llinfo.getOrCreateDFile(dtype), dtype->line_num, 128, 0, DINode::FlagZero, nullptr,
                    DINodeArray());
                // Now we create the elements
                std::vector<Metadata*> debug_struct_members;
                DirectTypeAST* s8type = getBuiltInType(TK_S8);
                if (s8type->debug_type == nullptr) generateCode(s8type);
                debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, "data", nullptr, 0, 64, 0, 0,
                    DINode::FlagZero, getLlvmDebugPointer(s8type)));
                DirectTypeAST* s64type = getBuiltInType(TK_S64);
                if (s64type->debug_type == nullptr) generateCode(s64type);
                debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, "count", nullptr, 0, 64, 0, 64,
                    DINode::FlagZero, s64type->debug_type));
                // Create the array the way they like it, and replace the arrays           
                llinfo.DBuilder->replaceArrays(composite_type, llinfo.DBuilder->getOrCreateArray(debug_struct_members));
                dtype->debug_type = composite_type;
            }
            break;
        }
        case BASIC_TYPE_VOID: {
            dtype->llvm_type = Type::getVoidTy(*llinfo.TheContext);
            if (llinfo.DBuilder) {
                // Actually, void* does not exist in LLVM, we use u8*. 
                // Mostly this is used when we have a void*. 
                dtype->debug_type = llinfo.DBuilder->createBasicType("u8", 8, dwarf::DW_ATE_unsigned);
            }
            return;
        }
        default:
            assert(!"Unknonw basic type on llvm generation");
        }
        break;
    }
    case AST_POINTER_TYPE: {
        auto ptype = (PointerTypeAST *)ast;
        if (ptype->llvm_type) return;
        if (!isDirectType(ptype->points_to_type)) {
            assert(!"Only pointers to direct types are supported for now");
        }
        auto dtype = (DirectTypeAST *)ptype->points_to_type;
        if ((dtype->basic_type == BASIC_TYPE_CUSTOM) && isTypeStruct(dtype->custom_type)) {
            auto stype = (StructTypeAST *)dtype->custom_type;
            // shortcut pointers to structs
            if (stype->llvm_type) {
                ptype->llvm_type = stype->llvm_type->getPointerTo();
                if (llinfo.DBuilder) ptype->debug_type = getLlvmDebugPointer(stype);
            } else {
                // When a struct has a self reference pointer, we are in this case
                Type *tp = StructType::create(*llinfo.TheContext, stype->decl->varname);
                stype->llvm_type = tp;
                ptype->llvm_type = tp->getPointerTo();
            }
        } else {
            generateCode(dtype);
            assert(dtype->llvm_type != nullptr);
            ptype->llvm_type = getLlvmPointer(dtype);
            // Musings, should there be a centralize unique pointer type for a given type?
            if (llinfo.DBuilder) ptype->debug_type = getLlvmDebugPointer(dtype);
        }
        break;
    }
    case AST_ARRAY_TYPE: {
        auto atype = (ArrayTypeAST *)ast;
        if (atype->llvm_type != nullptr) return;
        generateCode(atype->array_of_type);
        assert(atype->array_of_type->llvm_type != nullptr);
        if (isStaticArray(atype)) {
            atype->llvm_type = ArrayType::get(atype->array_of_type->llvm_type, atype->num_elems);
            if (llinfo.DBuilder) {
                SmallVector<Metadata*, 1> subrange;
                subrange.push_back(
                    llinfo.DBuilder->getOrCreateSubrange(0, atype->num_elems - 1));
                atype->debug_type = llinfo.DBuilder->createArrayType(atype->num_elems, 0,
                    atype->array_of_type->debug_type, llinfo.DBuilder->getOrCreateArray(subrange));
            }
        } else {
            // common parts for sized and dynamic arrays
            std::vector<Type *> array_members;
            u64 internal_struct_size = 128; // .data and count
            array_members.push_back(getLlvmPointer(atype->array_of_type)); // .data
            array_members.push_back(Type::getInt64Ty(*llinfo.TheContext)); // This is .count
            const char *llvm_name = "SizedArray";
            if (isDynamicArray(atype)) {
                array_members.push_back(Type::getInt64Ty(*llinfo.TheContext)); // This is .reserved_size                
                llvm_name = "DynamicArray";
                internal_struct_size += 64;
            }
            atype->llvm_type = StructType::create(*llinfo.TheContext, array_members, llvm_name);

            if (llinfo.DBuilder) {
                // Ok, this is rather complicated. For debug struct types in llvm, is needed to 
                // first create the container object, but without members. 
                auto* composite_type = llinfo.DBuilder->createStructType(llinfo.getDebugScope(), llvm_name,
                    llinfo.getOrCreateDFile(atype), atype->line_num, internal_struct_size, 0, DINode::FlagZero, nullptr,
                    DINodeArray());
                // Now we create the elements
                SmallVector<Metadata*, 3> debug_struct_members;
                debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, "data", nullptr, 0, 64, 0, 0,
                    DINode::FlagZero, getLlvmDebugPointer(atype->array_of_type)));
                DirectTypeAST* s64type = getBuiltInType(TK_S64);
                if (s64type->debug_type == nullptr) generateCode(s64type);
                debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, "count", nullptr, 0, 64, 0, 64,
                    DINode::FlagZero, s64type->debug_type));
                if (isDynamicArray(atype)) {
                    debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, "reserved_size", nullptr,
                        0, 64, 0, 128, DINode::FlagZero, s64type->debug_type));
                }
                // Create the array the way they like it, and replace the arrays           
                llinfo.DBuilder->replaceArrays(composite_type, llinfo.DBuilder->getOrCreateArray(debug_struct_members));
                atype->debug_type = composite_type;
            }
        }
        break;
    }
    case AST_STRUCT_TYPE: {
        auto stype = (StructTypeAST *)ast;
        std::vector<Type *> struct_members;

        if (stype->llvm_type) {
            // how does this play with debug types?
            auto llvm_stype = static_cast<StructType *>(stype->llvm_type);
            if (!llvm_stype->isOpaque()) {
                if (llinfo.DBuilder) assert(stype->debug_type);
                break;
            }
        }

        for (auto decl : stype->struct_scope.decls) {
            generateCode(decl->specified_type);
            struct_members.push_back(decl->specified_type->llvm_type);
        }
        if (stype->llvm_type) {
            auto llvm_stype = static_cast<StructType *>(stype->llvm_type);
            llvm_stype->setBody(struct_members);            
        } else {
            stype->llvm_type = StructType::create(*llinfo.TheContext, struct_members, stype->decl->varname);
        }

        if (llinfo.DBuilder) {
            // This poses a problem. In llvm, the correct way to get the size in bits is using the built
            // llvm type with Layout, but since we generate llvm and debug types at the same time, if we
            // had a struct with a pointer to itself, this would fail. We could assume we know the type size in bits
            // to get past this, or do dual traversal, one for llvm and one for debug types
            // For now, we use our size and no alignments
            auto& Layout = llinfo.TheModule->getDataLayout();
            auto composite_type = llinfo.DBuilder->createStructType(llinfo.getDebugScope(), stype->decl->varname,
                llinfo.getOrCreateDFile(stype), stype->line_num, stype->size_in_bytes * 8,
                0, DINode::FlagZero, nullptr, DINodeArray());
            stype->debug_type = composite_type;

            std::vector<Metadata*> debug_struct_members;
            u64 offset = 0;
            for (auto decl : stype->struct_scope.decls) {
                debug_struct_members.push_back(llinfo.DBuilder->createMemberType(composite_type, decl->varname,
                    llinfo.getOrCreateDFile(decl), decl->line_num, Layout.getTypeSizeInBits(decl->specified_type->llvm_type),
                    Layout.getABITypeAlign(decl->specified_type->llvm_type).value(), offset,
                    DINode::FlagZero, decl->specified_type->debug_type));
                offset += Layout.getTypeSizeInBits(decl->specified_type->llvm_type);
            }

            llinfo.DBuilder->replaceArrays(composite_type, llinfo.DBuilder->getOrCreateArray(debug_struct_members));
        }
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)ast;
        if (isVoidType(run->expr_type)) break;

        assert(run->new_ast->ast_type == AST_LITERAL);
        auto lit = (LiteralAST *)run->new_ast;

        generateCode(lit);
        run->codegen = lit->codegen;        
        break;
    }
    case AST_CAST: {
        auto cast = (CastAST *)ast;        
        if (cast->dstType->ast_type == AST_ARRAY_TYPE) {
            assert(cast->isImplicit);
            auto dstType = (ArrayTypeAST *)cast->dstType;
            if (!isSizedArray(dstType)) {
                assert(!"Unsupported cast");
                exit(-1);
            }
            assert(cast->srcType->ast_type == AST_ARRAY_TYPE);
            // ensure the llvm type is there 
            generateCode(cast->dstType);

            auto srcType = (ArrayTypeAST *)cast->srcType;
            switch (srcType->array_type) {
            case ArrayTypeAST::SIZED_ARRAY: {
                assert(!"We should never be here, no need to convert from sized to sized");
                break;
            }
            case ArrayTypeAST::DYNAMIC_ARRAY: {
                assert(!"Unimplemented in llvm");
                break;
            }
            case ArrayTypeAST::STATIC_ARRAY: {
                IRBuilder<> TmpB(&llinfo.llvm_function->getEntryBlock(),
                    llinfo.llvm_function->getEntryBlock().begin());
                auto local_array = TmpB.CreateAlloca(cast->dstType->llvm_type, nullptr, "Internal Array cast");
                // first, assign the data pointer
                // create a GEP for the data on the new Sized array
                assert(cast->expr->ast_type == AST_IDENTIFIER);
                auto id = (IdentifierAST *)cast->expr;
                std::vector<Value *> idx;
                auto decl = id->decl;
                // This is a cop-out, code to handle arrays inside other arrays or structs not supported yet
                assert(id->next == nullptr);
                Value *val_zero = ConstantInt::get(llvm_u32, 0);
                Value *val_one = ConstantInt::get(llvm_u32, 1);
                idx.push_back(val_zero);
                idx.push_back(val_zero);

                generateCode(dstType->array_of_type);
                // Get the address of the casted static array, do an llvm to pointer to element (from array)
                Value *array_data_ptr = llinfo.Builder->CreatePointerCast(decl->codegen,
                    dstType->array_of_type->llvm_type->getPointerTo());
                // And then do a store
                Value *local_data = llinfo.Builder->CreateGEP(
                    cast->dstType->llvm_type,
                    local_array, idx);
                llinfo.Builder->CreateStore(array_data_ptr, local_data);
                
                // And now do the same for the count
                Value *count = ConstantInt::get(llvm_u64, srcType->num_elems);
                idx.clear();
                idx.push_back(val_zero);
                idx.push_back(val_one);
                Value *local_count = llinfo.Builder->CreateGEP(
                    cast->dstType->llvm_type,
                    local_array, idx);
                llinfo.Builder->CreateStore(count, local_count);
                cast->codegen = local_array;
                break;
            }
            default:
                assert(!"We should never be here, unknown array type");
            }
        } else {
            generateCode(cast->dstType);
            generateCode(cast->expr);
            switch (cast->castop) {
            case CASTOP_NOP: {
                cast->codegen = cast->expr->codegen;
                break;
            }
            case CASTOP_TRUNC: {
                cast->codegen = llinfo.Builder->CreateTrunc(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_ZEXT: {
                cast->codegen = llinfo.Builder->CreateZExt(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_SEXT: {
                cast->codegen = llinfo.Builder->CreateSExt(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_FP2UI: {
                cast->codegen = llinfo.Builder->CreateFPToUI(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_FP2SI: {
                cast->codegen = llinfo.Builder->CreateFPToSI(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_SI2FP: {
                cast->codegen = llinfo.Builder->CreateSIToFP(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_UI2FP: {
                cast->codegen = llinfo.Builder->CreateUIToFP(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_FPTRUNC: {
                cast->codegen = llinfo.Builder->CreateFPTrunc(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_FPEXT: {
                cast->codegen = llinfo.Builder->CreateFPExt(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_FPBOOL: {
                // Issue a comparison with the expr, and then assign that to bool
                bool isDouble = cast->expr->expr_type->size_in_bytes == 8;
                auto* val = llinfo.Builder->CreateFCmpUNE(cast->expr->codegen, ConstantFP::get(isDouble ? llvm_f64 : llvm_f32, 0.0));
                cast->codegen = llinfo.Builder->CreateZExt(val, Type::getInt8Ty(*llinfo.TheContext));
                break;
            }
            case CASTOP_BITCAST: {
                cast->codegen = llinfo.Builder->CreateBitCast(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_PTR2INT: {
                cast->codegen = llinfo.Builder->CreatePtrToInt(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            case CASTOP_INT2PTR: {
                cast->codegen = llinfo.Builder->CreateIntToPtr(cast->expr->codegen, cast->dstType->llvm_type);
                break;
            }
            default:
                assert(!"Unsupported cast");
            }
        }
        break;
    }
    case AST_NULL_PTR: {
        auto nptr = (NullPtrAST *)ast;
        if (nptr->type_to_null) {
            if (!nptr->type_to_null->llvm_type) generateCode(nptr->type_to_null);
            nptr->codegen = ConstantPointerNull::get((PointerType *)nptr->type_to_null->llvm_type);
        } else {
            // assume we are in a corner case and use a default s64 ptr
            nptr->codegen = ConstantPointerNull::get(Type::getInt64PtrTy(*llinfo.TheContext));
        }
        break;
    }
    case AST_NEW: {
        auto nast = (NewAllocAST *)ast;
        if (isTypePointer(nast->expr_type)) {
            auto ptype = (PointerTypeAST *)nast->expr_type;
            generateCode(ptype);
            if (!decl_malloc->codegen) generateCode(decl_malloc);
            std::vector<Value *> ArgsV;
            ArgsV.push_back(ConstantInt::get(llvm_u64, ptype->points_to_type->size_in_bytes));
            auto llvm_call = llinfo.Builder->CreateCall((llvm::Function*)decl_malloc->codegen, ArgsV, "call");
            // And now we need a cast
            nast->codegen = llinfo.Builder->CreatePointerCast(llvm_call, ptype->llvm_type);
        } else {
            // Only arrays make sense otherwise
            assert(isTypeArray(nast->expr_type));
            auto atype = (ArrayTypeAST *)nast->expr_type;
            // Only support this for now
            assert(isSizedArray(atype) && (atype->num_elems > 0));

            generateCode(atype);
            if (!decl_malloc->codegen) generateCode(decl_malloc);
            std::vector<Value *> ArgsV;
            ArgsV.push_back(ConstantInt::get(llvm_u64, atype->size_in_bytes));
            auto llvm_call = llinfo.Builder->CreateCall((llvm::Function*)decl_malloc->codegen, ArgsV, "call");
            // And now we need a cast
            nast->codegen = llinfo.Builder->CreatePointerCast(llvm_call, atype->array_of_type->llvm_type->getPointerTo());
        }
        break;
    }
    default:
        assert(!"Unknonw AST type on LLVM generation");
    }
}


/*

C:\code\c2>..\llvm\build\Release\bin\llc -march=x86-64 -mattr=help
Available CPUs for this target:

amdfam10       - Select the amdfam10 processor.
athlon         - Select the athlon processor.
athlon-4       - Select the athlon-4 processor.
athlon-fx      - Select the athlon-fx processor.
athlon-mp      - Select the athlon-mp processor.
athlon-tbird   - Select the athlon-tbird processor.
athlon-xp      - Select the athlon-xp processor.
athlon64       - Select the athlon64 processor.
athlon64-sse3  - Select the athlon64-sse3 processor.
atom           - Select the atom processor.
barcelona      - Select the barcelona processor.
bdver1         - Select the bdver1 processor.
bdver2         - Select the bdver2 processor.
bdver3         - Select the bdver3 processor.
bdver4         - Select the bdver4 processor.
bonnell        - Select the bonnell processor.
broadwell      - Select the broadwell processor.
btver1         - Select the btver1 processor.
btver2         - Select the btver2 processor.
c3             - Select the c3 processor.
c3-2           - Select the c3-2 processor.
cannonlake     - Select the cannonlake processor.
core-avx-i     - Select the core-avx-i processor.
core-avx2      - Select the core-avx2 processor.
core2          - Select the core2 processor.
corei7         - Select the corei7 processor.
corei7-avx     - Select the corei7-avx processor.
generic        - Select the generic processor.
geode          - Select the geode processor.
goldmont       - Select the goldmont processor.
haswell        - Select the haswell processor.
i386           - Select the i386 processor.
i486           - Select the i486 processor.
i586           - Select the i586 processor.
i686           - Select the i686 processor.
icelake        - Select the icelake processor.
ivybridge      - Select the ivybridge processor.
k6             - Select the k6 processor.
k6-2           - Select the k6-2 processor.
k6-3           - Select the k6-3 processor.
k8             - Select the k8 processor.
k8-sse3        - Select the k8-sse3 processor.
knl            - Select the knl processor.
knm            - Select the knm processor.
lakemont       - Select the lakemont processor.
nehalem        - Select the nehalem processor.
nocona         - Select the nocona processor.
opteron        - Select the opteron processor.
opteron-sse3   - Select the opteron-sse3 processor.
penryn         - Select the penryn processor.
pentium        - Select the pentium processor.
pentium-m      - Select the pentium-m processor.
pentium-mmx    - Select the pentium-mmx processor.
pentium2       - Select the pentium2 processor.
pentium3       - Select the pentium3 processor.
pentium3m      - Select the pentium3m processor.
pentium4       - Select the pentium4 processor.
pentium4m      - Select the pentium4m processor.
pentiumpro     - Select the pentiumpro processor.
prescott       - Select the prescott processor.
sandybridge    - Select the sandybridge processor.
silvermont     - Select the silvermont processor.
skx            - Select the skx processor.
skylake        - Select the skylake processor.
skylake-avx512 - Select the skylake-avx512 processor.
slm            - Select the slm processor.
westmere       - Select the westmere processor.
winchip-c6     - Select the winchip-c6 processor.
winchip2       - Select the winchip2 processor.
x86-64         - Select the x86-64 processor.
yonah          - Select the yonah processor.
znver1         - Select the znver1 processor.

Available features for this target:

16bit-mode                    - 16-bit mode (i8086).
32bit-mode                    - 32-bit mode (80386).
3dnow                         - Enable 3DNow! instructions.
3dnowa                        - Enable 3DNow! Athlon instructions.
64bit                         - Support 64-bit instructions.
64bit-mode                    - 64-bit mode (x86_64).
adx                           - Support ADX instructions.
aes                           - Enable AES instructions.
atom                          - Intel Atom processors.
avx                           - Enable AVX instructions.
avx2                          - Enable AVX2 instructions.
avx512bitalg                  - Enable AVX-512 Bit Algorithms.
avx512bw                      - Enable AVX-512 Byte and Word Instructions.
avx512cd                      - Enable AVX-512 Conflict Detection Instructions.
avx512dq                      - Enable AVX-512 Doubleword and Quadword Instructions.
avx512er                      - Enable AVX-512 Exponential and Reciprocal Instructions.
avx512f                       - Enable AVX-512 instructions.
avx512ifma                    - Enable AVX-512 Integer Fused Multiple-Add.
avx512pf                      - Enable AVX-512 PreFetch Instructions.
avx512vbmi                    - Enable AVX-512 Vector Byte Manipulation Instructions.
avx512vbmi2                   - Enable AVX-512 further Vector Byte Manipulation Instructions.
avx512vl                      - Enable AVX-512 Vector Length eXtensions.
avx512vnni                    - Enable AVX-512 Vector Neural Network Instructions.
avx512vpopcntdq               - Enable AVX-512 Population Count Instructions.
bmi                           - Support BMI instructions.
bmi2                          - Support BMI2 instructions.
broadwell                     - Intel Broadwell processors.
cannonlake                    - Intel Cannonlake processors.
clflushopt                    - Flush A Cache Line Optimized.
clwb                          - Cache Line Write Back.
clzero                        - Enable Cache Line Zero.
cmov                          - Enable conditional move instructions.
cx16                          - 64-bit with cmpxchg16b.
ermsb                         - REP MOVS/STOS are fast.
f16c                          - Support 16-bit floating point conversion instructions.
fast-gather                   - Indicates if gather is reasonably fast..
fast-lzcnt                    - LZCNT instructions are as fast as most simple integer ops.
fast-partial-ymm-or-zmm-write - Partial writes to YMM/ZMM registers are fast.
fast-scalar-fsqrt             - Scalar SQRT is fast (disable Newton-Raphson).
fast-shld-rotate              - SHLD can be used as a faster rotate.
fast-vector-fsqrt             - Vector SQRT is fast (disable Newton-Raphson).
fma                           - Enable three-operand fused multiple-add.
fma4                          - Enable four-operand fused multiple-add.
fsgsbase                      - Support FS/GS Base instructions.
fxsr                          - Support fxsave/fxrestore instructions.
gfni                          - Enable Galois Field Arithmetic Instructions.
glm                           - Intel Goldmont processors.
haswell                       - Intel Haswell processors.
ibt                           - Support CET Indirect-Branch-Tracking instructions.
icelake                       - Intel Icelake processors.
idivl-to-divb                 - Use 8-bit divide for positive values less than 256.
idivq-to-divl                 - Use 32-bit divide for positive values less than 2^32.
knl                           - Intel Knights Landing processors.
lea-sp                        - Use LEA for adjusting the stack pointer.
lea-uses-ag                   - LEA instruction needs inputs at AG stage.
lwp                           - Enable LWP instructions.
lzcnt                         - Support LZCNT instruction.
macrofusion                   - Various instructions can be fused with conditional branches.
mmx                           - Enable MMX instructions.
movbe                         - Support MOVBE instruction.
mpx                           - Support MPX instructions.
mwaitx                        - Enable MONITORX/MWAITX timer functionality.
pad-short-functions           - Pad short functions.
pclmul                        - Enable packed carry-less multiplication instructions.
pku                           - Enable protection keys.
popcnt                        - Support POPCNT instruction.
prefetchwt1                   - Prefetch with Intent to Write and T1 Hint.
prfchw                        - Support PRFCHW instructions.
rdrnd                         - Support RDRAND instruction.
rdseed                        - Support RDSEED instruction.
rtm                           - Support RTM instructions.
sahf                          - Support LAHF and SAHF instructions.
sgx                           - Enable Software Guard Extensions.
sha                           - Enable SHA instructions.
shstk                         - Support CET Shadow-Stack instructions.
skx                           - Intel Skylake Server processors.
skylake                       - Intel Skylake processors.
slm                           - Intel Silvermont processors.
slow-3ops-lea                 - LEA instruction with 3 ops or certain registers is slow.
slow-incdec                   - INC and DEC instructions are slower than ADD and SUB.
slow-lea                      - LEA instruction with certain arguments is slow.
slow-pmulld                   - PMULLD instruction is slow.
slow-shld                     - SHLD instruction is slow.
slow-two-mem-ops              - Two memory operand instructions are slow.
slow-unaligned-mem-16         - Slow unaligned 16-byte memory access.
slow-unaligned-mem-32         - Slow unaligned 32-byte memory access.
soft-float                    - Use software floating point features..
sse                           - Enable SSE instructions.
sse-unaligned-mem             - Allow unaligned memory operands with SSE instructions.
sse2                          - Enable SSE2 instructions.
sse3                          - Enable SSE3 instructions.
sse4.1                        - Enable SSE 4.1 instructions.
sse4.2                        - Enable SSE 4.2 instructions.
sse4a                         - Support SSE 4a instructions.
ssse3                         - Enable SSSE3 instructions.
tbm                           - Enable TBM instructions.
vaes                          - Promote selected AES instructions to AVX512/AVX registers.
vpclmulqdq                    - Enable vpclmulqdq instructions.
x87                           - Enable X87 float instructions.
xop                           - Enable XOP instructions.
xsave                         - Support xsave instructions.
xsavec                        - Support xsavec instructions.
xsaveopt                      - Support xsaveopt instructions.
xsaves                        - Support xsaves instructions.
*/

void llvm_compile(FileAST* root, FileObject& obj_file, const llvm_options& opt, llvm_timing& timing,
                  const char* output_name)
{   
    Timer timer;

    CPU_SAMPLE("LLVM compile");

    fs::path p = root->filename;
    auto fname = p.filename();

    timer.startTimer();
    llinfo.TheContext = std::make_unique<LLVMContext>();
    llinfo.TheModule = std::make_unique<Module>(fname.string(), *llinfo.TheContext);
    
    // Initialize the target registry etc.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    llinfo.TheModule->setTargetTriple(TargetTriple);
    llinfo.TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::Max, "Dwarf Version", 4);
    llinfo.TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::Warning, "CodeView", 1);
    llinfo.TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::Warning, "Debug Info Version", 3);
    unsigned int wchar_size = 0;
#ifdef PLATFORM_WINDOWS
    wchar_size = 2;
#else // PLATFORM_LINUX
    wchar_size = 4;
#endif                
    llinfo.TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::Error, "wchar_size", wchar_size);
    llinfo.TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::Max, "PIC Level", 2);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    auto CPU = "corei7-avx";
    // Should this be avx2 or such?
    auto Features = "";

    auto RM = Optional<Reloc::Model>();
    auto TheTargetMachine =
        Target->createTargetMachine(TargetTriple, CPU, Features, TargetOptions{}, RM);

    llinfo.TheModule->setDataLayout(TheTargetMachine->createDataLayout());
    llvm_u32 = Type::getInt32Ty(*llinfo.TheContext);
    llvm_u64 = Type::getInt64Ty(*llinfo.TheContext);
    llvm_f32 = Type::getFloatTy(*llinfo.TheContext);
    llvm_f64 = Type::getDoubleTy(*llinfo.TheContext);
    llinfo.Builder = std::make_unique<IRBuilder<>>(*llinfo.TheContext);

    if (opt.option_optimize) {
        llinfo.fpass = std::make_unique<legacy::FunctionPassManager>(llinfo.TheModule.get());

        /*
        llinfo.PMBuilder.OptLevel = 3;
        llinfo.PMBuilder.VerifyOutput = true;
        llinfo.PMBuilder.VerifyInput = true;
        llinfo.PMBuilder.populateFunctionPassManager(*llinfo.fpass);
        llinfo.PMBuilder.populateModulePassManager(llinfo.pass);
        */

        // Promote allocas to registers.
        llinfo.fpass->add(createPromoteMemoryToRegisterPass());
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        llinfo.fpass->add(createInstructionCombiningPass());
        // Reassociate expressions.
        llinfo.fpass->add(createReassociatePass());
        // Eliminate Common SubExpressions.
        llinfo.fpass->add(createGVNPass());
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        llinfo.fpass->add(createCFGSimplificationPass());

        llinfo.fpass->doInitialization();

    }

    if (opt.option_debug_info) {
        llinfo.DBuilder = std::make_unique<DIBuilder>(*llinfo.TheModule);

        DIFile* root_file = llinfo.getOrCreateDFile(root);

        // the 1 here is just a hack, we have to define some language
        llinfo.RootCU = llinfo.DBuilder->createCompileUnit(30, root_file, "RAD compiler", 0, "", 0);
        root->scope->debug_scope = root_file;
    }
//    printf("Creating DebugLexical / RootCU %p Root_File %p \n", RootCU, root_file);

    // look for the malloc and free calls (which new depends on)
    for (auto decl : root->global_scope.decls) {
        if (!strcmp(decl->varname, "malloc")) {
            decl_malloc = decl;
        }
        if (!strcmp(decl->varname, "free")) {
            decl_free = decl;
        }
    }
    // Do the actual compile
    generateCode(root);

    timing.codegenTime = timer.stopTimer();

    timer.startTimer();

    {
        std::error_code EC;
        raw_fd_ostream dest(obj_file.getFilename(), EC);

        if (EC) {
            errs() << "Could not open file: " << EC.message();
            assert(false);
            return;
        }

       

        auto FileType = llvm::CGFT_ObjectFile;

        if (TheTargetMachine->addPassesToEmitFile(llinfo.pass, dest, nullptr, FileType)) {
            errs() << "TheTargetMachine can't emit a file of this type";
            assert(false);
            return;
        }

        if (llinfo.DBuilder) llinfo.DBuilder->finalize();
        if (llinfo.fpass) llinfo.fpass->doFinalization();

#if 0
        // we can do this with the parameter to print llvm
        llinfo.TheModule->print(outs(), nullptr, false, true);
        outs().flush();
#endif

        llinfo.pass.run(*llinfo.TheModule);

        dest.flush();

        timing.bingenTime = timer.stopTimer();
    }

	if (!opt.option_quiet) {
		outs() << "Object generation [" << obj_file.getFilename() << "] completed, now linking...\n";
		outs().flush();
	}

    {
        CPU_SAMPLE("LLVM external link");
        timer.startTimer();
        int retcode = link_object(obj_file, root->imports, output_name, opt.option_debug_info);
        timing.linkTime = timer.stopTimer();
        if (retcode != 0) {
            printf("Error, compilation failed!!!\n\n");
            return;
        } else {
            fs::remove(obj_file.getFilename());
        }
    }

    if (opt.option_llvm_print) {
        llinfo.TheModule->print(outs(), nullptr, false, true);
        outs().flush();
    }
}

