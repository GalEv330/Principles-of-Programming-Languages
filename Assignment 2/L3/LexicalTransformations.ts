import { ClassExp, ProcExp, Exp, Program, makeProcExp, Binding, CExp, DefineExp, isAppExp, isAtomicExp, isCExp, isClassExp, isDefineExp, isIfExp, isLetExp, isLitExp, isProcExp, isProgram, makeAppExp, makeBinding, makeBoolExp, makeDefineExp, makeIfExp, makeLetExp, makeLitExp, makePrimOp, makeProgram, makeVarDecl, makeVarRef } from "./L3-ast";
import { Result, makeFailure, makeOk } from "../shared/result";
import { NonEmptyList, first, rest } from "../shared/list";
import { makeSymbolSExp } from "./L3-value";

/*
Purpose: Transform ClassExp to ProcExp
Signature: class2proc(classExp)
Type: ClassExp => ProcExp
*/

export const class2proc = (exp: ClassExp): ProcExp =>
    makeProcExp(exp.fields, [makeProcForMethods(exp.methods as NonEmptyList<Binding>)]);

export const makeProcForMethods = (methods: NonEmptyList<Binding>): ProcExp =>
    makeProcExp([makeVarDecl('msg')], [methodsConditions(methods)]);

export const methodsConditions = (methods: NonEmptyList<Binding>): CExp =>
    methods.length === 1
        ? makeIfExp
        (makeAppExp(makePrimOp("eq?"), [makeVarRef('msg'), makeLitExp(makeSymbolSExp(first(methods).var.var))]),
            makeAppExp(first(methods).val, []),
            makeBoolExp(false)
        )
        : makeIfExp
            (makeAppExp(makePrimOp("eq?"), [makeVarRef('msg'), makeLitExp(makeSymbolSExp(first(methods).var.var))]),
            makeAppExp(first(methods).val, []),
            methodsConditions(rest(methods) as NonEmptyList<Binding>)
        );


/*
Purpose: Transform all class forms in the given AST to procs
Signature: lexTransform(AST)
Type: [Exp | Program] => Result<Exp | Program>
*/

export const lexTransform = (exp: Exp | Program): Result<Exp | Program> =>
    isCExp(exp) ? makeOk(lexTransformCExp(exp)) :
    isDefineExp(exp) ? makeOk(lexTransformDefineExp(exp)) :    
    isProgram(exp) ? makeOk(lexTransformProgram(exp)) :
    makeFailure("Unexpected AST node");

export const lexTransformCExp = (exp: CExp): CExp =>
    isAtomicExp(exp) ? exp :
    isClassExp(exp) ? class2proc(exp) :
    isIfExp(exp) ? makeIfExp(lexTransformCExp(exp.test), lexTransformCExp(exp.then), lexTransformCExp(exp.alt)) :  
    isAppExp(exp) ? makeAppExp(lexTransformCExp(exp.rator), exp.rands.map(lexTransformCExp)) :
    isProcExp(exp) ? makeProcExp(exp.args, exp.body.map(lexTransformCExp)) :
    isLetExp(exp) ? makeLetExp(exp.bindings.map(b => makeBinding(b.var.var, lexTransformCExp(b.val))), exp.body.map(lexTransformCExp)) :
    isLitExp(exp) ? makeLitExp(exp.val) :
    exp;

export const lexTransformDefineExp = (exp: DefineExp): DefineExp =>
    makeDefineExp(exp.var, lexTransformCExp(exp.val));

export const lexTransformProgram = (exp: Program): Program =>
    makeProgram(exp.exps.map(e => isDefineExp(e) ? lexTransformDefineExp(e) : lexTransformCExp(e)));

