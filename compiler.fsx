// #r "nuget: Lokad.ILPack, 0.2.0"

exception MismatchedBrackets of string

/// Basic language, directly represents the brainfuck AST.
type Instruction = 
    | Add
    | Sub
    | PtrLeft
    | PtrRight
    | Input
    | Output
    | LeftBracket
    | RightBracket
    | Dump

/// Higher level representation, represents operations in a way which makes them easier to compile.
type Instruction' =
    | Add of int
    | Sub of int
    | PtrLeft of int
    | PtrRight of int
    | Set of int // we can optimize to an explicit set to zero instead of decrement operation :)
    | Input
    | Output
    | LeftBracket
    | RightBracket
    | Dump

/// Which mode the program is running in, batch or repl operation.
type Mode =
    | Batch
    | Repl

let readAllText filePath = System.IO.File.ReadAllText(filePath)

/// Transforms a given input character to the matching brainfuck instruction, if any.
let tokenToInstruction (c: char) : Instruction option =
    match c with
        | '+' -> Some Instruction.Add
        | '-' -> Some Instruction.Sub
        | '<' -> Some Instruction.PtrLeft
        | '>' -> Some Instruction.PtrRight
        | ',' -> Some Instruction.Input
        | '.' -> Some Instruction.Output
        | '[' -> Some Instruction.LeftBracket
        | ']' -> Some Instruction.RightBracket
        | '#' -> Some Instruction.Dump
        | _ -> None

/// Transform the instruction from the ast form to the higher level representation.
let transformInstruction (i: Instruction) : Instruction' =
    match i with
    | Instruction.Add -> Add 1
    | Instruction.Sub -> Sub 1
    | Instruction.PtrLeft -> PtrLeft 1
    | Instruction.PtrRight -> PtrRight 1
    | Instruction.Input -> Input
    | Instruction.Output -> Output
    | Instruction.LeftBracket -> LeftBracket
    | Instruction.RightBracket -> RightBracket
    | Instruction.Dump -> Dump

/// Given a string with brainfuck code, produces a stream of brainfuck instructions.
let tokenizeContent (contents: string) = [for c in contents -> tokenToInstruction(c)] |> List.choose id

/// Given a stream of brainfuck instructions, performs some basic optimizations to make compiling it efficiently easier.
let basicOptimizationPass (instructions: list<Instruction'>) : list<Instruction'> =

    let rec optimize (acc: list<Instruction'>, is: list<Instruction'>) =

        match is with

        // convert patterns of alternating sequences of +, -
        | Add a::rest ->
            match acc with
            | Add b::xs -> optimize (Add (a + b)::xs, rest)
            | Sub b::xs -> optimize (Add (a - b)::xs, rest)
            | _ -> optimize (Add(a)::acc, rest)
        | Sub a::rest ->
            match acc with
            | Sub b::xs -> optimize (Sub (a + b)::xs, rest)
            | Add b::xs -> optimize (Sub (a - b)::xs, rest)
            | _ -> optimize (Sub(a)::acc, rest)

        // ... as well as <, > into a single instruction.
        | PtrLeft a::rest ->
            match acc with
            | PtrLeft b::xs -> optimize (PtrLeft (a + b)::xs, rest)
            | PtrRight b::xs -> optimize (PtrLeft (a - b)::xs, rest)
            | _ -> optimize (PtrLeft(a)::acc, rest)
        | PtrRight a::rest ->
            match acc with
            | PtrRight b::xs -> optimize (PtrRight (a + b)::xs, rest)
            | PtrLeft b::xs -> optimize (PtrRight (a - b)::xs, rest)
            | _ -> optimize (PtrRight(a)::acc, rest)

        | x::xs -> optimize(x::acc, xs)
        | [] -> acc

    List.rev (optimize ([], instructions))

/// Given a stream of brainfuck instructions, performs a pass to do any special rewrites which are now possible (more may have been enabled by the previous pass)
let rewriteOptimizationPass (instructions: list<Instruction'>) : list<Instruction'> =

    let rec optimize (acc: list<Instruction'>, is: list<Instruction'>) =

        match is with

            // optimize [-] (increment towards zero) to a direct set to 0
            | LeftBracket::(Sub _)::RightBracket::rest -> optimize (Set(0)::acc, rest)

            | x::xs -> optimize(x::acc, xs)
            | [] -> acc
    
    List.rev (optimize ([], instructions)) 

/// Given a stream of brainfuck instructions, performs a pass to clean up any obviously no-op instructions.
let cleanOptimizationPass (instructions: list<Instruction'>) : list<Instruction'> =

    let rec optimize (acc: list<Instruction'>, is: list<Instruction'>) =

        match is with

            // skip instructions when we happen to see just [] by itself, does nothing
            | LeftBracket::RightBracket::rest -> optimize (acc, rest)

            // clear out empty add, sub
            | Add a::rest when a = 0 -> optimize(acc, rest)
            | Sub a::rest when a = 0 -> optimize(acc, rest)

            // ... ptr left, ptr right
            | PtrLeft a::rest when a = 0 -> optimize(acc, rest)
            | PtrRight a::rest when a = 0 -> optimize(acc, rest)

            | x::xs -> optimize(x::acc, xs)
            | [] -> acc
    
    List.rev (optimize ([], instructions))

/// Given a stream of brainfuck instructions, produces an optimized list of instructions.
let optimizeInstructions (instructions: list<Instruction>) : list<Instruction'> =
    [for i in instructions -> transformInstruction(i)]
    |> basicOptimizationPass
    |> rewriteOptimizationPass
    |> cleanOptimizationPass

/// Given a brainfuck program, produces an optimized form of the program.
let optimizeProgram contents =
    tokenizeContent (contents)
    |> optimizeInstructions

open System
open System.Reflection
open System.Reflection.Emit

let createDynamicAssemblyWithMethodBuilder (assemblyName : string, moduleName: string, typeName: string, methodName: string) =

    let assemblyName : AssemblyName = new AssemblyName(assemblyName)
    let assemblyBuilder : AssemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run) // #FIXME: figure out how we're gonna save the assembly
    let moduleBuilder : ModuleBuilder = assemblyBuilder.DefineDynamicModule(moduleName)
    let typeBuilder : TypeBuilder = moduleBuilder.DefineType(typeName, TypeAttributes.Public)
    let methodBuilder : MethodBuilder = typeBuilder.DefineMethod(methodName, MethodAttributes.Public ||| MethodAttributes.Static, null, null)
    let _ = (assemblyBuilder.EntryPoint = methodBuilder)

    (assemblyBuilder, methodBuilder,  typeBuilder)

let emitAssemblyWithStaticMethod (assemblyName : string, moduleName: string, typeName: string, methodName: string, emitter: ILGenerator -> unit) : unit -> unit =

    let (assemblyBuilder, methodBuilder, typeBuilder) = createDynamicAssemblyWithMethodBuilder(assemblyName, moduleName, typeName, methodName)
    
    let ilGenerator : ILGenerator = methodBuilder.GetILGenerator()
    emitter ilGenerator
    
    let newType = typeBuilder.CreateType()
    let assembly : Assembly = assemblyBuilder;

    // let generator = Lokad.ILPack.AssemblyGenerator();
    // generator.GenerateAssembly(assembly, "Brainiac.dll");

    fun () -> ignore (newType.GetMethod("Main").Invoke(null, null)); ()

/// Defines the offsets for a given [] pair
type Scope = {
    Start: Label * int
    End: Label * int
}

type CompilationContext = {
    Current: int // what instruction are we currently dealing with in our instruction stream
    Scopes: list<Scope> // a list of scopes for the conditional jumps in brainfuck, with associated IL labels and offsets for each into the original non-IL instruction stream
}

let executeProgram contents =

    let memorySize: int = 65536;
    let memoryStackOffset: int = 0;
    let memoryArrayType: Type = typeof<array<byte>>
    let memoryType: Type = typeof<byte>

    let stackPointerValue: int = 0;
    let stackPointerOffset: int = 1;
    let stackPointerType: Type = typeof<int>

    let optimizedProgram = optimizeProgram(contents)

    let compilePrelude (generator : ILGenerator, instructions: list<Instruction'>) : CompilationContext =

        let findMatchingBracketOffset(instructions: list<Instruction'>) : int =

            let rec findMatchingOffset(is: list<Instruction'>, currentOffset: int, counter: int) =
                match is with
                | LeftBracket::xs -> findMatchingOffset(xs, currentOffset + 1, counter - 1)
                | RightBracket::_ when counter = 0 -> currentOffset
                | RightBracket::xs -> findMatchingOffset(xs, currentOffset + 1, counter + 1)
                | _::xs -> findMatchingOffset(xs, currentOffset + 1, counter)
                | [] -> raise (MismatchedBrackets("found mismatched brackets, can not compile program!"))
            
            findMatchingOffset(instructions, 0, 1)

        let rec compileBrackets (generator: ILGenerator, ctx: CompilationContext, instructions: list<Instruction'>) : CompilationContext =
            match instructions with
            | LeftBracket::xs ->
                let newScope: Scope = { Start = (generator.DefineLabel(), ctx.Current); End = (generator.DefineLabel(), ctx.Current + findMatchingBracketOffset(instructions)) }
                compileBrackets(generator, {ctx with Current = ctx.Current + 1; Scopes = newScope::ctx.Scopes}, xs)
            | _::xs ->
                compileBrackets(generator, {ctx with Current = ctx.Current + 1}, xs)
            | [] ->
                ctx

        // emit definitions for all of our labels
        let compilationContext: CompilationContext = compileBrackets (generator, { Current = 0; Scopes = [] }, instructions)

        // emit definitions for our two locals, must be declared upfront
        // #FIXME: this is kinda hacky, maybe we should save this information in the compilation context instead of hardcoding the offsets?
        let _ = generator.DeclareLocal(memoryArrayType)
        let _ = generator.DeclareLocal(stackPointerType)

        // emit something like:
        // int[] memory = new int[memorySize];
        // on the stack at index 0  
        generator.Emit(OpCodes.Ldc_I4, memorySize)
        generator.Emit(OpCodes.Newarr, memoryType)
        generator.Emit(OpCodes.Stloc, memoryStackOffset)

        // create our "stack pointer"
        generator.Emit(OpCodes.Ldc_I4, stackPointerValue)
        generator.Emit(OpCodes.Stloc, stackPointerOffset)

        { compilationContext with Current = 0 }
    
    let compileAdd (generator: ILGenerator, value: int) =
        // emit memory[stackPointerOffset] = memory[stackPointerOffset] + value;
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Add)
        generator.Emit(OpCodes.Conv_U1)
        generator.Emit(OpCodes.Stelem_I1)

    let compileSub (generator: ILGenerator, value: int) =
        // emit memory[stackPointerOffset] = memory[stackPointerOffset] - value;
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Sub)
        generator.Emit(OpCodes.Conv_U1)
        generator.Emit(OpCodes.Stelem_I1)
    
    let compilePtrLeft (generator: ILGenerator, value: int) =
        // emit stackPointerOffset -= value;
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Sub)
        generator.Emit(OpCodes.Stloc, stackPointerOffset)

    let compilePtrRight (generator: ILGenerator, value: int) =
        // emit stackPointerOffset += value;
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Add)
        generator.Emit(OpCodes.Stloc, stackPointerOffset)
    
    let compileSet (generator: ILGenerator, value: int) =
        // emit memory[stackPointerOffset] = value;
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Stelem_I1)

    let compileOutput (generator: ILGenerator) =
        let writeParams = [|typeof<char>|]
        let writeMethodInfo = typeof<Console>.GetMethod("Write", writeParams)
        // emit Console.Write(memory[stackPointerOffset])
        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.EmitCall(OpCodes.Call, writeMethodInfo, null)
        generator.Emit(OpCodes.Nop) // #FIXME: uhh
    
    let compileLeftBracket (generator: ILGenerator, ctx: CompilationContext) =

        let scope = List.find (fun(e) -> let (_, offset) = e.Start in offset = ctx.Current) ctx.Scopes
        let (startLabel, _) = scope.Start
        let (targetLabel, _) = scope.End

        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)
        
        // jump if current memory cell is zero, otherwise we do nothing
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, 0)

        generator.Emit(OpCodes.Beq, targetLabel)
        generator.MarkLabel(startLabel)
    
    let compileRightBracket (generator: ILGenerator, ctx: CompilationContext) =

        let scope = List.find (fun(e) -> let (_, offset) = e.End in offset = ctx.Current) ctx.Scopes
        let (endLabel, _) = scope.End
        let (startLabel, _) = scope.Start

        generator.Emit(OpCodes.Ldloc, memoryStackOffset)
        generator.Emit(OpCodes.Ldloc, stackPointerOffset)

        // jump back if current memory cell is nonzero, otherwise we do nothing
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, 0)

        generator.Emit(OpCodes.Bne_Un, startLabel)
        generator.MarkLabel(endLabel)

    let rec compile (generator : ILGenerator, ctx: CompilationContext, instructions: list<Instruction'>) =
        match instructions with
        | Add(v)::xs ->
            compileAdd (generator, v);
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Sub(v)::xs ->
            compileSub (generator, v);
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | PtrLeft(v)::xs ->
            compilePtrLeft(generator, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | PtrRight(v)::xs ->
            compilePtrRight(generator, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Set(v)::xs ->
            compileSet(generator, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Input::xs ->
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Output::xs ->
            compileOutput(generator)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | LeftBracket::xs ->
            compileLeftBracket(generator, ctx)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | RightBracket::xs ->
            compileRightBracket(generator, ctx)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Dump::xs ->
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | [] -> generator.Emit(OpCodes.Ret)

    let compiledProgramFunction = emitAssemblyWithStaticMethod(
        "BrainiacAssembly",
        "BrainiacModule",
        "BrainiacMain",
        "Main",
        fun g ->
            let ctx = compilePrelude (g, optimizedProgram) in
                compile (g, ctx,  optimizedProgram)
    )

    compiledProgramFunction ()

let executeProgramInFile path =
    let contents = readAllText path in
        executeProgram contents