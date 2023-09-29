open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Threading
open System.IO

open Lokad.ILPack
open CommandLine

[<CLIMutable>] // #HACK: this is extremely cursed, curse you .NET
type Options = {

  [<Option('f', "file", Required = true, HelpText = "Input file.", SetName = "file")>] File : string option;
  [<Option('i', "input", Required = true, HelpText = "Input program.", SetName = "program")>] Program: string option;
  
  [<Option('b', "build", Required = false, Default = false, HelpText = "Output a compiled version of the input program, instead of executing directly.")>] Build : bool; 
  [<Option('o', "output", Required = false, HelpText = "Output program assembly name, if not set, will use the name of the input program (or otherwise Output.dll)")>] Output: string option;

}

exception MismatchedBrackets of string
exception InvalidOptionsException of string

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

/// Reads all the text in the file, returning it as a string.
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

let optimizeProgramInFile path =
    let contents = readAllText path in
        optimizeProgram contents

let createDynamicAssemblyWithMethodBuilder (assemblyName : string, moduleName: string, typeName: string, methodName: string) =

    let assemblyName : AssemblyName = new AssemblyName(assemblyName)
    let assemblyBuilder : AssemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    let moduleBuilder : ModuleBuilder = assemblyBuilder.DefineDynamicModule(moduleName)
    let typeBuilder : TypeBuilder = moduleBuilder.DefineType(typeName, TypeAttributes.Public)
    let methodBuilder : MethodBuilder = typeBuilder.DefineMethod(methodName, MethodAttributes.Public ||| MethodAttributes.Static, null, null)
    let _ = (assemblyBuilder.EntryPoint = methodBuilder)

    (assemblyBuilder, methodBuilder,  typeBuilder)

let emitAssembly (assemblyName : string, moduleName: string, typeName: string, methodName: string, emitter: ILGenerator -> unit) : Assembly * Type =

    let (assemblyBuilder, methodBuilder, typeBuilder) = createDynamicAssemblyWithMethodBuilder(assemblyName, moduleName, typeName, methodName)
    
    let ilGenerator : ILGenerator = methodBuilder.GetILGenerator()
    emitter ilGenerator
    
    let newType = typeBuilder.CreateType()
    (assemblyBuilder, newType)

let writeAssemblyToFile (assembly: Assembly, outputName: string) : unit =

    let generator = AssemblyGenerator();
    generator.GenerateAssembly(assembly, outputName);

let executeProgramInAssembly (mainType: Type) : unit =

    (fun () -> ignore (mainType.GetMethod("Main").Invoke(null, null))) ()

/// Defines the offsets for a given [] pair
type Scope = {
    Start: Label * int
    End: Label * int
}

/// Holds the current compilation context.
type CompilationContext = {
    Current: int // what instruction are we currently dealing with in our instruction stream
    Scopes: list<Scope> // a list of scopes for the conditional jumps, with associated created labels and offsets for each into the original instruction stream
    StackPointerOffset: int // local index on the stack where the stack pointer lives
    MemoryStackOffset: int // local index on the stack where the memory array lives
}

/// Builds the program passed in the string to a dynamic assembly.
let buildProgramToAssembly contents =

    let memorySize: int = 65536;
    let memoryArrayType: Type = typeof<array<byte>>
    let memoryType: Type = typeof<byte>

    let stackPointerValue: int = 0;
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

        // emit definitions for our two locals, must be declared upfront
        let memory = generator.DeclareLocal(memoryArrayType)
        let stack = generator.DeclareLocal(stackPointerType)

        // emit definitions for all of our labels
        let ctx: CompilationContext = compileBrackets (
            generator,
            {
                Current = 0;
                Scopes = [];
                StackPointerOffset = stack.LocalIndex;
                MemoryStackOffset = memory.LocalIndex
            },
            instructions
        )

        // create something like:
        // int[] memory = new int[memorySize];
        // on the stack at the index previously declared
        generator.Emit(OpCodes.Ldc_I4, memorySize)
        generator.Emit(OpCodes.Newarr, memoryType)
        generator.Emit(OpCodes.Stloc, ctx.MemoryStackOffset)

        // push 0 onto the stack for our "stack pointer"
        generator.Emit(OpCodes.Ldc_I4, stackPointerValue)
        generator.Emit(OpCodes.Stloc, ctx.StackPointerOffset)

        { ctx with Current = 0 }
    
    let compileAdd (generator: ILGenerator, ctx: CompilationContext, value: int) =
        // emit memory[stackPointerOffset] = memory[stackPointerOffset] + value;
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Add)
        generator.Emit(OpCodes.Conv_U1)
        generator.Emit(OpCodes.Stelem_I1)

    let compileSub (generator: ILGenerator, ctx: CompilationContext, value: int) =
        // emit memory[stackPointerOffset] = memory[stackPointerOffset] - value;
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Sub)
        generator.Emit(OpCodes.Conv_U1)
        generator.Emit(OpCodes.Stelem_I1)
    
    let compilePtrLeft (generator: ILGenerator, ctx: CompilationContext, value: int) =
        // emit stackPointerOffset -= value;
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Sub)
        generator.Emit(OpCodes.Stloc, ctx.StackPointerOffset)

    let compilePtrRight (generator: ILGenerator, ctx: CompilationContext, value: int) =
        // emit stackPointerOffset += value;
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Add)
        generator.Emit(OpCodes.Stloc, ctx.StackPointerOffset)
    
    let compileSet (generator: ILGenerator, ctx: CompilationContext, value: int) =
        // emit memory[stackPointerOffset] = value;
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldc_I4, value)
        generator.Emit(OpCodes.Stelem_I1)

    let compileOutput (generator: ILGenerator, ctx: CompilationContext) =
        let writeParams = [|typeof<char>|]
        let writeMethodInfo = typeof<Console>.GetMethod("Write", writeParams)
        // emit Console.Write(memory[stackPointerOffset])
        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        generator.Emit(OpCodes.Ldelem_U1)
        generator.EmitCall(OpCodes.Call, writeMethodInfo, null)
        generator.Emit(OpCodes.Nop) // #FIXME: uhh
    
    let compileLeftBracket (generator: ILGenerator, ctx: CompilationContext) =

        let scope = List.find (fun(e) -> let (_, offset) = e.Start in offset = ctx.Current) ctx.Scopes
        let (startLabel, _) = scope.Start
        let (targetLabel, _) = scope.End

        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)
        
        // jump if current memory cell is zero, otherwise we do nothing
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, 0)

        generator.Emit(OpCodes.Beq, targetLabel)
        generator.MarkLabel(startLabel)
    
    let compileRightBracket (generator: ILGenerator, ctx: CompilationContext) =

        let scope = List.find (fun(e) -> let (_, offset) = e.End in offset = ctx.Current) ctx.Scopes
        let (endLabel, _) = scope.End
        let (startLabel, _) = scope.Start

        generator.Emit(OpCodes.Ldloc, ctx.MemoryStackOffset)
        generator.Emit(OpCodes.Ldloc, ctx.StackPointerOffset)

        // jump back if current memory cell is nonzero, otherwise we do nothing
        generator.Emit(OpCodes.Ldelem_U1)
        generator.Emit(OpCodes.Ldc_I4, 0)

        generator.Emit(OpCodes.Bne_Un, startLabel)
        generator.MarkLabel(endLabel)

    let rec compile (generator : ILGenerator, ctx: CompilationContext, instructions: list<Instruction'>) =
        match instructions with
        | Add(v)::xs ->
            compileAdd (generator, ctx, v);
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Sub(v)::xs ->
            compileSub (generator, ctx, v);
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | PtrLeft(v)::xs ->
            compilePtrLeft(generator, ctx, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | PtrRight(v)::xs ->
            compilePtrRight(generator, ctx, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Set(v)::xs ->
            compileSet(generator, ctx, v)
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Input::xs ->
            compile(generator, { ctx with Current = ctx.Current + 1 }, xs)
        | Output::xs ->
            compileOutput(generator, ctx)
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

    let (compiledAssembly, compiledType) = emitAssembly(
        "BrainiacAssembly",
        "BrainiacModule",
        "BrainiacMain",
        "Main",
        fun g ->
            let ctx = compilePrelude (g, optimizedProgram) in
                compile (g, ctx,  optimizedProgram)
    )

    (compiledAssembly, compiledType)

/// Executes the brainfuck program in the passed string.
let executeProgram contents =
    let (_, t) = buildProgramToAssembly contents in
        executeProgramInAssembly t

/// Executes the brainfuck program in the file at the path.
let executeProgramInFile path =
    let contents = readAllText path in
        let (_, t) = buildProgramToAssembly contents in
            executeProgramInAssembly t

/// Returns the string in title case, in our case we use it just for assembly names.
let asTitleCase str =
    System.Globalization.CultureInfo.InvariantCulture.TextInfo.ToTitleCase str

/// Builds the brainfuck program in the file at the path.
let buildProgramInFile (path, outputAssembly) =
    let contents = readAllText path in
        let (program, _) = buildProgramToAssembly contents in
            let outputName = Option.defaultValue (asTitleCase (Path.GetFileNameWithoutExtension path) + ".dll") outputAssembly
            writeAssemblyToFile (program, outputName)

/// Builds the brainfuck program passed as a string.
let buildProgram (contents, outputAssembly) =
    let (program, _) = buildProgramToAssembly contents in
        let outputName = Option.defaultValue "Output.dll" outputAssembly
        writeAssemblyToFile (program, outputName)

/// Handles any potential argument parsing errors.
let fail (errors : IEnumerable<Error>) =
    for e in errors do
        printfn "error: %s" (e.ToString())
    1 // nonzero return as to indicate error

/// Hack around the fact that the command line parser may break on a non-US culture like turkish.
let fixCulture() : unit =
    let _ = Thread.CurrentThread.CurrentCulture = CultureInfo("en-US")
    ()

[<EntryPoint>]
[<DynamicDependency(DynamicallyAccessedMemberTypes.All, typeof<Options>)>] // #HACK: this way, when trimming the output binary we don't remove stuff we need to dynamically access on our CLI options type
let main argv =
    fixCulture
    let result = Parser.Default.ParseArguments<Options> argv
    match result with
    | :? CommandLine.Parsed<Options> as parsed ->
        match (parsed.Value.File, parsed.Value.Program, parsed.Value.Build) with
        | (Some path, None, false) -> executeProgramInFile(path); 0
        | (Some path, None, true) -> buildProgramInFile(path, parsed.Value.Output); 0
        | (None, Some program, false) -> executeProgram(program); 0
        | (None, Some program, true) -> buildProgram(program, parsed.Value.Output); 0
        | (Some _, Some _, _) -> raise (InvalidOptionsException("brainiac: can not specify both file path and input program, must use one or the other!"))
        | (None, None, _) -> raise (InvalidOptionsException("brainiac: did not specify either input file path with: -f some_file.bf or input program with: -i '++++.', please specify one! use the --help!"))
    | :? CommandLine.NotParsed<Options> as notParsed -> fail notParsed.Errors
    | _ -> 1 // nonzero return as to indicate error