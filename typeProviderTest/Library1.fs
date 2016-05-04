module typeProviderTest.Provider

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

// This defines the type provider. When this will be compiled as a DLL file, we can add this type, as a reference
// to the type provider, in a project

[<TypeProvider>] 
type ProviderTest() as this= (* This is the type of the type provider we will use later
     Inheriting from this type provides implementations of ITypeProvider *)
    inherit TypeProviderForNamespaces() (* It allow to inject a namespace with types into the assembly built by the compiler. 
    There are other options but this one is the only used *)
    
    // The namespace provided to differentiate the classes with the same name but located in different namespace
    let ns = "typeProviderTest.Provided"    
    
    // The assembly = the file that contains metadata that specifies types and ressources available within the assembly file .dll
    // or .exe
    let asm = Assembly.GetExecutingAssembly()

    let createType name (parameters: obj []) =
        let aString = parameters.[0] :?> string
        // We define here a property
        let myProp = 
            ProvidedProperty(
                "MyProperty", // name of the property
                typeof<string>, // type of the Property
                IsStatic = true, // 
                GetterCode = (fun _ -> <@@ aString @@>) (* F# Quotation = in F# Core Library : wrap a piece of code 
                in the quotation. When the compiler gets to the point to compile that piece of code, it creates an AST 
                (Abstract Syntax Tree) and then it stops. And then Instead of generating the IL (Intermediate Language)
                to be running on run-time it gives back the AST and we can do what we want with it.*)
            )

        let instanceProperty =
            ProvidedProperty(
                "InstanceProperty",
                typeof<string>,
                IsStatic = false,
                GetterCode = fun args -> <@@ unbox<string> (%%args.[0]:obj) @@>
            )
        let myMethod = 
            ProvidedMethod(
                "InstanceMethod",
                [ProvidedParameter("Prefix", typeof<string>);ProvidedParameter("Middle", typeof<string>)],
                typeof<string>,
                IsStaticMethod = false,
                InvokeCode = fun args -> <@@ (%%args.[1]:string) + " : " + (%%args.[2]:string) + unbox<string> (%%args.[0]:obj) @@>
            )


        let myMethodNum2 = 
            ProvidedMethod(
                "InstanceMethodNum2",
                [ProvidedParameter("a", typeof<int>);ProvidedParameter("b", typeof<int>)],
                typeof<_>,
                IsStaticMethod = false,
                InvokeCode = fun args -> <@@ [(%%args.[1]:int);(%%args.[2]:int)] @@>
            )

        let cstor = 
            ProvidedConstructor(
                [ProvidedParameter("instanceString", typeof<string>)],
                InvokeCode = fun args -> <@@ box (%%args.[0]:string) @@>
            )
        // We define here a type that will have the property previously defined.
        let myType =
            ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>) 
        myType.AddMember myProp
        myType.AddMember instanceProperty
        myType.AddMember myMethod
        myType.AddMember myMethodNum2
        myType.AddMember cstor
        myType

    let provider =
        ProvidedTypeDefinition(asm, ns , "MyParaProvider", Some typeof<obj>)

    let parameters = 
        [ProvidedStaticParameter("AString",typeof<string>)]
    do
        
        provider.DefineStaticParameters(parameters, createType)
        this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
    do()

