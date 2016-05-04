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


    // Creation of type provider with parameters + instanciation of a class
    let createType name (parameters: obj []) = // function with the name of the type and the parameters in args
        let aParam = parameters.[0] (* :?> string   this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        // We define here a property
        let myProperty = 
            ProvidedProperty(
                "MyProperty", // name of the property
                typeof<string>, // type of the Property
                IsStatic = true, 
                GetterCode = (fun _ -> <@@ aParam @@>) (* F# Quotation = in F# Core Library : wrap a piece of code 
                in the quotation. When the compiler gets to the point to compile that piece of code, it creates an AST 
                (Abstract Syntax Tree) and then it stops. And then Instead of generating the IL (Intermediate Language)
                to be running on run-time it gives back the AST and we can do what we want with it.*)
            )


        // This is the part where we create a type provider with which we can provide a type and instanciate this type, and use methods
        let instanceProperty =
            ProvidedProperty( // This is the function to instanciate a type that we can find in the ProvidedTypes.fs file
                "InstanceProperty",
                typeof<string>,
                IsStatic = false, // This is not a static property which means that the function used for the GetterCode has arguments
                GetterCode = fun args -> <@@ unbox<string> (%%args.[0]:obj) @@>
            )

        // First method to add a prefix and middle part to the string
        let myMethod = 
            ProvidedMethod(
                "InstanceMethod",
                [ProvidedParameter("Prefix", typeof<string>);ProvidedParameter("Middle", typeof<string>)],
                typeof<string>,
                IsStaticMethod = false,
                InvokeCode = fun args -> <@@ (%%args.[1]:string) + " : " + (%%args.[2]:string) + " : " + unbox<string> (%%args.[0]:obj) @@>
            )

        // Second Method
        let myMethodNum2 = 
            ProvidedMethod(
                "InstanceMethodNum2",
                [ProvidedParameter("a", typeof<int>);ProvidedParameter("b", typeof<int>)],
                typeof<int list>,
                IsStaticMethod = false,
                InvokeCode = fun args -> <@@ [(%%args.[1]:int);(%%args.[2]:int)] @@>
            )

        // Constructor that needs one argument as a parameter (string).
        // This argument is boxed i.e "loses" its type to become a generic object.
        // Being unboxed is the opposite: to recover you type
        let constructeur = 
            ProvidedConstructor(
                [ProvidedParameter("instanceString", typeof<string>)],
                InvokeCode = fun args -> <@@ box (%%args.[0]:string) @@>
            )
        // We define here a type that will have the property previously defined.
        let myType =
            ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>) 

        // We add all the different possible Members to our type
        myType.AddMember myProperty
        myType.AddMember instanceProperty
        myType.AddMember myMethod
        myType.AddMember myMethodNum2
        myType.AddMember constructeur
        myType // That's the return value of the createType function

    let providedType =
        ProvidedTypeDefinition(asm, ns , "ParameterProvider", Some typeof<obj>)

    let parameters = 
        [ProvidedStaticParameter("s",typeof<string>)]
    do
        
        providedType.DefineStaticParameters(parameters, createType) 
        (* we make a callback to the createType function previously defined, and what's returned by the createType function will be the type provided *)
        this.AddNamespace(ns, [providedType]) // We add the providedType to the namespace

[<assembly:TypeProviderAssembly>]
    do()

