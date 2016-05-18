
#r "bin/Debug/typeProviderTest.dll" // Type Provider compiled as a DLL file
open typeProviderTest.Provided //namespace
open typeProviderTest

//#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"

open FSharp.Data
open System


type e = Provided.RealProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"helloYou()" , "type":"send" , "nextState":5  } ,
    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"goodMorning()" , "type":"receive" , "nextState":3  } ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"goodAfternoon()" , "type":"receive" , "nextState":2  } ,
    { "currentState":5 , "localRole":"Me", "partner":"Her" , "label":"helloHer()" , "type":"send" , "nextState":4  } ] """ >
let v = new e()
let a = v.instanciate().send(new e.helloYou() , e.You.instance)
//let a = v.send(new Ask(),new You())
//let a = v.receive(new Ask(),new You()).send(new response(),new You()).finish(1)

(****  Exemple for showing singleton ****)
type test = 
    new () = {}


let o = new test()
let p = new test()
let first = (o=p)


let g = e.You.instance
let h = e.You.instance
let second = (g=h)
let m = new e.You()
let n = new e.You()
let third = (m=n)

(* 
module typeProviderTest.Provider

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

//#r "../../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"

open FSharp.Data
open System
(*

let example = """ 
  [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"helloYou()" , "type":"send" , "nextState":5  } ,
    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"goodMorning()" , "type":"receive" , "nextState":3  } ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"goodAfternoon()" , "type":"receive" , "nextState":2  } ,
    { "currentState":5 , "localRole":"Me", "partner":"Her" , "label":"helloHer()" , "type":"send" , "nextState":4  } ] """


type Message() = 
    member this.hey(a:int) =
        (a, new Message())


type HelloYou() =
    inherit Message()
        member this.hey(a:int) =
        (a+1, new HelloYou())

    
let test = HelloYou()
type HelloHer() =
    inherit Message()
        member this.hey(a:int) =
        (a+2, new HelloYou())

type GoodMorning() =
    inherit Message()
        member this.hey(a:int) =
        (a+3,new HelloHer())

type GoodAfternoon() =
    inherit Message()
        member this.hey(a:int) =
        (a+4,new GoodMorning())

let hello = new HelloYou()


type receiveType<'a,'b> = 
   // val State:int
  //  val Typing:Type
   // val Typing: Message
   val Instance:'b
    new (setB:'b) =
        {Instance=setB;} 
    member this.receive(hey:'a)= // use the next and the next iteration 1 more. Le type ici doit etre HelloWorld
        // Do something i.e find the next type
        this.Instance


type End = 
    new() = {}

let R1 = receiveType<End,End>(End())
R1.receive(End())
R1.GetType()
let R2 = receiveType<HelloYou,receiveType<End,End>>(R1)
let R3 = receiveType<HelloYou,receiveType<HelloYou,receiveType<End,End>>>(R2)



let s = new HelloHer()
let t = s.hey(0)
let u = snd t
let v = HelloYou().GetType()
 
let mapping = Map.empty<_,Type>.
                Add("helloYou()",HelloYou().GetType()).
                Add("helloHer()",HelloHer().GetType()).
                Add("goodMorning()",GoodMorning().GetType()).
                Add("goodAfternoon()",GoodAfternoon().GetType())
   

type sendType<'a> = class
    val Next:int
    val Type:string
    new (state,stateType) =
        {Next = state;Type = stateType;}                   
end

type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

let protocol = ScribbleProtocole.Parse(example)

let findCurrentIndex current (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) = // gerer les cas
    let mutable inc = 0
    let mutable index = -1 
    for event in fsmInstance do
        match event.CurrentState with
            |n when n=current -> index <- inc
            | _ -> inc <- inc + 1
    index

let findNext index (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) =
    (fsmInstance.[index].NextState)

let findNextIndex current (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) =
    let index = findCurrentIndex current fsmInstance in
    let next = findNext index fsmInstance in
    findCurrentIndex next fsmInstance

findNextIndex 1 protocol

(*
type receiveType = 
    val State:int
    val Typing:Type
   // val Typing: Message
    new (current,currentType:'a) =
        {State = current;Typing=currentType;} 
    member this.receive(hey:int)= // use the next and the next iteration 1 more. Le type ici doit etre HelloWorld
        // Do something i.e find the next type
        let nextState = findNext (findCurrentIndex this.State protocol) protocol
        let nextStateIndex = findNextIndex this.State protocol in
        let typeString = protocol.[nextStateIndex].Label
        let typing = Map.find typeString mapping
        let realType = typing.GetType()
        new receiveType(nextState,realType)      *)

let typeString = protocol.[0].Label
let typage = Map.find "helloYou()" mapping
let truc = typage.GetType()
let essaye = new receiveType(1,truc)
essaye.Typing
essaye.State
let toz = typeof<HelloYou>
let s = essaye.receive()
s.Typing
s.State
let t = s
t.State
t.Typing
let u = t.receive(new GoodMorning())
u.State*)
type End = class
    new ()  = {}
    member this.finish something =
        printf("Okkk")
end
//type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

// This defines the type provider. When this will be compiled as a DLL file, we can add this type, as a reference
// to the type provider, in a project
[<TypeProvider>] 
type ProviderTest() as this= (* This is the type of the type provider we will use later
     Inheriting from this type provides implementations of ITypeProvider *)
    inherit TypeProviderForNamespaces() (* It allow to inject a namespace with types into the assembly built by the compiler. 
    There are other options but this one is the only used *)
    (*type Message() = 
        member this.hey(a:int) =
            (a, new Message())*)


    (*type HelloYou() =
        inherit Message()
            member this.hey(a:int) =
            (a+1, new HelloYou())

    
    let test = HelloYou()
    type HelloHer() =
        inherit Message()
            member this.hey(a:int) =
            (a+2, new HelloYou())

    type GoodMorning() =
        inherit Message()
            member this.hey(a:int) =
            (a+3,new HelloHer())

    type GoodAfternoon() =
        inherit Message()
            member this.hey(a:int) =
            (a+4,new GoodMorning())*)

   // let hello = new HelloYou()
    // The namespace provided to differentiate the classes with the same name but located in different namespace
    let ns = "typeProviderTest.Provided"    
    
    // The assembly = the file that contains metadata that specifies types and ressources available within the assembly file .dll
    // or .exe
    let asm = Assembly.GetExecutingAssembly()

    // Creation of type provider with parameters + instanciation of a class
(*    let createType name (parameters: obj []) = // function with the name of the type and the parameters in args
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        // We define here a property

       // let protocol = ScribbleProtocole.Parse(example)
        //let hello = new HelloYou()
        let myProperty = 
            ProvidedProperty(
                "MyProperty", // name of the property
                typeof<string>, // type of the Property
                IsStatic = true, 
                GetterCode = (fun _ -> <@@ fsm @@>) (* F# Quotation = in F# Core Library : wrap a piece of code 
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
                [ProvidedParameter("Prefix", typeof<int>)],
                typeof<ProvidedMethod>,
                InvokeCode = fun args -> <@@ (%%args.[1]:>string)  + unbox<string> (%%args.[0]:obj) @@>
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

    let hello = ProvidedTypeDefinition(asm,ns,"Hello",Some typeof<obj>)
    let bye =  ProvidedTypeDefinition(asm,ns,"Bye",Some typeof<obj>)
    
    let parameters = 
        [ProvidedStaticParameter("SomeString",typeof<string>)]
    do
        
        providedType.DefineStaticParameters(parameters, createType) 
        (* we make a callback to the createType function previously defined, and what's returned by the createType function will be the type provided with all its method and everything *)
        this.AddNamespace(ns, [providedType;hello;bye]) // We add the providedType to the namespace


        *)


    let makeOneType (n:int) = 
        let t = ProvidedTypeDefinition("Type" + string n,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>)
        t.AddMember(ctor)
        t
    
    let n = 1
    let types = [for i in 1..n -> makeOneType i]
    let addProp (l:ProvidedTypeDefinition list) =
        match l with
        |[] -> ()
        |[a] -> let myMethod = ProvidedMethod("send",[ProvidedParameter("Label", typeof<string>)],typeof<End>,
                                                InvokeCode = fun args -> <@@ new End() @@>) in
                a.AddMember(myMethod)
                let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                GetterCode = fun args -> <@@ "Type2" @@>)
                a.AddMember(myProp)
        |hd::tl -> let nextType = tl.Head
                   let myMethod = ProvidedMethod("send",[ProvidedParameter("Label", typeof<string>)],typeof<int>,
                                                  InvokeCode = fun args -> <@@ 5 @@>)
                   hd.AddMember(myMethod)
                   let name = hd.GetTypeInfo().Name
                   let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                GetterCode = fun args -> <@@ name @@>)
                   hd.AddMember(myProp)
                   //addProp tl
    let v = addProp types
    let t = [makeOneType 1]

    (*let c = nextType.GetConstructors().[0]
                                                           Expr.NewObject(c, [])*)

    (*let createTypes name (parameters:obj[]) =

        let myType = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>)
        let myProp = ProvidedProperty(name, typeof<string>, IsStatic = true,
                                        GetterCode = fun args -> <@@ "Hello world" @@>)
        myType.AddMember(myProp)

        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor(
                        [ProvidedParameter("InnerState", typeof<string>)],
                        InvokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>,
                            GetterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        let myMethod = ProvidedMethod("InstanceMethod",[ProvidedParameter("Prefix", typeof<string>)],typeof<string>,
                            InvokeCode = fun args -> <@@ (%%args.[1])  + unbox<string> (%%args.[0]:obj) @@>)
        myType.AddMember(myMethod)

        myType

    let providedType = ProvidedTypeDefinition(asm,ns,"SPECIAL",Some typeof<obj>)
    let parameters = 
        [ProvidedStaticParameter("Something",typeof<string>)]*)
    do
        this.AddNamespace(ns,t)
       // providedType.DefineStaticParameters(parameters,createTypes)
       // this.AddNamespace(ns, [providedType])
[<assembly:TypeProviderAssembly>]
    do()

 *)