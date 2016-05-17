﻿module typeProviderTest.Provider

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System

type End = class
    new ()  = {}
    member this.finish something =
        printf("Ok")
end

type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

// This defines the type provider. When this will be compiled as a DLL file, we can add this type, as a reference
// to the type provider, in a project
[<TypeProvider>]
type ProviderTest(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    let ns = "typeProviderTest.Provided"
    let asm = Assembly.GetExecutingAssembly()


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

    let findNextIndex currentState (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) =
        let index = findCurrentIndex currentState fsmInstance in
        let next = findNext index fsmInstance in
        findCurrentIndex next fsmInstance

    let rec alreadySeen (liste:string list) (s:string) =
        match liste with
            | [] -> false
            | hd::tl -> if hd.Equals(s) then
                            true
                        else
                            alreadySeen tl s


    let makeRoleTypes (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) = 
        let mutable liste = [fsmInstance.[0].LocalRole]
        let mutable listeType = []
        let t =  ProvidedTypeDefinition(asm,ns,fsmInstance.[0].LocalRole,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
        t.AddMember(ctor)
        listeType <- t::listeType
        let mutable mapping = Map.empty<_,System.Type>.Add(fsmInstance.[0].LocalRole,t)
        for event in fsmInstance do
            if not(alreadySeen liste event.Partner) then
                let t = ProvidedTypeDefinition(asm,ns,event.Partner,Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                t.AddMember(ctor)
                mapping <- mapping.Add(event.Partner,t)
                liste <- event.Partner::liste
                listeType <- t::listeType
        (mapping,listeType)

    let makeLabelTypes (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) = 
        let mutable liste = []
        let mutable listeType = []
        let mutable mapping = Map.empty<_,System.Type>
        for event in fsmInstance do
            if not(alreadySeen liste event.Label) then
                let name = event.Label.Replace("(","").Replace(")","") 
                let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                t.AddMember(ctor)
                mapping <- mapping.Add(event.Label,t)
                liste <- event.Label::liste
                listeType <- t::listeType
        (mapping,listeType)

    let makeStateType (n:int) (s:string) = 
        let t = ProvidedTypeDefinition(asm,ns, s + string n,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "MakeStateType" :> obj @@>)
        t.AddMember(ctor)
        t
    let makeStateType (n:int) = makeStateType n "Type"

    let rec addProp (l:ProvidedTypeDefinition list) index (mLabel:Map<string,Type>) (mRole:Map<string,Type>) (fsmInstance:FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>.Root []) =
        if (index <> -1) then
            match l with
            |[] -> ()
            |[a] -> match fsmInstance.[index].Type with
                    |"send" ->  let myMethod = ProvidedMethod("send",[ProvidedParameter("Label",mLabel.[fsmInstance.[index].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[index].Partner])],
                                                                typeof<End>,InvokeCode = fun args -> <@@ new End() @@>) in
                                a.AddMember(myMethod)
                    |"receive" -> let myMethod = ProvidedMethod("receive",[ProvidedParameter("Label",mLabel.[fsmInstance.[index].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[index].Partner])],
                                                                typeof<End>,InvokeCode = fun args -> <@@ new End() @@>) in
                                  a.AddMember(myMethod)
                    | _ -> printfn "Not correct"
                    let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                    GetterCode = fun args -> <@@ "essaye Bateau" @@>)
                    a.AddMember(myProp)
            |hd::tl -> let nextType = tl.Head
                       match fsmInstance.[index].Type with
                       |"send" -> let myMethod = ProvidedMethod("send",[ProvidedParameter("Label",mLabel.[fsmInstance.[index].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[index].Partner])],
                                                                  nextType,InvokeCode = (fun args -> let c = nextType.GetConstructors().[0]
                                                                                                     Expr.NewObject(c, [])))
                                  hd.AddMember(myMethod)
                       |"receive" -> let myMethod = ProvidedMethod("receive",[ProvidedParameter("Label",mLabel.[fsmInstance.[index].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[index].Partner])],
                                                                    nextType, InvokeCode = (fun args -> let c = nextType.GetConstructors().[0]
                                                                                                        Expr.NewObject(c, [])))
                                     hd.AddMember(myMethod)
                       | _ -> printfn "Not correct"
                       let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                    GetterCode = fun args -> <@@ "Test" @@>)
                       hd.AddMember(myProp)
                       let nextIndex = findNextIndex fsmInstance.[index].CurrentState fsmInstance
                       addProp tl nextIndex mLabel mRole fsmInstance

   
    let fsm = """ [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"helloYou()" , "type":"send" , "nextState":5  } ,
                    { "currentState":4 , "localRole":"Me", "partner":"You" , "label":"goodMorning()" , "type":"receive" , "nextState":3  } ,
                    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"goodAfternoon()" , "type":"receive" , "nextState":2  } ,
                    { "currentState":5 , "localRole":"Me", "partner":"Her" , "label":"helloHer()" , "type":"send" , "nextState":4  } ] """
   (* let createType (name:string) (parameters:obj[]) =
        fsm <- parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let protocol = ScribbleProtocole.Parse(fsm)
        let n = protocol.Length
        let listTypes = [for i in 1..n -> makeStateType i]
        let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "Weird stuff" @@>  )
        t.AddMember(ctor)
        let myMethod = ProvidedMethod("instanciate",[], listTypes.Head,InvokeCode = (fun args -> let c = listTypes.Head.GetConstructors().[0]
                                                                                                 Expr.NewObject(c, [])))
        t.AddMember(myMethod)
        t.AddMembers(listTypes)
        let tupleLabel = makeLabelTypes protocol
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        addProp listTypes (findCurrentIndex 1 protocol) (fst tupleLabel) (fst tupleRole) protocol
        let tmpList = List.append list1 list2
        let totalList = List.append listTypes tmpList
        *)

    let createTypes() =
        let protocol = ScribbleProtocole.Parse(fsm)
        let n = protocol.Length
        let types = [for i in 1..n -> makeStateType i]
        let tupleLabel = makeLabelTypes protocol
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        addProp types (findCurrentIndex 1 protocol) (fst tupleLabel) (fst tupleRole) protocol
        let tmpList = List.append list1 list2
        let totalList = List.append types tmpList
        totalList
        

    let providedType = ProvidedTypeDefinition(asm,ns,"RealProvider",Some typeof<obj>)
    let parameters = [ProvidedStaticParameter("Something",typeof<string>)]
    do
        providedType.DefineStaticParameters(parameters,createType)
        //let c = totalList <- providedType::totalList // c = useless
        this.AddNamespace(ns, createTypes())
[<assembly:TypeProviderAssembly>]
    do()

