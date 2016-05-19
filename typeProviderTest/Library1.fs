module typeProviderTest.Provider

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


    let findCurrentIndex current (fsmInstance:ScribbleProtocole.Root []) = // gerer les cas
        let mutable inc = 0
        let mutable index = -1 
        for event in fsmInstance do
            match event.CurrentState with
                |n when n=current -> index <- inc
                | _ -> inc <- inc + 1
        index

    let findNext index (fsmInstance:ScribbleProtocole.Root []) =
        (fsmInstance.[index].NextState)

    let findNextIndex currentState (fsmInstance:ScribbleProtocole.Root []) =
        let index = findCurrentIndex currentState fsmInstance in
        let next = findNext index fsmInstance in
        findCurrentIndex next fsmInstance

    let findSameNext nextState  (fsmInstance:ScribbleProtocole.Root [])  =
        let mutable list = []
        let mutable inc = 0
        for event in fsmInstance do
            if event.NextState = nextState then
                list <- inc::list
            inc <- inc+1
        list

    let rec alreadySeen (liste:string list) (s:string) =
        match liste with
            | [] -> false
            | hd::tl -> if hd.Equals(s) then
                            true
                        else
                            alreadySeen tl s

    let makeRoleTypes (fsmInstance:ScribbleProtocole.Root []) = 
        let mutable liste = [fsmInstance.[0].LocalRole]
        let mutable listeType = []
        let t =  ProvidedTypeDefinition(fsmInstance.[0].LocalRole,Some typeof<obj>)
        let ctor = ProvidedConstructor([ProvidedParameter("Voir",typeof<int>)], InvokeCode = fun args -> <@@ "le Role est unique" :> obj @@>) // add argument later
        t.AddMember(ctor)
        let myProp = ProvidedProperty("instance", t, IsStatic = true,
                                                GetterCode = (fun args -> Expr.NewObject(ctor,[]) ))
        t.AddMember(myProp)
        listeType <- t::listeType
        let mutable mapping = Map.empty<_,System.Type>.Add(fsmInstance.[0].LocalRole,t)
        for event in fsmInstance do
            if not(alreadySeen liste event.Partner) then
                let t = ProvidedTypeDefinition(event.Partner,Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "le Role est unique" :> obj @@>) // add argument later
                t.AddMember(ctor)
                let myProp = ProvidedProperty("instance", t, IsStatic = true,
                                                    GetterCode = (fun args -> Expr.NewObject(ctor,[]) ))
                t.AddMember(myProp)
                mapping <- mapping.Add(event.Partner,t)
                liste <- event.Partner::liste
                listeType <- t::listeType
        (mapping,listeType)

    let makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) = 
        let mutable liste = []
        let mutable listeType = []
        let mutable mapping = Map.empty<_,System.Type>
        for event in fsmInstance do
            if not(alreadySeen liste event.Label) then
                let name = event.Label.Replace("(","").Replace(")","") 
                let t = ProvidedTypeDefinition(name,Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                t.AddMember(ctor)
                mapping <- mapping.Add(event.Label,t)
                liste <- event.Label::liste
                listeType <- t::listeType
        (mapping,listeType)

    let makeStateType (n:int) (s:string) = 
        let t = ProvidedTypeDefinition(s + string n,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "MakeStateType" :> obj @@>)
        t.AddMember(ctor)
        t
    let makeStateType (n:int) = makeStateType n "State"
    
    let findSameCurrent currentState  (fsmInstance:ScribbleProtocole.Root [])  =
        let mutable list = []
        let mutable inc = 0
        for event in fsmInstance do
            if event.CurrentState = currentState then
                list <- inc::list
            inc <- inc+1
        list

    let rec findProvidedType (list:'a list) stateValue =
        match stateValue with
        |1 -> list.Head
        |k -> findProvidedType list.Tail (stateValue-1)


    let rec goingThrough (methodName:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) (mLabel:Map<string,Type>) (mRole:Map<string,Type>) (fsmInstance:ScribbleProtocole.Root []) =
         match indexList with
         |[] -> // Last state: no next state possible
                let myMethod = ProvidedMethod(methodName,[],typeof<unit>,InvokeCode = fun args -> <@@ printfn "finish" @@>) in
                aType.AddMember(myMethod)
                //printfn " There is a mistake, no index? should never happen, weird issue!!! "
         |[b] -> let nextType = findProvidedType providedList fsmInstance.[b].NextState
                 let c = nextType.GetConstructors().[0]
                 let expression = Expr.NewObject(c, [])
                 let myMethod = ProvidedMethod(methodName,[ProvidedParameter("Label",mLabel.[fsmInstance.[b].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[b].Partner])],
                                                                          nextType,InvokeCode = fun args -> expression) in
                 aType.AddMember(myMethod)
         |hd::tl -> let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                    let c = nextType.GetConstructors().[0]
                    let expression = Expr.NewObject(c, [])
                    let myMethod = ProvidedMethod(methodName,[ProvidedParameter("Label",mLabel.[fsmInstance.[hd].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[hd].Partner])],
                                                                             nextType,InvokeCode = fun args -> expression) in
                    aType.AddMember(myMethod)    
                    goingThrough methodName providedList aType tl mLabel mRole fsmInstance


    let rec addProperty (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (currentState:int) (mLabel:Map<string,Type>) (mRole:Map<string,Type>) (fsmInstance:ScribbleProtocole.Root []) =
        let indexOfState = findCurrentIndex currentState fsmInstance
        //if (indexOfState <> -1) then // J'en ai plus besoin géré par method goingThrough
        match providedList with
            |[] -> ()
            |[aType] -> let indexList = findSameCurrent currentState fsmInstance 
                        let mutable methodName = "finish"
                        if indexOfState <> -1 then
                            methodName <- fsmInstance.[indexOfState].Type
                        match methodName with
                            |"send" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            |"receive" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            |"finish" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                        GetterCode = fun args -> <@@ "essaye Bateau" @@>)
                        aType.AddMember(myProp)
            |hd::tl ->  let indexList = findSameCurrent currentState fsmInstance 
                        let mutable methodName = "finish"
                        if indexOfState <> -1 then
                            methodName <- fsmInstance.[indexOfState].Type
                        match methodName with
                            |"send" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            |"receive" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                       GetterCode = fun args -> <@@ "Test" @@>)
                        hd.AddMember(myProp)
                        addProperty providedListStatic tl (currentState + 1) mLabel mRole fsmInstance      

    let contains (aSet:Set<'a>) x = Set.exists ((=) x) aSet

    let numberOfState (fsmInstance:ScribbleProtocole.Root []) = 
        let mutable setSeen = Set.empty
        let mutable counter = 0
        for event in fsmInstance do
            if (not(contains setSeen event.CurrentState) || not(contains setSeen event.NextState)) then
                setSeen <- setSeen.Add(event.CurrentState)
                setSeen <- setSeen.Add(event.NextState)
        setSeen.Count

    let createType (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
//we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let protocol = ScribbleProtocole.Parse(fsm)
        let n = numberOfState protocol
        let listTypes = [for i in 1..n -> makeStateType i]
        let tupleLabel = makeLabelTypes protocol
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        let currentState = 1
        addProperty listTypes listTypes currentState (fst tupleLabel) (fst tupleRole) protocol
        let stuff = list2.ToString()
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "hey" + string n @@>  )
        let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
        let myMethod = ProvidedMethod("instanciate",[], listTypes.Head,InvokeCode = (fun args -> let c = listTypes.Head.GetConstructors().[0] // Changer sa : pas forcement state1
                                                                                                 Expr.NewObject(c, [])))  
        t.AddMember(ctor)
        t.AddMember(myMethod)
        t.AddMembers(list2)
        t.AddMembers(list1)
        t.AddMembers(listTypes)
        t

    let providedType = ProvidedTypeDefinition(asm,ns,"RealProvider",Some typeof<obj>)
    let parameters = [ProvidedStaticParameter("Something",typeof<string>)]

    do
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [providedType] )
[<assembly:TypeProviderAssembly>]
    do()

