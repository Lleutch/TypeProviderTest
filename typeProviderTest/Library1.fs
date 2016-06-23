module typeProviderTest.Provider

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System

type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

type Agent<'T> = MailboxProcessor<'T>




// This defines the type provider. When this will be compiled as a DLL file, we can add this type, as a reference
// to the type provider, in a project
[<TypeProvider>]
type ProviderTest(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    let ns = "typeProviderTest.Provided"
    let asm = Assembly.LoadFrom(config.RuntimeAssembly)


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

    // Changer sa avec des OR et tout sa mais faire gaffe à pas faire de la merde
    let rec alreadySeen (liste:string list) (s:string) =
        match liste with
            | [] -> false
            | hd::tl -> if hd.Equals(s) then
                            true
                        else
                            alreadySeen tl s

    let findSameCurrent currentState  (fsmInstance:ScribbleProtocole.Root [])  =
        let mutable list = []
        let mutable inc = 0
        for event in fsmInstance do
            if event.CurrentState = currentState then
                list <- inc::list
            inc <- inc+1
        list

    // Test this function by changing t with t+1 and see the mistakes happen  -> generate the useless ProvidedTypeDefinition and throw exception cause it
    // is not added to the assembly.
    let rec findProvidedType (providedList:ProvidedTypeDefinition list) stateValue =
        match providedList with
            |[] -> // Useless case, t is useless but we need this case due to pattern matching exhaustiveness.
                   let t = ProvidedTypeDefinition("CodingMistake",None)
                   t.AddXmlDoc("This State Was not found in the list of state types generated. This is probaly due to the way this list of state types is generated. This Should never Happen!!!! This Case should never happen, because we have generated the states correctly!!!!!")
                   t
            |[a] -> let t = ref 0
                    if System.Int32.TryParse(a.Name.Replace("State",""),t) && (!t)=stateValue then
                        a
                    else 
                        findProvidedType [] stateValue    
            |hd::tl -> let t = ref 0
                       if System.Int32.TryParse(hd.Name.Replace("State",""),t) && (!t)=stateValue then
                           hd
                       else
                           findProvidedType tl stateValue      


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
        let mutable mapping = Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole,t)
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

    let makeChoiceType (nextType:ProvidedTypeDefinition) (event:ScribbleProtocole.Root) (choiceType:ProvidedTypeDefinition) =
        let c = nextType.GetConstructors().[0]
        let expression = Expr.NewObject(c, [])  
        let name = event.Label.Replace("(","").Replace(")","") 
        let t = ProvidedTypeDefinition(name, None, IsErased = false)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
        t.AddMember(ctor)
        let myMethod = ProvidedMethod("next",[],nextType,InvokeCode = fun args -> expression) in
        t.AddMember(myMethod) 
        t.SetBaseTypeDelayed(fun() -> choiceType.DeclaringType.GetNestedType("LabelChoice"+ string event.CurrentState))
        t

    // Refactorer un max
    let makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) = 
        let mutable listeLabelSeen = []
        let mutable listeType = []
        let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
        for event in fsmInstance do
            if (event.Type.Contains("choice") && not(alreadySeen listeLabelSeen event.Label)) then
                let choiceType = ProvidedTypeDefinition("LabelChoice"+ string event.CurrentState, Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                choiceType.AddMember(ctor)
                let myMethod = ProvidedMethod("labelChoice" + string event.CurrentState ,[],typeof<unit>,InvokeCode = fun args -> <@@ () @@>) in
                choiceType.AddMember(myMethod)
                mapping <- mapping.Add("LabelChoice"+ string event.CurrentState,choiceType)
                listeType <- choiceType::listeType  
                let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
                let rec aux (liste:int list) =
                    match liste with
                        |[] -> ()
                        |[aChoice] -> if not(alreadySeen listeLabelSeen fsmInstance.[aChoice].Label) then
                                        let nextType = findProvidedType providedList fsmInstance.[aChoice].NextState
                                        let state = fsmInstance.[aChoice]
                                        let t = makeChoiceType nextType state choiceType
                                        mapping <- mapping.Add(state.Label,t)
                                        listeLabelSeen <- state.Label::listeLabelSeen
                                        listeType <- t::listeType     
                        |hd::tl -> if not(alreadySeen listeLabelSeen fsmInstance.[hd].Label) then
                                        let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                                        let state = fsmInstance.[hd]
                                        let t = makeChoiceType nextType state choiceType 
                                        mapping <- mapping.Add(state.Label,t)
                                        listeLabelSeen <- state.Label::listeLabelSeen
                                        listeType <- t::listeType  
                                        aux tl 
                in aux listIndexChoice 
            else if not(alreadySeen listeLabelSeen event.Label) then
                let name = event.Label.Replace("(","").Replace(")","") 
                let t = ProvidedTypeDefinition(name,Some typeof<obj>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                t.AddMember(ctor)
                mapping <- mapping.Add(event.Label,t)
                listeLabelSeen <- event.Label::listeLabelSeen
                listeType <- t::listeType
        (mapping,listeType)

    let makeStateType (n:int) (s:string) = 
        let t = ProvidedTypeDefinition(s + string n,Some typeof<obj>)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "MakeStateType" :> obj @@>)
        t.AddMember(ctor)
        t
    let makeStateType (n:int) = makeStateType n "State"


    let rec goingThrough (methodName:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
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

    // REfactorer cela au max
    let rec addProperty (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
        let currentState = stateList.Head
        let indexOfState = findCurrentIndex currentState fsmInstance
        let indexList = findSameCurrent currentState fsmInstance 
        let mutable methodName = "finish"
        if indexOfState <> -1 then
            methodName <- fsmInstance.[indexOfState].Type
        match providedList with
            |[] -> ()
            |[aType] -> match methodName with
                            |"send" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            |"receive" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                         let c = labelType.GetConstructors().[0]
                                         let expression = Expr.NewObject(c,[])
                                         let myMethod = ProvidedMethod("receive",[], labelType,InvokeCode = fun args -> expression )in
                                         aType.AddMember(myMethod) 
                            |"finish" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                        GetterCode = fun args -> <@@ "essaye Bateau" @@>)
                        aType.AddMember(myProp)
            |hd::tl ->  match methodName with
                            |"send" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            |"receive" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                         let c = labelType.GetConstructors().[0]
                                         let expression = Expr.NewObject(c,[]) 
                                         let myMethod = ProvidedMethod("receive",[], labelType,InvokeCode = fun args -> expression )in
                                         hd.AddMember(myMethod)
                            |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                       GetterCode = fun args -> <@@ "Test" @@>)
                        hd.AddMember(myProp)
                        addProperty providedListStatic tl (stateList.Tail) mLabel mRole fsmInstance      

    let contains (aSet:Set<'a>) x = Set.exists ((=) x) aSet

    let stateSet (fsmInstance:ScribbleProtocole.Root []) =
        let firstState = fsmInstance.[0].CurrentState
        let mutable setSeen = Set.empty
        let mutable counter = 0
        for event in fsmInstance do
            if (not(contains setSeen event.CurrentState) || not(contains setSeen event.NextState)) then
                setSeen <- setSeen.Add(event.CurrentState)
                setSeen <- setSeen.Add(event.NextState)
        (setSeen.Count,setSeen,firstState)


    let createType (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
//we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let protocol = ScribbleProtocole.Parse(fsm)
        let triple= stateSet protocol
        let n,stateSet,firstState = triple
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let firstStateType = findProvidedType listTypes firstState
        let myMethod = ProvidedMethod("instanciate",[], firstStateType,InvokeCode = (fun args -> let c = firstStateType.GetConstructors().[0] 
                                                                                                 Expr.NewObject(c, [])))  
        let tupleLabel = makeLabelTypes protocol listTypes
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        addProperty listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol
        let stuff = list2.ToString()
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "hey" + string n @@>  )
        let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
        let memberList = listTypes |> List.append list2 |> List.append list1 
        t.AddMembers(memberList)
        t.AddMember(myMethod)
        t.AddMember(ctor)
        t

    let providedType = ProvidedTypeDefinition(asm,ns,"RealProvider",Some typeof<obj>)
    let parameters = [ProvidedStaticParameter("Something",typeof<string>)]

    do
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [providedType] )
[<assembly:TypeProviderAssembly>]
    do()

