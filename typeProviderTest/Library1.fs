module typeProviderTest.Provider

open FSharp.Core.CompilerServices

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly

open Microsoft.FSharp.Data
open System
open System.Net.Sockets
open System.IO
open System.Threading


type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":1 , "localRole":"Me", "partner":"You" , "label":"hello()" , "type":"send" , "nextState":2  } ] """>

type ILocalType() = class end// Interface
    //abstract GetMessage : unit -> byte []
type RoleType = class end  // Super Class Qui ne sert a rien pour le moment

type Agent<'T> = MailboxProcessor<'T>

type Message =
    |SendMessage of ILocalType * string
    |ReceiveMessage of ILocalType * AsyncReplyChannel<ILocalType> * string

type AgentSender(tcpClientSend:TcpClient) =
    
    let stream = tcpClientSend.GetStream()

    let send (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |ReceiveMessage (message,channel,role) -> 
                    () // Montrer que ce cas est un erreur (Throw une erreur?)
                    return! loop()      
                |SendMessage (message,role) -> // Faire plus de choses
                    do! stream.AsyncWrite("HEY"B)//  SE SERAIT COOL COMME SA message.GetMessage())
                    return! loop()
            }
        in loop()

    let mutable agentSender = None

    // Ne sert a rien pour le moment et meme plus tard ne devrait servir a rien
    member this.SendMessage(message) =
        match (agentSender:Option<MailboxProcessor<Message>>) with
            |None -> ()
            |Some sending -> sending.Post(Message.SendMessage message)
    member this.Start() =
        let truc = Agent.Start(send)
        agentSender <- Some truc

    

type AgentReceiver(ipAddress,port) =

    let mutable clientMap = Map.empty

    let readAllBytes (s : Stream) = 
        let ms = new MemoryStream()
        s.CopyTo(ms)
        ms.ToArray()

    let binding (tcpListenerReceive:TcpListener) (actor:Agent<Message>) = 
        let rec loop () = async {
            let client = tcpListenerReceive.AcceptTcpClient()
            let stream = client.GetStream()
            // Lit le role de ce stream
            let readRole = readAllBytes stream
            clientMap <- clientMap.Add(readRole.ToString(),stream)
            return! loop()
            }
        in loop()

    let receive (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
                |SendMessage (message,role)-> 
                    () // Montrer que ce cas est un erreur (Throw une erreur?) = pour faire du debuggage
                    return! loop()      
                |ReceiveMessage (message,channel,role) -> // Faire plus de choses i.e verifier que c'est correct.
                    let stream = clientMap.[role]
                    let read = readAllBytes stream
                    // A changer c'est juste pour s'en rappeller
                    if ( read.Length > 0 ) then
                        channel.Reply(message)
                    return! loop() 
            }
        in loop()


    let mutable tcpListenerReceive = None
    let mutable agentReceiver = None 
    
    member this.Start()=
        let truc = new TcpListener(Net.IPAddress.Parse(ipAddress),port)
        tcpListenerReceive <- Some truc // A changer
        match (tcpListenerReceive:Option<TcpListener>) with 
            |None -> ()
            |Some receiving -> receiving.Start()
                               let agentClients = Agent.Start(binding receiving) 
                               let chose = Agent.Start(receive)
                               agentReceiver <- Some chose
    
    // Pour close le listener a faire dans finish.
    member this.Stop() =
        for client in clientMap do
            client.Value.Close()
        match tcpListenerReceive with
            |None -> ()
            |Some receive -> receive.Stop()
    // Ne sert a rien pour le moment et meme plus tard ne devrait servir a rien
    member this.ReceiveMessage(message) =
        match agentReceiver with
            |Some receive -> receive.PostAndAsyncReply(fun _ -> Message.ReceiveMessage message)
            |None -> async{
                        return new ILocalType()
                     }        
    // Sa sert a quoi?
    member this.ReadMessage(message:string) =
        message




type AgentRouter(agentMap:Map<string,AgentSender>,agentReceiving:AgentReceiver) =
    let agentMapping = agentMap
    let agentReceiver = agentReceiving

    let sendAndReceive (agentRouter:Agent<Message>) =
        let rec loop () = async{
            let!  msg = agentRouter.Receive()
            match msg with
                |SendMessage (message,role) ->
                    let agentSender = agentMapping.[role]
                    agentSender.SendMessage(message,role)
                    return! loop()
                |ReceiveMessage (message,channel,role) -> // Faire quelque chose
                    let! replyMessage = agentReceiver.ReceiveMessage(message,channel,role)
                    channel.Reply(replyMessage)
                    return! loop()
            }
        in loop()
    
    let agentRouter = Agent.Start(sendAndReceive)

    member this.Start() =
        agentReceiver.Start()
        for sender in agentMapping do
            sender.Value.Start()
                
    member this.SendMessage(message) =
        agentRouter.Post(Message.SendMessage message)
    
    member this.ReceiveMessage(message)=
        let (msg,role) = message
        async{
            let! replyMessage = agentRouter.PostAndAsyncReply(fun channel -> Message.ReceiveMessage (msg,channel,role))
            return replyMessage
        }

// This defines the type provider. When this will be compiled as a DLL file, we can add this type, as a reference
// to the type provider, in a project
[<TypeProvider>]
type ProviderTest(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    let ns = "typeProviderTest.Provided"
    let asm = Assembly.LoadFrom(config.RuntimeAssembly)

    // DEFINING THE AGENTROUTER AGENTSENDERSSS AGENTRECEIVER

    // A MODIFIERERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR = A ENGENDRER DANS LE TYPE PROVIDER

    // THE CODE FOR GENERATING TYPE PROViDER

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
        let t =  ProvidedTypeDefinition(fsmInstance.[0].LocalRole, baseType = Some typeof<RoleType> , IsErased = false)
        let ctor = ProvidedConstructor([ProvidedParameter("Voir",typeof<int>)], InvokeCode = fun args -> <@@ "le Role est unique" :> obj @@>) // add argument later
        t.AddMember(ctor)
        //t.SetBaseTypeDelayed(fun() -> RoleType) // FAIRE UN TRUC LA POUR INTERFACER .SetBaseTypeDelayed(fun() -> ILocalType)
        let myProp = ProvidedProperty("instance", t, IsStatic = true,
                                                GetterCode = (fun args -> Expr.NewObject(ctor,[]) ))
        t.AddMember(myProp)
        listeType <- t::listeType
        let mutable mapping = Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole,t)
        for event in fsmInstance do
            if not(alreadySeen liste event.Partner) then
                let t = ProvidedTypeDefinition(event.Partner,baseType = Some typeof<RoleType>)
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
        let myMethod = ProvidedMethod("next",[],nextType,InvokeCode = fun args -> expression) 
        let method2 =  ProvidedMethod("GetMessage",[],typeof<byte []>,InvokeCode = fun args -> <@@ name @@>)  
        t.AddMember(myMethod) 
        t.AddMember(method2)
        t.SetBaseTypeDelayed(fun() -> choiceType.DeclaringType.GetNestedType("LabelChoice"+ string event.CurrentState))
        t

    // Refactorer un max
    let makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) = 
        let mutable listeLabelSeen = []
        let listeType = []
        let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
        for event in fsmInstance do
            if (event.Type.Contains("choice") && not(alreadySeen listeLabelSeen event.Label)) then
                let choiceType = ProvidedTypeDefinition("LabelChoice"+ string event.CurrentState, Some typeof<ILocalType>,IsErased = false)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                choiceType.AddMember(ctor)
                let myMethod = ProvidedMethod("labelChoice" + string event.CurrentState ,[],typeof<unit>,InvokeCode = fun args -> <@@ () @@>) in
                choiceType.AddMember(myMethod)
                mapping <- mapping.Add("LabelChoice"+ string event.CurrentState,choiceType)
                choiceType::listeType |> ignore 
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
                                        t::listeType |> ignore     
                        |hd::tl -> if not(alreadySeen listeLabelSeen fsmInstance.[hd].Label) then
                                        let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                                        let state = fsmInstance.[hd]
                                        let t = makeChoiceType nextType state choiceType 
                                        mapping <- mapping.Add(state.Label,t)
                                        listeLabelSeen <- state.Label::listeLabelSeen
                                        t::listeType |> ignore
                                        aux tl 
                in aux listIndexChoice 
            else if not(alreadySeen listeLabelSeen event.Label) then
                let name = event.Label.Replace("(","").Replace(")","") 
                let t = ProvidedTypeDefinition(name,Some typeof<ILocalType>, IsErased = false)
                let myMethod =  ProvidedMethod("GetMessage",[],typeof<byte []>,InvokeCode = fun args -> <@@ name @@>) 
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "We'll see later" :> obj @@>) // add argument later
                t.AddMember(ctor)
                t.AddMember(myMethod)
                mapping <- mapping.Add(event.Label,t)
                listeLabelSeen <- event.Label::listeLabelSeen
                t::listeType |> ignore
        (mapping,listeType)

    let makeStateType (n:int) (s:string) = 
        let t = ProvidedTypeDefinition(s + string n,Some typeof<obj>,IsErased=false)
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "MakeStateType" :> obj @@>)
        t.AddMember(ctor)
        t
    let makeStateType (n:int) = makeStateType n "State"


    let rec goingThrough (methodName:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) (agentRouter:AgentRouter) =
         match indexList with
         |[] -> // Last state: no next state possible
                let myMethod = ProvidedMethod(methodName,[],typeof<unit>,InvokeCode = fun args -> <@@ printfn "finish" @@>) in
                aType.AddMember(myMethod)
                //printfn " There is a mistake, no index? should never happen, weird issue!!! "
         |[b] -> let nextType = findProvidedType providedList fsmInstance.[b].NextState
                 let labelType = mLabel.[fsmInstance.[b].Label]
                 let exprLabel = Expr.NewObject(labelType.GetConstructors().[0], [])
                 let c = nextType.GetConstructors().[0]
                 let exprState = Expr.NewObject(c, [])
                 let expression =
                    match methodName with
                        |"send" -> <@@ let router = agentRouter.SendMessage(%%exprLabel,fsmInstance.[b].Partner)     //agentMap.[fsmInstance.[hd].Partner]
                                       %%exprState  @@>
                        |"receive" -> <@@ let router = agentRouter.ReceiveMessage(%%exprLabel,fsmInstance.[b].Partner)     //agentMap.[fsmInstance.[hd].Partner]
                                          %%exprState  @@>
                        |_ -> <@@ printfn "Not correct" @@>
                 let myMethod = ProvidedMethod(methodName,[ProvidedParameter("Label",mLabel.[fsmInstance.[b].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[b].Partner])],
                                                                          nextType,InvokeCode = fun args -> expression) in
                 aType.AddMember(myMethod)
         |hd::tl -> let nextType = findProvidedType providedList fsmInstance.[hd].NextState
                    let labelType = mLabel.[fsmInstance.[hd].Label]
                    let exprLabel = Expr.NewObject(labelType.GetConstructors().[0], [])
                    let c = nextType.GetConstructors().[0]
                    let exprState = Expr.NewObject(c, [])
                    let expression =
                        match methodName with
                            |"send" -> <@@ let router = agentRouter.SendMessage(%%exprLabel,fsmInstance.[hd].Partner)     //agentMap.[fsmInstance.[hd].Partner]
                                           %%exprState  @@>
                            |"receive" -> <@@ let router = agentRouter.ReceiveMessage(%%exprLabel,fsmInstance.[hd].Partner)     //agentMap.[fsmInstance.[hd].Partner]
                                           %%exprState  @@>
                            |_ -> <@@ printfn "Not correct" @@>
                    let myMethod = ProvidedMethod(methodName,[ProvidedParameter("Label",mLabel.[fsmInstance.[hd].Label]);ProvidedParameter("Role",mRole.[fsmInstance.[hd].Partner])],
                                                                             nextType,InvokeCode = fun args -> expression) in
                    aType.AddMember(myMethod)    
                    goingThrough methodName providedList aType tl mLabel mRole fsmInstance agentRouter

    // REfactorer cela au max
    let rec addProperty (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) (agentRouter:AgentRouter)=
        let currentState = stateList.Head
        let indexOfState = findCurrentIndex currentState fsmInstance
        let indexList = findSameCurrent currentState fsmInstance 
        let mutable methodName = "finish"
        if indexOfState <> -1 then
            methodName <- fsmInstance.[indexOfState].Type
        match providedList with
            |[] -> ()
            |[aType] -> match methodName with
                            |"send" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                            |"receive" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                            |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                         let c = labelType.GetConstructors().[0]
                                         let expression = Expr.NewObject(c,[])
                                         let myMethod = ProvidedMethod("receive",[], labelType,InvokeCode = fun args -> expression )in
                                         aType.AddMember(myMethod) 
                            |"finish" -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance agentRouter
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                        GetterCode = fun args -> <@@ "essaye Bateau" @@>)
                        aType.AddMember(myProp)
            |hd::tl ->  match methodName with
                            |"send" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                            |"receive" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                            |"choice" -> let labelType = mLabel.["LabelChoice"+ string currentState]
                                         let c = labelType.GetConstructors().[0]
                                         let expression = Expr.NewObject(c,[]) 
                                         let myMethod = ProvidedMethod("receive",[], labelType,InvokeCode = fun args -> expression )in
                                         hd.AddMember(myMethod)
                            |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance agentRouter
                            | _ -> printfn "Not correct"
                        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true,
                                                       GetterCode = fun args -> <@@ "Test" @@>)
                        hd.AddMember(myProp)
                        addProperty providedListStatic tl (stateList.Tail) mLabel mRole fsmInstance agentRouter

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

// LA PARTIE A GERER ENCOOOOOOOOORE

    let createType (name:string) (parameters:obj[]) =

        let mutable agentMapSender =  Map.empty 
        let mutable agentReceiver = new AgentReceiver("127.0.0.1",2342)

        let modifyReceiver ipAddress port = 
            agentReceiver <- new AgentReceiver(ipAddress,port)

        let modifySenders (mapping:Map<string,string*int>) =
            for event in mapping do
                let tcpClient = new System.Net.Sockets.TcpClient(fst(event.Value),snd(event.Value))
                agentMapSender <- agentMapSender.Add(event.Key,new AgentSender(tcpClient))
        
        // USEFULL LATER NOT NOW
        let modifyLocaly port (n:int) =
            for i in 1..n do
                let tcpClient = new System.Net.Sockets.TcpClient("127.0.0.1",(port+i))
                agentMapSender <- agentMapSender.Add((port+i).ToString(),new AgentSender(tcpClient)) // A CHANGER
                
 // LA PARTIE A GERER ENCOOOOOOOOORE FINNNNN

        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let local = parameters.[1] :?> bool
        let senders = parameters.[2] :?> Map<string,string*int>
        let receiver = parameters.[3] :?> string*int



        let senders = if (senders.IsEmpty && local) then 
                        "ON CHANGE LE SENDERS EN FONCTION DE NOMBRE DE ROLE"
        let receiver = "pareil"
            
        (*
         if not(local) then 
            let senders = parameters.[2] :?> Option<Map<string,string*int>>
            let receiver = parameters.[3] :?> Option<string*int>
            match senders,receiver with
                |Some s, Some r -> modifyReceiver (fst(r)) (snd(r))
                                   modifySenders s
                |Some s, None -> modifySenders s
                |None, Some r -> modifyReceiver (fst(r)) (snd(r))
                |None, None -> ()
        *)
        
        let protocol = ScribbleProtocole.Parse(fsm)
        let triple= stateSet protocol
        let n,stateSet,firstState = triple
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let firstStateType = findProvidedType listTypes firstState
        let tupleLabel = makeLabelTypes protocol listTypes
        let tupleRole = makeRoleTypes protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)
        let numberOfRoles = list2.Length

        let agentRouter = new AgentRouter(agentMapSender,agentReceiver)
        addProperty listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol agentRouter
        let stuff = list2.ToString()
        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "hey" + string n @@> )
        let myMethod = ProvidedMethod("Start",[], firstStateType,InvokeCode = (fun args -> let c = firstStateType.GetConstructors().[0] 
                                                                                           agentRouter.Start()
                                                                                           Expr.NewObject(c, [])))  
        let t = ProvidedTypeDefinition(asm,ns,name,Some typeof<obj>)
        let memberList = listTypes |> List.append list2 |> List.append list1 
        t.AddMembers(memberList)
        t.AddMember(myMethod)
        t.AddMember(ctor)
        t


    // let asembly = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
    // let c = asembly.AddTypes

    let providedType = ProvidedTypeDefinition(asm,ns,"RealProvider",Some typeof<obj>)
    let parameters = [ProvidedStaticParameter("Protocol",typeof<string>);
                      ProvidedStaticParameter("Local",typeof<bool>);
                      ProvidedStaticParameter("Senders",typeof<Option<Map<string,string*int>>>,parameterDefaultValue = Map.empty);
                      ProvidedStaticParameter("Receiver",typeof<Option<string*int>>,parameterDefaultValue= ("127.0.0.1",5000) )]

    // ProvidedStaticParameter("SampleIsList", typeof<bool>, parameterDefaultValue = false) INSTEAD OF OPTIONSSSSSS 

    do
        providedType.DefineStaticParameters(parameters,createType)
        this.AddNamespace(ns, [providedType] )
[<assembly:TypeProviderAssembly>]
    do()