
#r "bin/Debug/typeProviderTest.dll" // Type Provider compiled as a DLL file
open typeProviderTest.Provided //namespace
open typeProviderTest

#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r "../packages/FSharp.Compiler.Service.3.0.0.0/lib/net40/FSharp.Compiler.Service.dll"

open FSharp.Data
open System.Reflection
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO


(*type provid = Provided.RealProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"World" , "label":"toz()" , "type":"send" , "nextState": 3} ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"e()", "type":"receive" , "nextState":2  } ] """ > *)

type providedType = Provided.RealProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"World" , "label":"Hello()" , "type":"send" , "nextState": 4} ,
    { "currentState":4 , "localRole":"Me", "partner":"World" , "label":"goodMorning()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":4 , "localRole":"Me", "partner":"World" , "label":"goodAfternoon()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":4 , "localRole":"Me", "partner":"World" , "label":"goodNight()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":3 , "localRole":"Me", "partner":"World" , "label":"goodNight()" , "type":"receive" , "nextState": 2 } ] """ >


let providedInstance = new providedType()
let s = providedType.
let c = providedInstance.instanciate().send(new providedType.Hello(),providedType.World.instance).receive()

type g1 = providedType.goodAfternoon
type g2 = providedType.goodMorning
type g3 = providedType.goodNight
match c with
    | :? g1 -> let g = new g1()
               g.next()    
    | :? g2 -> let g = new g2()
               g.next()    
    | :? int -> let g = new g3()
                g.next()

let inbox = MailboxProcessor.






//let b = v.instanciate().send(new e.b(), e.Her.instance)

(*type a1 = provid.a
type b1 = provid.b
type c1 = provid.c
let b = new b1()
let g = b.next().receive(new provid.e(),provid.Her.instance).receive()
let f (x:provid.LabelChoice5) =
    match x with
        | :? a1 -> "pasa"
        | :? b1 -> "hi"
        | :? c1 -> "something"
        |_ ->
*)
(*
type provid = Provided.RealProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"World" , "label":"toz()" , "type":"send" , "nextState": 5} ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"a()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"b()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"c()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"e()", "type":"receive" , "nextState":4  },
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"f()", "type":"choice" , "nextState":2  }, 
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"g()", "type":"choice" , "nextState":2  }  ] """ >

module Person =
    type People() = class end

    type Student() =
        inherit People() 

    type Teacher() =
        inherit People()

module toz = 
    let f (x:Person.People) =
        match x with
            | :? Person.Student -> "Student"
            | :? Person.*)
