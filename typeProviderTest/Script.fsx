
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

type provid = Provided.RealProvider<""" 
  [ { "currentState":1 , "localRole":"Me", "partner":"World" , "label":"toz()" , "type":"send" , "nextState": 5} ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"a()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"b()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":5 , "localRole":"Me", "partner":"World" , "label":"c()" , "type":"choice" , "nextState": 3 } ,
    { "currentState":3 , "localRole":"Me", "partner":"Her" , "label":"e()", "type":"receive" , "nextState":4  },
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"f()", "type":"choice" , "nextState":2  }, 
    { "currentState":4 , "localRole":"Her", "partner":"You" , "label":"g()", "type":"choice" , "nextState":2  }  ] """ >
let v = new provid()
let d = v.instanciate().send(new provid.toz(),provid.World.instance).receive()
//let b = v.instanciate().send(new e.b(), e.Her.instance)

type a1 = provid.a
type b1 = provid.b
type c1 = provid.c
let b = new b1()
let g = b.next().receive(new provid.e(),provid.Her.instance).receive()
let f (x:provid.LabelChoice5) =
    match x with
        | :? a1 -> "pasa"
        | :? b1 -> 
        | _ -> ""

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