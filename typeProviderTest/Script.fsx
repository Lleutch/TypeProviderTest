// En savoir plus sur F# sur le site http://fsharp.org. Consultez le projet 'Didacticiel F#'
// pour obtenir une aide supplémentaire sur la programmation F#.

#r "obj/Debug/typeProviderTest.dll" // Type Provider compiled as a DLL file
open typeProviderTest.Provided //namespace
open typeProviderTest

type test = Provided.MyParaProvider<"Ok">

test.MyProperty


let testons = test("see")

testons.InstanceProperty

testons.InstanceMethod("Before","Ensuite")

testons.InstanceMethodNum2(3,10)