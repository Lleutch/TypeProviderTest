
#r "obj/Debug/typeProviderTest.dll" // Type Provider compiled as a DLL file
open typeProviderTest.Provided //namespace
open typeProviderTest


(* Examples of what we can do *)

// Type defined thanks to the parameter we provide <  >
type test = Provided.ParameterProvider<"test">

// Simple static property
let property = test.MyProperty

// Use of the constructor to instanciate
let testInstance = test("last") 

testInstance.InstanceProperty // val it : string = "last"

testInstance.InstanceMethod("prefix","middle") // val it : string = "prefix : middle : last"

testInstance.InstanceMethodNum2(5,10) // val it : List<int> = [5; 10]

