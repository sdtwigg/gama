package gama

trait HWDataTypesAPI {
  // Core Data Types
  type Data = gama.Data
  
  // ELEMENT
  type Element = gama.Element
  type Digital = gama.Digital

  type UInt = gama.UInt
  val UInt = gama.UInt
  
  type SInt = gama.SInt
  val SInt = gama.SInt
  
  type Bool = gama.Bool
  val Bool = gama.Bool

  // AGGREGATE
  type Aggregate = gama.Aggregate

  type Vec[D<:Data] = gama.Vec[D]
  val Vec = gama.Vec

  type HardwareTuple = gama.HardwareTuple
  type Bundle = gama.Bundle
  type BundleReflection = gama.BundleReflection
  type Anon = gama.Anon
  
  // MODULE
  type Module[IOT<:Data] = gama.Module[IOT]
  val Module = gama.Module
  
  // Helper Traits
  type AnyModule = Module[_<:Data]
  type AnyVec    = Vec[_<:Data]
  
  // For 'self-documenting code' purposes
  type Bits = gama.UInt
  val Bits = gama.UInt

}
