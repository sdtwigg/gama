package gama
package frontend
package api

trait HWDataTypesAPI {
  // Core Data Types
  type Data = gama.frontend.Data
  
  // ELEMENT
  type Element = gama.frontend.Element
  type Digital = gama.frontend.Digital
  
  type UIntLike = gama.frontend.UIntLike

  type UInt = gama.frontend.UInt
  val UInt = gama.frontend.UInt
  
  type SInt = gama.frontend.SInt
  val SInt = gama.frontend.SInt
  
  type Bool = gama.frontend.Bool
  val Bool = gama.frontend.Bool

  // AGGREGATE
  type Aggregate = gama.frontend.Aggregate

  type Vec[D<:Data] = gama.frontend.Vec[D]
  val Vec = gama.frontend.Vec

  type HardwareTuple = gama.frontend.HardwareTuple
  type Bundle = gama.frontend.Bundle
  type BundleReflection = gama.frontend.BundleReflection
  type Anon = gama.frontend.Anon
  
  // MODULE
  type EnclosingModule = gama.frontend.EnclosingModule
  type Module[IOT<:Data] = gama.frontend.Module[IOT]
  val Module = gama.frontend.Module
  
  // Helper Traits
  type AnyModule = gama.frontend.Module[_<:Data]
  type AnyVec    = gama.frontend.Vec[_<:Data]
  
  // For 'self-documenting code' purposes
  type Bits = gama.frontend.UInt
  val Bits = gama.frontend.UInt

  // Select implicits
  type Muxable[D<:Data]      = gama.frontend.Muxable[D]
  type Storable[D<:Data]     = gama.frontend.Storable[D]
  type Vectorizable[D<:Data] = gama.frontend.Vectorizable[D]
  type ConnectTo[To<:Data, From<:Data] = gama.frontend.implementation.ConnectTo[To, From]
  type BiConnect[LT<:Data, RT<:Data]   = gama.frontend.implementation.BiConnect[LT, RT]


}
